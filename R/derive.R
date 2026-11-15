#' Derive values from reported values
#'
#' Adds derived records (data origin `"derived"`) computed from reported
#' records. Each derived record stores the ids of its input records, the
#' formula and the software that computed it, and is never mixed with
#' reported values: the inputs keep their own records.
#'
#' Derivations:
#' * `"percent"`: `100 * events / denominator` where a percentage was not
#'   reported.
#' * `"log_ratio"`: natural logarithm of a ratio estimate and its limits.
#' * `"se_log_ratio"`: standard error of the log ratio from a confidence
#'   interval, `(log(upper) - log(lower)) / (2 * qnorm(1 - (1 - level) / 2))`.
#'   Requires a stated confidence level, or `assume_level`.
#' * `"sd_from_se"`: `se * sqrt(n)`, with `n` from `n` or from a unique
#'   `arm_n` record for the same arm. Only applied to dispersions labelled
#'   SE.
#'
#' @param x An `sc_extraction`.
#' @param what Derivations to run.
#' @param n Optional named numeric vector of arm sizes (names are arm
#'   labels) for `"sd_from_se"`.
#' @param assume_level Confidence level (in percent) to assume when it is
#'   not stated; `NULL` (the default) skips such intervals.
#' @return The extraction with derived records appended.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_derive(sc_extract(doc, "clinical_trial"), c("se_log_ratio", "sd_from_se"))
#' d <- x$records[x$records$data_origin == "derived", ]
#' d[, c("outcome", "arm", "statistic", "value_num", "formula")]
#' @export
sc_derive <- function(x, what = c("percent", "log_ratio", "se_log_ratio", "sd_from_se"),
                      n = NULL, assume_level = NULL) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  what <- match.arg(what, several.ok = TRUE)
  r <- x$records
  live <- r[r$status == "reported" & r$data_origin != "derived" & !is.na(r$observation_id), , drop = FALSE]
  sw <- paste0("R ", R.version$major, ".", R.version$minor, " (stats); sciparseR ", pkg_version("sciparseR"))
  out <- list()
  add <- function(base, statistic, value, inputs, formula, notes = NA_character_) {
    rec <- base
    rec$statistic <- statistic
    rec$value_num <- value
    rec$value <- format(signif(value, 6), scientific = FALSE, trim = TRUE)
    rec$data_origin <- "derived"
    rec$status <- "reported"
    rec$inputs <- paste(inputs, collapse = ",")
    rec$formula <- formula
    rec$software <- sw
    rec$method <- paste0("derive:", formula_name(formula))
    rec$evidence_id <- NA_character_
    rec$validation_status <- NA_character_
    rec$notes <- notes
    out[[length(out) + 1L]] <<- rec
  }
  pick <- function(ro, stat) ro[ro$statistic == stat & !is.na(ro$value_num), , drop = FALSE][1, , drop = FALSE]
  for (o in unique(live$observation_id)) {
    ro <- live[live$observation_id == o, , drop = FALSE]
    base <- ro[1, , drop = FALSE]
    stats_here <- ro$statistic
    if ("percent" %in% what && all(c("events", "denominator") %in% stats_here) && !"percent" %in% stats_here) {
      e <- pick(ro, "events")
      d <- pick(ro, "denominator")
      if (nrow(e) && nrow(d) && d$value_num > 0) {
        add(base, "percent", 100 * e$value_num / d$value_num, c(e$record_id, d$record_id), "100 * events / denominator")
        out[[length(out)]]$unit <- "%"
      }
    }
    measure <- ro$effect_measure[!is.na(ro$effect_measure)][1]
    is_ratio <- !is.na(measure) && measure %in% ratio_measures
    if (is_ratio && "log_ratio" %in% what) {
      for (s in intersect(c("estimate", "ci_lower", "ci_upper"), stats_here)) {
        v <- pick(ro, s)
        if (nrow(v) && v$value_num > 0) add(base, paste0("log_", s), log(v$value_num), v$record_id, paste0("log(", s, ")"))
      }
    }
    if (is_ratio && "se_log_ratio" %in% what && all(c("ci_lower", "ci_upper") %in% stats_here)) {
      lo <- pick(ro, "ci_lower")
      hi <- pick(ro, "ci_upper")
      lvl <- ro$ci_level[!is.na(ro$ci_level)][1]
      note <- NA_character_
      if (is.na(lvl) && !is.null(assume_level)) {
        lvl <- assume_level
        note <- paste0("confidence level not stated; ", assume_level, "% assumed by the analyst")
      }
      if (!is.na(lvl) && lo$value_num > 0 && hi$value_num > lo$value_num) {
        z <- stats::qnorm(1 - (1 - lvl / 100) / 2)
        add(base, "se_log", (log(hi$value_num) - log(lo$value_num)) / (2 * z), c(lo$record_id, hi$record_id),
            sprintf("(log(ci_upper) - log(ci_lower)) / (2 * qnorm(%s))", format(1 - (1 - lvl / 100) / 2)), note)
      }
    }
    if ("sd_from_se" %in% what && "se" %in% stats_here) {
      se <- pick(ro, "se")
      arm <- base$arm
      nn <- NA_real_
      src <- NA_character_
      if (!is.null(n) && !is.na(arm) && arm %in% names(n)) {
        nn <- unname(n[[arm]])
        src <- "n supplied by the analyst"
      } else if (!is.na(arm)) {
        an <- r[r$field == "arm_n" & r$status == "reported" & tolower(r$arm %|NA|% "") == tolower(arm) & !is.na(r$value_num), , drop = FALSE]
        if (length(unique(an$value_num)) == 1L) {
          nn <- an$value_num[1]
          src <- paste0("n from record ", an$record_id[1])
        }
      }
      if (!is.na(nn) && nn > 0) {
        ins <- c(se$record_id, if (grepl("^n from record", src %|NA|% "")) sub("^n from record ", "", src))
        add(base, "sd", se$value_num * sqrt(nn), ins, "se * sqrt(n)", src)
        out[[length(out)]]$dispersion_type <- "sd"
      }
    }
  }
  if (!length(out)) return(x)
  new <- do.call(rbind, out)
  new$record_id <- sprintf("rec-%05d", nrow(r) + seq_len(nrow(new)))
  x$records <- rbind(r, new)
  x
}

formula_name <- function(f) {
  if (grepl("^100", f)) return("percent")
  if (grepl("^log\\(", f)) return("log_ratio")
  if (grepl("qnorm", f)) return("se_log_ratio")
  if (grepl("sqrt\\(n\\)", f)) return("sd_from_se")
  "formula"
}

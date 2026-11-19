#' Derive values from reported values
#'
#' Adds derived records (data origin `"derived"`) computed from reported
#' records. Each derived record stores the ids of its input records, the
#' formula and the software that computed it, and is never mixed with
#' reported values: the inputs keep their own records. Only records with
#' status `"reported"` are inputs, so values that [sc_validate()] marked
#' `"inconsistent"` are never used.
#'
#' Derivations:
#' * `"percent"`: `100 * events / denominator` where a percentage was not
#'   reported.
#' * `"log_ratio"`: natural logarithm of a ratio estimate and its limits.
#' * `"se_log_ratio"`: standard error of the log ratio from a confidence
#'   interval, `(log(upper) - log(lower)) / (2 * qnorm(1 - (1 - level) / 2))`.
#'   Requires a stated confidence level, or `assume_level`. Never derived
#'   from an interval marked credible or prediction.
#' * `"sd_from_se"`: `se * sqrt(n)`, only for dispersions labelled SE.
#'   `n` must be the number analyzed for that result. It is taken, in this
#'   order, from an `n` or denominator reported with the result, from `n`,
#'   or, only with `allow_randomized_n = TRUE`, from a unique `arm_n`
#'   record for the arm (the allocated arm size, which can differ from the
#'   number analyzed; the record notes say so). Otherwise no SD is derived.
#'
#' @param x An `sc_extraction`.
#' @param what Derivations to run.
#' @param n Optional named numeric vector of arm sizes (names are arm
#'   labels) for `"sd_from_se"`.
#' @param allow_randomized_n Use the allocated arm size when no analyzed
#'   `n` is reported or supplied. Off by default.
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
                      n = NULL, assume_level = NULL, allow_randomized_n = FALSE) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  if (!is.null(assume_level) && (!is.numeric(assume_level) || length(assume_level) != 1L ||
                                 !is.finite(assume_level) || assume_level <= 0 || assume_level >= 100)) {
    abort("`assume_level` must be a single number strictly between 0 and 100.")
  }
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
    rec$data_origin <- rep("derived", nrow(rec))
    rec$status <- rep("reported", nrow(rec))
    rec$inputs <- paste(inputs, collapse = ",")
    rec$formula <- formula
    rec$software <- sw
    rec$method <- paste0("derive:", formula_name(formula))
    rec$evidence_id <- rep(NA_character_, nrow(rec))
    rec$validation_status <- rep(NA_character_, nrow(rec))
    rec$notes <- notes
    out[[length(out) + 1L]] <<- rec
  }
  pick <- function(ro, stat) {
    v <- ro[ro$statistic %in% stat & !is.na(ro$value_num), , drop = FALSE]
    v[seq_len(min(1L, nrow(v))), , drop = FALSE]
  }
  for (o in unique(live$observation_id)) {
    ro <- live[live$observation_id == o, , drop = FALSE]
    base <- ro[1, , drop = FALSE]
    stats_here <- ro$statistic[!is.na(ro$value_num)]
    if ("percent" %in% what && all(c("events", "denominator") %in% stats_here) && !"percent" %in% stats_here) {
      e <- pick(ro, "events")
      d <- pick(ro, "denominator")
      if (nrow(e) && nrow(d) && isTRUE(d$value_num > 0)) {
        add(base, "percent", 100 * e$value_num / d$value_num, c(e$record_id, d$record_id), "100 * events / denominator")
        out[[length(out)]]$unit <- "%"
      }
    }
    measure <- ro$effect_measure[!is.na(ro$effect_measure)][1]
    is_ratio <- !is.na(measure) && measure %in% ratio_measures
    if (is_ratio && "log_ratio" %in% what) {
      for (s in intersect(c("estimate", "ci_lower", "ci_upper"), stats_here)) {
        v <- pick(ro, s)
        if (nrow(v) && isTRUE(v$value_num > 0)) add(base, paste0("log_", s), log(v$value_num), v$record_id, paste0("log(", s, ")"))
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
      # a credible or prediction interval is not a confidence interval: no
      # standard error is read from it
      itype <- c(lo$interval_type, hi$interval_type)
      if (any(itype %in% c("credible", "prediction"))) lvl <- NA_real_
      if (!is.na(lvl) && lvl > 0 && lvl < 100 && isTRUE(lo$value_num > 0 && hi$value_num > lo$value_num)) {
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
      n_ids <- character()
      own <- ro[ro$statistic %in% c("n", "denominator") & !is.na(ro$value_num), , drop = FALSE]
      if (nrow(own)) {
        # the number analyzed for this result, reported with it
        nn <- own$value_num[1]
        n_ids <- own$record_id[1]
        src <- sprintf("n = %s reported with this result (record %s)", format(nn), n_ids)
      } else if (!is.null(n) && !is.na(arm) && arm %in% names(n)) {
        nn <- unname(n[[arm]])
        src <- sprintf("n = %s supplied by the analyst", format(nn))
      } else if (allow_randomized_n && !is.na(arm)) {
        an <- r[r$field == "arm_n" & r$status == "reported" & tolower(r$arm %|NA|% "") == tolower(arm) &
                  !is.na(r$value_num), , drop = FALSE]
        if (length(unique(an$value_num)) == 1L) {
          nn <- an$value_num[1]
          n_ids <- an$record_id[1]
          src <- sprintf(paste("n = %s is the arm size from record %s (allocation), not a reported",
                               "number analyzed for this result; assumed by request"), format(nn), n_ids)
        }
      }
      if (!is.na(nn) && nn > 0) {
        add(base, "sd", se$value_num * sqrt(nn), c(se$record_id, n_ids), "se * sqrt(n)", src)
        out[[length(out)]]$dispersion_type <- "sd"
      }
    }
  }
  if (!length(out)) return(x)
  new <- do.call(rbind, out)
  # running sc_derive() again adds only what is not already derived
  have <- r[r$data_origin %in% "derived" & r$status != "superseded", , drop = FALSE]
  key <- function(d) paste(d$statistic, d$observation_id, d$inputs, d$formula, sep = "|")
  new <- new[!key(new) %in% key(have), , drop = FALSE]
  if (!nrow(new)) return(x)
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

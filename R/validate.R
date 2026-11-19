#' Validate extracted values
#'
#' Runs deterministic scientific checks over an `sc_extraction` and records
#' each finding in `x$validation`. Record level `validation_status` becomes
#' `"pass"`, `"not_checked"`, `"warning"`, `"fail"` or `"exception"`. A reported value that
#' fails a rule of severity error (and has no exception) gets status
#' `"inconsistent"`: it is kept, shown in the review queue and not used by
#' [sc_derive()]. Validating again restores `"reported"` when the failure is
#' gone.
#'
#' Rules:
#' * `count_nonnegative`: counts and events are not negative, denominators
#'   are positive.
#' * `events_le_denominator`: events do not exceed their denominator.
#' * `percent_consistent`: a reported percentage matches events over
#'   denominator within rounding. A count and percentage with no stated
#'   denominator (an n (%) cell) gets result `"not_checked"`; no
#'   denominator is assumed.
#' * `ci_ordered`: lower limit < upper limit and the estimate lies inside.
#' * `ratio_positive`: ratio measures and their limits are positive.
#' * `p_range`: p-values lie in \[0, 1\].
#' * `value_domain`: values are finite; SD and SE are not negative; a
#'   percentage reported with a count lies in \[0, 100\] (a percent change
#'   without a count may be signed or exceed 100); quartiles and range
#'   limits are ordered around the median. Derived values whose inputs fail
#'   an error rule become `"inconsistent"` too.
#' * `p_ci_agreement`: for ratio and difference measures with a 95% CI,
#'   whether the CI excludes the null agrees with p < 0.05.
#' * `estimate_bound`: comparative estimates name an outcome and both arms
#'   of the comparison.
#' * `dispersion_type`: dispersion values labelled neither SD nor SE.
#' * `analyzed_le_randomized`: analyzed numbers do not exceed randomized.
#' * `arm_sizes_sum`: per-arm allocation numbers add up to the randomized
#'   number.
#' * `timepoint_bound`: an observation without a time point when the same
#'   outcome is reported at defined time points.
#' * `cross_source_conflict`: the same quantity (outcome, arm, comparison,
#'   measure, statistic, adjustment) reported with different values in
#'   text and tables. Different arms, time points or analysis sets are
#'   never merged: they are compared only when one side leaves the context
#'   unbound, and such matches are reported as possible conflicts; so are
#'   matches whose time point, analysis set or arms are missing on both
#'   sides. Values that agree within the rounding of their printed digits
#'   (0.6 and 0.62) do not conflict.
#' * `vocabulary`, `type` and `multiplicity`: categorical values belong to
#'   the field's allowed values; numeric fields parse as numbers, integer
#'   fields hold whole numbers, logical fields true or false; a field that
#'   is not `multiple` has one value.
#'
#' @param x An `sc_extraction`.
#' @param exceptions Optional data frame with columns `rule`, `id` (a record
#'   or observation id) and `justification`. Matching findings are kept and
#'   reported with result `"exception"`.
#' @param tolerance Absolute tolerance for percentage checks, in percentage
#'   points, added to the rounding implied by the reported decimals.
#' @return The extraction with `validation` filled and record
#'   `validation_status` updated.
#' @examples
#' doc <- sc_parse(system.file("extdata", "contradictory.pdf", package = "sciparseR"))
#' x <- sc_validate(sc_extract(doc, "numerical_results"))
#' x$validation[x$validation$result != "pass", c("rule", "result", "message")]
#' @export
sc_validate <- function(x, exceptions = NULL, tolerance = 0.1) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  # every run starts from the reported values, not from an earlier verdict
  x$records$status[x$records$status == "inconsistent"] <- "reported"
  r <- x$records
  # derived values are checked through their inputs, not as reported values
  live <- r$status != "superseded" & r$data_origin != "derived"
  out <- list()
  add <- function(rule, severity, result, ids, obs, msg) {
    out[[length(out) + 1L]] <<- list(rule = rule, severity = severity, result = result,
                                     record_ids = paste(ids, collapse = ","),
                                     observation_id = obs %|NA|% NA_character_, message = msg)
  }
  val <- function(ro, stat) {
    v <- ro$value_num[ro$statistic %in% stat & !is.na(ro$value_num)]
    if (length(v)) v[1] else NA_real_
  }
  rid <- function(ro, stat) ro$record_id[ro$statistic %in% stat]
  obs_ids <- unique(r$observation_id[live & !is.na(r$observation_id)])
  for (o in obs_ids) {
    ro <- r[live & r$observation_id %in% o, , drop = FALSE]
    ev <- val(ro, "events")
    den <- val(ro, "denominator")
    pct <- val(ro, "percent")
    for (stat in c("events", "count")) {
      v <- val(ro, stat)
      if (!is.na(v)) {
        ok <- v >= 0 && abs(v - round(v)) < 1e-8
        add("count_nonnegative", "error", if (ok) "pass" else "fail", rid(ro, stat), o,
            if (ok) "count is a non-negative integer" else sprintf("count %s is negative or not an integer", v))
      }
    }
    if (!is.na(den)) {
      ok <- den > 0
      add("count_nonnegative", "error", if (ok) "pass" else "fail", rid(ro, "denominator"), o,
          if (ok) "denominator is positive" else "denominator is not positive")
    }
    if (!is.na(ev) && !is.na(den)) {
      ok <- ev <= den
      add("events_le_denominator", "error", if (ok) "pass" else "fail", rid(ro, c("events", "denominator")), o,
          if (ok) "events do not exceed the denominator" else sprintf("events (%s) exceed the denominator (%s)", ev, den))
      if (!is.na(pct) && den > 0) {
        txt <- ro$value[ro$statistic %in% "percent"][1]
        dec <- if (grepl("\\.", txt)) nchar(sub("^.*\\.", "", txt)) else 0L
        calc <- 100 * ev / den
        ok <- abs(calc - pct) <= 0.5 * 10^(-dec) + tolerance
        add("percent_consistent", "warning", if (ok) "pass" else "fail", rid(ro, c("events", "denominator", "percent")), o,
            sprintf("reported %s%%, computed %.2f%%", txt, calc))
      }
    }
    for (f in domain_findings(ro)) add(f[[1]], "error", f[[2]], f[[3]], o, f[[4]])
    # "12 (99)" in an n (%) column: the denominator is not stated with the
    # value, so the percentage is not checked; the arm size in a header is
    # an allocation number, not necessarily this outcome's denominator
    cnt <- val(ro, c("count", "events"))
    if (!is.na(pct) && !is.na(cnt) && is.na(den)) {
      a <- ro$arm[1]
      an <- r$value_num[live & r$field == "arm_n" & !is.na(r$value_num) & !is.na(a) & r$arm %in% a]
      hint <- if (length(unique(an)) == 1L) sprintf("; with the arm size %s it would be %.1f%%", an[1], 100 * cnt / an[1]) else ""
      add("percent_consistent", "info", "not_checked", rid(ro, c("count", "events", "percent")), o,
          paste0("denominator not stated with the value; percentage not checked", hint))
    }
    est <- val(ro, "estimate")
    lo <- val(ro, "ci_lower")
    hi <- val(ro, "ci_upper")
    measure <- ro$effect_measure[!is.na(ro$effect_measure)][1]
    if (!is.na(lo) && !is.na(hi)) {
      ok <- lo < hi && (is.na(est) || (est >= lo && est <= hi))
      add("ci_ordered", "error", if (ok) "pass" else "fail", rid(ro, c("estimate", "ci_lower", "ci_upper")), o,
          if (ok) "confidence limits ordered around the estimate" else
            sprintf("limits %s to %s are not ordered around estimate %s", lo, hi, est))
    }
    if (!is.na(measure) && measure %in% ratio_measures) {
      vals <- c(est, lo, hi)
      vals <- vals[!is.na(vals)]
      if (length(vals)) {
        ok <- all(vals > 0)
        add("ratio_positive", "error", if (ok) "pass" else "fail", rid(ro, c("estimate", "ci_lower", "ci_upper")), o,
            if (ok) "ratio estimate and limits are positive" else "a ratio estimate or limit is not positive")
      }
    }
    p <- val(ro, "p_value")
    if (!is.na(p)) {
      ok <- p >= 0 && p <= 1
      add("p_range", "error", if (ok) "pass" else "fail", rid(ro, "p_value"), o,
          if (ok) "p-value within [0, 1]" else sprintf("p-value %s outside [0, 1]", p))
      level <- ro$ci_level[!is.na(ro$ci_level)][1]
      op <- ro$p_operator[ro$statistic %in% "p_value"][1] %|NA|% "="
      # significance at 0.05 is known only for an exact p, for "p < x" with
      # x <= 0.05 and for "p > x" with x >= 0.05
      sig <- if (op %in% c("<", "\u2264")) {
        if (p <= 0.05) TRUE else NA
      } else if (op %in% c(">", "\u2265")) {
        if (p >= 0.05) FALSE else NA
      } else p < 0.05
      excludes <- NA
      if (ok && !is.na(sig) && !is.na(measure) && !is.na(lo) && !is.na(hi) && isTRUE(level == 95)) {
        null <- if (measure %in% ratio_measures) 1 else 0
        excludes <- lo > null || hi < null
        # a limit equal to the null at the printed precision is indeterminate
        if (lo == null || hi == null) excludes <- NA
      }
      if (!is.na(excludes)) {
        agree <- excludes == sig || abs(p - 0.05) < 0.005
        add("p_ci_agreement", "warning", if (agree) "pass" else "fail", rid(ro, c("ci_lower", "ci_upper", "p_value")), o,
            if (agree) "CI and p-value agree on significance at 0.05" else
              sprintf("CI %s to %s %s the null but p %s %s", lo, hi, if (excludes) "excludes" else "includes", op, p))
      }
    }
    if (!is.na(est) && any(ro$field == "result")) {
      ok <- !is.na(ro$outcome[1]) && !is.na(ro$arm[1]) && !is.na(ro$comparator[1])
      miss <- c(outcome = is.na(ro$outcome[1]), arm = is.na(ro$arm[1]), comparator = is.na(ro$comparator[1]))
      add("estimate_bound", "error", if (ok) "pass" else "fail", rid(ro, "estimate"), o,
          if (ok) "estimate bound to an outcome and a comparison" else
            paste("estimate not bound to:", paste(names(miss)[miss], collapse = ", ")))
    }
    if (any(ro$statistic %in% "dispersion")) {
      add("dispersion_type", "warning", "fail", rid(ro, "dispersion"), o,
          "dispersion is not labelled as SD or SE; it is not assumed to be either")
    }
  }
  # study level counts
  rn <- r[live & r$field == "randomized_n" & !is.na(r$value_num), , drop = FALSE]
  an <- r[live & r$field == "analyzed_n" & !is.na(r$value_num), , drop = FALSE]
  if (nrow(rn) && nrow(an)) {
    ok <- all(an$value_num <= max(rn$value_num))
    add("analyzed_le_randomized", "error", if (ok) "pass" else "fail", c(rn$record_id, an$record_id), NA,
        sprintf("analyzed %s; randomized %s", paste(an$value_num, collapse = "/"), paste(unique(rn$value_num), collapse = "/")))
  }
  if (nrow(rn) && length(unique(rn$value_num)) > 1L) {
    add("cross_source_conflict", "error", "fail", rn$record_id, NA,
        sprintf("randomized number reported as %s", paste(unique(rn$value_num), collapse = " and ")))
  }
  arm_n <- r[live & r$field == "arm_n" & !is.na(r$value_num) & grepl("allocation", r$method %|NA|% ""), , drop = FALSE]
  arm_n <- r[live & r$field == "arm_n" & !is.na(r$value_num) & r$method == "rules:arm_n", , drop = FALSE]
  if (nrow(rn) && nrow(arm_n) >= 2L) {
    tot <- sum(arm_n$value_num[!duplicated(arm_n$arm)])
    ok <- tot == max(rn$value_num)
    add("arm_sizes_sum", "warning", if (ok) "pass" else "fail", c(rn$record_id, arm_n$record_id), NA,
        sprintf("arm sizes sum to %s; randomized %s", tot, max(rn$value_num)))
  }
  # time points never merged silently
  res <- r[live & r$field == "result" & !is.na(r$outcome) & !is.na(r$value_num), , drop = FALSE]
  if (nrow(res)) {
    key <- outcome_key(res$outcome)
    for (k in unique(key)) {
      sub <- res[key == k, , drop = FALSE]
      tps <- unique(sub$timepoint[!is.na(sub$timepoint)])
      un <- sub[is.na(sub$timepoint), , drop = FALSE]
      if (length(tps) >= 2L && nrow(un)) {
        add("timepoint_bound", "warning", "fail", un$record_id, NA,
            sprintf("outcome '%s' is reported at %s; these values have no time point", k, paste(tps, collapse = ", ")))
      }
    }
  }
  # conflicts between sources
  for (f in conflict_findings(r[live, , drop = FALSE])) do.call(add, f)
  # vocabulary and type checks for categorical/numeric fields
  for (fd in x$schema$fields) {
    sub <- r[live & r$field == fd$name & r$status == "reported", , drop = FALSE]
    if (!nrow(sub)) next
    if (fd$type %in% c("numeric", "integer")) {
      bad <- is.na(sub$value_num)
      if (any(bad)) add("type", "error", "fail", sub$record_id[bad], NA, paste0("non-numeric value in numeric field ", fd$name))
    }
    if (fd$type == "integer") {
      bad <- !is.na(sub$value_num) & abs(sub$value_num - round(sub$value_num)) > 1e-8
      if (any(bad)) add("type", "error", "fail", sub$record_id[bad], NA, paste0("non-integer value in integer field ", fd$name))
    }
    if (fd$type == "logical") {
      bad <- !tolower(sub$value) %in% c("true", "false", "yes", "no")
      if (any(bad)) add("type", "error", "fail", sub$record_id[bad], NA, paste0("non-logical value in logical field ", fd$name))
    }
    if (fd$type == "categorical" && length(fd$values)) {
      bad <- !sub$value %in% fd$values
      if (any(bad)) add("vocabulary", "error", "fail", sub$record_id[bad], NA,
                        paste0("value outside the allowed values of ", fd$name, ": ", paste(unique(sub$value[bad]), collapse = ", ")))
    }
    if (!isTRUE(fd$multiple) && length(unique(sub$value)) > 1L) {
      add("multiplicity", "warning", "fail", sub$record_id, NA,
          paste0("single-valued field ", fd$name, " has ", length(unique(sub$value)), " different values"))
    }
  }
  v <- bind_entity(lapply(out, as.data.frame, stringsAsFactors = FALSE), "validation")
  v <- apply_exceptions(v, exceptions)
  x$validation <- v
  x$records$validation_status <- record_status(x$records, v)
  # a reported value that fails a hard rule (an error, not excepted) is
  # inconsistent: it stays in the records but is not used by sc_derive();
  # validating again after a correction can make it reported again
  hard <- v$record_ids[v$severity == "error" & v$result == "fail"]
  bad <- unique(unlist(strsplit(hard, ",", fixed = TRUE)))
  mark <- x$records$record_id %in% bad & x$records$status == "reported"
  x$records$status[mark] <- "inconsistent"
  # a derived value whose input is inconsistent is inconsistent too, and so
  # on down the chain of derivations
  repeat {
    rr <- x$records
    gone <- rr$record_id[rr$status == "inconsistent"]
    dep <- rr$data_origin %in% "derived" & rr$status == "reported" & !is.na(rr$inputs) &
      vapply(strsplit(as.character(ifelse(is.na(rr$inputs), "", rr$inputs)), "[,; ]+"), function(z) any(z %in% gone), logical(1))
    if (!any(dep)) break
    x$records$status[dep] <- "inconsistent"
    x$records$notes[dep] <- "an input of this derived value failed validation"
  }
  x
}

# Domain of each statistic: finite numbers; SD, SE and dispersion not
# negative; a percentage of a count within 0 to 100 (a percent change with
# no count may be signed or above 100); quartiles ordered around the
# median and range limits ordered.
domain_findings <- function(ro) {
  out <- list()
  nonfinite <- is.nan(ro$value_num) | is.infinite(ro$value_num)
  if (any(nonfinite)) out[[length(out) + 1L]] <- list("value_domain", "fail", ro$record_id[nonfinite], "value is not a finite number")
  disp <- ro$statistic %in% c("sd", "se", "dispersion") & !is.na(ro$value_num)
  if (any(disp)) {
    ok <- ro$value_num[disp] >= 0
    out[[length(out) + 1L]] <- list("value_domain", if (all(ok)) "pass" else "fail", ro$record_id[disp],
                                    if (all(ok)) "SD and SE are not negative" else "an SD or SE is negative")
  }
  pct <- ro$statistic %in% "percent" & !is.na(ro$value_num)
  if (any(pct) && any(ro$statistic %in% c("events", "count", "denominator"))) {
    ok <- ro$value_num[pct] >= 0 & ro$value_num[pct] <= 100
    out[[length(out) + 1L]] <- list("value_domain", if (all(ok)) "pass" else "fail", ro$record_id[pct],
                                    if (all(ok)) "percentage of a count within 0 to 100" else
                                      sprintf("percentage %s of a count is outside 0 to 100", paste(ro$value[pct][!ok], collapse = ", ")))
  }
  g <- function(s) {
    z <- ro$value_num[ro$statistic %in% s & !is.na(ro$value_num)]
    if (length(z)) z[1] else NA_real_
  }
  ord <- list(c("q1", "median", "q3"), c("min", "median", "max"))
  for (o in ord) {
    z <- vapply(o, g, numeric(1))
    if (sum(!is.na(z)) < 2L) next
    ok <- !is.unsorted(z[!is.na(z)])
    out[[length(out) + 1L]] <- list("value_domain", if (ok) "pass" else "fail", ro$record_id[ro$statistic %in% o],
                                    if (ok) paste(o, collapse = " <= ") else sprintf("%s are not ordered", paste(o, collapse = ", ")))
  }
  out
}

outcome_key <- function(x) {
  x <- tolower(x)
  x <- gsub("\\b(the|a|an)\\b", " ", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

conflict_findings <- function(r) {
  res <- r[r$field == "result" & !is.na(r$value_num) & r$status == "reported" & !is.na(r$outcome), , drop = FALSE]
  if (!nrow(res)) return(list())
  src <- ifelse(grepl("table", res$method), "table", "text")
  ctx <- paste(outcome_key(res$outcome), res$effect_measure %|NA|% "", res$statistic, sep = "|")
  out <- list()
  done <- character()
  # only pairs from different sources with the same context can conflict:
  # look those up by context instead of scanning all pairs
  if (length(unique(src)) < 2L) return(out)
  by_ctx <- split(seq_len(nrow(res)), ctx)
  for (i in seq_len(nrow(res))) {
    cand <- by_ctx[[ctx[i]]]
    for (j in cand[cand > i & src[cand] != src[i]]) {
      same_adj <- identical(res$adjusted[i], res$adjusted[j]) || is.na(res$adjusted[i]) || is.na(res$adjusted[j])
      if (!same_adj) next
      tp_eq <- identical(res$timepoint[i], res$timepoint[j])
      as_eq <- identical(res$analysis_set[i], res$analysis_set[j])
      tp_wild <- is.na(res$timepoint[i]) || is.na(res$timepoint[j])
      as_wild <- is.na(res$analysis_set[i]) || is.na(res$analysis_set[j])
      po_eq <- identical(res$population[i], res$population[j])
      po_wild <- is.na(res$population[i]) || is.na(res$population[j])
      if (!(tp_eq || tp_wild) || !(as_eq || as_wild) || !(po_eq || po_wild)) next
      # arms: a text estimate often names no arms ("the hazard ratio for
      # death was 0.57"); unknown arms match, but only as a possible conflict
      arm_eq <- identical(res$arm[i], res$arm[j]) && identical(res$comparator[i], res$comparator[j])
      arm_wild <- (is.na(res$arm[i]) && is.na(res$comparator[i])) || (is.na(res$arm[j]) && is.na(res$comparator[j]))
      if (!(arm_eq || arm_wild)) next
      # a definite conflict needs the same known time point, analysis set
      # and arms, and the same adjustment; context missing on either side,
      # or on both, leaves it possible
      known <- function(z) !is.na(z[i]) && !is.na(z[j]) && z[i] == z[j]
      exact <- known(res$timepoint) && known(res$analysis_set) && known(res$arm) && known(res$comparator) &&
        po_eq && identical(res$adjusted[i], res$adjusted[j])
      key <- paste(sort(c(res$observation_id[i], res$observation_id[j])), collapse = "~")
      if (key %in% done) next
      differ <- abs(res$value_num[i] - res$value_num[j]) > 1e-9
      # 0.6 and 0.62 can be one value printed at two precisions
      if (differ && abs(res$value_num[i] - res$value_num[j]) <= half_unit(res$value[i]) + half_unit(res$value[j]) + 1e-12) {
        out[[length(out) + 1L]] <- list("cross_source_conflict", "info", "pass", c(res$record_id[i], res$record_id[j]),
                                        res$observation_id[i], sprintf("%s %s and %s agree within rounding (%s)", res$statistic[i],
                                                                       res$value[i], res$value[j], res$outcome[i]))
        next
      }
      if (!differ) {
        out[[length(out) + 1L]] <- list("cross_source_conflict", "info", "pass", c(res$record_id[i], res$record_id[j]),
                                        res$observation_id[i], sprintf("%s %s agrees between text and table (%s)", res$statistic[i], res$value[i], res$outcome[i]))
        next
      }
      done <- c(done, key)
      out[[length(out) + 1L]] <- list(
        "cross_source_conflict", "error", if (exact) "fail" else "possible_conflict",
        c(res$record_id[i], res$record_id[j]), res$observation_id[i],
        sprintf("%s for '%s' is %s in the %s and %s in the %s%s", res$statistic[i], res$outcome[i],
                res$value[i], src[i], res$value[j], src[j],
                if (exact) "" else " (arms, time point, analysis set, population or adjustment unbound on one side)"))
    }
  }
  out
}

# Half the last printed digit: 0.05 for "0.6", 0.005 for "0.62".
half_unit <- function(txt) {
  t <- sub("[eE].*$", "", gsub("[^0-9.\u00b7]", "", txt %|NA|% ""))
  dec <- if (grepl("[.\u00b7]", t)) nchar(sub("^.*[.\u00b7]", "", t)) else 0L
  0.5 * 10^(-dec)
}

apply_exceptions <- function(v, exceptions) {
  v$justification <- rep(NA_character_, nrow(v))
  if (is.null(exceptions) || !nrow(v)) return(v)
  if (!all(c("rule", "id", "justification") %in% names(exceptions))) {
    abort("`exceptions` needs columns rule, id and justification.")
  }
  for (k in seq_len(nrow(exceptions))) {
    e <- exceptions[k, ]
    if (is.na(e$justification) || !nzchar(e$justification)) abort("Every exception needs a justification.")
    hit <- v$rule == e$rule & v$result %in% c("fail", "possible_conflict") &
      (v$observation_id %in% e$id | vapply(strsplit(v$record_ids, ","), function(z) e$id %in% z, logical(1)))
    v$result[hit] <- "exception"
    v$justification[hit] <- e$justification
  }
  v
}

record_status <- function(records, v) {
  st <- rep("pass", nrow(records))
  st[!records$record_id %in% unlist(strsplit(v$record_ids, ","))] <- NA_character_
  rank <- c(pass = 0, not_checked = 0.5, exception = 1, warning = 2, possible_conflict = 2, fail = 3)
  for (i in seq_len(nrow(v))) {
    ids <- strsplit(v$record_ids[i], ",")[[1]]
    res <- if (v$result[i] == "fail" && v$severity[i] == "warning") "warning" else v$result[i]
    if (res == "possible_conflict") res <- "warning"
    ix <- match(ids, records$record_id)
    ix <- ix[!is.na(ix)]
    for (j in ix) if (is.na(st[j]) || rank[[res]] > rank[[st[j]]]) st[j] <- res
  }
  st
}

#' Review queue
#'
#' Lists the records that need human attention, most important first:
#' validation failures, conflicts, ambiguous or unsupported values,
#' missing required fields, estimates without a bound context, and values
#' whose `source_quality` flags a weak source (`"ocr"`,
#' `"ocr_low_confidence"`, `"table_possible"`).
#'
#' @param x An `sc_extraction`, validated or not (validation is run if
#'   needed).
#' @return A data frame with one row per record to review, its reason and
#'   the location of its evidence.
#' @examples
#' doc <- sc_parse(system.file("extdata", "contradictory.pdf", package = "sciparseR"),
#'                config = sc_config(render = FALSE))
#' q <- sc_review_queue(sc_extract(doc, "numerical_results"))
#' q[, c("priority", "field", "statistic", "value", "reason")]
#' @export
sc_review_queue <- function(x) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  if (!nrow(x$validation)) x <- sc_validate(x)
  r <- x$records[x$records$status != "superseded", , drop = FALSE]
  v <- x$validation[x$validation$result %in% c("fail", "possible_conflict"), , drop = FALSE]
  reasons <- list()
  for (i in seq_len(nrow(v))) {
    for (id in strsplit(v$record_ids[i], ",")[[1]]) {
      reasons[[length(reasons) + 1L]] <- data.frame(record_id = id, priority = if (v$severity[i] == "error") 1L else 2L,
                                                    reason = paste0(v$rule[i], ": ", v$message[i]), stringsAsFactors = FALSE)
    }
  }
  st <- r[r$status %in% c("ambiguous", "unsupported", "not_reported", "not_assessed", "missing", "inconsistent"), , drop = FALSE]
  for (i in seq_len(nrow(st))) {
    reasons[[length(reasons) + 1L]] <- data.frame(record_id = st$record_id[i],
                                                  priority = if (st$status[i] %in% c("unsupported", "inconsistent", "not_assessed")) 1L else 3L,
                                                  reason = paste0("status ", st$status[i], if (!is.na(st$notes[i])) paste0(": ", st$notes[i]) else ""),
                                                  stringsAsFactors = FALSE)
  }
  # values read from a weak source: OCR, low OCR confidence, or a table
  # that is only possible
  sq <- r[!is.na(r$source_quality) & r$status %in% c("reported", "ambiguous"), , drop = FALSE]
  for (i in seq_len(nrow(sq))) {
    reasons[[length(reasons) + 1L]] <- data.frame(record_id = sq$record_id[i],
                                                  priority = if (grepl("low_confidence|table_possible", sq$source_quality[i])) 2L else 3L,
                                                  reason = paste0("source quality: ", sq$source_quality[i]), stringsAsFactors = FALSE)
  }
  if (!length(reasons)) {
    return(data.frame(priority = integer(), record_id = character(), field = character(), statistic = character(),
                      value = character(), outcome = character(), arm = character(), reason = character(),
                      page = integer(), text = character()))
  }
  q <- do.call(rbind, reasons)
  q <- q[order(q$priority, q$record_id), , drop = FALSE]
  q <- stats::aggregate(reason ~ record_id + priority, q, function(z) paste(unique(z), collapse = " | "))
  q <- q[order(q$priority, q$record_id), , drop = FALSE]
  q <- q[!duplicated(q$record_id), , drop = FALSE]
  rr <- r[match(q$record_id, r$record_id), ]
  ev <- x$evidence[match(rr$evidence_id, x$evidence$evidence_id), ]
  out <- data.frame(priority = q$priority, record_id = q$record_id, field = rr$field, statistic = rr$statistic,
                    value = rr$value, outcome = rr$outcome, arm = rr$arm, reason = q$reason,
                    page = ev$page, text = ev$text, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' Record a human correction
#'
#' Adds a corrected record with data origin `"human_corrected"` that
#' supersedes the original. The original record and its evidence are kept
#' (status `"superseded"`), so the correction history is auditable.
#'
#' @param x An `sc_extraction`.
#' @param record_id Record to correct.
#' @param value Corrected value (character; numeric values are parsed).
#' @param reviewer Reviewer name or identifier.
#' @param reason Why the value was corrected.
#' @param ... Other record fields to change, such as `arm`, `outcome`,
#'   `timepoint`, `statistic` or `dispersion_type`.
#' @return The updated extraction (validation is cleared and should be
#'   rerun).
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "numerical_results")
#' id <- x$records$record_id[x$records$statistic %in% "dispersion"][1]
#' x <- sc_correct(x, id, value = "13.2", reviewer = "reviewer 1",
#'                 reason = "methods state values are mean and SD",
#'                 statistic = "sd", dispersion_type = "sd")
#' tail(x$records[, c("record_id", "statistic", "status", "data_origin", "supersedes")], 2)
#' @export
sc_correct <- function(x, record_id, value, reviewer, reason, ...) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  check_string(record_id, "record_id")
  check_string(reviewer, "reviewer")
  check_string(reason, "reason")
  if (!nzchar(trimws(reviewer)) || !nzchar(trimws(reason))) abort("`reviewer` and `reason` must not be empty.")
  i <- match(record_id, x$records$record_id)
  if (is.na(i)) abort("Unknown record: ", record_id)
  if (x$records$status[i] == "superseded") abort("Record ", record_id, " is already superseded.")
  new <- x$records[i, , drop = FALSE]
  changes <- list(...)
  bad <- setdiff(names(changes), names(new))
  if (length(bad)) abort("Unknown record fields: ", paste(bad, collapse = ", "))
  for (nm in names(changes)) new[[nm]] <- changes[[nm]]
  new$value <- as.character(value)
  new$value_num <- as_num(value)
  new$status <- rep("reported", nrow(new))
  new$data_origin <- rep("human_corrected", nrow(new))
  new$supersedes <- record_id
  new$reviewer <- reviewer
  new$notes <- reason
  new$corrected_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  new$method <- rep("human_review", nrow(new))
  new$validation_status <- rep(NA_character_, nrow(new))
  new$record_id <- sprintf("rec-%05d", nrow(x$records) + 1L)
  x$records$status[i] <- "superseded"
  # values derived from the corrected record are stale: supersede them too,
  # and run sc_derive() again to rebuild them from the correction
  stale <- x$records$data_origin %in% "derived" & x$records$status != "superseded" &
    vapply(strsplit(x$records$inputs %|NA|% "", ",", fixed = TRUE), function(z) record_id %in% z, logical(1))
  x$records$status[stale] <- "superseded"
  x$records$notes[stale] <- paste0("input ", record_id, " was corrected")
  x$records <- rbind(x$records, new)
  x$validation <- empty_entity("validation")
  x
}

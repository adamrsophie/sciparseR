# Numerical results from text and tables. Each number is a separate record
# with its own evidence; numbers that belong together (an estimate with its
# confidence limits and p-value, a count with its denominator) share an
# observation id. Context (outcome, arm, comparator, time point, analysis
# set, adjustment) is bound only from explicit text or table structure and
# the source of each binding is recorded in the `binding` column.

measure_full <- c(
  standardized_mean_difference = "standardi[sz]ed mean differences?",
  incidence_rate_ratio = "incidence rate ratios?",
  hazard_ratio = "hazard ratios?",
  odds_ratio = "odds ratios?",
  risk_ratio = "risk ratios?|relative risks?",
  rate_ratio = "rate ratios?",
  mean_difference = "mean differences?|differences? in means",
  risk_difference = "risk differences?|absolute risk differences?"
)
measure_abbr <- c(
  hazard_ratio = "a?HRs?", odds_ratio = "a?ORs?", risk_ratio = "a?RRs?",
  incidence_rate_ratio = "IRRs?", standardized_mean_difference = "SMDs?",
  mean_difference = "MDs?", risk_difference = "RDs?"
)
ratio_measures <- c("hazard_ratio", "odds_ratio", "risk_ratio", "rate_ratio", "incidence_rate_ratio")

find_measures <- function(text) {
  out <- list()
  for (nm in names(measure_full)) {
    m <- gregexpr(paste0("(?i)\\b(?:(adjusted|unadjusted|crude) )?(?:", measure_full[[nm]], ")\\b"), text, perl = TRUE)[[1]]
    if (m[1] > 0) for (j in seq_along(m)) out[[length(out) + 1L]] <- c(nm, m[j], attr(m, "match.length")[j])
  }
  for (nm in names(measure_abbr)) {
    m <- gregexpr(paste0("\\b(?:", measure_abbr[[nm]], ")\\b"), text, perl = TRUE)[[1]]
    if (m[1] > 0) for (j in seq_along(m)) out[[length(out) + 1L]] <- c(nm, m[j], attr(m, "match.length")[j])
  }
  if (!length(out)) return(data.frame(measure = character(), start = integer(), length = integer()))
  d <- data.frame(measure = vapply(out, `[`, "", 1), start = as.integer(vapply(out, `[`, "", 2)),
                  length = as.integer(vapply(out, `[`, "", 3)), stringsAsFactors = FALSE)
  d <- d[order(d$start, -d$length), , drop = FALSE]
  keep <- rep(TRUE, nrow(d))
  for (i in seq_len(nrow(d))[-1]) {
    prev <- which(keep[seq_len(i - 1L)])
    if (length(prev) && any(d$start[i] < d$start[prev] + d$length[prev])) keep[i] <- FALSE
  }
  d[keep, , drop = FALSE]
}

adjusted_status <- function(prefix_text, token) {
  if (grepl("(?i)\\bunadjusted\\b|\\bcrude\\b", prefix_text, perl = TRUE)) return(FALSE)
  if (grepl("(?i)\\badjusted\\b", prefix_text, perl = TRUE) || grepl("^a[A-Z]", token)) return(TRUE)
  NA
}

subject_verbs <- "\\b(occurred|was|were|decreased|increased|improved|reduced|declined|changed|differed|did not differ|fell|rose|developed|died)\\b"

sentence_subject <- function(st) {
  s <- sub("^(?i)(in|among|for) the [^,]{1,60}(population|analysis|set|group|cohort),\\s*", "", st, perl = TRUE)
  m <- regexpr(subject_verbs, s, perl = TRUE, ignore.case = TRUE)
  if (m < 2) return(NA_character_)
  subj <- trimws(substr(s, 1, m - 1L))
  subj <- sub("(?i)^(the|a|an)\\s+", "", subj, perl = TRUE)
  if (grepl(paste0("(?i)", paste(c(measure_full, "\\bratio\\b|\\bdifference\\b|\\bincidence\\b"), collapse = "|")), subj, perl = TRUE)) return(NA_character_)
  if (nchar(subj) > 80 || !nzchar(subj) || grepl("^[0-9]", subj)) return(NA_character_)
  tolower(subj)
}

arm_aliases <- function(arms) {
  arms <- arms[!is.na(arms) & nzchar(arms)]
  arms <- arms[!duplicated(tolower(arms))]
  if (!length(arms)) return(character())
  stats::setNames(arms, tolower(arms))
}

match_arm <- function(phrase, arms) {
  if (is.na(phrase) || !length(arms)) return(NA_character_)
  p <- tolower(trimws(sub("\\s*\\((n|N)\\s*=\\s*[0-9,]+\\)", "", phrase)))
  p <- sub("\\s+(group|arm|cohort)$", "", p)
  hit <- names(arms)[names(arms) == p]
  if (!length(hit)) hit <- names(arms)[vapply(names(arms), function(a) grepl(paste0("\\b", gsub("([.()+*?])", "\\\\\\1", a), "\\b"), p), logical(1))]
  if (length(hit) == 1L) unname(arms[hit]) else NA_character_
}

arms_in_text <- function(text, arms) {
  if (!length(arms)) return(character())
  pos <- vapply(names(arms), function(a) {
    m <- regexpr(paste0("(?i)\\b", gsub("([.()+*?])", "\\\\\\1", a), "\\b"), text, perl = TRUE)
    as.integer(m)
  }, integer(1))
  found <- pos > 0
  unname(arms[names(arms)[found][order(pos[found])]])
}

rule_text_statistics <- function(col, units, arms) {
  arms <- arm_aliases(arms)
  u <- units[units$type %in% c("paragraph", "footnote") & units$section_label %in% c("abstract", "results", NA), , drop = FALSE]
  ci_re <- paste0("^[^0-9]{0,40}?(?:([0-9]{2}(?:\\.[0-9]+)?)%\\s*(?:CI|confidence interval|CrI|credible interval)[,:]?\\s*)?\\(?\\s*(",
                  num_re, ")\\s*(?:to|\u2013|\u2014|-|,)\\s*(", num_re, ")")
  p_re <- paste0("\\b[Pp]\\s*(=|<|>|\u2264|\u2265)\\s*(", num_re, ")")
  for (i in seq_len(nrow(u))) {
    txt <- u$text[i]
    bid <- u$block_id[i]
    sents <- split_sentences(txt)
    for (s in seq_len(nrow(sents))) {
      st <- sents$text[s]
      off <- sents$start[s] - 1L
      ev_at <- function(a, len) add_evidence(col, block_id = bid, char_start = off + a,
                                             char_end = off + a + len - 1L,
                                             value_text = substr(st, a, a + len - 1L))
      outcome <- sentence_subject(st)
      out_bind <- if (is.na(outcome)) "outcome=unbound" else "outcome=sentence_subject"
      tp <- find_timepoint(st)
      aset <- find_analysis_set(st)
      ctx_bind <- c(out_bind, if (!is.na(tp)) "timepoint=sentence", if (!is.na(aset)) "analysis_set=sentence")
      sent_arms <- arms_in_text(st, arms)
      # effect measures
      ms <- find_measures(st)
      claimed <- integer()
      for (k in seq_len(nrow(ms))) {
        mstart <- ms$start[k]
        mend <- mstart + ms$length[k]
        nxt <- if (k < nrow(ms)) ms$start[k + 1L] - 1L else nchar(st)
        win <- substr(st, mend, nxt)
        em <- regexpr(paste0("^[^0-9.;]{0,30}?(?:was |of |=|:|,|\\()\\s*(", num_re, ")"), win, perl = TRUE)
        if (em < 0) em <- regexpr(paste0("^\\s*(", num_re, ")"), win, perl = TRUE)
        if (em < 0) next
        ecs <- attr(em, "capture.start")[1]
        ecl <- attr(em, "capture.length")[1]
        est_txt <- substr(win, ecs, ecs + ecl - 1L)
        est_pos <- mend + ecs - 1L
        prefix <- substr(st, max(1L, mstart - 25L), mstart + ms$length[k] - 1L)
        token <- substr(st, mstart, mend - 1L)
        adj <- adjusted_status(prefix, token)
        m_out <- outcome
        m_bind <- out_bind
        fo <- regmatches(win, regexec("^\\s+for (?:the )?([A-Za-z][A-Za-z -]{1,60}?) (?:was|were|of|is)\\b", win, perl = TRUE))[[1]]
        if (length(fo)) {
          m_out <- tolower(fo[2])
          m_bind <- "outcome=measure_for_phrase"
        }
        obs <- next_observation(col)
        cmp <- if (length(sent_arms) >= 2L) sent_arms[1:2] else c(NA_character_, NA_character_)
        bind <- paste(c(m_bind, ctx_bind[-1], if (!is.na(cmp[1])) "comparison=sentence_arm_order" else "comparison=unbound",
                        if (is.na(adj)) "adjustment=not_stated" else "adjustment=sentence"), collapse = "; ")
        common <- list(outcome = m_out, timepoint = tp, analysis_set = aset, effect_measure = ms$measure[k],
                       adjusted = adj, arm = cmp[1], comparator = cmp[2], binding = bind,
                       observation_id = obs, method = "rules:text_effect")
        do.call(add_record, c(list(col, "result", est_txt, statistic = "estimate", value_num = as_num(est_txt),
                                   evidence_id = ev_at(est_pos, ecl)), common))
        rest <- substring(win, ecs + ecl)
        rest_off <- est_pos + ecl
        cm <- regexpr(ci_re, rest, perl = TRUE)
        if (cm > 0 && grepl("(?i)CI|confidence|^\\s*[(,;]", substr(rest, 1, attr(cm, "match.length")), perl = TRUE)) {
          cs <- attr(cm, "capture.start")
          cl <- attr(cm, "capture.length")
          lvl <- if (cl[1] > 0) as_num(substr(rest, cs[1], cs[1] + cl[1] - 1L)) else NA_real_
          lo <- substr(rest, cs[2], cs[2] + cl[2] - 1L)
          hi <- substr(rest, cs[3], cs[3] + cl[3] - 1L)
          common$ci_level <- lvl
          do.call(add_record, c(list(col, "result", lo, statistic = "ci_lower", value_num = as_num(lo),
                                     evidence_id = ev_at(rest_off + cs[2] - 1L, cl[2])), common))
          do.call(add_record, c(list(col, "result", hi, statistic = "ci_upper", value_num = as_num(hi),
                                     evidence_id = ev_at(rest_off + cs[3] - 1L, cl[3])), common))
          if (is.na(lvl)) col$records[[length(col$records)]]$notes <- "confidence level not stated"
        }
        pm <- regexpr(p_re, rest, perl = TRUE)
        if (pm > 0) {
          cs <- attr(pm, "capture.start")
          cl <- attr(pm, "capture.length")
          pv <- substr(rest, cs[2], cs[2] + cl[2] - 1L)
          common$p_operator <- substr(rest, cs[1], cs[1] + cl[1] - 1L)
          do.call(add_record, c(list(col, "result", pv, statistic = "p_value", value_num = as_num(pv),
                                     evidence_id = ev_at(rest_off + cs[2] - 1L, cl[2])), common))
          claimed <- c(claimed, rest_off + pm - 1L)
        }
      }
      # counts: "24 of 120 patients (20.0%) in the Drug A group"
      cm <- gregexpr("\\b([0-9][0-9,]*) (?:of|out of|/) ([0-9][0-9,]*)(?: [a-z]+){0,2}(?: \\(([0-9]+(?:\\.[0-9]+)?)%\\))?(?: (?:in|among|for|with|receiving) (?:the )?([A-Za-z0-9 -]{1,40}?) (?:group|arm|cohort)\\b)?", st, perl = TRUE)[[1]]
      if (cm[1] > 0) {
        cs <- attr(cm, "capture.start")
        cl <- attr(cm, "capture.length")
        for (j in seq_along(cm)) {
          grab <- function(g) substr(st, cs[j, g], cs[j, g] + cl[j, g] - 1L)
          arm_phrase <- if (cl[j, 4] > 0) grab(4) else NA_character_
          arm <- match_arm(arm_phrase, arms)
          obs <- next_observation(col)
          common <- list(outcome = outcome, timepoint = tp, analysis_set = aset, arm = arm,
                         binding = paste(c(ctx_bind, if (!is.na(arm)) "arm=group_phrase" else "arm=unbound"), collapse = "; "),
                         observation_id = obs, method = "rules:text_counts")
          do.call(add_record, c(list(col, "result", grab(1), statistic = "events", value_num = as_num(grab(1)),
                                     evidence_id = ev_at(cs[j, 1], cl[j, 1])), common))
          do.call(add_record, c(list(col, "result", grab(2), statistic = "denominator", value_num = as_num(grab(2)),
                                     evidence_id = ev_at(cs[j, 2], cl[j, 2])), common))
          if (cl[j, 3] > 0) {
            do.call(add_record, c(list(col, "result", grab(3), statistic = "percent", value_num = as_num(grab(3)), unit = "%",
                                       evidence_id = ev_at(cs[j, 3], cl[j, 3])), common))
          }
        }
      }
      # mean with unlabelled or labelled dispersion: "8.1 +/- 13.2"
      pmm <- gregexpr(paste0("(", num_re, ")\\s*\u00b1\\s*(", num_re, ")"), st, perl = TRUE)[[1]]
      if (pmm[1] > 0) {
        cs <- attr(pmm, "capture.start")
        cl <- attr(pmm, "capture.length")
        dtype <- if (grepl("\\bSD\\b|standard deviation", st)) "sd" else if (grepl("\\bSEM?\\b|standard error", st)) "se" else NA_character_
        arm <- {
          a <- regmatches(st, regexpr("(?i)(?:with|in the|among) ([A-Za-z0-9 -]{1,40}?)(?: group| arm)?[.;,]?$", st, perl = TRUE))
          if (length(a)) match_arm(sub("(?i)^(with|in the|among) ", "", sub("[.;,]$", "", a), perl = TRUE), arms) else NA_character_
        }
        for (j in seq_along(pmm)) {
          grab <- function(g) substr(st, cs[j, g], cs[j, g] + cl[j, g] - 1L)
          obs <- next_observation(col)
          unit <- regmatches(substring(st, cs[j, 2] + cl[j, 2]), regexpr("^\\s*(mm Hg|mmHg|kg/m2|kg|cm|mg/dL|mmol/L|%|points?|years?)", substring(st, cs[j, 2] + cl[j, 2])))
          common <- list(outcome = outcome, timepoint = tp, analysis_set = aset, arm = arm,
                         unit = if (length(unit)) trimws(unit) else NA_character_,
                         binding = paste(c(ctx_bind, if (!is.na(arm)) "arm=sentence_phrase" else "arm=unbound"), collapse = "; "),
                         observation_id = obs, method = "rules:text_mean_pm")
          do.call(add_record, c(list(col, "result", grab(1), statistic = "mean", value_num = as_num(grab(1)),
                                     evidence_id = ev_at(cs[j, 1], cl[j, 1])), common))
          common$dispersion_type <- if (is.na(dtype)) "unspecified" else dtype
          do.call(add_record, c(list(col, "result", grab(2), statistic = if (is.na(dtype)) "dispersion" else dtype,
                                     value_num = as_num(grab(2)), status = if (is.na(dtype)) "ambiguous" else "reported",
                                     notes = if (is.na(dtype)) "plus-minus value without a stated SD or SE label" else NA_character_,
                                     evidence_id = ev_at(cs[j, 2], cl[j, 2])), common))
        }
      }
      # median with IQR in text
      mm <- regexpr(paste0("(?i)median[^0-9]{0,40}?(", num_re, ")[^0-9(]{0,20}\\((?:IQR|interquartile range)[,:]?\\s*(", num_re, ")\\s*(?:to|\u2013|-|,)\\s*(", num_re, ")\\)"), st, perl = TRUE)
      if (mm > 0) {
        cs <- attr(mm, "capture.start")
        cl <- attr(mm, "capture.length")
        obs <- next_observation(col)
        common <- list(outcome = outcome, timepoint = tp, analysis_set = aset, binding = paste(ctx_bind, collapse = "; "),
                       observation_id = obs, method = "rules:text_median_iqr")
        for (g in 1:3) {
          v <- substr(st, cs[g], cs[g] + cl[g] - 1L)
          do.call(add_record, c(list(col, "result", v, statistic = c("median", "q1", "q3")[g], value_num = as_num(v),
                                     evidence_id = ev_at(cs[g], cl[g])), common))
        }
      }
      # p-values not attached to an effect measure
      pall <- gregexpr(p_re, st, perl = TRUE)[[1]]
      if (pall[1] > 0) {
        cs <- attr(pall, "capture.start")
        cl <- attr(pall, "capture.length")
        for (j in seq_along(pall)) {
          if (any(abs((pall[j]) - claimed) <= 1)) next
          pv <- substr(st, cs[j, 2], cs[j, 2] + cl[j, 2] - 1L)
          add_record(col, "result", pv, statistic = "p_value", value_num = as_num(pv),
                     p_operator = substr(st, cs[j, 1], cs[j, 1] + cl[j, 1] - 1L),
                     outcome = outcome, timepoint = tp, analysis_set = aset,
                     binding = paste(c(ctx_bind, "comparison=unbound"), collapse = "; "),
                     observation_id = next_observation(col), evidence_id = ev_at(cs[j, 2], cl[j, 2]),
                     method = "rules:text_p_value")
        }
      }
    }
  }
}

# ---- tables --------------------------------------------------------------

stat_hint <- function(label) {
  l <- tolower(label)
  if (grepl("mean\\s*\\(sd\\)|mean\\s*\u00b1\\s*sd", l)) return("mean_sd")
  if (grepl("mean\\s*\\(se[m]?\\)|mean\\s*\u00b1\\s*se", l)) return("mean_se")
  if (grepl("median\\s*\\(iqr\\)|median\\s*\\(interquartile", l)) return("median_iqr")
  if (grepl("median\\s*\\(range\\)", l)) return("median_range")
  if (grepl("n/n\\s*\\(%\\)|no\\./total", l)) return("events_total_pct")
  if (grepl("\\bn\\s*\\(%\\)|no\\.\\s*\\(%\\)|number \\(%\\)", l)) return("count_pct")
  if (grepl("\\(%\\)$|, %$", l)) return("percent")
  NA_character_
}

clean_row_label <- function(label) {
  parts <- trimws(strsplit(label, ",\\s+")[[1]])
  hint_idx <- which(!is.na(vapply(parts, stat_hint, character(1))))
  unit_idx <- setdiff(seq_along(parts)[-1], hint_idx)
  list(outcome = parts[1],
       unit = if (length(unit_idx)) paste(parts[unit_idx], collapse = ", ") else NA_character_)
}

column_headers <- function(cells, n_cols) {
  h <- cells[cells$is_header & nzchar(cells$text), , drop = FALSE]
  vapply(seq_len(n_cols), function(j) {
    hj <- h[h$col <= j & h$col + h$col_span - 1L >= j, , drop = FALSE]
    hj <- hj[order(hj$row), , drop = FALSE]
    if (!nrow(hj)) NA_character_ else paste(hj$text, collapse = " | ")
  }, character(1))
}

column_role <- function(header, arms) {
  if (is.na(header)) return(list(role = "unknown"))
  last <- trimws(utils::tail(strsplit(header, "|", fixed = TRUE)[[1]], 1))
  ms <- find_measures(last)
  if (nrow(ms)) {
    lvl <- regmatches(last, regexpr("[0-9]{2}(?=%\\s*(CI|confidence))", last, perl = TRUE))
    return(list(role = "effect", measure = ms$measure[1], ci_level = if (length(lvl)) as_num(lvl) else NA_real_,
                adjusted = adjusted_status(header, substr(last, ms$start[1], ms$start[1] + ms$length[1] - 1L))))
  }
  if (grepl("(?i)^p(\\s*value)?\\*?$|^p-value|^p for", last, perl = TRUE)) return(list(role = "p"))
  n <- regmatches(last, regexpr("(?<=[nN]\\s=\\s)[0-9,]+|(?<=[nN]=)[0-9,]+", last, perl = TRUE))
  arm <- match_arm(last, arms)
  if (is.na(arm) && length(n)) arm <- trimws(sub("\\s*\\((n|N)\\s*=\\s*[0-9,]+\\)", "", last))
  if (is.na(arm) && grepl("(?i)placebo|control|intervention|treatment|usual care|group", last, perl = TRUE)) arm <- last
  if (!is.na(arm)) {
    if (grepl("(?i)^(total|overall|all)", arm, perl = TRUE)) arm <- "total"
    return(list(role = "arm", arm = arm, n = if (length(n)) n else NA_character_))
  }
  list(role = "unknown")
}

parse_cell_value <- function(txt, hint, role) {
  t <- trimws(txt)
  rng <- "\\s*(?:to|\u2013|\u2014|-|,)\\s*"
  if (role == "p") {
    m <- regmatches(t, regexec(paste0("^([<>\u2264\u2265=])?\\s*(", num_re, ")$"), t, perl = TRUE))[[1]]
    if (length(m)) return(list(stats = "p_value", values = m[3], operator = if (nzchar(m[2])) m[2] else "="))
    return(NULL)
  }
  if (role == "effect") {
    m <- regmatches(t, regexec(paste0("^(?:([A-Za-z]{2,4})\\s+)?(", num_re, ")\\s*\\((", num_re, ")", rng, "(", num_re, ")\\)$"), t, perl = TRUE))[[1]]
    if (length(m)) return(list(stats = c("estimate", "ci_lower", "ci_upper"), values = m[3:5], prefix = m[2]))
    m <- regmatches(t, regexec(paste0("^(", num_re, ")$"), t, perl = TRUE))[[1]]
    if (length(m)) return(list(stats = "estimate", values = m[2]))
    return(NULL)
  }
  two <- regmatches(t, regexec(paste0("^(", num_re, ")\\s*\\((", num_re, ")%?\\)$"), t, perl = TRUE))[[1]]
  frac <- regmatches(t, regexec(paste0("^(", num_re, ")\\s*/\\s*(", num_re, ")(?:\\s*\\((", num_re, ")%?\\))?$"), t, perl = TRUE))[[1]]
  three <- regmatches(t, regexec(paste0("^(", num_re, ")\\s*\\((", num_re, ")", rng, "(", num_re, ")\\)$"), t, perl = TRUE))[[1]]
  pm <- regmatches(t, regexec(paste0("^(", num_re, ")\\s*\u00b1\\s*(", num_re, ")$"), t, perl = TRUE))[[1]]
  one <- regmatches(t, regexec(paste0("^(", num_re, ")%?$"), t, perl = TRUE))[[1]]
  h <- hint %|NA|% "none"
  if (length(frac)) {
    vals <- frac[2:4]
    st <- c("events", "denominator", "percent")
    keep <- nzchar(vals)
    return(list(stats = st[keep], values = vals[keep]))
  }
  if (length(two)) {
    st <- switch(h, mean_sd = c("mean", "sd"), mean_se = c("mean", "se"),
                 count_pct = c("count", "percent"), events_total_pct = c("count", "percent"),
                 c("value", "parenthetical"))
    return(list(stats = st, values = two[2:3], ambiguous = h == "none"))
  }
  if (length(three)) {
    st <- switch(h, median_iqr = c("median", "q1", "q3"), median_range = c("median", "min", "max"),
                 c("value", "interval_lower", "interval_upper"))
    return(list(stats = st, values = three[2:4], ambiguous = !h %in% c("median_iqr", "median_range")))
  }
  if (length(pm)) {
    st <- switch(h, mean_sd = c("mean", "sd"), mean_se = c("mean", "se"), c("mean", "dispersion"))
    return(list(stats = st, values = pm[2:3], ambiguous = !h %in% c("mean_sd", "mean_se")))
  }
  if (length(one)) {
    st <- switch(h, percent = "percent", count_pct = "count", "value")
    return(list(stats = st, values = one[2], ambiguous = st == "value"))
  }
  NULL
}

rule_table_statistics <- function(col, doc, arms) {
  arms <- arm_aliases(arms)
  tabs <- doc$tables[doc$tables$n_rows > 0L, , drop = FALSE]
  for (ti in seq_len(nrow(tabs))) {
    tb <- tabs[ti, ]
    cells <- doc$cells[doc$cells$table_id == tb$table_id, , drop = FALSE]
    if (!is.na(tb$continuation_status) && tb$continuation_status == "joined") next
    cont <- doc$tables$table_id[doc$tables$continuation_of %in% tb$table_id & doc$tables$continuation_status %in% "joined"]
    caption <- doc$captions$text[match(tb$caption_id, doc$captions$caption_id)]
    caption <- caption %|NA|% ""
    baseline <- grepl("(?i)baseline|characteristics|demographic", caption, perl = TRUE)
    field <- if (baseline) "baseline" else "result"
    tp <- if (baseline) "baseline" else find_timepoint(caption)
    aset <- find_analysis_set(caption)
    hdr <- column_headers(cells, tb$n_cols)
    roles <- lapply(hdr, column_role, arms = arms)
    roles[[1]] <- list(role = "label")
    arm_cols <- which(vapply(roles, function(r) r$role == "arm" && !identical(r$arm, "total"), logical(1)))
    cmp <- if (length(arm_cols) == 2L) c(roles[[arm_cols[1]]]$arm, roles[[arm_cols[2]]]$arm) else c(NA_character_, NA_character_)
    # arm sizes from headers
    for (j in arm_cols) {
      r <- roles[[j]]
      if (is.na(r$n)) next
      hc <- cells[cells$is_header & cells$col <= j & cells$col + cells$col_span - 1L >= j & grepl(r$n, cells$text, fixed = TRUE), , drop = FALSE]
      if (!nrow(hc)) next
      pos <- regexpr(r$n, hc$text[1], fixed = TRUE)
      ev <- add_evidence(col, cell_id = hc$cell_id[1], char_start = pos, char_end = pos + nchar(r$n) - 1L, value_text = r$n)
      add_record(col, "arm_n", r$n, statistic = "count", value_num = as_num(r$n), arm = r$arm, unit = "participants",
                 evidence_id = ev, method = "rules:table_header_n", binding = "arm=table_column_header",
                 notes = "n in column header; whether this is the randomized or analyzed number is not stated")
    }
    body_cells <- cells[!cells$is_header, , drop = FALSE]
    if (length(cont)) {
      extra <- doc$cells[doc$cells$table_id %in% cont & !doc$cells$is_header, , drop = FALSE]
      body_cells <- rbind(body_cells, extra)
    }
    for (rk in unique(paste(body_cells$table_id, body_cells$row))) {
      rc <- body_cells[paste(body_cells$table_id, body_cells$row) == rk, , drop = FALSE]
      lab_cell <- rc[rc$col == 1L, , drop = FALSE]
      label <- if (nrow(lab_cell)) lab_cell$text[1] else ""
      if (!nzchar(label)) next
      hint <- stat_hint(label)
      cl <- clean_row_label(label)
      row_tp <- find_timepoint(label) %|NA|% tp
      row_obs <- NULL
      for (k in seq_len(nrow(rc))) {
        cc <- rc[k, ]
        if (cc$col == 1L) next
        r <- roles[[cc$col]]
        if (r$role == "label") next
        col_hint <- if (!is.na(hdr[cc$col])) stat_hint(hdr[cc$col]) else NA_character_
        h <- hint %|NA|% col_hint
        base <- list(outcome = tolower(cl$outcome), unit = cl$unit, timepoint = row_tp, analysis_set = aset)
        bind <- c("outcome=table_row_label", if (!is.na(row_tp)) "timepoint=table_caption_or_row", if (!is.na(aset)) "analysis_set=table_caption")
        if (r$role == "arm") {
          base$arm <- r$arm
          bind <- c(bind, "arm=table_column_header")
        } else if (r$role %in% c("effect", "p")) {
          base$arm <- cmp[1]
          base$comparator <- cmp[2]
          bind <- c(bind, if (!is.na(cmp[1])) "comparison=table_arm_column_order" else "comparison=unbound")
          if (r$role == "effect") {
            base$effect_measure <- r$measure
            base$ci_level <- r$ci_level
            base$adjusted <- r$adjusted
            bind <- c(bind, if (is.na(r$adjusted)) "adjustment=not_stated" else "adjustment=table_header")
          }
        }
        base$binding <- paste(bind, collapse = "; ")
        base$method <- paste0("rules:table_", r$role)
        stat_name <- switch(r$role, effect = "estimate", p = "p_value", "value")
        if (!nzchar(cc$text)) {
          if (r$role %in% c("effect", "p")) {
            do.call(add_record, c(list(col, field, NA_character_, statistic = stat_name, status = "not_reported",
                                       evidence_id = add_evidence(col, cell_id = cc$cell_id),
                                       observation_id = {
                                         if (is.null(row_obs)) row_obs <- next_observation(col)
                                         row_obs
                                       }, notes = "empty table cell"), base))
          }
          next
        }
        if (isTRUE(cc$missing_token)) {
          do.call(add_record, c(list(col, field, NA_character_, statistic = stat_name, status = "missing",
                                     evidence_id = add_evidence(col, cell_id = cc$cell_id, char_start = 1L,
                                                                char_end = nchar(cc$text), value_text = cc$text),
                                     observation_id = next_observation(col),
                                     notes = paste0("missing value marker '", cc$text, "' in source")), base))
          next
        }
        pv <- parse_cell_value(cc$text, h, r$role)
        # the effect estimate and the p-value of one row form one observation
        obs <- if (r$role %in% c("effect", "p")) {
          if (is.null(row_obs)) row_obs <- next_observation(col)
          row_obs
        } else next_observation(col)
        if (is.null(pv)) {
          do.call(add_record, c(list(col, field, cc$text, statistic = "unparsed", status = "ambiguous",
                                     evidence_id = add_evidence(col, cell_id = cc$cell_id, char_start = 1L,
                                                                char_end = nchar(cc$text), value_text = cc$text),
                                     observation_id = obs, notes = "cell text not recognized as a statistic"), base))
          next
        }
        if (r$role == "effect" && !is.null(pv$prefix) && nzchar(pv$prefix)) {
          pm <- find_measures(pv$prefix)
          if (nrow(pm)) base$effect_measure <- pm$measure[1]
        }
        if (r$role == "p") base$p_operator <- pv$operator
        search_from <- 1L
        for (g in seq_along(pv$stats)) {
          v <- pv$values[g]
          pos <- regexpr(v, substring(cc$text, search_from), fixed = TRUE)
          st0 <- if (pos > 0) search_from + pos - 1L else NA_integer_
          if (!is.na(st0)) search_from <- st0 + nchar(v)
          ev <- add_evidence(col, cell_id = cc$cell_id, char_start = st0,
                             char_end = if (is.na(st0)) NA_integer_ else st0 + nchar(v) - 1L, value_text = v)
          s_name <- pv$stats[g]
          amb <- isTRUE(pv$ambiguous) && s_name %in% c("value", "parenthetical", "interval_lower", "interval_upper", "dispersion")
          rec <- c(list(col, field, v, statistic = s_name, value_num = as_num(v), evidence_id = ev,
                        observation_id = obs, status = if (amb) "ambiguous" else "reported",
                        notes = if (amb) "statistic type not stated in row label or column header" else NA_character_), base)
          if (s_name %in% c("sd", "se", "dispersion")) rec$dispersion_type <- if (s_name == "dispersion") "unspecified" else s_name
          if (s_name == "percent") rec$unit <- "%"
          do.call(add_record, rec)
        }
      }
    }
  }
}

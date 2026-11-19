# Deterministic rules for study design, PICO, methods and outcomes.

design_sections <- c("title", "abstract", "methods", NA)

rule_design <- function(col, units, fields) {
  u <- units[units$section_label %in% design_sections, , drop = FALSE]
  if ("study_design" %in% fields) {
    # Each mention is classified by the words just before it: "this
    # randomized trial", "we conducted a cohort study" describe the current
    # study; "a previous meta-analysis", "informed by a systematic review"
    # describe other work and never decide the design.
    found <- NULL
    for (i in seq_len(nrow(u))) {
      v <- match_vocab(u$text[i], design_vocabulary)
      if (!nrow(v)) next
      ss <- split_sentences(u$text[i])
      for (k in seq_len(nrow(v))) {
        si <- max(which(ss$start <= v$start[k]))
        before <- tolower(substr(u$text[i], ss$start[si], v$start[k] - 1L))
        near <- substring(before, max(1L, nchar(before) - 60L))
        last_at <- function(re) {
          m <- gregexpr(re, near, perl = TRUE)[[1]]
          if (m[1] > 0) max(m) else 0L
        }
        bg <- last_at("\\b(previous|prior|earlier|published|existing|other|recent|informed|based on|according to|et al)\\b|\\[[0-9]")
        cu <- last_at("\\b(this|our|the present|the current|we (conducted|performed|designed|report|did|undertook)|was an?|is an?)\\b")
        cls <- if (u$section_label[i] %in% "title" || cu > bg) "current" else if (bg > 0L) "background" else "neutral"
        found <- rbind(found, data.frame(v[k, ], block = u$block_id[i], cls = cls, stringsAsFactors = FALSE))
      }
    }
    if (!is.null(found)) {
      prio <- names(design_vocabulary)
      tier <- match(found$cls, c("current", "neutral", "background"))
      found <- found[order(tier, match(found$term, prio)), , drop = FALSE]
      best <- found[1, ]
      same_tier <- unique(found$term[found$cls == best$cls])
      others <- setdiff(unique(found$term), best$term)
      status <- "reported"
      note <- if (length(others)) paste("also mentioned:", paste(others, collapse = ", ")) else NA_character_
      if (best$cls == "background") {
        status <- "ambiguous"
        note <- "design terms are mentioned only for other studies"
      } else if (length(same_tier) > 1L) {
        status <- "ambiguous"
        note <- paste("competing design statements:", paste(same_tier, collapse = ", "))
      }
      txt <- u$text[match(best$block, u$block_id)]
      ev <- add_evidence(col, block_id = best$block, char_start = best$start,
                         char_end = best$start + best$length - 1L,
                         value_text = substr(txt, best$start, best$start + best$length - 1L))
      add_record(col, "study_design", best$term, status = status, evidence_id = ev,
                 method = "rules:design_vocabulary", notes = note)
    }
  }
  if ("design_features" %in% fields) {
    seen <- character()
    for (i in seq_len(nrow(u))) {
      v <- match_vocab(u$text[i], feature_vocabulary)
      for (k in seq_len(nrow(v))) {
        if (v$term[k] %in% seen) next
        seen <- c(seen, v$term[k])
        ev <- add_evidence(col, block_id = u$block_id[i], char_start = v$start[k],
                           char_end = v$start[k] + v$length[k] - 1L,
                           value_text = substr(u$text[i], v$start[k], v$start[k] + v$length[k] - 1L))
        add_record(col, "design_features", v$term[k], evidence_id = ev, method = "rules:feature_vocabulary")
      }
    }
  }
}

# Split an arm list ("Drug A 10 mg once daily (n = 120) or placebo") at
# top level separators.
split_arms <- function(x) {
  depth <- 0L
  out <- character()
  cur <- ""
  toks <- strsplit(x, "")[[1]]
  i <- 1L
  while (i <= length(toks)) {
    ch <- toks[i]
    if (ch == "(") depth <- depth + 1L
    if (ch == ")") depth <- max(0L, depth - 1L)
    rest <- paste(toks[i:min(length(toks), i + 9L)], collapse = "")
    sep <- if (depth == 0L) regmatches(rest, regexpr("^(, or | or | versus | vs\\.? |, and | and |, )", rest)) else character()
    if (length(sep)) {
      out <- c(out, cur)
      cur <- ""
      i <- i + nchar(sep)
      next
    }
    cur <- paste0(cur, ch)
    i <- i + 1L
  }
  trimws(c(out, cur))
}

dose_re <- paste0("[0-9]+(?:\\.[0-9]+)?\\s?(?:mg/kg|mg|g|\u00b5g|mcg|ug|IU|mL|ml|units?)",
                  "(?:\\s(?:once|twice|three times|four times)\\s(?:daily|weekly|a day|per day)|\\s(?:daily|weekly|per day|bid|tid|qd|od))?")

arm_name <- function(x) {
  x <- sub("\\s*\\((n|N)\\s*=\\s*[0-9,]+\\)", "", x)
  x <- sub(paste0("\\s*", dose_re), "", x, perl = TRUE)
  x <- sub("^(receive|receiving|either|the)\\s+", "", x)
  trimws(sub("[.,;]+$", "", x))
}

rule_methods <- function(col, units, fields) {
  u <- units[units$section_label %in% c("abstract", "methods", "results", NA), , drop = FALSE]
  arms <- character()
  for (i in seq_len(nrow(u))) {
    txt <- u$text[i]
    bid <- u$block_id[i]
    sents <- split_sentences(txt)
    for (s in seq_len(nrow(sents))) {
      st <- sents$text[s]
      off <- sents$start[s] - 1L
      ev_at <- function(m_start, m_len) {
        add_evidence(col, block_id = bid, char_start = off + m_start,
                     char_end = off + m_start + m_len - 1L,
                     value_text = substr(st, m_start, m_start + m_len - 1L))
      }
      # randomized sample size
      if ("randomized_n" %in% fields) {
        # a participant noun is required next to the number, so years
        # ("Between 2015 and 2017, patients were randomized"), durations and
        # site counts are never read as the sample size
        who <- "(?:patients|participants|subjects|women|men|adults|children|infants|neonates|people|persons|individuals|volunteers|students|residents|pregnancies)"
        m <- regexpr(paste0("(?i)(?:total of |^|\\b)([0-9]{1,3}(?:,[0-9]{3})+|[0-9]+) (?:[a-z-]+ ){0,2}", who,
                            "\\b(?: [a-z-]+){0,3} (?:were |was )?(?:randomly (?:assigned|allocated)|randomi[sz]ed|underwent randomi[sz]ation)"), st, perl = TRUE)
        if (m < 0) m <- regexpr(paste0("(?i)\\b(?:randomly (?:assigned|allocated)|randomi[sz]ed) (?:a total of )?([0-9]{1,3}(?:,[0-9]{3})+|[0-9]+) (?:[a-z-]+ ){0,2}", who, "\\b"), st, perl = TRUE)
        if (m < 0) m <- regexpr("(?i)\\bof the ([0-9]{1,3}(?:,[0-9]{3})+|[0-9]+) randomi[sz]ed\\b", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          n <- substr(st, cs, cs + cl - 1L)
          add_record(col, "randomized_n", n, statistic = "count", value_num = as_num(n),
                     evidence_id = ev_at(cs, cl), method = "rules:randomized_n", unit = "participants")
        }
      }
      if ("analyzed_n" %in% fields) {
        m <- regexpr("(?i)\\b([0-9][0-9,]*) (?:[a-z-]+ ){0,2}(?:were|was) (?:included in the (?:[a-z-]+ )?analys[ie]s|analy[sz]ed)", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          n <- substr(st, cs, cs + cl - 1L)
          add_record(col, "analyzed_n", n, statistic = "count", value_num = as_num(n),
                     evidence_id = ev_at(cs, cl), method = "rules:analyzed_n", unit = "participants",
                     analysis_set = find_analysis_set(st))
        }
      }
      # arms, doses, per arm n, intervention and comparator
      m <- regexpr("(?i)(?:randomly (?:assigned|allocated)|randomi[sz]ed)(?: [^.]{0,40}?)? to (?:receive )?(.+)$", st, perl = TRUE)
      if (m > 0) {
        cs <- attr(m, "capture.start")[1]
        seg <- sub("[.]$", "", substr(st, cs, nchar(st)))
        parts <- split_arms(seg)
        pos <- cs
        roles <- character()
        # arms that share a name ("Drug A 5 mg" and "Drug A 10 mg") are
        # told apart by their dose and schedule, which stay in the label
        base <- vapply(parts, arm_name, "")
        dose_of <- vapply(parts, function(p) {
          d <- regmatches(p, regexpr(dose_re, p, perl = TRUE))
          if (length(d)) d else ""
        }, "")
        same <- base %in% base[duplicated(tolower(base))] & nzchar(dose_of)
        for (pi in seq_along(parts)) {
          p <- parts[pi]
          ploc <- regexpr(p, substring(st, pos), fixed = TRUE)
          pstart <- if (ploc > 0) pos + ploc - 1L else NA_integer_
          nm <- if (same[pi]) paste(base[pi], dose_of[pi]) else base[pi]
          if (!nzchar(nm)) next
          arms <- c(arms, nm)
          role <- if (grepl("(?i)placebo|control|usual care|standard (of )?care|sham|no treatment|vehicle", nm, perl = TRUE)) "comparator" else "intervention"
          roles <- c(roles, role)
          ev <- if (is.na(pstart)) NA_character_ else ev_at(pstart, nchar(p))
          if ("arms" %in% fields) add_record(col, "arms", nm, evidence_id = ev, method = "rules:allocation_sentence", arm = nm)
          d <- regmatches(p, regexpr(dose_re, p, perl = TRUE))
          if (length(d) && "dose" %in% fields) {
            dl <- regexpr(d, substring(st, pstart), fixed = TRUE)
            add_record(col, "dose", d, evidence_id = ev_at(pstart + dl - 1L, nchar(d)),
                       method = "rules:dose_pattern", arm = nm)
          }
          an <- regexpr("\\((?:n|N)\\s*=\\s*([0-9,]+)\\)", p, perl = TRUE)
          if (an > 0 && "arm_n" %in% fields) {
            acs <- attr(an, "capture.start")[1]
            acl <- attr(an, "capture.length")[1]
            v <- substr(p, acs, acs + acl - 1L)
            add_record(col, "arm_n", v, statistic = "count", value_num = as_num(v),
                       evidence_id = ev_at(pstart + acs - 1L, acl), method = "rules:arm_n", arm = nm,
                       unit = "participants", notes = "number allocated to the arm as stated")
          }
          if (role %in% fields) add_record(col, role, nm, evidence_id = ev, method = "rules:allocation_sentence",
                                           arm = nm, notes = if (role == "comparator") "role from control keyword" else "role: arm without control keyword")
        }
      }
      if ("allocation_ratio" %in% fields) {
        m <- regexpr("\\b([0-9]+(?::[0-9]+)+)\\s+(?:ratio|allocation)", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          add_record(col, "allocation_ratio", substr(st, cs, cs + cl - 1L),
                     evidence_id = ev_at(cs, cl), method = "rules:allocation_ratio")
        }
      }
      if ("follow_up" %in% fields) {
        m <- regexpr("(?i)\\b(?:followed(?: up)? for|follow-up (?:of|was|period of)|median follow-up (?:of|was)) (?:a median of )?([0-9]+(?:\\.[0-9]+)?) (days?|weeks?|months?|years?)", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")
          cl <- attr(m, "capture.length")
          v <- substr(st, cs[1], cs[1] + cl[1] - 1L)
          unit <- substr(st, cs[2], cs[2] + cl[2] - 1L)
          add_record(col, "follow_up", paste(v, unit), statistic = if (grepl("(?i)median", st, perl = TRUE)) "median" else "duration",
                     value_num = as_num(v), unit = unit, evidence_id = ev_at(cs[1], cs[2] + cl[2] - cs[1]),
                     method = "rules:follow_up")
        }
      }
      if ("assessment_times" %in% fields) {
        m <- regexpr("(?i)\\b(?:at|after) ((?:week|month)s? [0-9]+(?:(?:, | and |, and )[0-9]+)+|[0-9]+(?:(?:, | and |, and )[0-9]+)+ (?:weeks|months|years|days))", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          add_record(col, "assessment_times", substr(st, cs, cs + cl - 1L), evidence_id = ev_at(cs, cl),
                     method = "rules:assessment_times")
        }
      }
      if ("analysis_population" %in% fields) {
        v <- match_vocab(st, analysis_set_vocabulary)
        if (nrow(v) && grepl("(?i)analys|population|principle|set\\b", st, perl = TRUE)) {
          if (nrow(v) > 1L && "intention_to_treat" %in% v$term && "modified_intention_to_treat" %in% v$term) {
            v <- v[v$term != "intention_to_treat", , drop = FALSE]
          }
          for (k in seq_len(nrow(v))) {
            add_record(col, "analysis_population", v$term[k], evidence_id = ev_at(v$start[k], v$length[k]),
                       method = "rules:analysis_set_vocabulary")
          }
        }
      }
      # population and eligibility
      if (any(c("population", "eligibility_inclusion") %in% fields)) {
        m <- regexpr("(?i)^(.{3,200}?) (?:were|was) (?:eligible|included if|enrolled if)", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          pop <- substr(st, cs, cs + cl - 1L)
          if ("population" %in% fields) add_record(col, "population", pop, evidence_id = ev_at(cs, cl), method = "rules:eligibility_sentence")
          if ("eligibility_inclusion" %in% fields) add_record(col, "eligibility_inclusion", st, evidence_id = ev_at(1L, nchar(st)), method = "rules:eligibility_sentence")
        } else if ("eligibility_inclusion" %in% fields && grepl("(?i)\\binclusion criteria\\b", st, perl = TRUE)) {
          add_record(col, "eligibility_inclusion", st, evidence_id = ev_at(1L, nchar(st)), method = "rules:criteria_sentence")
        }
      }
      if ("eligibility_exclusion" %in% fields && grepl("(?i)\\bexclusion criteria\\b|\\bwere excluded if\\b|\\bexcluded (patients|participants) (with|who)\\b", st, perl = TRUE)) {
        add_record(col, "eligibility_exclusion", st, evidence_id = ev_at(1L, nchar(st)), method = "rules:criteria_sentence")
      }
      # outcomes
      if ("primary_outcome" %in% fields) {
        m <- regexpr("(?i)\\bthe primary (?:outcome|end ?point|efficacy outcome)s? (?:was|were|is) (?:the )?(.+?)[.]?$", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          cl <- attr(m, "capture.length")[1]
          v <- substr(st, cs, cs + cl - 1L)
          add_record(col, "primary_outcome", v, evidence_id = ev_at(cs, cl), method = "rules:primary_outcome_sentence",
                     outcome = v, timepoint = find_timepoint(v))
        }
      }
      if ("secondary_outcomes" %in% fields) {
        m <- regexpr("(?i)\\bsecondary (?:outcomes|end ?points) (?:included|were|are|comprised) (.+?)[.]?$", st, perl = TRUE)
        if (m > 0) {
          cs <- attr(m, "capture.start")[1]
          seg <- substr(st, cs, cs + attr(m, "capture.length")[1] - 1L)
          for (p in split_arms(seg)) {
            pl <- regexpr(p, seg, fixed = TRUE)
            add_record(col, "secondary_outcomes", p, evidence_id = ev_at(cs + pl - 1L, nchar(p)),
                       method = "rules:secondary_outcome_sentence", outcome = p, timepoint = find_timepoint(p))
          }
        }
      }
    }
  }
  unique(arms)
}

# User defined fields with a regular expression.
rule_user_field <- function(col, units, field) {
  u <- units
  if (!is.null(field$sections)) u <- u[u$section_label %in% field$sections, , drop = FALSE]
  found <- 0L
  for (i in seq_len(nrow(u))) {
    if (is.null(field$pattern)) break
    txt <- u$text[i]
    m <- gregexpr(field$pattern, txt, perl = TRUE)[[1]]
    if (m[1] < 0) next
    cs <- attr(m, "capture.start")
    cl <- attr(m, "capture.length")
    for (j in seq_along(m)) {
      if (is.null(cs) || !ncol(cs)) {
        s0 <- m[j]
        l0 <- attr(m, "match.length")[j]
      } else {
        s0 <- cs[j, 1]
        l0 <- cl[j, 1]
      }
      v <- substr(txt, s0, s0 + l0 - 1L)
      if (field$type == "categorical") {
        mv <- match_vocab(v, field$vocabulary)
        v <- if (nrow(mv)) mv$term[1] else v
      }
      ev <- add_evidence(col, block_id = u$block_id[i], char_start = s0, char_end = s0 + l0 - 1L, value_text = v)
      add_record(col, field$name, v,
                 value_num = if (field$type %in% c("numeric", "integer")) as_num(v) else NA_real_,
                 unit = field$unit %||% NA_character_, evidence_id = ev,
                 method = "rules:user_pattern")
      found <- found + 1L
      if (!field$multiple) return(invisible(found))
    }
  }
  if (!found && field$type == "categorical" && is.null(field$pattern)) {
    for (i in seq_len(nrow(u))) {
      mv <- match_vocab(u$text[i], field$vocabulary)
      if (!nrow(mv)) next
      ev <- add_evidence(col, block_id = u$block_id[i], char_start = mv$start[1],
                         char_end = mv$start[1] + mv$length[1] - 1L,
                         value_text = substr(u$text[i], mv$start[1], mv$start[1] + mv$length[1] - 1L))
      add_record(col, field$name, mv$term[1], evidence_id = ev, method = "rules:user_vocabulary")
      found <- found + 1L
      if (!field$multiple) break
    }
  }
  invisible(found)
}

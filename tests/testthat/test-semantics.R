test_that("numbers are read as whole tokens", {
  tok <- function(t) regmatches(t, gregexpr(num_re, t, perl = TRUE))[[1]]
  expect_equal(as_num(tok("p = 0·02")), 0.02)
  expect_equal(as_num(tok("HR 0·62 (0·41 to 0·94)")), c(0.62, 0.41, 0.94))
  # a decimal comma is never read as its integer prefix
  expect_length(tok("P = 0,05"), 0)
  expect_length(tok("12,5 mg"), 0)
  expect_equal(as_num(tok("1,234 patients")), 1234)
  expect_equal(as_num(tok("(0.41,0.94)")), c(0.41, 0.94))
  expect_equal(as_num(tok("0.62, 0.41")), c(0.62, 0.41))
  expect_equal(as_num(tok("5×10³ cells")), 5000)
  expect_equal(as_num(tok("2.5 x 10^-3")), 0.0025)
  expect_equal(as_num(tok("5·10⁻³")), 0.005)
  expect_equal(as_num(tok("−3.4")), -3.4)
})

test_that("unsupported decimal marks never become zero", {
  r <- result_records("The hazard ratio for death with Drug A versus placebo was 0·62 (95% CI 0·41 to 0·94; p = 0·02).")
  expect_equal(r$value_num[r$statistic == "p_value"], 0.02)
  expect_equal(r$value_num[r$statistic == "estimate"], 0.62)
  r2 <- sc_extract(jats_text("P = 0,05."), "numerical_results")$records
  expect_false(any(r2$statistic %in% "p_value" & r2$value_num %in% 0))
})

test_that("JATS markup keeps paragraph breaks, powers and fractions", {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0(
    '<article><body><sec><title>Results</title><p>Counts were 5 &#215; 10<sup>3</sup> cells per m<sup>2</sup>',
    ' in H<sub>2</sub>O, see<sup><xref ref-type="bibr" rid="r1">1</xref></sup>.</p>',
    '<p>Half is <inline-formula><math xmlns="http://www.w3.org/1998/Math/MathML"><mfrac><mn>1</mn><mn>2</mn>',
    '</mfrac></math></inline-formula>.</p>', jats_table(c("Outcome", "Drug A", "Placebo"),
    list(c("Deaths", "<p>10</p><p>20</p>", "3<break/>4"))),
    '</sec></body><back><ref-list><ref id="r1"><mixed-citation>Ref.</mixed-citation></ref></ref-list></back></article>'), f)
  d <- sc_parse(f)
  expect_true(any(grepl("5 × 10³ cells per m² in H₂O", d$blocks$text)))
  expect_true(any(grepl("Half is 1/2", d$blocks$text)))
  cell <- d$cells$text[d$cells$row == 2 & d$cells$col == 2]
  expect_equal(cell, "10 20")
  expect_equal(d$cells$text[d$cells$row == 2 & d$cells$col == 3], "3 4")
  # superscript citations keep their marker and offsets
  b <- d$blocks$text[match(d$citations$block_id, d$blocks$block_id)]
  expect_equal(substring(b, d$citations$char_start, d$citations$char_end), "1")
  x <- sc_extract(d, "numerical_results")$records
  expect_false(any(x$value_num %in% c(1020, 103)))
})

test_that("PDF powers and unit exponents are not footnote markers", {
  d <- sc_parse(layout_fixture("scientific_superscripts.pdf"))
  v <- d$cells$text[d$cells$col == 2 & d$cells$row > 1]
  expect_equal(v, c("5 x 10³", "7 m²", "6 x 10⁴"))
  expect_true(all(is.na(d$cells$markers)))
  # letter markers in the trial table are still markers
  t <- sc_parse(fixture("trial_tables.pdf"))
  expect_true("a" %in% t$cells$markers)
})

test_that("arms that differ only by dose stay separate", {
  m <- "150 patients were randomized to Drug A 5 mg (n = 50), Drug A 10 mg (n = 50), or placebo (n = 50)."
  d <- jats_text("Death occurred in 5 of 50 patients in the Drug A 10 mg group and 9 of 50 in the placebo group.", methods = m)
  x <- sc_validate(sc_extract(d, "clinical_trial"))
  r <- x$records
  expect_setequal(r$value[r$field == "arms"], c("Drug A 5 mg", "Drug A 10 mg", "placebo"))
  expect_equal(r$arm[r$field == "dose"], c("Drug A 5 mg", "Drug A 10 mg"))
  expect_equal(r$arm[r$statistic %in% "events"], c("Drug A 10 mg", "placebo"))
  v <- x$validation
  expect_equal(v$result[v$rule == "arm_sizes_sum"], "pass")
  # one dose per drug keeps the plain name
  d2 <- jats_text("", methods = "Patients were randomized to Drug A 10 mg once daily (n = 50) or placebo (n = 50).")
  r2 <- sc_extract(d2, "trial_details")$records
  expect_setequal(r2$value[r2$field == "arms"], c("Drug A", "placebo"))
})

test_that("a comparison named in a table is kept in its direction", {
  est <- function(h, cap = "Outcomes", m = "Patients were randomly assigned to receive Drug A or placebo.") {
    d <- jats_text("", methods = m, extra = jats_table(h, list(c("Death, n (%)", "10 (10.0)", "20 (20.0)", "2.25 (1.20 to 4.22)")), caption = cap))
    r <- sc_extract(d, "numerical_results")$records
    r[r$statistic %in% "estimate", c("value_num", "arm", "comparator", "binding")]
  }
  a <- est(c("Outcome", "Drug A", "Placebo", "Odds ratio (Placebo versus Drug A), 95% CI"))
  expect_equal(c(a$arm, a$comparator), c("placebo", "Drug A"))
  expect_equal(a$value_num, 2.25)
  expect_match(a$binding, "table_header_cue")
  b <- est(c("Outcome", "Drug A", "Placebo", "OR (95% CI)"), cap = "Outcomes, placebo vs Drug A")
  expect_equal(c(b$arm, b$comparator), c("placebo", "Drug A"))
  # no cue: active versus control by convention, and said so
  c1 <- est(c("Outcome", "Drug A", "Placebo", "Odds ratio (95% CI)"))
  expect_equal(c(c1$arm, c1$comparator), c("Drug A", "placebo"))
  expect_match(c1$binding, "control_arm_convention")
  # two active arms and no cue: the direction is not guessed
  m2 <- "Patients were randomly assigned to receive Drug A or Drug B."
  e <- est(c("Outcome", "Drug A", "Drug B", "Odds ratio (95% CI)"), m = m2)
  expect_true(is.na(e$arm) && is.na(e$comparator))
  f <- est(c("Outcome", "Drug A", "Drug B", "Odds ratio, Drug B vs Drug A (95% CI)"), m = m2)
  expect_equal(c(f$arm, f$comparator), c("Drug B", "Drug A"))
})

test_that("p-values keep their test, operator and interval kind", {
  x <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.80 (95% CI 0.60 to 1.10); the interaction test had p = 0.02."), "numerical_results")
  r <- x$records
  p <- r[r$statistic %in% "p_value", ]
  expect_equal(p$test_role, "interaction")
  expect_false(p$observation_id %in% r$observation_id[r$statistic %in% "estimate"])
  w <- sc_observations(x, wide = TRUE)
  expect_false("p_value" %in% names(w))
  expect_equal(w[["p_value_interaction"]][2], 0.02)
  # the test of the estimate itself stays with it, operator and level kept
  x2 <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.50 (99% CI 0.20 to 0.80; p &lt; 0.001)."), "numerical_results")
  w2 <- sc_observations(x2, wide = TRUE)
  expect_equal(nrow(w2), 1L)
  expect_equal(c(w2$p_operator, w2$interval_type), c("<", "confidence"))
  expect_equal(w2$ci_level, 99)
  expect_match(w2$origins, "estimate:reported")
  expect_true(all(c("test_role", "p_operator", "ci_level", "interval_type") %in% names(sc_observations(x2))))
  # credible intervals are not confidence intervals
  x3 <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.70 (95% credible interval 0.40 to 0.95)."), "numerical_results")
  expect_equal(unique(na.omit(x3$records$interval_type)), "credible")
  # a table with a main and an interaction p-value
  d <- jats_text("", extra = jats_table(c("Outcome", "Drug A", "Placebo", "Odds ratio (95% CI)", "P value", "P for interaction"),
                                         list(c("Death, n (%)", "10 (10)", "20 (20)", "0.50 (0.30 to 0.80)", "0.01", "0.70"))))
  w3 <- sc_observations(sc_extract(d, "numerical_results"), wide = TRUE)
  eff <- w3[!is.na(w3$estimate), ]
  expect_equal(eff$p_value, 0.01)
  expect_equal(w3$p_value_interaction[!is.na(w3$p_value_interaction)], 0.70)
})

test_that("the wide view refuses two values of one statistic", {
  x <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.50 (95% CI 0.20 to 0.80)."), "numerical_results")
  extra <- x$records[x$records$statistic == "estimate", ]
  extra$record_id <- "rec-extra"
  extra$value_num <- 0.55
  x$records <- rbind(x$records, extra)
  expect_error(sc_observations(x, wide = TRUE), "two values of the same statistic")
  expect_equal(sum(sc_observations(x)$statistic == "estimate"), 2L)
})

test_that("no standard error is read from a credible interval", {
  x <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.70 (95% credible interval 0.40 to 0.95)."), "numerical_results")
  expect_false("se_log" %in% sc_derive(x, "se_log_ratio")$records$statistic)
  y <- sc_extract(jats_text("The hazard ratio for death with Drug A versus placebo was 0.70 (95% CI 0.40 to 0.95)."), "numerical_results")
  expect_true("se_log" %in% sc_derive(y, "se_log_ratio")$records$statistic)
})

test_that("negated design statements are not positive classifications", {
  rec <- function(t, s = "study_design") sc_extract(jats_text("", methods = t), s)$records
  a <- rec("This study was not randomized and was not double-blind.")
  expect_false(any(a$value %in% c("randomized_controlled_trial", "double_blind")))
  b <- rec("This was a non-randomized, open-label study.")
  expect_false("randomized_controlled_trial" %in% b$value)
  expect_true("open_label" %in% b$value)
  c1 <- rec("We did not conduct a randomized trial; this was a prospective cohort study.")
  expect_equal(c1$value[c1$field == "study_design"], "cohort")
  # positive control
  d <- rec("This was a randomized, double-blind trial.")
  expect_true(all(c("randomized_controlled_trial", "double_blind") %in% d$value))
})

test_that("custom categorical fields keep every hit and the source token", {
  f <- sc_field("setting", "Care setting", "categorical",
                vocabulary = c(primary = "\\bprimary care\\b|\\bgeneral practice\\b", hospital = "\\bhospitals?\\b"),
                multiple = TRUE)
  x <- sc_extract(jats_text("", methods = "Patients were recruited in general practice and in two hospitals."), sc_schema(fields = list(f)))
  r <- x$records[x$records$field == "setting", ]
  expect_setequal(r$value, c("primary", "hospital"))
  ev <- x$evidence$value_text[match(r$evidence_id, x$evidence$evidence_id)]
  expect_setequal(ev, c("general practice", "hospitals"))
  # a pattern capture is kept verbatim in the evidence
  g <- sc_field("setting", "Care setting", "categorical",
                vocabulary = c(primary = "general practice"), pattern = "recruited in (general practice)")
  y <- sc_extract(jats_text("", methods = "Patients were recruited in general practice."), sc_schema(fields = list(g)))
  expect_equal(y$records$value[y$records$field == "setting"], "primary")
  expect_equal(y$evidence$value_text[match(y$records$evidence_id[1], y$evidence$evidence_id)], "general practice")
})

test_that("a changed n supersedes the derived SD", {
  d <- jats_text("", methods = "Patients were randomly assigned to receive Drug A or placebo.",
                 extra = jats_table(c("Outcome", "Drug A", "Placebo"), list(c("Change in SBP, mean (SE)", "-8.0 (2.0)", "-3.0 (1.5)"))))
  x <- sc_extract(d, "numerical_results")
  a <- sc_derive(x, "sd_from_se", n = c("Drug A" = 100, placebo = 100))
  sd_a <- function(z) z$records[z$records$statistic %in% "sd" & z$records$arm %in% "Drug A", ]
  expect_equal(sd_a(a)$value_num, 20)
  b <- sc_derive(a, "sd_from_se", n = c("Drug A" = 25, placebo = 100))
  s <- sd_a(b)
  expect_equal(s$value_num[s$status == "reported"], 10)
  expect_equal(s$status[s$value_num == 20], "superseded")
  expect_equal(s$supersedes[s$status == "reported"], s$record_id[s$value_num == 20])
  expect_equal(s$assumptions[s$status == "reported"], "n=25 supplied")
  # the same n again adds nothing
  expect_equal(nrow(sc_derive(b, "sd_from_se", n = c("Drug A" = 25, placebo = 100))$records), nrow(b$records))
})

test_that("impossible inputs never give accepted derived values", {
  y <- sc_extract(jats_text("Death occurred in 130 of 100 patients in the Drug A group."), "numerical_results")
  for (z in list(sc_derive(y, "percent"), sc_derive(sc_validate(y), "percent"), sc_validate(sc_derive(y, "percent")))) {
    expect_false(any(z$records$data_origin == "derived" & z$records$status == "reported"))
  }
  # an input that becomes impossible later takes its derived values with it
  ok <- sc_derive(sc_extract(jats_text("Death occurred in 30 of 100 patients in the Drug A group."), "numerical_results"), "percent")
  expect_true(any(ok$records$data_origin == "derived"))
  ok$records$value_num[ok$records$statistic == "events"] <- 130
  v <- sc_validate(ok)
  expect_equal(v$records$status[v$records$data_origin == "derived"], "inconsistent")
})

test_that("statistic domains are enforced", {
  dom <- function(rows, h = c("Outcome", "Drug A", "Placebo")) {
    v <- sc_validate(sc_extract(jats_text("", extra = jats_table(h, rows)), "numerical_results"))
    v$validation[v$validation$rule == "value_domain", ]
  }
  expect_true(any(dom(list(c("Pain score, mean (SD)", "5.0 (-1.2)", "4.0 (1.1)")))$result == "fail"))
  expect_true(any(dom(list(c("Pain score, mean (SE)", "5.0 (-0.3)", "4.0 (0.2)")))$result == "fail"))
  expect_true(any(dom(list(c("Death, n (%)", "10 (120.0)", "20 (20.0)")))$result == "fail"))
  expect_true(any(dom(list(c("Length of stay, median (IQR)", "5 (7 to 9)", "4 (3 to 6)")))$result == "fail"))
  # valid values pass, and a signed percent change is not a proportion
  expect_false(any(dom(list(c("Death, n (%)", "10 (10.0)", "20 (20.0)")))$result == "fail"))
  r <- sc_extract(jats_text("Body weight changed by -12.5% with Drug A."), "numerical_results")
  expect_false(any(sc_validate(r)$validation$result[sc_validate(r)$validation$rule == "value_domain"] == "fail"))
})

test_that("corrections need a reviewer and a reason and carry a time", {
  x <- sc_extract(jats_text("Death occurred in 30 of 100 patients in the Drug A group."), "numerical_results")
  id <- x$records$record_id[x$records$statistic == "events"]
  expect_error(sc_correct(x, id, "31", reviewer = " ", reason = "typo"), "must not be empty")
  expect_error(sc_correct(x, id, "31", reviewer = "AB", reason = ""), "must not be empty")
  y <- sc_correct(x, id, "31", reviewer = "AB", reason = "typo in source")
  new <- y$records[y$records$supersedes %in% id, ]
  expect_match(new$corrected_at, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T")
  expect_equal(new$observation_id, x$records$observation_id[x$records$record_id == id])
})

test_that("rounded restatements are not contradictions", {
  conf <- function(a, b) {
    d <- jats_text(paste0("The hazard ratio for death with Drug A versus placebo was ", a, " (95% CI 0.40 to 0.95)."),
                   methods = "Patients were randomly assigned to receive Drug A or placebo.",
                   extra = jats_table(c("Outcome", "Drug A", "Placebo", "Hazard ratio (95% CI)"),
                                      list(c("Death, n (%)", "10 (10.0)", "20 (20.0)", paste0(b, " (0.40 to 0.95)")))))
    v <- sc_validate(sc_extract(d, "numerical_results"))$validation
    v[v$rule == "cross_source_conflict" & grepl("estimate", v$message), "result"]
  }
  expect_equal(conf("0.6", "0.62"), "pass")
  expect_true(conf("0.6", "0.72") %in% c("fail", "possible_conflict"))
  # neither side states a time point or analysis set: possible, not definite
  expect_equal(conf("0.60", "0.72"), "possible_conflict")
})

test_that("n (%) cells without a denominator are marked not checked", {
  d <- jats_text("", extra = jats_table(c("Outcome", "Drug A (n=100)", "Placebo (n=100)"), list(c("Death, n (%)", "12 (99)", "20 (20)"))))
  v <- sc_validate(sc_extract(d, "clinical_trial"))
  pc <- v$validation[v$validation$rule == "percent_consistent", ]
  expect_true(all(pc$result == "not_checked"))
  expect_match(pc$message[1], "not checked; with the arm size 100 it would be 12.0%")
  expect_false("denominator" %in% v$records$statistic)
  expect_true(all(v$records$validation_status[v$records$statistic %in% "percent"] == "not_checked"))
})

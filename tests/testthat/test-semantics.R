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

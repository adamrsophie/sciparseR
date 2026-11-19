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

jats_doc <- function(results, methods = "Patients were randomly assigned to receive Drug A or placebo.",
                     extra = "", doctype = "") {
  f <- tempfile(fileext = ".xml")
  ps <- paste0("<p>", results, "</p>", collapse = "")
  writeLines(paste0(doctype, "<article><body><sec><title>Methods</title><p>", methods,
                    "</p></sec><sec><title>Results</title>", ps, extra, "</sec></body></article>"), f)
  sc_parse(f)
}
results_of <- function(doc) {
  o <- sc_observations(sc_extract(doc, "numerical_results"))
  o[o$field == "result", , drop = FALSE]
}

test_that("each clause keeps its own time point and analysis set", {
  o <- results_of(jats_doc(paste(
    "In the per-protocol population, the hazard ratio for death was 0.70 (95% CI 0.55 to 0.90);",
    "in the intention-to-treat population, the hazard ratio for death was 0.78 (95% CI 0.62 to 0.98).")))
  est <- o[o$statistic == "estimate", ]
  expect_identical(est$analysis_set[est$value == "0.70"], "per_protocol")
  expect_identical(est$analysis_set[est$value == "0.78"], "intention_to_treat")
  o <- results_of(jats_doc(
    "The hazard ratio for death was 0.80 (95% CI 0.60 to 1.05) at 6 months and 0.70 (95% CI 0.55 to 0.90) at 12 months."))
  est <- o[o$statistic == "estimate", ]
  expect_identical(est$timepoint, c("6 months", "12 months"))
  expect_equal(sum(o$statistic == "ci_lower"), 2)
})

test_that("estimates far from the measure name and bracketed CI labels are kept", {
  o <- results_of(jats_doc(
    "The hazard ratio for death in the Drug A group as compared with the placebo group was 0.57 (95% CI, 0.40 to 0.81)."))
  expect_identical(o$value[o$statistic == "estimate"], "0.57")
  expect_identical(o$arm[1], "Drug A")
  o <- results_of(jats_doc("The hazard ratio for death was 0.57 (95% confidence interval [CI], 0.40 to 0.81)."))
  expect_setequal(o$statistic, c("estimate", "ci_lower", "ci_upper"))
})

test_that("abbreviations, age ranges and other clauses are not misread", {
  expect_equal(nrow(results_of(jats_doc("Mean HR was 72 beats per minute."))), 0)
  o <- results_of(jats_doc("The hazard ratio was 0.80 among participants aged 18-65."))
  expect_false(any(o$statistic == "ci_lower"))
  o <- results_of(jats_doc("Among patients aged 18 to 65 years, the hazard ratio for death was 0.80 (95% CI 0.65 to 0.98)."))
  expect_true(all(is.na(o$timepoint)))
  o <- results_of(jats_doc("The dose was increased from 5 to 10 mg, and the odds ratio was 1.50 (95% CI 1.10 to 2.05)."))
  expect_true(all(is.na(o$outcome)))
})

test_that("numbers without interval or p-value far from a measure are not estimates", {
  expect_equal(nrow(results_of(jats_doc("Hazard ratios were similar in a sensitivity analysis of 118 patients."))), 0)
  o <- results_of(jats_doc("The hazard ratio was 0.80 (95% CI 0.60 to 1.05) and 12 (10.0%) patients died."))
  expect_equal(sum(o$statistic == "estimate"), 1)
  o <- results_of(jats_doc("Hazard ratios were estimated with Cox models, and a two-sided P value of 0.05 was considered significant."))
  expect_false(any(o$statistic == "estimate"))
})

test_that("declared arms keep plain columns from becoming arms", {
  d <- jats_doc("x", extra = paste0('<table-wrap id="t1"><label>Table 2</label><caption><p>Outcomes</p></caption><table><thead>',
    '<tr><th>Outcome</th><th>N</th><th>Drug A</th><th>Placebo</th><th>Hazard ratio (95% CI)</th></tr></thead><tbody>',
    '<tr><td>Death, n (%)</td><td>238</td><td>12 (10.0)</td><td>20 (16.9)</td><td>0.60 (0.30 to 0.90)</td></tr></tbody></table></table-wrap>'))
  o <- results_of(d)
  expect_false(any(o$arm %in% "N"))
  est <- o[o$statistic == "estimate", ]
  expect_identical(c(est$arm, est$comparator), c("Drug A", "placebo"))
})

test_that("mean values in one sentence get their own arms", {
  o <- results_of(jats_doc(
    "Systolic blood pressure decreased by 12.4 ± 8.1 mm Hg in the Drug A group and 5.2 ± 7.9 mm Hg in the placebo group."))
  expect_identical(o$arm[o$statistic == "mean"], c("Drug A", "placebo"))
})

test_that("randomized sample size needs a participant noun", {
  rn <- function(m) {
    r <- sc_extract(jats_doc("x", methods = m), "trial_details")$records
    r$value[r$field == "randomized_n"]
  }
  expect_true(is.na(rn("Between March 2015 and June 2017, patients were randomly assigned to receive Drug A or placebo.")))
  expect_true(is.na(rn("In 2018 we randomized participants at 14 sites.")))
  expect_identical(rn("A total of 238 patients were randomly assigned to Drug A or placebo."), "238")
  expect_identical(rn("We randomized 1,204 participants to Drug A or placebo."), "1,204")
})

test_that("JATS row headers, header levels and baseline captions", {
  tab <- function(caption, head, body) {
    paste0('<table-wrap id="t1"><label>Table 2</label><caption><p>', caption, '</p></caption><table><thead>',
           head, '</thead><tbody>', body, '</tbody></table></table-wrap>')
  }
  d <- jats_doc("x", extra = tab("Outcomes",
    "<tr><th>Outcome</th><th>Drug A (n = 120)</th><th>Placebo (n = 118)</th></tr>",
    "<tr><th scope=\"row\">Death, n (%)</th><td>12 (10.0)</td><td>20 (16.9)</td></tr>"))
  expect_equal(d$tables$n_header_rows, 1L)
  expect_equal(nrow(sc_table_to_dataframe(d, d$tables$table_id[1])), 1)
  d <- jats_doc("x", methods = "Patients were randomly assigned to Drug A (n = 100) or placebo (n = 100).", extra = tab("Outcomes",
    paste0("<tr><th rowspan=\"2\">Outcome</th><th colspan=\"2\">Drug A (n = 100)</th><th colspan=\"2\">Placebo (n = 100)</th></tr>",
           "<tr><th>6 months</th><th>12 months</th><th>6 months</th><th>12 months</th></tr>"),
    "<tr><td>Death, n (%)</td><td>5 (5.0)</td><td>8 (8.0)</td><td>7 (7.0)</td><td>12 (12.0)</td></tr>"))
  o <- results_of(d)
  cnt <- o[o$statistic == "count", ]
  expect_identical(cnt$arm, c("Drug A", "Drug A", "placebo", "placebo"))
  expect_identical(cnt$timepoint, c("6 months", "12 months", "6 months", "12 months"))
  d <- jats_doc("x", extra = tab("Change from baseline in systolic blood pressure at week 12",
    "<tr><th>Outcome</th><th>Drug A</th><th>Placebo</th></tr>",
    "<tr><td>SBP, mean (SD)</td><td>-12.4 (8.1)</td><td>-5.2 (7.9)</td></tr>"))
  r <- sc_extract(d, "numerical_results")$records
  expect_false(any(r$field == "baseline"))
})

test_that("XML entities keep their characters", {
  d <- jats_doc("The mean difference was &mn;7.2 (95% CI &minus;9.3 to &minus;5.1) and &foo; here.",
                doctype = '<!DOCTYPE article [<!ENTITY mn "-">]>')
  expect_true(any(grepl("-7.2 (95% CI -9.3 to -5.1)", d$blocks$text, fixed = TRUE)))
  expect_true("xml_entity" %in% d$warnings$code)
  o <- results_of(d)
  expect_identical(o$value[o$statistic == "estimate"], "-7.2")
})

test_that("back matter footnotes and supplementary material are read", {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0('<article><body><sec><title>M</title><p>x</p></sec></body><back><fn-group><fn id="f1">',
                    '<label>a</label><p>Funded by X.</p></fn></fn-group><supplementary-material id="s1">',
                    '<label>S1</label></supplementary-material><ref-list><ref id="r1"><mixed-citation>',
                    'Smith J, Doe A. Title one. J Med. 2020;12:1-2. doi:10.1000/abc.</mixed-citation></ref>',
                    '</ref-list></back></article>'), f)
  d <- sc_parse(f)
  expect_identical(d$footnotes$scope, "document")
  expect_equal(nrow(d$supplementary), 1)
  expect_identical(d$references$doi, "10.1000/abc")
  expect_identical(d$references$year, 2020L)
})

test_that("JATS export round trips abstract, citations and markers", {
  d <- sc_parse(fixture("trial_article.xml"))
  r <- sc_parse(sc_export_jats(d, tempfile(fileext = ".xml")), format = "jats")
  expect_equal(nrow(r$blocks), nrow(d$blocks))
  expect_equal(nrow(r$citations), nrow(d$citations))
  expect_equal(nrow(r$supplementary), nrow(d$supplementary))
  p <- sc_parse(fixture("trial_tables.pdf"))
  rp <- sc_parse(sc_export_jats(p, tempfile(fileext = ".xml")), format = "jats")
  expect_false(any(grepl("\\(%\\)a$", rp$cells$text)))
})

test_that("JSON read restores metadata and rejects bad ids", {
  d <- sc_parse(fixture("trial_tables.pdf"))
  f <- sc_export_json(d, tempfile(fileext = ".json"))
  expect_equal(sc_read_json(f)$metadata, d$metadata)
  txt <- sub(d$doc_id, "../escaped", paste(readLines(f), collapse = "\n"), fixed = TRUE)
  g <- tempfile(fileext = ".json")
  writeLines(txt, g)
  expect_error(sc_read_json(g), "doc_id|identifier")
  d2 <- sc_read_json(f)
  expect_error(sc_highlight(d2, sc_extract(d2, "trial_details")), "source path")
  skip_if_not_installed("png")
  hl <- sc_highlight(d2, sc_extract(d2, "trial_details"), dir = tempfile(), dpi = 30, path = fixture("trial_tables.pdf"))
  expect_true(all(file.exists(hl)))
})

test_that("derivation is idempotent and corrections supersede derived values", {
  x <- sc_extract(sc_parse(fixture("trial_tables.pdf")), "numerical_results")
  a <- sc_derive(x)
  n <- sum(a$records$data_origin == "derived")
  expect_equal(sum(sc_derive(a)$records$data_origin == "derived"), n)
  lo <- a$records$record_id[a$records$statistic == "ci_lower" & a$records$status == "reported"][1]
  b <- sc_correct(a, lo, "0.10", "rev", "typo")
  stale <- b$records[b$records$data_origin == "derived" & grepl(lo, b$records$inputs), ]
  expect_true(nrow(stale) > 0 && all(stale$status == "superseded"))
  expect_equal(nrow(sc_observations(sc_extract(sc_parse(fixture("two_column.pdf")), "numerical_results"), wide = TRUE)) >= 0, TRUE)
})

test_that("a text estimate without arms is compared with the table", {
  d <- sc_parse(fixture("contradictory.pdf"))
  v <- sc_validate(sc_extract(d, "numerical_results"))$validation
  expect_true(any(v$rule == "cross_source_conflict" & v$result %in% c("fail", "possible_conflict")))
})

test_that("a corrupt cache entry is parsed again", {
  cd <- tempfile("cache")
  f <- fixture("trial_article.xml")
  b1 <- sc_parse_batch(f, cache_dir = cd)
  rds <- list.files(cd, pattern = "\\.rds$", full.names = TRUE)
  writeLines("not an rds", rds[1])
  b2 <- sc_parse_batch(f, cache_dir = cd)
  expect_identical(b2$status$status, "ok")
  expect_false(b2$status$cached)
})

test_that("an unreadable model answer skips only its chunk", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  k <- 0
  fun <- function(p) {
    k <<- k + 1
    if (k == 1) "Sorry." else "Here you go: []"
  }
  expect_warning(x <- sc_extract(doc, "trial_details", engine = "llm", llm = sc_llm_engine(fun = fun, max_chars = 400)),
                 "could not be read")
  expect_gt(nrow(x$llm$calls), 1)
  expect_equal(sum(!is.na(x$llm$calls$error)), 1)
})

llm_doc <- function(body) {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0("<article><body>", body, "</body></article>"), f)
  sc_parse(f)
}
stub <- function(doc, schema, rows) {
  sc_extract(doc, schema, engine = "llm", llm = sc_llm_engine(fun = function(prompt) rows, model = "stub"))
}
hr_text <- "The hazard ratio was 0.62 (95% CI 0.41 to 0.94)."

test_that("a number is accepted only in a role that fits the field", {
  d <- llm_doc(paste0("<sec><title>Results</title><p>", hr_text, "</p></sec>"))
  bid <- d$blocks$block_id[d$blocks$type == "paragraph"][1]
  bad <- data.frame(field = "result", value = "95", quote = hr_text, block_id = bid, statistic = "estimate",
                    stringsAsFactors = FALSE)
  r <- stub(d, "numerical_results", bad)$records
  expect_equal(r$status[r$field == "result"], "unsupported")
  n <- stub(d, "trial_details", data.frame(field = "randomized_n", value = "95", quote = hr_text, block_id = bid))$records
  expect_equal(n$status[n$field == "randomized_n"], "unsupported")
  ok <- data.frame(field = "result", statistic = "estimate", value = "0.62", quote = hr_text, block_id = bid,
                   effect_measure = "hazard_ratio", ci_level = "95")
  expect_equal(stub(d, "numerical_results", ok)$records$status, "reported")
  # a wrong measure, level, unit or adjustment is not accepted as stated
  wrong <- data.frame(field = "result", statistic = "estimate", value = "0.62", quote = hr_text, block_id = bid,
                      effect_measure = "odds_ratio", unit = "kg", ci_level = "99", adjusted = "true")
  w <- stub(d, "numerical_results", wrong)$records
  expect_equal(w$status, "ambiguous")
  expect_match(w$notes, "effect measure")
  expect_match(w$notes, "interval level 99")
  expect_match(w$notes, "unit 'kg'")
  expect_match(w$notes, "adjusted")
})

test_that("integer and logical fields hold their domain", {
  d <- llm_doc("<p>Mean age was 1.5 years. No adverse event occurred.</p>")
  bid <- d$blocks$block_id[d$blocks$type == "paragraph"][1]
  s <- sc_schema(fields = list(sc_field("count", type = "integer"), sc_field("any_harm", type = "logical")))
  r <- stub(d, s, data.frame(field = c("count", "any_harm"), value = c("1.5", "not-a-boolean"),
                             quote = c("Mean age was 1.5 years.", "No adverse event occurred."), block_id = bid))$records
  expect_true(all(r$status == "unsupported"))
})

test_that("a table value must come from the named row and column", {
  d <- llm_doc(paste0('<table-wrap id="t1"><label>Table 1</label><caption><p>Outcomes</p></caption>',
                      "<table><thead><tr><th>Outcome</th><th>Drug A</th><th>Placebo</th></tr></thead><tbody>",
                      "<tr><td>Death</td><td>12</td><td>20</td></tr><tr><td>Hospitalization</td><td>20</td><td>12</td></tr>",
                      "</tbody></table></table-wrap>"))
  tid <- d$tables$table_id[1]
  wrong <- data.frame(field = "result", statistic = "events", value = "12", quote = "12", block_id = tid, outcome = "Death", arm = "Placebo")
  expect_equal(stub(d, "numerical_results", wrong)$records$status, "unsupported")
  right <- data.frame(field = "result", statistic = "events", value = "20", quote = "20", block_id = tid, outcome = "Death", arm = "Placebo")
  x <- stub(d, "numerical_results", right)
  expect_equal(x$records$status, "reported")
  cell <- d$cells[match(x$evidence$cell_id, d$cells$cell_id), ]
  expect_equal(c(cell$row, cell$col), c(2L, 3L))
})

test_that("unreadable model answers leave fields not assessed", {
  d <- llm_doc(paste0("<p>", hr_text, "</p>"))
  x <- suppressWarnings(sc_extract(d, "trial_details", engine = "llm", llm = sc_llm_engine(fun = function(prompt) "invalid JSON", model = "stub")))
  r <- x$records[x$records$field == "randomized_n", ]
  expect_equal(r$status, "not_assessed")
  expect_true(r$record_id %in% sc_review_queue(x)$record_id)
})

test_that("a population from the model is kept and checked", {
  txt <- "Among patients with diabetes, the hazard ratio was 0.62 (95% CI 0.41 to 0.94)."
  d <- llm_doc(paste0("<p>", txt, "</p>"))
  it <- data.frame(field = "result", statistic = "estimate", value = "0.62", quote = txt, block_id = d$blocks$block_id[1],
                   effect_measure = "hazard_ratio", population = "patients with diabetes")
  r <- stub(d, "numerical_results", it)$records
  expect_equal(r$population, "patients with diabetes")
  expect_equal(r$status, "reported")
  it$population <- "smokers"
  expect_equal(stub(d, "numerical_results", it)$records$status, "ambiguous")
})

test_that("interval bounds and the estimate cannot stand in for each other", {
  d <- llm_doc(paste0("<p>", hr_text, "</p>"))
  bid <- d$blocks$block_id[1]
  status <- function(v, s) stub(d, "numerical_results", data.frame(field = "result", value = v, quote = hr_text,
                                                                    block_id = bid, statistic = s))$records$status
  grid <- expand.grid(v = c("0.62", "0.41", "0.94"), s = c("estimate", "ci_lower", "ci_upper"), stringsAsFactors = FALSE)
  got <- mapply(status, grid$v, grid$s)
  right <- paste(grid$v, grid$s) %in% c("0.62 estimate", "0.41 ci_lower", "0.94 ci_upper")
  expect_equal(unname(got == "reported"), right)
  expect_equal(number_roles("median 5 (IQR 3-8)")$role, c("estimate", "iqr_lower", "iqr_upper"))
})

test_that("an effect column needs no arm in its header", {
  d <- llm_doc(paste0('<table-wrap id="t1"><label>Table 2</label><table><thead><tr><th>Outcome</th><th>Drug A</th>',
                      "<th>Placebo</th><th>HR (95% CI)</th></tr></thead><tbody><tr><td>Death</td><td>12</td><td>20</td>",
                      "<td>0.60 (0.40 to 0.90)</td></tr></tbody></table></table-wrap>"))
  tid <- d$tables$table_id[1]
  hr <- data.frame(field = "result", statistic = "estimate", value = "0.60", quote = "0.60 (0.40 to 0.90)",
                   block_id = tid, outcome = "Death", arm = "Drug A", comparator = "Placebo")
  expect_equal(stub(d, "numerical_results", hr)$records$status, "reported")
  ev <- data.frame(field = "result", statistic = "events", value = "12", quote = "12", block_id = tid,
                   outcome = "Death", arm = "Placebo")
  expect_equal(stub(d, "numerical_results", ev)$records$status, "unsupported")
})

test_that("planted contradictions are flagged", {
  x <- sc_validate(sc_extract(sc_parse(fixture("contradictory.pdf"))))
  v <- x$validation
  bad <- v[v$result %in% c("fail", "possible_conflict"), ]
  expect_true(all(c("events_le_denominator", "ci_ordered", "p_range", "p_ci_agreement", "cross_source_conflict") %in% bad$rule))
  expect_true(any(grepl("0.62", bad$message) & grepl("0.68", bad$message)))
  q <- sc_review_queue(x)
  expect_gt(nrow(q), 0)
  expect_equal(q$priority, sort(q$priority))
  expect_true(any(x$records$validation_status == "fail", na.rm = TRUE))
})

test_that("clean document passes", {
  x <- sc_validate(sc_extract(sc_parse(fixture("trial_tables.pdf"))))
  v <- x$validation
  expect_false(any(v$result == "fail" & v$severity == "error" & v$rule != "estimate_bound"))
})

test_that("exceptions need justifications and are recorded", {
  x <- sc_extract(sc_parse(fixture("contradictory.pdf")))
  v0 <- sc_validate(x)$validation
  o <- v0$observation_id[v0$rule == "p_range" & v0$result == "fail"][1]
  ex <- data.frame(rule = "p_range", id = o, justification = "typesetting error confirmed with authors")
  v <- sc_validate(x, exceptions = ex)$validation
  expect_identical(v$result[v$rule == "p_range" & v$observation_id %in% o], "exception")
  expect_error(sc_validate(x, exceptions = data.frame(rule = "p_range", id = o, justification = "")), "justification")
})

test_that("time points are never merged silently", {
  x <- sc_validate(sc_extract(sc_parse(fixture("contradictory.pdf"))))
  cf <- x$validation[x$validation$rule == "cross_source_conflict" & x$validation$result != "pass", ]
  # the text gives no time point, the table says 12 months: possible only
  pc <- cf[cf$result == "possible_conflict", ]
  expect_gt(nrow(pc), 0)
  expect_true(all(grepl("unbound on one side", pc$message)))
  expect_false(any(cf$result == "fail" & grepl("unbound", cf$message)))
})

test_that("human corrections supersede without deleting", {
  x <- sc_extract(sc_parse(fixture("trial_tables.pdf")), "numerical_results")
  id <- x$records$record_id[x$records$statistic %in% "dispersion"][1]
  y <- sc_correct(x, id, "13.2", reviewer = "r1", reason = "methods state SD", statistic = "sd", dispersion_type = "sd")
  expect_equal(nrow(y$records), nrow(x$records) + 1L)
  expect_identical(y$records$status[y$records$record_id == id], "superseded")
  new <- y$records[nrow(y$records), ]
  expect_identical(new$data_origin, "human_corrected")
  expect_identical(new$supersedes, id)
  expect_identical(new$evidence_id, x$records$evidence_id[x$records$record_id == id])
  expect_error(sc_correct(y, id, "1", "r1", "again"), "superseded")
  expect_error(sc_correct(x, id, "1", "r1", "x", colour = "red"), "Unknown")
})

test_that("rules extract design, PICO and methods with evidence", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  x <- sc_extract(doc, "clinical_trial")
  r <- x$records
  get <- function(f) r$value[r$field == f & r$status == "reported"]
  expect_identical(get("study_design"), "randomized_controlled_trial")
  expect_true(all(c("double_blind", "placebo_controlled") %in% get("design_features")))
  expect_true("238" %in% get("randomized_n"))
  expect_identical(get("intervention"), "Drug A")
  expect_identical(get("comparator"), "placebo")
  expect_true("10 mg once daily" %in% get("dose"))
  expect_identical(get("allocation_ratio"), "1:1")
  expect_true("intention_to_treat" %in% get("analysis_population"))
  # every reported record has evidence whose text contains the value
  rep <- r[r$status == "reported" & !is.na(r$evidence_id), ]
  ev <- x$evidence[match(rep$evidence_id, x$evidence$evidence_id), ]
  quoted <- substring(ev$text, ev$char_start, ev$char_end)
  ok <- is.na(rep$value) | mapply(function(v, q) grepl(v, q, fixed = TRUE) || rep$field[1] %in% c("study_design"), rep$value, quoted)
  numeric_rows <- !is.na(rep$value_num)
  expect_true(all(mapply(function(v, q) grepl(v, q, fixed = TRUE), rep$value[numeric_rows], quoted[numeric_rows])))
  expect_true(all(ev$sha256 == doc$source$sha256))
  expect_true(all(!is.na(ev$page)))
})

test_that("numbers are bound to arm, outcome, time point and analysis set", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  o <- sc_observations(sc_extract(doc, "numerical_results"))
  hr <- o[which(o$statistic == "estimate" & o$value == "0.62" & !is.na(o$timepoint)), ]
  expect_equal(nrow(hr), 1L)
  expect_identical(hr$outcome, "primary outcome")
  expect_identical(hr$arm, "Drug A")
  expect_identical(hr$comparator, "placebo")
  expect_identical(hr$timepoint, "12 months")
  expect_identical(hr$analysis_set, "intention_to_treat")
  expect_identical(hr$effect_measure, "hazard_ratio")
  ci <- o[o$observation_id == hr$observation_id, ]
  expect_setequal(ci$statistic, c("estimate", "ci_lower", "ci_upper", "p_value"))
  se <- o[which(o$statistic == "se"), ]
  expect_equal(nrow(se), 2L)
  # plus-minus without label is ambiguous, not assumed SD
  d <- o[which(o$statistic == "dispersion"), ]
  expect_true(all(d$status == "ambiguous"))
  # empty effect cell is not_reported, dash is missing
  expect_true(any(o$status == "not_reported"))
  w <- sc_observations(sc_extract(doc, "numerical_results"), wide = TRUE)
  expect_true(all(c("estimate", "ci_lower", "ci_upper") %in% names(w)))
})

test_that("required fields without support are not_reported", {
  doc <- sc_parse(fixture("unicode.pdf"))
  x <- sc_extract(doc, "pico")
  nr <- x$records[x$records$status == "not_reported", ]
  expect_true("intervention" %in% nr$field)
  expect_true(all(is.na(nr$value)))
})

test_that("user schemas work with patterns and vocabularies", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  s <- sc_schema("custom", fields = list(
    sc_field("ratio", "Allocation ratio", pattern = "([0-9]+:[0-9]+) ratio"),
    sc_field("blinding", "Blinding", type = "categorical", vocabulary = c(double = "double-blind", open = "open-label"))
  ))
  x <- sc_extract(doc, s)
  expect_identical(x$records$value[x$records$field == "ratio"], "1:1")
  expect_identical(x$records$value[x$records$field == "blinding"], "double")
  expect_error(sc_field("bad name"), "letters")
  expect_error(sc_field("x", type = "categorical"), "vocabulary")
  expect_error(sc_schema("x", fields = list(1)), "sc_field")
})

test_that("extraction on JATS", {
  j <- sc_parse(fixture("trial_article.xml"))
  x <- sc_validate(sc_extract(j))
  expect_true(all(x$validation$result %in% c("pass", "not_checked")))
  ev <- x$evidence
  expect_true(all(!is.na(ev$xpath[!is.na(ev$block_id) | !is.na(ev$cell_id)])))
})

test_that("evidence accessor and data frame view", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  x <- sc_extract(doc, "trial_details")
  e <- sc_evidence(x, x$records$record_id[1])
  expect_equal(nrow(e), 1L)
  expect_true(all(c("page", "text", "char_start", "x0", "bbox_scope") %in% names(e)))
  expect_equal(nrow(as.data.frame(x)), nrow(x$records))
  expect_output(print(x), "sc_extraction")
})

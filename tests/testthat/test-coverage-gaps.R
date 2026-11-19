test_that("upside-down pages are detected and parse like upright ones", {
  d <- sc_parse(fixture("rotated180.pdf"))
  up <- sc_parse(fixture("trial_tables.pdf"))
  expect_equal(d$pages$rotation, 180)
  expect_equal(d$pages$rotation_status, "detected")
  expect_equal(d$pages$rotation_source, "rotate_entry")
  expect_equal(d$tables$label, up$tables$label)
  expect_equal(sc_table_to_matrix(d, d$tables$table_id[1]), sc_table_to_matrix(up, up$tables$table_id[1]))
  ev <- sc_evaluate(d, truth("trial_tables.pdf"))
  expect_equal(metric(ev, "table_cell_accuracy"), 1)
  # upright pages stay at 0 degrees
  expect_true(all(up$pages$rotation == 0))
})

test_that("sc_to_pdf_space maps boxes back to PDF user space", {
  up <- sc_parse(fixture("trial_tables.pdf"))
  rot <- sc_parse(fixture("rotated180.pdf"))
  a <- sc_to_pdf_space(up, up$tables)
  b <- sc_to_pdf_space(rot, rot$tables)
  expect_true(all(a$pdf_space_status == "converted"))
  # upright: y flips about the page height
  h <- up$pages$height[1]
  expect_equal(a$pdf_y1, h - up$tables$y0, tolerance = 1e-6)
  # 180 degrees: x flips too, and the box is the mirror of the upright one
  w <- rot$pages$width[1]
  expect_equal(b$pdf_x0, w - rot$tables$x1, tolerance = 1e-6)
  expect_equal(b$pdf_y0, rot$tables$y0, tolerance = 1e-6)
})

test_that("OCR words go through the full layout pipeline", {
  skip_if_not_installed("tesseract")
  skip_on_cran()
  d <- sc_parse(fixture("scanned_tables.pdf"), config = sc_config(ocr = "auto"))
  expect_equal(d$tables$label, c("Table 1", "Table 2"))
  expect_true(all(c("methods", "results") %in% d$sections$label))
  expect_true(all(startsWith(d$tables$method, "ocr_tesseract")))
  expect_true(all(startsWith(d$cells$status, "ocr_")))
  expect_true(all(d$spans$source == "ocr_tesseract"))
})

test_that("PDF header labels get row spans", {
  d <- sc_parse(fixture("trial_tables.pdf"))
  c1 <- d$cells[d$cells$table_id == d$tables$table_id[1] & d$cells$is_header, ]
  expect_equal(c1$row_span[c1$text == "Characteristic"], 2L)
  expect_equal(c1$row_span[c1$text == "P value"], 2L)
  expect_equal(c1$col_span[c1$text == "Treatment group"], 2L)
  ev <- sc_evaluate(d, truth("trial_tables.pdf"))
  expect_equal(metric(ev, "table_spans"), 1)
})

test_that("populations are bound to results and keep apart in conflicts", {
  expect_equal(unique(result_records("Among patients with diabetes, the hazard ratio for death was 0.62 (95% CI 0.45 to 0.85).")$population),
               "patients with diabetes")
  expect_equal(unique(result_records("In the subgroup of current smokers, the hazard ratio for death was 0.90 (95% CI 0.70 to 1.15).")$population),
               "current smokers")
  # analysis sets and bare counts are not populations
  expect_true(all(is.na(result_records("In the intention-to-treat population, the hazard ratio for death was 0.78 (95% CI 0.62 to 0.98).")$population)))
  expect_true(all(is.na(result_records("Death occurred in 24 of 120 patients (20.0%) in the Drug A group.")$population)))
  # a subgroup value next to the overall value is not a conflict
  d <- jats_text(c("Among patients with diabetes, the hazard ratio for death with Drug A versus placebo was 0.62 (95% CI 0.45 to 0.85).",
                   "Overall, the hazard ratio for death with Drug A versus placebo was 0.80 (95% CI 0.65 to 0.98)."))
  v <- sc_validate(sc_extract(d, "numerical_results"))
  ch <- v$validation
  expect_false(any(grepl("conflict", ch$rule) & ch$result == "fail"))
  expect_false(any(v$records$status == "inconsistent"))
  expect_true("population" %in% names(sc_observations(v)))
})

test_that("values failing an error rule are inconsistent and not derived from", {
  x <- sc_extract(sc_parse(fixture("contradictory.pdf")), "clinical_trial")
  v <- sc_validate(x)
  expect_true(any(v$records$status == "inconsistent"))
  bad <- v$records$record_id[v$records$status == "inconsistent"]
  failed <- unlist(strsplit(v$validation$record_ids[v$validation$result == "fail" & v$validation$severity == "error"], ","))
  expect_true(all(bad %in% failed))
  # validating again gives the same statuses
  expect_equal(sc_validate(v)$records$status, v$records$status)
  dv <- sc_derive(v)
  der <- dv$records[dv$records$data_origin == "derived", , drop = FALSE]
  expect_gt(nrow(der), 0)
  src <- unlist(strsplit(der$inputs[!is.na(der$inputs)], "[;, ]+"))
  expect_false(any(src %in% bad))
})

test_that("abbreviated measures not defined in the document are ambiguous", {
  r <- result_records("Heart rate: HR 72 (95% CI 70 to 74) beats per minute.")
  expect_true(all(r$status == "ambiguous"))
  expect_true(any(grepl("not defined", r$notes)))
  r2 <- result_records("The hazard ratio (HR) was reported. HR 0.62 (95% CI 0.45 to 0.85).")
  expect_false(any(r2$status == "ambiguous"))
})

test_that("batches can keep documents in the cache only", {
  files <- c(fixture("single_column.pdf"), fixture("trial_article.xml"), "missing.pdf")
  expect_error(sc_parse_batch(files, keep = FALSE), "cache_dir")
  b <- sc_parse_batch(files, cache_dir = tempfile(), keep = FALSE)
  expect_true(all(vapply(b$documents, is.null, logical(1))))
  expect_equal(b$status$status, c("ok", "ok", "failed"))
  d <- sc_batch_document(b, "trial_article.xml")
  expect_s3_class(d, "sc_document")
  expect_equal(d$doc_id, b$status$doc_id[2])
  expect_error(sc_batch_document(b, 3), "failed")
})

test_that("arm names with pattern characters do not break matching", {
  x <- "drug a {n = 120) [x]|$^.*+?\\"
  expect_true(grepl(regex_escape(x), x, perl = TRUE))
  expect_true(grepl(regex_escape(x), x))
})

test_that("GriTS-Con and TEDS give the known values", {
  a <- matrix(letters[1:12], 4, 3, byrow = TRUE)
  expect_equal(grits_con(a, a), 1)
  expect_equal(grits_con(a, a[-2, ]), 18 / 21)
  cells <- function(rows) do.call(rbind, lapply(seq_along(rows), function(r) {
    data.frame(row = r, col = seq_along(rows[[r]]), row_span = 1L, col_span = 1L, text = rows[[r]], is_header = FALSE)
  }))
  t1 <- table_tree(cells(list(c("a", "b"), c("c", "d"))))
  t2 <- table_tree(cells(list(c("a", "b", "c", "d"))))
  # delete one tr, then move c and d under the other: 1 + 2 edits
  expect_equal(tree_edit_distance(t1, t2), 3)
  expect_equal(teds(cells(list(c("a", "b"))), cells(list(c("a", "b")))), 1)
})

test_that("evaluation reports text, detection and table structure metrics", {
  ev <- sc_evaluate(sc_parse(fixture("trial_tables.pdf")), truth("trial_tables.pdf"))
  expect_equal(metric(ev, "character_error_rate"), 0)
  expect_equal(metric(ev, "table_detection_recall_vertical"), 1)
  expect_equal(metric(ev, "table_detection_precision_vertical"), 1)
  expect_equal(metric(ev, "table_grits_con"), 1)
  expect_equal(metric(ev, "table_teds"), 1)
})

test_that("extraction is scored against planted values", {
  x <- sc_extract(sc_parse(fixture("trial_tables.pdf")), "clinical_trial")
  e <- sc_evaluate_extraction(x, truth("trial_tables.pdf"))
  # the plus-minus value without an SD or SE label stays ambiguous, so it is
  # a candidate but not accepted output
  expect_equal(metric(e, "value_recall"), 60 / 61)
  expect_equal(metric(e, "candidate_value_recall"), 1)
  expect_equal(metric(e, "binding_accuracy"), 1)
  expect_gt(metric(e, "value_precision"), 0.9)
  expect_error(sc_evaluate_extraction(x, list()), "no `extraction`")
})

test_that("sc_benchmark reports time and memory", {
  b <- sc_benchmark(fixture("single_column.pdf"), times = 1)
  expect_equal(b$pages, 2L)
  expect_true(b$seconds > 0)
  expect_true(is.finite(b$peak_mb))
})

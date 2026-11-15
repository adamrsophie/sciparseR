test_that("fixtures parse to their ground truth", {
  for (f in c("single_column.pdf", "two_column.pdf", "broken_order.pdf", "trial_tables.pdf",
              "contradictory.pdf", "rotated.pdf", "unicode.pdf", "figure_equation.pdf", "scanned.pdf")) {
    ev <- sc_evaluate(sc_parse(fixture(f)), truth(f))
    expect_gte(metric(ev, "block_recall"), 1 - 1e-9, label = paste(f, "block recall"))
    if (length(metric(ev, "reading_order_pairs"))) {
      expect_equal(metric(ev, "reading_order_pairs"), 1, label = paste(f, "reading order"))
    }
    expect_equal(metric(ev, "section_f1"), 1, label = paste(f, "sections"))
    if (length(metric(ev, "table_cell_accuracy"))) {
      expect_equal(metric(ev, "table_cell_accuracy"), 1, label = paste(f, "cells"))
      expect_equal(metric(ev, "table_header_rows"), 1, label = paste(f, "header rows"))
      expect_equal(metric(ev, "table_spans"), 1, label = paste(f, "spans"))
    }
  }
})

test_that("document carries provenance", {
  doc <- sc_parse(fixture("single_column.pdf"))
  expect_s3_class(doc, "sc_document")
  expect_match(doc$source$sha256, "^[0-9a-f]{64}$")
  expect_identical(doc$doc_id, paste0("doc-", substr(doc$source$sha256, 1, 16)))
  expect_identical(doc$schema_version, "1.0.0")
  expect_false(anyDuplicated(doc$blocks$block_id) > 0)
  expect_true(all(c("pdftools", "poppler") %in% c(doc$processing$backend$name, names(doc$processing$backend))))
  expect_identical(sc_parse(fixture("single_column.pdf"))$blocks$block_id, doc$blocks$block_id)
})

test_that("headers, footers, hyphenation and continuations", {
  doc <- sc_parse(fixture("single_column.pdf"))
  expect_true(all(c("header", "page_number") %in% doc$blocks$type))
  txt <- sc_text(doc)
  expect_match(txt, "randomized", fixed = TRUE)
  expect_false(grepl("random- ized", txt, fixed = TRUE))
  expect_false(grepl("Journal of Synthetic Medicine", txt, fixed = TRUE))
  expect_equal(sum(!is.na(doc$blocks$continues)), 1L)
  expect_equal(nrow(doc$footnotes), 1L)
})

test_that("rotation, image-only pages and Unicode", {
  rot <- sc_parse(fixture("rotated.pdf"))
  expect_equal(rot$pages$rotation[1], 90L)
  sc <- sc_parse(fixture("scanned.pdf"))
  expect_true(sc$pages$image_only[2])
  expect_identical(sc$pages$ocr_status[2], "needed")
  expect_true("image_only_page" %in% sc$warnings$code)
  u <- sc_text(sc_parse(fixture("unicode.pdf")))
  expect_match(u, "α")
  expect_match(u, "≤")
})

test_that("figures, equations and references", {
  fe <- sc_parse(fixture("figure_equation.pdf"))
  expect_equal(nrow(sc_figures(fe)), 1L)
  expect_identical(sc_figures(fe)$label, "Figure 1")
  eq <- sc_equations(fe)
  expect_identical(eq$label, "(1)")
  expect_true(is.na(eq$latex))
  expect_identical(eq$latex_status, "not_reconstructed")
  sc <- sc_parse(fixture("single_column.pdf"))
  r <- sc_references(sc)
  expect_equal(nrow(r), 3L)
  expect_true("10.1000/jsm.2020.001" %in% r$doi)
  expect_true("PMC7654321" %in% r$pmcid)
  ci <- sc_citations(sc)
  expect_true(all(r$ref_id %in% ci$ref_id))
  expect_true(all(ci$status == "linked"))
})

test_that("accessors and search", {
  doc <- sc_parse(fixture("two_column.pdf"))
  expect_equal(nrow(sc_pages(doc)), max(doc$blocks$page))
  expect_true(all(sc_blocks(doc, type = "paragraph")$type == "paragraph"))
  hits <- sc_find(doc, "column")
  expect_gt(nrow(hits), 0)
  loc <- sc_source(doc, hits$element_id[1])
  expect_equal(loc$page, 1L)
  expect_false(is.na(loc$x0))
  expect_error(sc_parse("does-not-exist.pdf"), "not found")
})

test_that("stream reading order differs from geometric order", {
  geo <- sc_parse(fixture("broken_order.pdf"))
  str <- sc_parse(fixture("broken_order.pdf"), config = sc_config(reading_order = "stream"))
  expect_equal(metric(sc_evaluate(geo, truth("broken_order.pdf")), "reading_order_pairs"), 1)
  expect_lt(metric(sc_evaluate(str, truth("broken_order.pdf")), "reading_order_pairs"), 1)
})

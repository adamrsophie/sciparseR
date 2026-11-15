test_that("JSON round trip restores documents and extractions", {
  for (f in c("trial_tables.pdf", "trial_article.xml")) {
    doc <- sc_parse(fixture(f))
    p <- sc_export_json(doc, tempfile(fileext = ".json"))
    expect_false(grepl(dirname(fixture(f)), paste(readLines(p), collapse = ""), fixed = TRUE))
    d2 <- sc_read_json(p)
    expect_s3_class(d2, "sc_document")
    for (e in c("blocks", "cells", "tables", "references", "sections")) expect_identical(d2[[e]], doc[[e]], label = paste(f, e))
    x <- sc_validate(sc_extract(doc))
    x2 <- sc_read_json(sc_export_json(x, tempfile(fileext = ".json")))
    expect_identical(x2$records, x$records)
    expect_identical(x2$evidence, x$evidence)
  }
})

test_that("JSON matches the published schema", {
  sch <- jsonlite::fromJSON(system.file("schema", "sc_document-1.0.0.json", package = "sciparseR"), simplifyVector = FALSE)
  doc <- sc_parse(fixture("trial_tables.pdf"))
  j <- jsonlite::fromJSON(sc_export_json(doc, tempfile()), simplifyVector = FALSE)
  expect_true(all(unlist(sch$required) %in% names(j)))
  for (e in c("blocks", "cells")) {
    expect_true(all(names(j[[e]][[1]]) %in% names(sch$properties[[e]]$items$properties)))
  }
})

test_that("Markdown, CSV, Parquet, JATS, evidence and table exports", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  md <- readLines(sc_export_markdown(doc, tempfile(fileext = ".md")))
  expect_true(any(grepl("^\\| Outcome", md)))
  expect_true(any(grepl("^## ", md)))
  csv <- sc_export_csv(doc, file.path(tempdir(), "csv-out"))
  expect_true(all(file.exists(csv)))
  expect_equal(nrow(utils::read.csv(csv[basename(csv) == "cells.csv"])), nrow(doc$cells))
  jf <- sc_export_jats(doc, tempfile(fileext = ".xml"))
  back <- sc_parse(jf, format = "jats")
  expect_equal(nrow(back$tables), nrow(doc$tables))
  expect_true(any(back$cells$col_span == 2L))
  x <- sc_extract(doc)
  ev <- utils::read.csv(sc_export_evidence(x, tempfile(fileext = ".csv")))
  expect_equal(nrow(ev), nrow(x$records))
  expect_true("evidence_char_start" %in% names(ev))
  tf <- sc_export_tables(doc, file.path(tempdir(), "tab-out"))
  expect_equal(length(tf), nrow(doc$tables) + 1L)
  skip_if_not_installed("arrow")
  pq <- sc_export_parquet(x, file.path(tempdir(), "pq-out"))
  expect_equal(nrow(arrow::read_parquet(pq[1])), nrow(x$records))
})

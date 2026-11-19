eval_doc <- function(body) {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0("<article><body>", body, "</body></article>"), f)
  sc_parse(f)
}
m_of <- function(e, n) e$value[e$metric == n]

test_that("extraction scores accepted output apart from candidates", {
  f <- system.file("extdata", "trial_tables.pdf", package = "sciparseR")
  x <- sc_extract(sc_parse(f))
  tr <- jsonlite::fromJSON(sub("\\.pdf$", ".truth.json", f), simplifyVector = FALSE)
  y <- x
  y$records$status[] <- "unsupported"
  e <- sc_evaluate_extraction(y, tr)
  expect_equal(m_of(e, "value_recall"), 0)
  expect_equal(m_of(e, "candidate_value_recall"), 1)
  # empty outcomes are not right
  y <- x
  y$records$outcome[] <- ""
  expect_lt(m_of(sc_evaluate_extraction(y, tr), "binding_accuracy"), 0.5)
  # every supplied context field is scored
  for (k in c("population", "unit", "adjusted", "ci_level", "p_operator")) {
    t2 <- tr
    t2$extraction <- lapply(t2$extraction, function(z) {
      z[[k]] <- switch(k, adjusted = FALSE, ci_level = 95, p_operator = "<", "intended")
      z
    })
    y <- x
    y$records[[k]] <- switch(k, adjusted = TRUE, ci_level = 12, p_operator = ">", "wrong")
    expect_lt(m_of(sc_evaluate_extraction(y, t2), "binding_accuracy"), 1)
  }
})

test_that("block matching is one to one and finds leading errors", {
  z <- eval_doc("<p>Repeated statement with more than forty characters for the matching algorithm.</p>")
  txt <- z$blocks$text[z$blocks$type == "paragraph"][1]
  e <- sc_evaluate(z, list(blocks = list(list(type = "paragraph", text = txt), list(type = "paragraph", text = txt))))
  expect_equal(m_of(e, "block_recall"), 0.5)
  original <- paste(rep("a long scientific sentence with exact numeric results", 4), collapse = " ")
  z <- eval_doc(paste0("<p>X", substr(original, 2, nchar(original)), "</p>"))
  e <- sc_evaluate(z, list(blocks = list(list(type = "paragraph", text = original))))
  expect_equal(m_of(e, "block_recall"), 1)
  expect_equal(m_of(e, "character_error_rate"), 1 / nchar(original))
})

test_that("table detection uses area IoU and names the vertical variant", {
  f <- system.file("extdata", "trial_tables.pdf", package = "sciparseR")
  z <- sc_parse(f)
  z$tables <- z$tables[1, , drop = FALSE]
  z$tables[, c("x0", "y0", "x1", "y1")] <- list(0, 0, 100, 100)
  z$cells <- z$cells[z$cells$table_id == z$tables$table_id[1], , drop = FALSE]
  tr <- list(tables = list(list(label = z$tables$label[1], page = z$tables$page[1], x0 = 0, x1 = 151,
                                top = 0, bottom = 75, cells = list())))
  # area IoU 7500 / 13825 = 0.54; the product of axis IoUs would be 0.497
  expect_equal(m_of(sc_evaluate(z, tr), "table_detection_recall"), 1)
  tr$tables[[1]]$x0 <- tr$tables[[1]]$x1 <- NULL
  e <- sc_evaluate(z, tr)
  expect_equal(m_of(e, "table_detection_recall_vertical"), 1)
  expect_false("table_detection_recall" %in% e$metric)
})

test_that("TEDS and GriTS-Con match their reference implementations", {
  cells <- function(txt) data.frame(row = 1L, col = 1L, row_span = 1L, col_span = 1L, text = txt, is_header = FALSE)
  expect_equal(teds(cells("a"), cells("b")), 2 / 3)
  expect_equal(teds(cells("A"), cells("a")), 2 / 3)
  expect_equal(teds(cells("a"), cells("a")), 1)
  expect_equal(grits_con(matrix("tide", 1, 1), matrix("diet", 1, 1)), 0.25)
  # values from Python difflib.SequenceMatcher
  expect_equal(seqmatch_size("abxcd", "abcd"), 4L)
  expect_equal(seqmatch_size("tide", "diet"), 1L)
  expect_equal(seqmatch_size("", "abc"), 0L)
})

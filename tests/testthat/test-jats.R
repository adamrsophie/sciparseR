test_that("JATS keeps semantic structure", {
  j <- sc_parse(fixture("trial_article.xml"))
  expect_identical(j$source$format, "jats")
  expect_false(is.na(j$metadata$title))
  expect_false(is.na(j$metadata$doi))
  expect_gte(nrow(sc_sections(j)), 4L)
  cl <- j$cells
  expect_true(any(cl$col_span == 2L & cl$text == "Treatment group"))
  expect_true(any(cl$row_span == 2L))
  eq <- sc_equations(j)
  expect_identical(eq$latex_status, "source_markup")
  expect_match(eq$latex, "beta", fixed = TRUE)
  expect_false(is.na(eq$mathml))
  expect_equal(nrow(sc_references(j)), 3L)
  expect_true(all(sc_citations(j)$status == "linked"))
  expect_equal(nrow(j$supplementary), 1L)
  expect_true(all(is.na(j$blocks$x0)))
  expect_true(all(!is.na(j$blocks$xpath)))
})

test_that("JATS reading refuses network and entities", {
  f <- tempfile(fileext = ".xml")
  writeLines(c('<?xml version="1.0"?>', '<!DOCTYPE article [<!ENTITY x SYSTEM "file:///etc/hosts">]>',
               '<article><front><article-meta><title-group><article-title>&x;</article-title>',
               '</title-group></article-meta></front><body><p>t</p></body></article>'), f)
  d <- suppressWarnings(tryCatch(sc_parse(f, format = "jats"), error = function(e) NULL))
  if (!is.null(d)) expect_false(grepl("localhost", if (is.na(d$metadata$title)) "" else d$metadata$title, fixed = TRUE))
})

test_that("PMC fetching is opt in", {
  expect_error(sc_fetch_pmc("PMC123", dest = tempdir(), allow_network = FALSE), "network")
})

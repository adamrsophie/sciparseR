nr <- sc_config(render = FALSE, tables = FALSE, figures = FALSE)

test_that("each page keeps its own rotation source", {
  d <- sc_parse(layout_fixture("mixed_rotation_sources.pdf"), config = nr)
  expect_equal(d$pages$rotation, c(180L, 180L))
  expect_equal(d$pages$rotation_source, c("rotate_entry", "content"))
  z <- sc_to_pdf_space(d, d$spans)
  p2 <- z[z$page == 2 & z$text == "This", ]
  expect_equal(p2$pdf_x0, p2$x0)
  expect_true(all(z$pdf_space_status == "converted"))
})

test_that("a square page with /Rotate 90 reads in order", {
  d <- sc_parse(layout_fixture("square_rotate_90.pdf"), config = nr)
  expect_equal(d$pages$rotation, 90L)
  expect_equal(d$pages$rotation_source, "rotate_entry")
  expect_match(sc_text(d), "This page contains several words on each line\\.\\s+This line establishes the reading direction\\.\\s+Results were observed in all study participants\\.")
})

test_that("render = FALSE renders no page", {
  n <- 0L
  env <- environment()
  trace("pdf_render_page", tracer = bquote(assign("n", get("n", envir = .(env)) + 1L, envir = .(env))),
        where = asNamespace("pdftools"), print = FALSE)
  on.exit(untrace("pdf_render_page", where = asNamespace("pdftools")))
  invisible(sc_parse(layout_fixture("upright.pdf"), config = nr))
  expect_equal(n, 0L)
  # the control: a render with figures does call it
  invisible(sc_parse(layout_fixture("side_by_side_figures.pdf")))
  expect_gt(n, 0L)
})

test_that("header spans never overlap", {
  cl <- as_entity(list(row = c(1L, 2L, 3L), col = rep(1L, 3), row_span = rep(1L, 3), col_span = rep(1L, 3),
                       is_header = rep(TRUE, 3), text = c("Treatment group", "", "Drug A")), "cells")
  out <- header_row_spans(cl, 3L)
  covered <- unlist(lapply(seq_len(nrow(out)), function(i) out$row[i]:(out$row[i] + out$row_span[i] - 1L)))
  expect_false(anyDuplicated(covered) > 0)
  expect_setequal(out$text, c("Treatment group", "Drug A"))
})

test_that("a section ends with its deepest descendant", {
  b <- as_entity(list(block_id = c("b1", "b2", "b3", "b4"), type = c("heading", "heading", "heading", "paragraph"),
                      text = c("1 Methods", "1.1 Analysis", "1.1.1 Details", "Final paragraph"), page = c(1L, 1L, 2L, 5L),
                      font_size = c(14, 12, 10, 10), reading_order = 1:4), "blocks")
  expect_equal(build_sections(b)$sections$page_end, c(5L, 5L, 5L))
})

test_that("the PDF abstract keeps every paragraph up to the next heading", {
  b <- as_entity(list(block_id = paste0("ab", 1:5), type = c("heading", "paragraph", "paragraph", "heading", "paragraph"),
                      text = c("Abstract", "Background and objective.", "Results and conclusions.", "Methods", "Method details."),
                      page = rep(1L, 5), font_size = rep(10, 5), reading_order = 1:5), "blocks")
  md <- pdf_metadata(layout_fixture("upright.pdf"), b, 1L)
  expect_equal(md$abstract, "Background and objective. Results and conclusions.")
})

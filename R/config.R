#' Parser configuration
#'
#' Collects every tunable setting of the parsing pipeline. The configuration
#' is stored in the parsed document and its hash is part of the cache key,
#' so a document parsed with different settings is never served from a
#' stale cache entry.
#'
#' @param reading_order Reading order algorithm: `"xycut"` (recursive XY-cut
#'   with column detection, the default) or `"stream"` (content stream order
#'   as reported by the PDF, useful as a baseline).
#' @param detect_headers Detect repeated page headers, footers and page
#'   numbers across pages and remove them from the body text flow.
#' @param repair_hyphenation Rejoin words hyphenated at line ends. Repair is
#'   conservative: the hyphen is removed only when the joined word occurs
#'   elsewhere in the document; otherwise the hyphen is kept.
#' @param render Render pages to bitmaps to detect ruling lines, figure
#'   regions and image-only pages. Without rendering, ruled tables are found
#'   from text alignment only and figure boxes are not reported. With
#'   `render = FALSE` no page is rasterized at all: page rotation comes from
#'   the page tree and the word layout, and a rotation whose source cannot
#'   be read is reported with source `"unknown"`.
#' @param render_dpi Resolution of the analysis rendering.
#' @param tables Detect and reconstruct tables.
#' @param figures Detect figures from captions and rendered ink.
#' @param ocr OCR policy: `"never"`, `"auto"` (OCR image-only pages when the
#'   tesseract package is installed) or `"always"`.
#' @param ocr_language Tesseract language code(s), for example `"eng"`.
#' @param ocr_dpi Rendering resolution for OCR.
#' @param column_gap Minimum width of a whitespace gutter between columns,
#'   in multiples of the median font size.
#' @param segment_gap Horizontal gap, in multiples of the line height, that
#'   splits a line into separate segments (columns or table cells).
#' @param header_band Fraction of the page height at the top and bottom
#'   searched for running headers and footers.
#' @param min_table_rows Minimum number of aligned rows for a table without
#'   a caption.
#' @param join_continuations Join a table continued on the next page into
#'   one table when the continuation is labelled and the column count
#'   matches. Otherwise continuations are only marked as possible.
#'
#' @return An object of class `sc_config`.
#' @examples
#' cfg <- sc_config(render = FALSE)
#' cfg$reading_order
#' @export
sc_config <- function(reading_order = c("xycut", "stream"),
                      detect_headers = TRUE,
                      repair_hyphenation = TRUE,
                      render = TRUE,
                      render_dpi = 72,
                      tables = TRUE,
                      figures = TRUE,
                      ocr = c("never", "auto", "always"),
                      ocr_language = "eng",
                      ocr_dpi = 300,
                      column_gap = 1.2,
                      segment_gap = 1.0,
                      header_band = 0.09,
                      min_table_rows = 3L,
                      join_continuations = TRUE) {
  reading_order <- match.arg(reading_order)
  ocr <- match.arg(ocr)
  flags <- list(detect_headers = detect_headers,
                repair_hyphenation = repair_hyphenation, render = render,
                tables = tables, figures = figures,
                join_continuations = join_continuations)
  for (nm in names(flags)) {
    if (!is.logical(flags[[nm]]) || length(flags[[nm]]) != 1L || is.na(flags[[nm]])) {
      abort("`", nm, "` must be TRUE or FALSE.")
    }
  }
  nums <- list(render_dpi = render_dpi, ocr_dpi = ocr_dpi,
               column_gap = column_gap, segment_gap = segment_gap,
               header_band = header_band, min_table_rows = min_table_rows)
  for (nm in names(nums)) {
    v <- nums[[nm]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v <= 0) {
      abort("`", nm, "` must be a single positive number.")
    }
  }
  if (header_band >= 0.5) abort("`header_band` must be below 0.5.")
  structure(c(list(reading_order = reading_order, ocr = ocr,
                   ocr_language = ocr_language), flags, nums),
            class = "sc_config")
}

#' @export
print.sc_config <- function(x, ...) {
  cat("<sc_config>\n")
  for (nm in names(x)) cat("  ", nm, ": ", format(x[[nm]]), "\n", sep = "")
  invisible(x)
}

config_hash <- function(config) {
  substr(hash_object(unclass(config)), 1L, 16L)
}

#' Tables
#'
#' `sc_tables()` lists detected tables with labels, captions, geometry,
#' header row counts and continuation links. `sc_table_cells()` returns the
#' cell grid with spans, header flags, footnote markers and missing value
#' markers. `sc_table_to_dataframe()` and `sc_table_to_matrix()` convert one
#' table (joined with its continuations when the join is certain);
#' `sc_table_caption()` and `sc_table_footnotes()` return its caption and
#' footnotes.
#'
#' Spanning cells are stored once with `row_span`/`col_span`; in the
#' matrix and data frame views the text fills only the first covered
#' position unless `fill_spans = TRUE`. Multi-row headers are combined into
#' column names with `" | "`.
#'
#' @param doc An `sc_document`.
#' @param table Table id or label (for example `"Table 2"`).
#' @param header Use detected header rows as column names.
#' @param fill_spans Repeat spanning cell text across covered positions.
#' @param join_continuations Append tables marked as certain continuations.
#' @return A data frame, a character matrix or a string.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' sc_tables(doc)[, c("table_id", "label", "n_rows", "n_cols", "n_header_rows")]
#' sc_table_to_dataframe(doc, "Table 2")
#' sc_table_footnotes(doc, "Table 1")
#' @name tables
NULL

resolve_table <- function(doc, table) {
  check_document(doc)
  check_string(table, "table")
  i <- match(table, doc$tables$table_id)
  if (is.na(i)) i <- which(tolower(doc$tables$label) == tolower(table) &
                             !doc$tables$continuation_status %in% "joined")[1]
  if (is.na(i)) abort("No table '", table, "'. Available: ", paste(doc$tables$label %|NA|% doc$tables$table_id, collapse = ", "))
  doc$tables[i, , drop = FALSE]
}

#' @rdname tables
#' @export
sc_tables <- function(doc) {
  check_document(doc)
  t <- doc$tables
  t$caption <- doc$captions$text[match(t$caption_id, doc$captions$caption_id)]
  t
}

#' @rdname tables
#' @export
sc_table_cells <- function(doc, table = NULL) {
  check_document(doc)
  if (is.null(table)) return(doc$cells)
  tb <- resolve_table(doc, table)
  doc$cells[doc$cells$table_id == tb$table_id, , drop = FALSE]
}

#' @rdname tables
#' @export
sc_table_to_matrix <- function(doc, table, fill_spans = FALSE, join_continuations = TRUE) {
  tb <- resolve_table(doc, table)
  ids <- tb$table_id
  if (join_continuations) {
    nxt <- tb$table_id
    repeat {
      k <- doc$tables$table_id[doc$tables$continuation_of %in% nxt & doc$tables$continuation_status %in% "joined"]
      if (!length(k)) break
      ids <- c(ids, k[1])
      nxt <- k[1]
    }
  }
  mats <- lapply(seq_along(ids), function(k) {
    cl <- doc$cells[doc$cells$table_id == ids[k], , drop = FALSE]
    if (k > 1L) cl <- cl[!cl$is_header, , drop = FALSE]
    if (!nrow(cl)) return(NULL)
    # every row a cell covers, so rows held only by row spans are kept
    rr <- sort(unique(unlist(mapply(function(r, n) r:(r + n - 1L), cl$row, pmax(1L, cl$row_span), SIMPLIFY = FALSE))))
    m <- matrix("", length(rr), max(cl$col + cl$col_span - 1L))
    for (i in seq_len(nrow(cl))) {
      r <- match(cl$row[i], rr)
      cols <- cl$col[i]:(cl$col[i] + cl$col_span[i] - 1L)
      rows <- r:min(length(rr), r + cl$row_span[i] - 1L)
      if (fill_spans) m[rows, cols] <- cl$text[i] else m[r, cl$col[i]] <- cl$text[i]
    }
    attr(m, "n_header") <- if (k == 1L) sum(rr %in% cl$row[cl$is_header]) else 0L
    m
  })
  mats <- Filter(Negate(is.null), mats)
  if (!length(mats)) return(matrix(character(), 0, 0))
  nc <- max(vapply(mats, ncol, integer(1)))
  nh <- attr(mats[[1]], "n_header")
  mats <- lapply(mats, function(m) if (ncol(m) < nc) cbind(m, matrix("", nrow(m), nc - ncol(m))) else m)
  out <- do.call(rbind, mats)
  attr(out, "n_header") <- nh
  out
}

#' @rdname tables
#' @export
sc_table_to_dataframe <- function(doc, table, header = TRUE, fill_spans = FALSE, join_continuations = TRUE) {
  m <- sc_table_to_matrix(doc, table, fill_spans = fill_spans, join_continuations = join_continuations)
  nh <- attr(m, "n_header") %||% 0L
  if (!length(m)) return(data.frame())
  if (header && nh > 0L) {
    hm <- sc_table_to_matrix(doc, table, fill_spans = TRUE, join_continuations = FALSE)[seq_len(nh), , drop = FALSE]
    nm <- vapply(seq_len(ncol(hm)), function(j) {
      v <- unique(hm[, j][nzchar(hm[, j])])
      if (!length(v)) paste0("V", j) else paste(v, collapse = " | ")
    }, character(1))
    body <- m[-seq_len(nh), , drop = FALSE]
  } else {
    nm <- paste0("V", seq_len(ncol(m)))
    body <- m
  }
  df <- as.data.frame(body, stringsAsFactors = FALSE)
  names(df) <- make.unique(nm)
  df
}

#' @rdname tables
#' @export
sc_table_caption <- function(doc, table) {
  tb <- resolve_table(doc, table)
  doc$captions$text[match(tb$caption_id, doc$captions$caption_id)] %|NA|% NA_character_
}

#' @rdname tables
#' @export
sc_table_footnotes <- function(doc, table) {
  tb <- resolve_table(doc, table)
  doc$footnotes[doc$footnotes$target_id %in% tb$table_id, , drop = FALSE]
}

#' Figures, equations, references and citations
#'
#' Accessors for figure regions (with captions), equations (with their
#' text, LaTeX and MathML when the source provides them, and a status that
#' says when LaTeX is unavailable), references (with parsed fields and
#' identifiers) and in-text citations (linked to references when the link
#' is certain).
#'
#' @param doc An `sc_document`.
#' @return A data frame.
#' @examples
#' doc <- sc_parse(system.file("extdata", "figure_equation.pdf", package = "sciparseR"))
#' sc_figures(doc)
#' sc_equations(doc)[, c("label", "text", "latex_status")]
#' j <- sc_parse(system.file("extdata", "trial_article.xml", package = "sciparseR"))
#' head(sc_references(j)[, c("label", "authors", "year", "doi")])
#' head(sc_citations(j))
#' @name entities
NULL

#' @rdname entities
#' @export
sc_figures <- function(doc) {
  check_document(doc)
  f <- doc$figures
  f$caption <- doc$captions$text[match(f$caption_id, doc$captions$caption_id)]
  f
}

#' @rdname entities
#' @export
sc_equations <- function(doc) {
  check_document(doc)
  doc$equations
}

#' @rdname entities
#' @export
sc_references <- function(doc) {
  check_document(doc)
  doc$references
}

#' @rdname entities
#' @export
sc_citations <- function(doc) {
  check_document(doc)
  doc$citations
}

#' Render a figure or region to PNG
#'
#' Renders the page of a figure (or any element with a bounding box) with
#' pdftools and crops it to the element's box. Coordinates are those of the
#' upright page in points.
#'
#' @param doc An `sc_document` parsed from a PDF.
#' @param id A figure id, table id or any element id with a bounding box.
#' @param file Output PNG file.
#' @param dpi Rendering resolution.
#' @param padding Padding around the box in points.
#' @param path Path of the source PDF, when the document does not hold it
#'   (a document read with [sc_read_json()]) or the file has moved. The
#'   checksum must still match.
#' @return The path of the PNG file, invisibly.
#' @examples
#' doc <- sc_parse(system.file("extdata", "figure_equation.pdf", package = "sciparseR"))
#' if (requireNamespace("png", quietly = TRUE) && nrow(sc_figures(doc))) {
#'   f <- sc_figure_crop(doc, sc_figures(doc)$figure_id[1], tempfile(fileext = ".png"))
#'   file.exists(f)
#' }
#' @export
sc_figure_crop <- function(doc, id, file, dpi = 150, padding = 4, path = NULL) {
  check_document(doc)
  if (!is.null(path)) doc$source$path <- path
  if (doc$source$format != "pdf") abort("Cropping needs a PDF document.")
  check_source_pdf(doc)
  if (!is_installed("png")) abort("The png package is needed to write PNG files.")
  loc <- sc_source(doc, id)
  if (is.na(loc$x0[1])) abort("Element ", id, " has no bounding box.")
  pg <- doc$pages[doc$pages$page == loc$page[1], ]
  bm <- pdftools::pdf_render_page(doc$source$path, page = loc$page[1], dpi = dpi, numeric = TRUE)
  s <- dpi / 72
  # rendered bitmap is in displayed (rotated) orientation, as are the
  # upright coordinates
  h <- dim(bm)[1]
  w <- dim(bm)[2]
  sx <- w / pg$width
  sy <- h / pg$height
  x0 <- max(1L, floor((loc$x0[1] - padding) * sx))
  x1 <- min(w, ceiling((loc$x1[1] + padding) * sx))
  y0 <- max(1L, floor((loc$y0[1] - padding) * sy))
  y1 <- min(h, ceiling((loc$y1[1] + padding) * sy))
  png::writePNG(bm[y0:y1, x0:x1, , drop = FALSE], file)
  invisible(file)
}

#' Highlight evidence on rendered pages
#'
#' Renders the pages that hold the given evidence (or elements) and draws
#' their bounding boxes, writing one PNG per page. Useful for reviewing
#' extracted values against the source.
#'
#' @param doc An `sc_document` parsed from a PDF.
#' @param x An `sc_extraction`, an evidence data frame from
#'   [sc_evidence()], or a character vector of element ids.
#' @param dir Output directory.
#' @param dpi Rendering resolution.
#' @param color Box color.
#' @param path Path of the source PDF, when the document does not hold it
#'   (a document read with [sc_read_json()]) or the file has moved. The
#'   checksum must still match.
#' @return The paths of the PNG files, invisibly.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "trial_details")
#' if (requireNamespace("png", quietly = TRUE)) {
#'   files <- sc_highlight(doc, x, dir = tempdir(), dpi = 40)
#'   basename(files)
#' }
#' @export
sc_highlight <- function(doc, x, dir = tempdir(), dpi = 100, color = "#d7263d", path = NULL) {
  check_document(doc)
  if (!is.null(path)) doc$source$path <- path
  if (doc$source$format != "pdf") abort("Highlighting needs a PDF document.")
  check_source_pdf(doc)
  if (!is_installed("png")) abort("The png package is needed to read and write PNG files.")
  if (inherits(x, "sc_extraction") && !identical(x$doc_id %||% doc$doc_id, doc$doc_id)) {
    abort("The extraction was made from a different document.")
  }
  loc <- if (inherits(x, "sc_extraction")) x$evidence else if (is.data.frame(x)) x else sc_source(doc, x)
  loc <- loc[!is.na(loc$page) & !is.na(loc$x0), , drop = FALSE]
  if (!nrow(loc)) abort("No evidence with a bounding box to highlight.")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  for (p in sort(unique(loc$page))) {
    pg <- doc$pages[doc$pages$page == p, ]
    bm <- pdftools::pdf_render_page(doc$source$path, page = p, dpi = dpi, numeric = TRUE)
    h <- dim(bm)[1]
    w <- dim(bm)[2]
    f <- file.path(dir, sprintf("%s-page-%03d.png", safe_file_name(doc$doc_id), p))
    lp <- loc[loc$page == p, , drop = FALSE]
    local({
      grDevices::png(f, width = w, height = h)
      dev <- grDevices::dev.cur()
      on.exit(grDevices::dev.off(dev))
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      graphics::plot.window(c(0, w), c(h, 0), xaxs = "i", yaxs = "i")
      graphics::rasterImage(bm, 0, h, w, 0)
      graphics::rect(lp$x0 * w / pg$width, lp$y0 * h / pg$height, lp$x1 * w / pg$width, lp$y1 * h / pg$height,
                     border = color, lwd = 2)
    })
    files <- c(files, f)
  }
  invisible(files)
}

# The source PDF must be at the recorded path with the recorded checksum.
check_source_pdf <- function(doc) {
  p <- doc$source$path
  if (is.null(p) || !length(p) || is.na(p[1]) || !nzchar(p[1])) {
    abort("The document has no source path (JSON exports leave it out by default). ",
          "Pass the PDF file as `path`.")
  }
  if (!file.exists(p)) abort("The source PDF is no longer at ", p, ". Pass its new location as `path`.")
  if (!identical(sha256_file(p), doc$source$sha256)) abort("The source PDF has changed since parsing (checksum differs).")
  invisible(TRUE)
}

#' Convert boxes to PDF user space
#'
#' sciparseR stores boxes in the displayed page space: PDF points with the
#' origin at the top left of the page as it is shown. Annotation tools and
#' other PDF software use PDF user space: the unrotated media box with the
#' origin at the bottom left. `sc_to_pdf_space()` undoes the page rotation
#' (when it comes from a `/Rotate` entry) and flips the y axis.
#'
#' @param doc An `sc_document` parsed from a PDF file.
#' @param x A data frame with columns `page`, `x0`, `y0`, `x1` and `y1`, such
#'   as [sc_blocks()], [sc_table_cells()] or [sc_evidence()].
#' @return `x` with columns `pdf_x0`, `pdf_y0` (lower left corner),
#'   `pdf_x1`, `pdf_y1` (upper right corner) and `pdf_space_status`:
#'   `"converted"`, `"rotation_assumed_entry"` (a page shown upside down whose
#'   rotation source could not be read; a `/Rotate` entry is assumed) or
#'   `"no_box"`.
#' @examples
#' doc <- sc_parse(system.file("extdata", "rotated.pdf", package = "sciparseR"))
#' sc_to_pdf_space(doc, sc_blocks(doc))[1, c("page", "x0", "y0", "pdf_x0", "pdf_y0")]
#' @export
sc_to_pdf_space <- function(doc, x) {
  check_document(doc)
  if (doc$source$format != "pdf") abort("PDF user space exists only for PDF documents.")
  if (!is.data.frame(x) || !all(c("page", "x0", "y0", "x1", "y1") %in% names(x))) {
    abort("`x` must be a data frame with columns page, x0, y0, x1 and y1.")
  }
  n <- nrow(x)
  out <- list(pdf_x0 = rep(NA_real_, n), pdf_y0 = rep(NA_real_, n), pdf_x1 = rep(NA_real_, n),
              pdf_y1 = rep(NA_real_, n), pdf_space_status = rep("no_box", n))
  pg <- doc$pages
  for (i in which(!is.na(x$page) & !is.na(x$x0))) {
    g <- pg[pg$page == x$page[i], , drop = FALSE]
    if (!nrow(g)) next
    rot <- g$rotation %|NA|% 0L
    src <- g$rotation_source %|NA|% "none"
    # content drawn rotated in the page stream is already in media space
    undo <- rot != 0L && src != "content"
    m <- if (undo) to_upright(x$x0[i], x$y0[i], x$x1[i], x$y1[i], rot, g$width, g$height) else
      list(x0 = x$x0[i], y0 = x$y0[i], x1 = x$x1[i], y1 = x$y1[i])
    mh <- g$media_height
    ox <- g$media_x0 %|NA|% 0
    oy <- g$media_y0 %|NA|% 0
    out$pdf_x0[i] <- ox + m$x0
    out$pdf_x1[i] <- ox + m$x1
    out$pdf_y0[i] <- oy + mh - m$y1
    out$pdf_y1[i] <- oy + mh - m$y0
    out$pdf_space_status[i] <- if (rot == 180L && src == "unknown") "rotation_assumed_entry" else "converted"
  }
  for (k in names(out)) x[[k]] <- out[[k]]
  x
}

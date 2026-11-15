#' Parse a scientific document
#'
#' Parses a born-digital PDF or a JATS XML file into an `sc_document`: a
#' versioned, provenance-aware representation with tidy tables of pages,
#' text spans, lines, blocks, sections, tables, table cells, figures,
#' equations, captions, footnotes, references, citations and supplementary
#' material.
#'
#' Every element has a stable identifier, a page number and reading order
#' where applicable, source coordinates when the backend provides them, and
#' the method and status of its extraction. Coordinates are PDF points with
#' the origin at the top-left corner of the displayed page (see
#' `vignette("document-model")`). JATS elements have no coordinates; their
#' locator is an XPath.
#'
#' @param path Path to a `.pdf` or JATS `.xml`/`.nxml` file.
#' @param format `"auto"` (from the file extension and content), `"pdf"` or
#'   `"jats"`.
#' @param config An [sc_config()] object.
#' @param id Optional source identifier stored in the document (defaults to
#'   the file name).
#'
#' @return An object of class `sc_document`.
#' @seealso [sc_parse_batch()], [sc_metadata()], [sc_blocks()], [sc_tables()]
#' @examples
#' f <- system.file("extdata", "single_column.pdf", package = "sciparseR")
#' doc <- sc_parse(f)
#' doc
#' sc_sections(doc)[, c("level", "title", "page_start")]
#' @export
sc_parse <- function(path, format = c("auto", "pdf", "jats"),
                     config = sc_config(), id = NULL) {
  check_string(path, "path")
  format <- match.arg(format)
  if (!file.exists(path)) abort("File not found: ", path)
  if (!inherits(config, "sc_config")) abort("`config` must be created by sc_config().")
  if (format == "auto") format <- detect_format(path)
  started <- now_utc()
  log <- new_log()
  parts <- switch(format,
    pdf = parse_pdf(path, config, log),
    jats = parse_jats(path, config, log)
  )
  if (format == "pdf" && config$ocr != "never") {
    parts <- apply_ocr(parts, path, config, log)
  }
  new_document(parts, path, format, config, log, started, id)
}

detect_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "pdf") return("pdf")
  if (ext %in% c("xml", "nxml", "jats")) return("jats")
  con <- file(path, "rb")
  on.exit(close(con))
  head <- readBin(con, "raw", 8L)
  if (length(head) >= 5L && identical(rawToChar(head[1:5]), "%PDF-")) return("pdf")
  if (length(head) >= 1L && rawToChar(head[1]) == "<") return("jats")
  abort("Cannot determine the format of ", basename(path), "; set `format`.")
}

new_document <- function(parts, path, format, config, log, started, id) {
  sha <- sha256_file(path)
  backend <- switch(format,
    pdf = list(name = "pdftools", version = pkg_version("pdftools"),
               poppler = tryCatch(pdftools::poppler_config()$version, error = function(e) NA_character_)),
    jats = list(name = "xml2", version = pkg_version("xml2"))
  )
  warn <- bind_entity(list(log$frame(), parts$warnings), "warnings")
  doc <- list(
    schema_version = sc_schema_version,
    doc_id = paste0("doc-", substr(sha, 1L, 16L)),
    source = list(id = id %||% basename(path), path = normalizePath(path, winslash = "/"),
                  file_name = basename(path), format = format, sha256 = sha,
                  size_bytes = file.size(path)),
    processing = list(package = "sciparseR", package_version = pkg_version("sciparseR"),
                      r_version = paste(R.version$major, R.version$minor, sep = "."),
                      backend = backend, config = unclass(config),
                      config_hash = config_hash(config), started_at = started,
                      finished_at = now_utc()),
    metadata = parts$metadata
  )
  for (ent in setdiff(names(.entity_specs), "warnings")) {
    doc[[ent]] <- as_entity(parts[[ent]], ent)
  }
  doc$warnings <- warn
  structure(doc, class = "sc_document")
}

#' @export
print.sc_document <- function(x, ...) {
  md <- x$metadata
  cat("<sc_document> ", x$doc_id, "\n", sep = "")
  cat("  source:   ", x$source$file_name, " (", x$source$format, ", sha256 ",
      substr(x$source$sha256, 1, 12), "...)\n", sep = "")
  if (!is.na(md$title %||% NA)) cat("  title:    ", strtrim(md$title, 70), "\n", sep = "")
  cat("  pages:    ", if (x$source$format == "pdf") nrow(x$pages) else "none (XML)", "\n", sep = "")
  cat("  blocks:   ", nrow(x$blocks), "   sections: ", nrow(x$sections),
      "   tables: ", nrow(x$tables), "   figures: ", nrow(x$figures), "\n", sep = "")
  cat("  equations: ", nrow(x$equations), "   references: ", nrow(x$references),
      "   citations: ", nrow(x$citations), "\n", sep = "")
  if (x$source$format == "pdf" && nrow(x$pages)) {
    img <- sum(x$pages$image_only %in% TRUE)
    if (img) cat("  image-only pages: ", img, " (OCR status: ",
                 paste(unique(x$pages$ocr_status[x$pages$image_only %in% TRUE]), collapse = ", "), ")\n", sep = "")
  }
  if (nrow(x$warnings)) cat("  warnings: ", nrow(x$warnings), " (see doc$warnings)\n", sep = "")
  cat("  parser:   ", x$processing$backend$name, " ", x$processing$backend$version,
      ", sciparseR ", x$processing$package_version, ", schema ", x$schema_version, "\n", sep = "")
  invisible(x)
}

#' @export
summary.sc_document <- function(object, ...) {
  ents <- setdiff(names(.entity_specs), "warnings")
  n <- vapply(ents, function(e) nrow(object[[e]]), integer(1))
  out <- data.frame(entity = ents, n = unname(n), stringsAsFactors = FALSE)
  print(out, row.names = FALSE)
  invisible(out)
}

#' Document accessors
#'
#' Accessors return the tidy tables of an `sc_document`.
#' `sc_metadata()` returns bibliographic metadata with the source of each
#' field; `sc_pages()` page geometry, rotation and OCR status;
#' `sc_blocks()` text blocks in reading order; `sc_sections()` the section
#' tree; `sc_text()` the reconstructed text.
#'
#' @param doc An `sc_document`.
#' @param type Optional block types to keep (for example `"paragraph"`,
#'   `"heading"`, `"caption"`, `"footnote"`, `"reference"`, `"header"`,
#'   `"footer"`, `"table"`, `"figure"`, `"equation"`, `"title"`).
#' @param page Optional page numbers to keep.
#' @return A data frame (a list for `sc_metadata()`, a string for
#'   `sc_text()`).
#' @examples
#' doc <- sc_parse(system.file("extdata", "two_column.pdf", package = "sciparseR"))
#' sc_pages(doc)
#' head(sc_blocks(doc, type = "paragraph")$text, 3)
#' cat(substr(sc_text(doc), 1, 300))
#' @name accessors
NULL

#' @rdname accessors
#' @export
sc_metadata <- function(doc) {
  check_document(doc)
  doc$metadata
}

#' @rdname accessors
#' @export
sc_pages <- function(doc) {
  check_document(doc)
  doc$pages
}

#' @rdname accessors
#' @export
sc_blocks <- function(doc, type = NULL, page = NULL) {
  check_document(doc)
  b <- doc$blocks[order(doc$blocks$reading_order), , drop = FALSE]
  if (!is.null(type)) b <- b[b$type %in% type, , drop = FALSE]
  if (!is.null(page)) b <- b[b$page %in% page, , drop = FALSE]
  rownames(b) <- NULL
  b
}

#' @rdname accessors
#' @export
sc_sections <- function(doc) {
  check_document(doc)
  doc$sections
}

#' @rdname accessors
#' @param raw If `TRUE`, return raw text of the text layer in reading order
#'   (lines kept, no hyphenation repair, headers included).
#' @param body_only If `TRUE` (default), drop running headers, footers and
#'   page numbers.
#' @export
sc_text <- function(doc, raw = FALSE, body_only = TRUE) {
  check_document(doc)
  b <- sc_blocks(doc)
  if (raw) {
    b <- b[!is.na(b$text_raw), , drop = FALSE]
    return(paste(b$text_raw, collapse = "\n\n"))
  }
  drop <- c("table", "figure")
  if (body_only) drop <- c(drop, "header", "footer", "page_number")
  b <- b[!b$type %in% drop & !is.na(b$text), , drop = FALSE]
  if (!nrow(b)) return("")
  txt <- b$text
  sep <- rep("\n\n", nrow(b))
  cont <- which(!is.na(b$continues) & b$continues %in% b$block_id)
  for (j in cont) {
    i <- match(b$continues[j], b$block_id)
    if (!is.na(i) && i < j) sep[j] <- " "
  }
  sep[1] <- ""
  out <- character()
  for (k in seq_len(nrow(b))) out <- c(out, sep[k], txt[k])
  paste(out, collapse = "")
}

#' Find text in a document
#'
#' Searches block text (and table cells) for a pattern and returns each
#' match with its location: page, block or cell, character offsets and
#' bounding box.
#'
#' @param doc An `sc_document`.
#' @param pattern Regular expression (Perl syntax) or fixed string.
#' @param fixed Treat `pattern` as a fixed string.
#' @param ignore_case Case-insensitive matching.
#' @param where `"blocks"`, `"cells"` or both.
#' @return A data frame with one row per match.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' sc_find(doc, "hazard ratio", ignore_case = TRUE)
#' @export
sc_find <- function(doc, pattern, fixed = FALSE, ignore_case = FALSE,
                    where = c("blocks", "cells")) {
  check_document(doc)
  check_string(pattern, "pattern")
  where <- match.arg(where, several.ok = TRUE)
  res <- list()
  scan <- function(text, ids, kind, pages, xp) {
    for (i in seq_along(text)) {
      if (is.na(text[i]) || !nzchar(text[i])) next
      m <- gregexpr(pattern, text[i], fixed = fixed, perl = !fixed, ignore.case = ignore_case)[[1]]
      if (m[1] < 0) next
      for (j in seq_along(m)) {
        len <- attr(m, "match.length")[j]
        res[[length(res) + 1L]] <<- data.frame(
          element_type = kind, element_id = ids[i], page = pages[i],
          match = substr(text[i], m[j], m[j] + len - 1L), char_start = m[j],
          char_end = m[j] + len - 1L, context = context_window(text[i], m[j], len),
          xpath = xp[i], stringsAsFactors = FALSE)
      }
    }
  }
  if ("blocks" %in% where) {
    b <- sc_blocks(doc)
    scan(b$text, b$block_id, "block", b$page, b$xpath)
  }
  if ("cells" %in% where && nrow(doc$cells)) {
    pg <- doc$tables$page[match(doc$cells$table_id, doc$tables$table_id)]
    scan(doc$cells$text, doc$cells$cell_id, "cell", pg, doc$cells$xpath)
  }
  if (!length(res)) {
    return(data.frame(element_type = character(), element_id = character(), page = integer(),
                      match = character(), char_start = integer(), char_end = integer(),
                      context = character(), xpath = character()))
  }
  out <- do.call(rbind, res)
  rownames(out) <- NULL
  out
}

context_window <- function(text, start, len, width = 40L) {
  a <- max(1L, start - width)
  b <- min(nchar(text), start + len - 1L + width)
  substr(text, a, b)
}

#' Locate the source of an element
#'
#' Returns where any element (block, span, line, cell, table, figure,
#' caption, footnote, equation, reference, section, citation) or an evidence
#' record came from: page, bounding box, text, XPath and the document
#' checksum, so a value can be traced back to its page region.
#'
#' @param doc An `sc_document`.
#' @param id Element identifier, or an evidence data frame returned by
#'   [sc_evidence()].
#' @return A one-row-per-id data frame.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' sc_source(doc, sc_tables(doc)$table_id[1])
#' @export
sc_source <- function(doc, id) {
  check_document(doc)
  if (is.data.frame(id)) {
    ev <- id
    return(data.frame(element_id = ev$evidence_id, element_type = "evidence",
                      page = ev$page, x0 = ev$x0, y0 = ev$y0, x1 = ev$x1, y1 = ev$y1,
                      text = ev$text, xpath = ev$xpath, sha256 = doc$source$sha256,
                      stringsAsFactors = FALSE))
  }
  keys <- list(blocks = "block_id", spans = "span_id", lines = "line_id", cells = "cell_id",
               tables = "table_id", figures = "figure_id", captions = "caption_id",
               footnotes = "footnote_id", equations = "equation_id", references = "ref_id",
               sections = "section_id", citations = "citation_id")
  rows <- lapply(id, function(one) {
    for (ent in names(keys)) {
      tab <- doc[[ent]]
      i <- match(one, tab[[keys[[ent]]]])
      if (is.na(i)) next
      r <- tab[i, , drop = FALSE]
      page <- if ("page" %in% names(r)) r$page else if (ent == "cells") doc$tables$page[match(r$table_id, doc$tables$table_id)] else
        if (ent == "sections") r$page_start else NA_integer_
      bb <- if (all(c("x0", "y0", "x1", "y1") %in% names(r))) c(r$x0, r$y0, r$x1, r$y1) else
        if (ent == "sections" || ent == "citations" || ent == "references") {
          bid <- r$block_id
          j <- match(bid, doc$blocks$block_id)
          if (is.na(j)) rep(NA_real_, 4) else unlist(doc$blocks[j, c("x0", "y0", "x1", "y1")])
        } else rep(NA_real_, 4)
      txt <- if ("text" %in% names(r)) r$text else if ("title" %in% names(r)) r$title else NA_character_
      if (ent == "citations") txt <- r$marker
      return(data.frame(element_id = one, element_type = sub("s$", "", ent), page = page,
                        x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4], text = txt,
                        xpath = if ("xpath" %in% names(r)) r$xpath else NA_character_,
                        sha256 = doc$source$sha256, stringsAsFactors = FALSE))
    }
    abort("Unknown element id: ", one)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

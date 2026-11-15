# Optional OCR through tesseract on pages rendered by pdftools. OCR words
# become spans with source "ocr_tesseract" and the engine reported word
# confidence (0 to 100). That confidence is the recognizer's own score, not
# a calibrated probability.

ocr_available <- function() is_installed("tesseract")

apply_ocr <- function(parts, path, cfg, log) {
  pages <- parts$pages
  todo <- if (cfg$ocr == "always") pages$page else
    pages$page[pages$ocr_status == "needed"]
  if (!length(todo)) return(parts)
  if (!ocr_available()) {
    pages$ocr_status[pages$page %in% todo] <- "unavailable"
    log$add("ocr", "ocr_unavailable",
            "OCR requested but the tesseract package is not installed; image-only pages have no text.",
            NA_integer_)
    parts$pages <- pages
    return(parts)
  }
  # render each page to a temporary PNG (never into the working directory)
  ocr <- tryCatch({
    eng <- tesseract::tesseract(cfg$ocr_language)
    lapply(todo, function(p) {
      pattern <- paste0(tempfile("ocr"), "_%d.%s")
      f <- pdftools::pdf_convert(path, format = "png", pages = p, dpi = cfg$ocr_dpi, filenames = pattern, verbose = FALSE)
      on.exit(unlink(f))
      tesseract::ocr_data(f, engine = eng)
    })
  }, error = function(e) e)
  if (inherits(ocr, "error")) {
    pages$ocr_status[pages$page %in% todo] <- "failed"
    log$add("ocr", "ocr_failed", conditionMessage(ocr), NA_integer_)
    parts$pages <- pages
    return(parts)
  }
  scale <- 72 / cfg$ocr_dpi
  ws <- list()
  notes <- list()
  for (k in seq_along(todo)) {
    p <- todo[k]
    d <- ocr[[k]]
    d <- d[nzchar(trimws(d$word)), , drop = FALSE]
    if (!NROW(d)) {
      pages$ocr_status[pages$page == p] <- "applied_no_text"
      next
    }
    bb <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
    ws[[length(ws) + 1L]] <- data.frame(
      page = p, stream_order = seq_len(nrow(d)), x0 = bb[, 1] * scale, y0 = bb[, 2] * scale,
      x1 = bb[, 3] * scale, y1 = bb[, 4] * scale, text = enc2utf8(d$word), space = TRUE,
      font_name = NA_character_, font_size = (bb[, 4] - bb[, 2]) * scale, bold = FALSE, italic = FALSE,
      source = "ocr_tesseract", confidence = d$confidence, stringsAsFactors = FALSE)
    pages$ocr_status[pages$page == p] <- "applied"
    med <- stats::median(d$confidence)
    if (med < 60) {
      notes[[length(notes) + 1L]] <- list("ocr", "ocr_low_confidence",
        sprintf("Median OCR word confidence %.0f is low; check the recognized text.", med), p)
    }
    notes[[length(notes) + 1L]] <- list("ocr", "ocr_applied",
      sprintf("OCR applied (tesseract, language %s, %d dpi); median word confidence %.0f.",
              cfg$ocr_language, cfg$ocr_dpi, med), p)
  }
  if (!length(ws)) {
    parts$pages <- pages
    return(parts)
  }
  # parse again with the OCR words in place of the text layer of those
  # pages, so headings, tables, figures and sections are found there too
  log2 <- new_log()
  out <- parse_pdf(path, cfg, log2, ocr_words = do.call(rbind, ws))
  log$rows <- log2$rows
  for (n in notes) log$add(n[[1]], n[[2]], n[[3]], n[[4]])
  # elements built from OCR words say so in their method
  ocr_pages <- unique(do.call(rbind, ws)$page)
  for (e in names(out)) {
    v <- out[[e]]
    if (is.data.frame(v) && all(c("page", "method") %in% names(v))) {
      ix <- v$page %in% ocr_pages & !is.na(v$method)
      v$method[ix] <- paste0("ocr_tesseract+", v$method[ix])
      out[[e]] <- v
    }
  }
  if (nrow(out$cells)) {
    on <- out$cells$table_id %in% out$tables$table_id[out$tables$page %in% ocr_pages]
    out$cells$status[on] <- paste0("ocr_", out$cells$status[on])
  }
  keep <- c("has_text_layer", "image_only", "ink_ratio", "ocr_status")
  out$pages[keep] <- pages[match(out$pages$page, pages$page), keep]
  out
}

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
  new_spans <- new_blocks <- list()
  for (k in seq_along(todo)) {
    p <- todo[k]
    d <- ocr[[k]]
    if (!NROW(d)) {
      pages$ocr_status[pages$page == p] <- "applied_no_text"
      next
    }
    bb <- do.call(rbind, lapply(strsplit(d$bbox, ","), as.numeric))
    w <- data.frame(page = p, stream_order = seq_len(nrow(d)), x0 = bb[, 1] * scale,
                    y0 = bb[, 2] * scale, x1 = bb[, 3] * scale, y1 = bb[, 4] * scale,
                    text = enc2utf8(d$word), space = TRUE, font_name = NA_character_,
                    font_size = (bb[, 4] - bb[, 2]) * scale, bold = FALSE, italic = FALSE,
                    stringsAsFactors = FALSE)
    w$span_id <- make_id("o", p, seq_len(nrow(w)))
    w$confidence <- d$confidence
    bl <- build_lines(w, cfg)
    lines <- bl$lines
    ub <- data.frame(id = paste0("l", lines$segment), x0 = lines$x0, y0 = lines$y0,
                     x1 = lines$x1, y1 = lines$y1, stringsAsFactors = FALSE)
    ord <- xy_order(ub, cfg$column_gap * median_or(lines$font_size, 10))
    seg_order <- as.integer(sub("^l", "", ord$id))
    lines <- lines[match(seg_order, lines$segment), , drop = FALSE]
    lines$leaf <- ord$leaf
    # group consecutive lines into paragraphs by vertical gap
    gap <- c(Inf, lines$y0[-1] - lines$y1[-nrow(lines)])
    grp <- cumsum(gap > 0.8 * median_or(lines$font_size, 10) | c(TRUE, diff(lines$leaf) != 0))
    for (g in unique(grp)) {
      ls <- lines[grp == g, , drop = FALSE]
      bid <- make_id("ob", p, g)
      b <- bbox_union(ls$x0, ls$y0, ls$x1, ls$y1)
      new_blocks[[length(new_blocks) + 1L]] <- list(
        block_id = bid, page = p, type = "paragraph", x0 = b[1], y0 = b[2], x1 = b[3], y1 = b[4],
        text_raw = paste(ls$text, collapse = "\n"), text = normalize_text(paste(ls$text, collapse = " ")),
        column = ls$leaf[1], method = "ocr_tesseract+layout_xycut", status = "ocr")
      bl$words$block_id[bl$words$segment %in% ls$segment] <- bid
    }
    sw <- bl$words
    new_spans[[length(new_spans) + 1L]] <- data.frame(
      span_id = sw$span_id, page = p, x0 = sw$x0, y0 = sw$y0, x1 = sw$x1, y1 = sw$y1,
      text = sw$text, font_size = NA_real_, superscript = FALSE,
      block_id = sw$block_id, source = "ocr_tesseract", confidence = sw$confidence,
      stringsAsFactors = FALSE)
    pages$ocr_status[pages$page == p] <- "applied"
    if (stats::median(d$confidence) < 60) {
      log$add("ocr", "ocr_low_confidence",
              sprintf("Median OCR word confidence %.0f is low; check the recognized text.", stats::median(d$confidence)), p)
    }
    log$add("ocr", "ocr_applied",
            sprintf("OCR applied (tesseract, language %s, %d dpi); median word confidence %.0f.",
                    cfg$ocr_language, cfg$ocr_dpi, stats::median(d$confidence)), p)
  }
  ob <- bind_entity(lapply(new_blocks, as.data.frame, stringsAsFactors = FALSE), "blocks")
  if (nrow(ob)) {
    if (cfg$ocr == "always") {
      parts$blocks <- parts$blocks[!parts$blocks$page %in% todo, , drop = FALSE]
      parts$spans <- parts$spans[!parts$spans$page %in% todo, , drop = FALSE]
    }
    all <- rbind(as_entity(parts$blocks, "blocks"), ob)
    all$tmp_order <- ifelse(is.na(all$reading_order), Inf, all$reading_order)
    all <- all[order(all$page, all$tmp_order, all$y0), , drop = FALSE]
    all$reading_order <- seq_len(nrow(all))
    all$tmp_order <- NULL
    parts$blocks <- all
    parts$spans <- bind_entity(list(parts$spans, bind_entity(new_spans, "spans")), "spans")
  }
  pages$has_text_layer <- pages$has_text_layer
  parts$pages <- pages
  parts
}

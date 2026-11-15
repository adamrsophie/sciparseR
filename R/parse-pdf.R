# PDF parsing pipeline: words, lines, marginal text, tables, figures,
# equations, reading order, blocks and document level structure.

body_font_size <- function(words) {
  if (!nrow(words)) return(10)
  sz <- round(words$font_size * 2) / 2
  w <- tapply(nchar(words$text), sz, sum)
  as.numeric(names(w)[which.max(w)])
}

upright_ink <- function(ink, rotation) {
  if (is.null(ink) || is.na(rotation) || rotation == 0L) return(ink)
  tt <- t(ink)
  if (rotation == 90L) tt[rev(seq_len(nrow(tt))), , drop = FALSE] else tt[, rev(seq_len(ncol(tt))), drop = FALSE]
}

convert_boxes <- function(df, geom, inverse = TRUE) {
  if (is.null(df) || !nrow(df) || !all(c("x0", "y0", "x1", "y1", "page") %in% names(df))) return(df)
  for (p in unique(df$page[!is.na(df$page)])) {
    g <- geom[geom$page == p, ]
    if (is.na(g$rotation) || g$rotation == 0L) next
    ix <- which(df$page == p)
    f <- if (inverse) from_upright else to_upright
    r <- f(df$x0[ix], df$y0[ix], df$x1[ix], df$y1[ix], g$rotation, g$width, g$height)
    df$x0[ix] <- r$x0
    df$y0[ix] <- r$y0
    df$x1[ix] <- r$x1
    df$y1[ix] <- r$y1
  }
  df
}

parse_pdf <- function(path, cfg, log) {
  words <- pdf_words(path)
  geom <- pdf_page_geometry(path, words)
  n_pages <- nrow(geom)
  if (nrow(words)) {
    words$span_id <- make_id("w", words$page, stats::ave(seq_len(nrow(words)), words$page, FUN = seq_along))
  } else {
    words$span_id <- character()
  }
  for (p in which(geom$rotation_status == "swapped_direction_unknown")) {
    log$add("pdf", "rotation_direction_unknown",
            "Page displayed with swapped dimensions but the rotation direction could not be determined; layout uses displayed coordinates.", p)
  }
  words <- convert_boxes(words, geom, inverse = FALSE)
  body_size <- body_font_size(words)
  upright_h <- ifelse(geom$rotation %in% c(90L, 270L), geom$media_height, geom$height)
  upright_w <- ifelse(geom$rotation %in% c(90L, 270L), geom$media_width, geom$width)
  # lines per page with globally unique segment ids
  all_words <- list()
  all_lines <- list()
  seg_off <- 0L
  row_off <- 0L
  for (p in seq_len(n_pages)) {
    wp <- words[words$page == p, , drop = FALSE]
    if (!nrow(wp)) next
    bl <- build_lines(wp, cfg)
    bl$words$segment <- bl$words$segment + seg_off
    bl$words$row <- bl$words$row + row_off
    bl$lines$segment <- bl$lines$segment + seg_off
    bl$lines$row <- bl$lines$row + row_off
    bl$lines$page <- p
    bl$lines$stream <- tapply(bl$words$stream_order, bl$words$segment, min)[as.character(bl$lines$segment)]
    seg_off <- max(bl$lines$segment)
    row_off <- max(bl$lines$row)
    all_words[[p]] <- bl$words
    all_lines[[p]] <- bl$lines
  }
  wl <- if (length(all_words)) do.call(rbind, all_words) else NULL
  ll <- if (length(all_lines)) do.call(rbind, all_lines) else NULL
  if (!is.null(ll)) {
    ll <- mark_marginal(ll, upright_h, cfg, body_size)
    ll$caption <- line_is_caption(ll$text) & is.na(ll$marginal)
  }
  if (!is.null(wl)) wl$block_id <- NA_character_
  if (!is.null(ll)) ll$block_id <- NA_character_
  vocab <- if (is.null(ll)) character() else doc_vocab(ll$text)
  pages <- geom
  pages$n_words <- vapply(seq_len(n_pages), function(p) sum(words$page == p), integer(1))
  pages$has_text_layer <- pages$n_words > 0L
  pages$image_only <- NA
  pages$ink_ratio <- NA_real_
  pages$ocr_status <- "not_needed"
  pages$n_columns <- NA_integer_
  blocks_all <- list()
  block_lines <- list()
  tables_all <- cells_all <- fns_all <- figs_all <- eqs_all <- list()
  order_counter <- 0L
  for (p in seq_len(n_pages)) {
    ink <- NULL
    if (cfg$render) {
      ink <- upright_ink(render_ink(path, p, cfg$render_dpi), geom$rotation[p])
      if (!is.null(ink)) pages$ink_ratio[p] <- mean(ink)
    }
    if (!pages$has_text_layer[p]) {
      pages$image_only[p] <- if (is.null(ink)) NA else pages$ink_ratio[p] > 0.002
      pages$ocr_status[p] <- if (isTRUE(pages$image_only[p]) || is.na(pages$image_only[p])) "needed" else "not_needed"
      if (isTRUE(pages$image_only[p])) {
        log$add("pdf", "image_only_page", "Page has ink but no text layer; text requires OCR.", p)
      } else if (is.na(pages$image_only[p])) {
        log$add("pdf", "empty_text_layer", "Page has no text layer; rendering disabled so image content is unknown.", p)
      }
      next
    }
    pages$image_only[p] <- FALSE
    lp <- ll[ll$page == p, , drop = FALSE]
    wp <- wl[wl$page == p, , drop = FALSE]
    body <- lp[is.na(lp$marginal), , drop = FALSE]
    col_left <- if (nrow(body)) min(body$x0) else 0
    col_right <- if (nrow(body)) max(body$x1) else upright_w[p]
    lp <- mark_equations(lp, col_left, col_right)
    rules <- if (is.null(ink)) data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double()) else find_rules(ink, cfg$render_dpi)
    tb <- detect_tables_page(lp, wp, rules, p, cfg, body_size, length(tables_all), log)
    used <- tb$used
    regions <- if (is.null(ink) || !cfg$figures) data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double()) else
      ink_regions(ink, wp, rules[0, , drop = FALSE], cfg$render_dpi)
    fg <- detect_figures_page(lp, regions, tb$units, used, p, cfg, upright_w[p], upright_h[p], length(figs_all))
    used <- c(used, fg$used)
    # units for reading order
    flow <- lp[is.na(lp$marginal) & !(lp$segment %in% used), , drop = FALSE]
    unit_kind <- list()
    units <- list()
    eq_groups <- unique(flow$equation[!is.na(flow$equation)])
    for (i in seq_len(nrow(flow))) {
      if (!is.na(flow$equation[i])) next
      id <- paste0("l", flow$segment[i])
      unit_kind[[id]] <- "line"
      units[[length(units) + 1L]] <- data.frame(id = id, x0 = flow$x0[i], y0 = flow$y0[i],
                                                x1 = flow$x1[i], y1 = flow$y1[i],
                                                stream = flow$stream[i], stringsAsFactors = FALSE)
    }
    eq_segs <- list()
    for (g in eq_groups) {
      ix <- which(flow$equation == g)
      id <- paste0("e", p, "_", g)
      unit_kind[[id]] <- "equation"
      eq_segs[[id]] <- flow$segment[ix]
      bb <- bbox_union(flow$x0[ix], flow$y0[ix], flow$x1[ix], flow$y1[ix])
      units[[length(units) + 1L]] <- data.frame(id = id, x0 = bb[1], y0 = bb[2], x1 = bb[3],
                                                y1 = bb[4], stream = min(flow$stream[ix]),
                                                stringsAsFactors = FALSE)
    }
    for (u in list(tb$units, fg$units)) {
      if (is.null(u)) next
      for (i in seq_len(nrow(u))) {
        unit_kind[[u$id[i]]] <- if (startsWith(u$id[i], "t-")) "table" else "figure"
        segs_in <- lp$segment[lp$x0 >= u$x0[i] - 1 & lp$x1 <= u$x1[i] + 1 & lp$y0 >= u$y0[i] - 1 & lp$y1 <= u$y1[i] + 1]
        st <- suppressWarnings(min(lp$stream[lp$segment %in% segs_in]))
        units[[length(units) + 1L]] <- data.frame(id = u$id[i], x0 = u$x0[i], y0 = u$y0[i],
                                                  x1 = u$x1[i], y1 = u$y1[i],
                                                  stream = if (is.finite(st)) st else Inf,
                                                  stringsAsFactors = FALSE)
      }
    }
    ub <- if (length(units)) do.call(rbind, units) else NULL
    if (!is.null(ub)) {
      ord <- if (cfg$reading_order == "xycut") {
        xy_order(ub, cfg$column_gap * body_size)
      } else {
        data.frame(id = ub$id[order(ub$stream, ub$y0)], leaf = 1L, stringsAsFactors = FALSE)
      }
      pages$n_columns[p] <- column_count(ub, cfg$column_gap * body_size)
    } else {
      ord <- data.frame(id = character(), leaf = integer())
    }
    raw_blocks <- build_page_blocks(ord, lp, p, body_size, upright_h[p], vocab, cfg, unit_kind)
    # marginal blocks: headers first, footers and page numbers last
    marg <- lp[!is.na(lp$marginal), , drop = FALSE]
    marg <- marg[order(marg$y0, marg$x0), , drop = FALSE]
    head_m <- marg[marg$marginal == "header", , drop = FALSE]
    foot_m <- marg[marg$marginal != "header", , drop = FALSE]
    mk_marg <- function(m) lapply(seq_len(nrow(m)), function(i) list(kind = "marginal", segs = m$segment[i], leaf = 0L))
    raw_blocks <- c(mk_marg(head_m), raw_blocks, mk_marg(foot_m))
    head_flag <- line_is_heading(lp, body_size)
    k <- 0L
    for (rb in raw_blocks) {
      k <- k + 1L
      order_counter <- order_counter + 1L
      bid <- make_id("b", p, k)
      if (rb$kind %in% c("line", "marginal")) {
        ls <- lp[match(rb$segs, lp$segment), , drop = FALSE]
        type <- if (rb$kind == "marginal") ls$marginal[1] else
          classify_block(ls, body_size, p, upright_h[p], all(head_flag[match(rb$segs, lp$segment)]))
        jl <- join_lines(ls$text, vocab, cfg$repair_hyphenation)
        if (jl$repairs > 0L) log$add("layout", "hyphenation_repaired",
                                     paste0(jl$repairs, " line end hyphen(s) removed using document vocabulary"), p, bid)
        bb <- bbox_union(ls$x0, ls$y0, ls$x1, ls$y1)
        blocks_all[[length(blocks_all) + 1L]] <- list(
          block_id = bid, page = p, type = type, x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
          text_raw = paste(ls$text, collapse = "\n"), text = normalize_text(jl$text),
          reading_order = order_counter, column = rb$leaf, font_size = stats::median(ls$font_size),
          bold = mean(ls$bold_ratio) >= 0.6, method = if (rb$kind == "marginal") "repeated_margin_text" else paste0("layout_", cfg$reading_order),
          status = if (rb$kind == "marginal") ls$marginal_status[1] else "extracted")
        block_lines[[bid]] <- ls
        wl$block_id[wl$segment %in% rb$segs] <- bid
        ll$block_id[ll$segment %in% rb$segs] <- bid
      } else if (rb$kind == "equation") {
        segs <- eq_segs[[rb$unit]]
        ls <- lp[match(segs, lp$segment), , drop = FALSE]
        ls <- ls[order(ls$x0), , drop = FALSE]
        is_lab <- grepl("^\\([0-9]{1,3}[a-z]?\\)$", trimws(ls$text))
        bb <- bbox_union(ls$x0, ls$y0, ls$x1, ls$y1)
        etxt <- normalize_text(paste(ls$text[!is_lab], collapse = " "))
        blocks_all[[length(blocks_all) + 1L]] <- list(
          block_id = bid, page = p, type = "equation", x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
          text_raw = paste(ls$text, collapse = " "), text = etxt, reading_order = order_counter,
          column = rb$leaf, font_size = stats::median(ls$font_size), bold = FALSE,
          method = "math_line_heuristic", status = "inferred")
        eqs_all[[length(eqs_all) + 1L]] <- list(
          equation_id = make_id("eq", p, length(eqs_all) + 1L), page = p,
          label = if (any(is_lab)) trimws(ls$text[is_lab][1]) else NA_character_,
          text = etxt, latex = NA_character_, mathml = NA_character_,
          latex_status = "not_reconstructed", x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
          block_id = bid, method = "math_line_heuristic")
        block_lines[[bid]] <- ls
        wl$block_id[wl$segment %in% segs] <- bid
        ll$block_id[ll$segment %in% segs] <- bid
      } else {
        u <- ub[ub$id == rb$unit, ]
        blocks_all[[length(blocks_all) + 1L]] <- list(
          block_id = bid, page = p, type = rb$kind, x0 = u$x0, y0 = u$y0, x1 = u$x1, y1 = u$y1,
          text_raw = NA_character_, text = NA_character_, reading_order = order_counter,
          column = rb$leaf, parent_id = rb$unit, method = "layout_unit", status = "extracted")
      }
    }
    for (t in tb$tables) {
      t$caption_line <- if (is.na(t$caption_segment)) NA_character_ else lp$text[lp$segment == t$caption_segment]
      tables_all[[length(tables_all) + 1L]] <- t
    }
    cells_all[[p]] <- tb$cells
    fns_all[[p]] <- tb$footnotes
    if (length(fg$figures)) {
      for (f in fg$figures) figs_all[[length(figs_all) + 1L]] <- f
    }
    if (!is.null(tb$cells) && nrow(tb$cells)) {
      used_w <- wl$span_id %in% unlist(lapply(tb$cells$span_ids, split_ids))
      wl$block_id[used_w] <- NA_character_
    }
  }
  blocks <- bind_entity(lapply(blocks_all, function(b) as.data.frame(b, stringsAsFactors = FALSE)), "blocks")
  blocks <- link_continuations(blocks)
  sec <- build_sections(blocks)
  blocks <- sec$blocks
  sections <- sec$sections
  rf <- build_references(blocks, block_lines, sections)
  blocks <- rf$blocks
  references <- rf$references
  # captions
  cap_blocks <- blocks[blocks$type == "caption", , drop = FALSE]
  captions <- as_entity(list(
    caption_id = sprintf("cap-%03d", seq_len(nrow(cap_blocks))),
    label = vapply(cap_blocks$text, caption_label, character(1)),
    text = cap_blocks$text, page = cap_blocks$page, x0 = cap_blocks$x0,
    y0 = cap_blocks$y0, x1 = cap_blocks$x1, y1 = cap_blocks$y1,
    block_id = cap_blocks$block_id), "captions")
  captions$target_type <- ifelse(grepl("^Table", captions$label), "table",
                                 ifelse(is.na(captions$label), NA_character_, "figure"))
  # tables entity
  tables <- bind_entity(lapply(tables_all, function(t) {
    as.data.frame(t[setdiff(names(t), c("caption_segment", "caption_line"))], stringsAsFactors = FALSE)
  }), "tables")
  if (nrow(tables)) {
    for (i in seq_along(tables_all)) {
      seg <- tables_all[[i]]$caption_segment
      if (is.na(seg)) next
      bid <- ll$block_id[ll$segment == seg]
      ci <- which(captions$block_id == bid)
      if (length(ci)) {
        tables$caption_id[i] <- captions$caption_id[ci[1]]
        tables$label[i] <- captions$label[ci[1]]
        captions$target_id[ci[1]] <- tables$table_id[i]
      }
    }
    tables <- link_table_continuations(tables, captions, pages, cfg, log)
  }
  cells <- bind_entity(cells_all, "cells")
  footnotes_t <- bind_entity(fns_all, "footnotes")
  figures <- bind_entity(lapply(figs_all, function(f) as.data.frame(f[setdiff(names(f), "caption_segment")], stringsAsFactors = FALSE)), "figures")
  for (i in seq_along(figs_all)) {
    seg <- figs_all[[i]]$caption_segment
    if (is.na(seg)) next
    bid <- ll$block_id[ll$segment == seg]
    ci <- which(captions$block_id == bid)
    if (length(ci)) {
      figures$caption_id[i] <- captions$caption_id[ci[1]]
      captions$target_id[ci[1]] <- figures$figure_id[i]
    }
  }
  fb <- blocks[blocks$type == "footnote", , drop = FALSE]
  footnotes_p <- as_entity(list(
    footnote_id = sprintf("fn-%s", fb$block_id), page = fb$page, scope = rep("page", nrow(fb)),
    marker = vapply(fb$text, function(t) {
      m <- regmatches(t, regexpr("^([*\u2020\u2021\u00a7\u00b6#]+|[a-z](?=\\s)|[0-9]{1,2}(?=\\s))", t, perl = TRUE))
      if (length(m)) m else NA_character_
    }, character(1)),
    text = fb$text, x0 = fb$x0, y0 = fb$y0, x1 = fb$x1, y1 = fb$y1, block_id = fb$block_id), "footnotes")
  equations <- bind_entity(lapply(eqs_all, as.data.frame, stringsAsFactors = FALSE), "equations")
  spans <- if (is.null(wl)) empty_entity("spans") else {
    wl$line_id <- sprintf("ln-%05d", wl$segment)
    as_entity(list(span_id = wl$span_id, page = wl$page, x0 = wl$x0, y0 = wl$y0, x1 = wl$x1,
                   y1 = wl$y1, text = wl$text, font_name = wl$font_name, font_size = wl$font_size,
                   bold = wl$bold, italic = wl$italic, superscript = wl$superscript,
                   line_id = wl$line_id, block_id = wl$block_id,
                   source = "pdf_text_layer"), "spans")
  }
  spans <- spans[order(spans$page, match(spans$block_id, blocks$block_id), spans$y0, spans$x0), , drop = FALSE]
  lines <- if (is.null(ll)) empty_entity("lines") else as_entity(list(
    line_id = sprintf("ln-%05d", ll$segment), page = ll$page, x0 = ll$x0, y0 = ll$y0, x1 = ll$x1,
    y1 = ll$y1, text = ll$text, font_size = ll$font_size, bold_ratio = ll$bold_ratio,
    n_spans = ll$n_spans, block_id = ll$block_id), "lines")
  citations <- build_citations(blocks, references, spans)
  geom_disp <- geom
  out <- list(
    pages = as_entity(pages, "pages"),
    spans = convert_boxes(spans, geom_disp),
    lines = convert_boxes(lines, geom_disp),
    blocks = convert_boxes(blocks, geom_disp),
    sections = sections,
    tables = convert_boxes(tables, geom_disp),
    cells = convert_boxes(merge_page(cells, tables), geom_disp, inverse = TRUE),
    figures = convert_boxes(figures, geom_disp),
    equations = convert_boxes(equations, geom_disp),
    captions = convert_boxes(captions, geom_disp),
    footnotes = convert_boxes(bind_entity(list(footnotes_p, footnotes_t), "footnotes"), geom_disp),
    references = references,
    citations = citations,
    supplementary = empty_entity("supplementary"),
    metadata = pdf_metadata(path, blocks, n_pages))
  out$cells <- as_entity(out$cells, "cells")
  out
}

merge_page <- function(cells, tables) {
  if (!nrow(cells)) return(cells)
  cells$page <- tables$page[match(cells$table_id, tables$table_id)]
  cells
}

column_count <- function(ub, thr) {
  g <- x_gaps(ub, thr)
  if (nrow(g)) return(as.integer(nrow(g) + 1L))
  if (!is.null(find_gutter(ub, thr))) 2L else 1L
}

# Mark a paragraph that continues the previous body paragraph across a
# column or page break.
link_continuations <- function(blocks) {
  if (!nrow(blocks)) return(blocks)
  body <- which(blocks$type == "paragraph")
  for (k in seq_along(body)[-1]) {
    i <- body[k - 1L]
    j <- body[k]
    between <- blocks$type[seq_len(nrow(blocks)) > i & seq_len(nrow(blocks)) < j]
    if (any(between %in% c("heading", "title"))) next
    brk <- blocks$page[j] != blocks$page[i] || blocks$column[j] != blocks$column[i]
    if (brk && !grepl("[.!?:]['\")]?$", blocks$text[i]) && grepl("^[a-z]", blocks$text[j])) {
      blocks$continues[j] <- blocks$block_id[i]
    }
  }
  blocks
}

link_table_continuations <- function(tables, captions, pages, cfg, log) {
  tables$continuation_status <- NA_character_
  for (i in seq_len(nrow(tables))) {
    cap <- captions$text[match(tables$caption_id[i], captions$caption_id)]
    lab <- tables$label[i]
    if (!is.na(cap) && grepl("\\(?\\bcont(inued|\\.|'d)\\b\\)?", cap, ignore.case = TRUE) && !is.na(lab)) {
      prev <- which(tables$label == lab & seq_len(nrow(tables)) < i & is.na(tables$continuation_of))
      if (length(prev)) {
        p <- prev[length(prev)]
        tables$continuation_of[i] <- tables$table_id[p]
        ok <- cfg$join_continuations && tables$n_cols[i] == tables$n_cols[p] && tables$page[i] == tables$page[p] + 1L
        tables$continuation_status[i] <- if (ok) "joined" else "possible"
        if (!ok) log$add("tables", "continuation_not_joined",
                         "Labelled continuation not joined because the column count or page differs.",
                         tables$page[i], tables$table_id[i])
      }
    } else if (is.na(tables$caption_id[i]) && i > 1L && tables$page[i] == tables$page[i - 1L] + 1L &&
               tables$n_cols[i] == tables$n_cols[i - 1L]) {
      tables$continuation_of[i] <- tables$table_id[i - 1L]
      tables$continuation_status[i] <- "possible"
    }
  }
  tables
}

pdf_metadata <- function(path, blocks, n_pages) {
  info <- tryCatch(pdftools::pdf_info(path), error = function(e) NULL)
  keys <- if (is.null(info)) list() else info$keys
  md <- list(title = NA_character_, authors = character(), doi = NA_character_,
             pmid = NA_character_, pmcid = NA_character_, journal = NA_character_,
             year = NA_integer_, abstract = NA_character_, keywords = character(),
             license = NA_character_, n_pages = n_pages, sources = list())
  if (nzchar(keys$Title %||% "")) {
    md$title <- keys$Title
    md$sources$title <- "pdf_info"
  } else {
    tb <- blocks[blocks$type == "title", , drop = FALSE]
    if (nrow(tb)) {
      md$title <- tb$text[1]
      md$sources$title <- "first_page_layout"
    }
  }
  if (nzchar(keys$Author %||% "")) {
    md$authors <- trimws(strsplit(keys$Author, "\\s*(;|,| and )\\s*")[[1]])
    md$sources$authors <- "pdf_info"
  }
  if (nzchar(keys$Keywords %||% "")) {
    md$keywords <- trimws(strsplit(keys$Keywords, "[;,]")[[1]])
    md$sources$keywords <- "pdf_info"
  }
  xmp <- if (is.null(info)) "" else info$metadata %||% ""
  d <- regmatches(xmp, regexpr(doi_regex, xmp))
  if (length(d)) {
    md$doi <- clean_doi(d)
    md$sources$doi <- "xmp"
  } else {
    fp <- blocks$text[blocks$page == 1L & !blocks$type %in% "reference" & !is.na(blocks$text)]
    d <- regmatches(fp, regexpr("(?i)(doi:?\\s*|doi\\.org/)10\\.[0-9]{4,9}/[^[:space:]]+", fp, perl = TRUE))
    if (length(d)) {
      md$doi <- clean_doi(sub("(?i)^.*?(10\\.)", "\\1", d[1], perl = TRUE))
      md$sources$doi <- "first_page_text"
    }
  }
  ab <- blocks[blocks$type == "heading" & grepl("^abstract$", tolower(blocks$text)), , drop = FALSE]
  if (nrow(ab)) {
    nxt <- blocks[blocks$reading_order > ab$reading_order[1] & blocks$type == "paragraph", , drop = FALSE]
    if (nrow(nxt)) {
      md$abstract <- nxt$text[1]
      md$sources$abstract <- "layout_after_abstract_heading"
    }
  }
  md
}

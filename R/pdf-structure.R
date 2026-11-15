# Figures, blocks, sections, captions, references and citations for PDF
# documents.

detect_figures_page <- function(lines, regions, table_units, used, page, cfg,
                                page_w, page_h, id_offset = 0L) {
  out <- list(figures = NULL, used = integer(), units = NULL)
  if (!cfg$figures || !nrow(lines)) return(out)
  if (!is.null(table_units) && nrow(regions)) {
    keep <- vapply(seq_len(nrow(regions)), function(i) {
      !any(regions$x0[i] < table_units$x1 + 4 & regions$x1[i] > table_units$x0 - 4 &
             regions$y0[i] < table_units$y1 + 4 & regions$y1[i] > table_units$y0 - 4)
    }, logical(1))
    regions <- regions[keep, , drop = FALSE]
  }
  caps <- lines[lines$caption & grepl("^(Supplementa(ry|l)\\s+)?(Figure|Fig\\.?|Scheme|Chart)", trimws(lines$text)), , drop = FALSE]
  taken <- rep(FALSE, nrow(regions))
  figs <- list()
  merge_near <- function(sel) {
    repeat {
      bb <- bbox_union(regions$x0[sel], regions$y0[sel], regions$x1[sel], regions$y1[sel])
      add <- which(!sel & !taken & regions$x0 < bb[3] + 12 & regions$x1 > bb[1] - 12 &
                     regions$y0 < bb[4] + 12 & regions$y1 > bb[2] - 12)
      if (!length(add)) return(sel)
      sel[add] <- TRUE
    }
  }
  for (ci in seq_len(nrow(caps))) {
    cp <- caps[ci, ]
    box <- c(NA_real_, NA_real_, NA_real_, NA_real_)
    method <- "caption_only"
    status <- "caption_only"
    if (nrow(regions)) {
      above <- which(!taken & regions$y1 <= cp$y0 + 2 & cp$y0 - regions$y1 <= 40 &
                       regions$x1 > cp$x0 - 20)
      below <- which(!taken & regions$y0 >= cp$y1 - 2 & regions$y0 - cp$y1 <= 40 &
                       regions$x1 > cp$x0 - 20)
      pick <- if (length(above)) above[which.max(regions$y1[above])] else if (length(below)) below[which.min(regions$y0[below])] else integer()
      if (length(pick)) {
        sel <- seq_len(nrow(regions)) == pick
        sel <- merge_near(sel)
        taken <- taken | sel
        box <- bbox_union(regions$x0[sel], regions$y0[sel], regions$x1[sel], regions$y1[sel])
        method <- "caption_anchor+rendered_ink"
        status <- "extracted"
      }
    }
    label <- regmatches(cp$text, regexpr("^(Supplementa(ry|l)\\s+)?(Figure|Fig\\.?|Scheme|Chart)\\s*S?[0-9]+[A-Za-z]?", cp$text))
    figs[[length(figs) + 1L]] <- list(page = page, label = normalize_figure_label(label),
                                      x0 = box[1], y0 = box[2], x1 = box[3], y1 = box[4],
                                      method = method, status = status,
                                      caption_segment = cp$segment)
  }
  big <- which(!taken & (regions$x1 - regions$x0) * (regions$y1 - regions$y0) >= 0.04 * page_w * page_h)
  for (i in big) {
    figs[[length(figs) + 1L]] <- list(page = page, label = NA_character_,
                                      x0 = regions$x0[i], y0 = regions$y0[i],
                                      x1 = regions$x1[i], y1 = regions$y1[i],
                                      method = "rendered_ink", status = "possible",
                                      caption_segment = NA_integer_)
  }
  if (!length(figs)) return(out)
  units <- list()
  for (k in seq_along(figs)) {
    figs[[k]]$figure_id <- make_id("f", page, id_offset + k)
    f <- figs[[k]]
    if (is.na(f$x0)) next
    inside <- lines$segment[!(lines$segment %in% used) & !lines$caption &
                              lines$x0 >= f$x0 - 2 & lines$x1 <= f$x1 + 2 &
                              lines$y0 >= f$y0 - 2 & lines$y1 <= f$y1 + 2]
    out$used <- c(out$used, inside)
    units[[length(units) + 1L]] <- data.frame(id = f$figure_id, x0 = f$x0, y0 = f$y0,
                                              x1 = f$x1, y1 = f$y1, stringsAsFactors = FALSE)
  }
  out$figures <- figs
  out$units <- if (length(units)) do.call(rbind, units) else NULL
  out
}

normalize_figure_label <- function(x) {
  if (!length(x) || is.na(x)) return(NA_character_)
  x <- sub("^Fig(\\.\\s*|\\s+)", "Figure ", x)
  x <- sub("^Tab\\.\\s*", "Table ", x)
  gsub("\\s+", " ", trimws(x))
}

caption_label <- function(text) {
  m <- regmatches(text, regexpr(caption_regex, trimws(text), perl = TRUE))
  if (!length(m)) return(NA_character_)
  normalize_figure_label(m)
}

# Group ordered units of one page into blocks.
build_page_blocks <- function(ord, lines, page, body_size, page_h, vocab, cfg,
                              unit_kind) {
  blocks <- list()
  cur <- NULL
  flush <- function() {
    if (is.null(cur)) return()
    blocks[[length(blocks) + 1L]] <<- cur
    cur <<- NULL
  }
  head_flag <- line_is_heading(lines, body_size)
  names(head_flag) <- lines$segment
  for (k in seq_len(nrow(ord))) {
    id <- ord$id[k]
    kind <- unit_kind[[id]]
    if (kind != "line") {
      flush()
      blocks[[length(blocks) + 1L]] <- list(kind = kind, unit = id, segs = integer(),
                                            leaf = ord$leaf[k])
      next
    }
    seg <- as.integer(sub("^l", "", id))
    ln <- lines[lines$segment == seg, ]
    is_head <- head_flag[[as.character(seg)]]
    if (!is.null(cur)) {
      prev <- lines[lines$segment == cur$segs[length(cur$segs)], ]
      gap <- ln$y0 - prev$y1
      same <- cur$leaf == ord$leaf[k] && gap >= -2 && gap <= 0.6 * body_size &&
        abs(ln$font_size - prev$font_size) <= 1 && !ln$caption &&
        is_head == cur$head && ln$x0 < max(lines$x1[lines$segment %in% cur$segs]) &&
        ln$x1 > min(lines$x0[lines$segment %in% cur$segs])
      if (same && is_head && ln$n_words + sum(lines$n_words[lines$segment %in% cur$segs]) > 24) same <- FALSE
      if (same) {
        cur$segs <- c(cur$segs, seg)
        next
      }
      flush()
    }
    cur <- list(kind = "line", segs = seg, leaf = ord$leaf[k], head = is_head)
  }
  flush()
  blocks
}

classify_block <- function(ls, body_size, page, page_h, is_head) {
  txt <- paste(ls$text, collapse = " ")
  fs <- stats::median(ls$font_size)
  if (line_is_caption(ls$text[1])) return("caption")
  if (is_head) {
    if (page == 1L && fs >= 1.3 * body_size && min(ls$y0) < 0.4 * page_h) return("title")
    return("heading")
  }
  if (fs <= 0.88 * body_size && min(ls$y0) > 0.7 * page_h) return("footnote")
  "paragraph"
}

# Assemble document level block, section, caption, reference and citation
# tables from per page results.
heading_number <- function(title) {
  m <- regmatches(title, regexpr("^[0-9]+(\\.[0-9]+)*\\.?(?=\\s)", title, perl = TRUE))
  if (!length(m)) NA_character_ else sub("\\.$", "", m)
}

section_label <- function(title) {
  t <- tolower(trimws(sub("^[0-9]+(\\.[0-9]+)*\\.?\\s+", "", title)))
  t <- sub("[:.]$", "", t)
  map <- list(
    abstract = "^(abstract|summary)$",
    introduction = "^(introduction|background)$",
    methods = "^(methods?|materials and methods|methods and materials|patients and methods|methodology|study design|experimental( section| procedures)?)$",
    results = "^results?$",
    discussion = "^(discussion|results and discussion)$",
    conclusions = "^(conclusions?|concluding remarks)$",
    references = "^(references|bibliography|literature cited|works cited)$",
    acknowledgments = "^acknowledge?ments?$",
    funding = "^funding",
    supplementary = "^(supplementary|supporting) (material|information)")
  for (nm in names(map)) if (grepl(map[[nm]], t)) return(nm)
  NA_character_
}

build_sections <- function(blocks) {
  hb <- blocks[blocks$type == "heading", , drop = FALSE]
  if (!nrow(hb)) {
    blocks$section_id <- rep(NA_character_, nrow(blocks))
    return(list(sections = empty_entity("sections"), blocks = blocks))
  }
  nums <- vapply(hb$text, heading_number, character(1))
  labels <- vapply(hb$text, section_label, character(1))
  depth <- ifelse(is.na(nums), NA_integer_, lengths(strsplit(nums, ".", fixed = TRUE)))
  sizes <- sort(unique(round(hb$font_size[is.na(depth) & is.na(labels)] * 2) / 2), decreasing = TRUE)
  lvl <- depth
  for (i in which(is.na(lvl))) {
    if (!is.na(labels[i])) {
      lvl[i] <- 1L
    } else if (any(!is.na(depth))) {
      ref <- tapply(hb$font_size[!is.na(depth)], depth[!is.na(depth)], stats::median)
      lvl[i] <- as.integer(names(ref)[which.min(abs(ref - hb$font_size[i]))])
    } else {
      lvl[i] <- match(round(hb$font_size[i] * 2) / 2, sizes)
    }
  }
  lvl[is.na(lvl)] <- 1L
  sid <- sprintf("s-%03d", seq_len(nrow(hb)))
  parent <- rep(NA_character_, nrow(hb))
  stack_id <- character()
  stack_lvl <- integer()
  for (i in seq_len(nrow(hb))) {
    while (length(stack_lvl) && stack_lvl[length(stack_lvl)] >= lvl[i]) {
      stack_id <- stack_id[-length(stack_id)]
      stack_lvl <- stack_lvl[-length(stack_lvl)]
    }
    if (length(stack_id)) parent[i] <- stack_id[length(stack_id)]
    stack_id <- c(stack_id, sid[i])
    stack_lvl <- c(stack_lvl, lvl[i])
  }
  inherited <- labels
  for (i in seq_len(nrow(hb))) {
    if (is.na(inherited[i]) && !is.na(parent[i])) inherited[i] <- inherited[match(parent[i], sid)]
  }
  # assign blocks to the deepest open section
  blocks$section_id <- rep(NA_character_, nrow(blocks))
  hpos <- match(hb$block_id, blocks$block_id)
  for (i in seq_along(hpos)) {
    end <- if (i < length(hpos)) hpos[i + 1L] - 1L else nrow(blocks)
    blocks$section_id[hpos[i]:end] <- sid[i]
  }
  page_end <- vapply(seq_along(sid), function(i) {
    p <- blocks$page[blocks$section_id %in% c(sid[i], sid[!is.na(parent) & parent == sid[i]])]
    as.integer(max(p, na.rm = TRUE))
  }, integer(1))
  secs <- as_entity(list(section_id = sid, parent_id = parent, level = lvl,
                         number = nums, title = hb$text, label = inherited,
                         block_id = hb$block_id, page_start = hb$page,
                         page_end = page_end, reading_order = hb$reading_order,
                         method = ifelse(is.na(nums), "font_style", "numbering+font_style")),
                    "sections")
  list(sections = secs, blocks = blocks)
}

# ---- references and citations ------------------------------------------

doi_regex <- "10\\.[0-9]{4,9}/[^[:space:]\"<>]+"

clean_doi <- function(x) sub("[].,;:)]+$", "", x)

parse_reference <- function(text) {
  t <- normalize_text(text)
  label <- regmatches(t, regexpr("^\\[?[0-9]{1,4}[].)]?(?=\\s)", t, perl = TRUE))
  body <- if (length(label)) trimws(sub("^\\[?[0-9]{1,4}[].)]?\\s*", "", t)) else t
  doi <- regmatches(t, regexpr(doi_regex, t, ignore.case = TRUE))
  pmid <- regmatches(t, regexpr("(?i)PMID:?\\s*[0-9]{5,9}", t, perl = TRUE))
  pmcid <- regmatches(t, regexpr("PMC[0-9]{5,9}", t))
  yr <- regmatches(body, regexpr("\\b(19|20)[0-9]{2}(?=[a-z]?[;.,):]|\\b)", body, perl = TRUE))
  parts <- trimws(strsplit(body, "(?<=[A-Za-z0-9)])\\.\\s+(?=[A-Z])", perl = TRUE)[[1]])
  authors <- title <- journal <- NA_character_
  method <- "heuristic_generic"
  if (length(parts) >= 3L && grepl("^[A-Z][A-Za-z'\u00c0-\u017f-]+\\s+[A-Z]{1,3}\\b", parts[1])) {
    authors <- parts[1]
    title <- parts[2]
    journal <- sub("\\s*[.;,]?\\s*(19|20)[0-9]{2}.*$", "", parts[3])
    method <- "heuristic_vancouver"
  }
  vol <- regmatches(body, regexpr("(?<=;)\\s*[0-9]+(?=(\\([0-9A-Za-z]+\\))?:)", body, perl = TRUE))
  pg <- regmatches(body, regexpr("(?<=:)\\s*e?[0-9]+(-e?[0-9]+)?", body, perl = TRUE))
  list(label = if (length(label)) gsub("[^0-9]", "", label) else NA_character_,
       text = t, authors = authors, title = title, journal = journal,
       year = if (length(yr)) as.integer(yr) else NA_integer_,
       volume = if (length(vol)) trimws(vol) else NA_character_,
       pages = if (length(pg)) trimws(pg) else NA_character_,
       doi = if (length(doi)) clean_doi(doi) else NA_character_,
       pmid = if (length(pmid)) gsub("[^0-9]", "", pmid) else NA_character_,
       pmcid = if (length(pmcid)) pmcid else NA_character_,
       method = method)
}

# Split reference section blocks into entries using numbering or hanging
# indents, then parse each entry.
build_references <- function(blocks, block_lines, sections) {
  rs <- sections$section_id[sections$label %in% "references"]
  rb <- blocks[blocks$section_id %in% rs & blocks$type %in% c("paragraph", "reference"), , drop = FALSE]
  if (!nrow(rb)) return(list(references = empty_entity("references"), blocks = blocks))
  entries <- list()
  start_re <- "^\\s*(\\[[0-9]{1,4}\\]|[0-9]{1,4}\\.)\\s"
  for (i in seq_len(nrow(rb))) {
    ls <- block_lines[[rb$block_id[i]]]
    txt <- ls$text
    starts <- grepl(start_re, txt)
    if (!any(starts)) {
      left <- min(ls$x0)
      starts <- ls$x0 <= left + 2
      if (all(starts)) starts <- c(TRUE, rep(FALSE, length(txt) - 1L))
    }
    starts[1] <- TRUE
    grp <- cumsum(starts)
    for (g in unique(grp)) {
      jt <- join_lines(txt[grp == g], character(), repair = FALSE)$text
      entries[[length(entries) + 1L]] <- c(parse_reference(jt),
                                           list(page = rb$page[i], block_id = rb$block_id[i]))
    }
  }
  refs <- bind_entity(lapply(entries, function(e) as.data.frame(e, stringsAsFactors = FALSE)), "references")
  refs$ref_id <- sprintf("r-%03d", seq_len(nrow(refs)))
  blocks$type[blocks$block_id %in% rb$block_id] <- "reference"
  list(references = refs, blocks = blocks)
}

expand_numeric_markers <- function(x) {
  parts <- trimws(strsplit(gsub("[\u2013\u2014\u2212]", "-", x), ",")[[1]])
  out <- integer()
  for (p in parts) {
    if (grepl("^[0-9]+-[0-9]+$", p)) {
      ab <- as.integer(strsplit(p, "-")[[1]])
      if (!anyNA(ab) && ab[2] >= ab[1] && ab[2] - ab[1] < 100) out <- c(out, seq(ab[1], ab[2]))
    } else if (grepl("^[0-9]+$", p)) {
      out <- c(out, as.integer(p))
    }
  }
  out
}

build_citations <- function(blocks, references, spans) {
  body <- blocks[blocks$type %in% c("paragraph", "caption", "footnote", "heading") &
                   !is.na(blocks$text), , drop = FALSE]
  out <- list()
  ref_label <- references$label
  add <- function(b, ids, marker, start, end, style) {
    if (!length(ids)) ids <- NA_character_
    for (rid in ids) {
      out[[length(out) + 1L]] <<- list(block_id = b$block_id, ref_id = rid,
                                       marker = marker, char_start = start,
                                       char_end = end, page = b$page, style = style,
                                       status = if (is.na(rid)) "unlinked" else "linked",
                                       xpath = b$xpath)
    }
  }
  for (i in seq_len(nrow(body))) {
    b <- body[i, ]
    txt <- b$text
    m <- gregexpr("\\[([0-9]{1,4}(\\s*[-\u2013,]\\s*[0-9]{1,4})*)\\]", txt, perl = TRUE)[[1]]
    if (m[1] > 0) {
      for (j in seq_along(m)) {
        mk <- substr(txt, m[j], m[j] + attr(m, "match.length")[j] - 1L)
        nums <- expand_numeric_markers(gsub("[][]", "", mk))
        ids <- references$ref_id[match(as.character(nums), ref_label)]
        add(b, ids, mk, m[j], m[j] + attr(m, "match.length")[j] - 1L, "numeric")
      }
    }
    m <- gregexpr("\\(([A-Z][A-Za-z'\u00c0-\u017f-]+)( et al\\.?| and [A-Z][A-Za-z'-]+)?,? ((19|20)[0-9]{2})[a-z]?\\)", txt, perl = TRUE)[[1]]
    if (m[1] > 0 && nrow(references)) {
      st <- attr(m, "capture.start")
      ln <- attr(m, "capture.length")
      for (j in seq_along(m)) {
        sur <- substr(txt, st[j, 1], st[j, 1] + ln[j, 1] - 1L)
        yr <- as.integer(substr(txt, st[j, 3], st[j, 3] + ln[j, 3] - 1L))
        hit <- which(startsWith(references$text %||% "", sur) & references$year %in% yr)
        if (!length(hit)) hit <- which(grepl(paste0("^", sur, "\\b"), references$authors) & references$year %in% yr)
        mk <- substr(txt, m[j], m[j] + attr(m, "match.length")[j] - 1L)
        add(b, if (length(hit) == 1L) references$ref_id[hit] else NA_character_, mk,
            m[j], m[j] + attr(m, "match.length")[j] - 1L, "author_year")
      }
    }
    if (!is.null(spans) && nrow(spans)) {
      sp <- spans[spans$block_id %in% b$block_id & spans$superscript &
                    grepl("^[0-9]+([,\u2013-][0-9]+)*$", spans$text), , drop = FALSE]
      for (j in seq_len(nrow(sp))) {
        nums <- expand_numeric_markers(sp$text[j])
        ids <- references$ref_id[match(as.character(nums), ref_label)]
        pos <- gregexpr(sp$text[j], txt, fixed = TRUE)[[1]]
        st <- if (length(pos) == 1L && pos[1] > 0) pos[1] else NA_integer_
        add(b, ids, sp$text[j], st, if (is.na(st)) NA_integer_ else st + nchar(sp$text[j]) - 1L,
            "superscript")
      }
    }
  }
  cit <- bind_entity(lapply(out, as.data.frame, stringsAsFactors = FALSE), "citations")
  if (nrow(cit)) cit$citation_id <- sprintf("c-%04d", seq_len(nrow(cit)))
  cit
}

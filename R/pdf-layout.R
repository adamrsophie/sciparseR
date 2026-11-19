# Layout analysis for one page at a time in upright page space: words to
# rows and line segments, running headers and footers, recursive XY-cut
# reading order, and blocks.

caption_regex <- paste0(
  "^(Supplementa(ry|l)\\s+)?(Table|Tab\\.|Figure|Fig\\.?|Box|Scheme|Chart|Exhibit)",
  "\\s*S?[0-9]+[A-Za-z]?\\b")

canonical_headings <- c(
  "abstract", "summary", "background", "introduction", "methods",
  "materials and methods", "methods and materials", "patients and methods",
  "results", "discussion", "conclusion", "conclusions", "references",
  "bibliography", "literature cited", "acknowledgments",
  "acknowledgements", "funding", "conflicts of interest",
  "competing interests", "limitations", "appendix", "supplementary material",
  "data availability", "author contributions", "keywords")

# Words of one page to rows (vertical overlap) and segments (horizontal
# gaps). Returns words with row/segment ids and a line (segment) frame.
build_lines <- function(w, cfg) {
  if (!nrow(w)) return(list(words = w, lines = NULL))
  w$h <- pmax(w$y1 - w$y0, 1)
  w$yc <- (w$y0 + w$y1) / 2
  w <- w[order(w$yc, w$x0), , drop = FALSE]
  row <- integer(nrow(w))
  band0 <- band1 <- numeric()
  for (i in seq_len(nrow(w))) {
    best <- 0L
    best_ov <- 0
    cand <- which(band1 >= w$y0[i] - 1)
    for (r in cand) {
      ov <- min(band1[r], w$y1[i]) - max(band0[r], w$y0[i])
      ratio <- ov / min(band1[r] - band0[r], w$h[i])
      if (ratio >= 0.5 && ratio > best_ov) {
        best <- r
        best_ov <- ratio
      }
    }
    if (best) {
      row[i] <- best
    } else {
      band0 <- c(band0, w$y0[i])
      band1 <- c(band1, w$y1[i])
      row[i] <- length(band0)
    }
  }
  w$row <- row
  w <- w[order(w$row, w$x0), , drop = FALSE]
  seg <- integer(nrow(w))
  s <- 0L
  for (r in unique(w$row)) {
    ix <- which(w$row == r)
    rh <- stats::median(w$h[ix])
    s <- s + 1L
    seg[ix[1]] <- s
    if (length(ix) > 1L) {
      for (k in 2:length(ix)) {
        gap <- w$x0[ix[k]] - w$x1[ix[k - 1]]
        if (gap > cfg$segment_gap * rh) s <- s + 1L
        seg[ix[k]] <- s
      }
    }
  }
  w$segment <- seg
  # superscripts: clearly smaller font, raised above the segment center
  w$superscript <- rep(FALSE, nrow(w))
  for (sg in unique(seg)) {
    ix <- which(seg == sg)
    if (length(ix) < 2L) next
    fs <- stats::median(w$font_size[ix])
    yc <- stats::median(w$yc[ix])
    w$superscript[ix] <- !is.na(w$font_size[ix]) & w$font_size[ix] < 0.8 * fs &
      w$yc[ix] < yc - 0.15 * stats::median(w$h[ix])
  }
  # a raised number that is a power of ten, a unit exponent or a charge is
  # part of the value ("5 x 10" raised "3", "7 m" raised "2"), not a
  # footnote or citation marker: it stays in the text as a superscript
  w$scientific <- rep(FALSE, nrow(w))
  for (sg in unique(seg)) {
    ix <- which(seg == sg)
    ix <- ix[order(w$x0[ix])]
    for (k in which(w$superscript[ix])) {
      if (k < 2L) next
      w$scientific[ix[k]] <- scientific_superscript(w$text[ix[k]], w$text[ix[k - 1L]],
                                                    if (k > 2L) w$text[ix[k - 2L]] else "")
    }
  }
  w$superscript[w$scientific] <- FALSE
  lines <- lapply(split(seq_len(nrow(w)), w$segment), function(ix) {
    ix <- ix[order(w$x0[ix])]
    txt <- w$text[ix[1]]
    if (length(ix) > 1L) {
      for (k in 2:length(ix)) {
        a <- ix[k - 1]
        b <- ix[k]
        gap <- w$x0[b] - w$x1[a]
        if (w$scientific[b]) {
          txt <- paste0(txt, script_text(w$text[b], sup_map, "^"))
          next
        }
        sep <- if (w$superscript[b]) "" else if (isTRUE(w$space[a]) || gap > 0.2 * w$h[a]) " " else ""
        txt <- paste0(txt, sep, w$text[b])
      }
    }
    normal <- ix[!w$superscript[ix]]
    if (!length(normal)) normal <- ix
    nchr <- nchar(w$text[ix])
    data.frame(segment = w$segment[ix[1]], row = w$row[ix[1]],
               x0 = min(w$x0[ix]), y0 = min(w$y0[normal]),
               x1 = max(w$x1[ix]), y1 = max(w$y1[normal]), text = txt,
               font_size = stats::median(w$font_size[normal]),
               bold_ratio = sum(nchr[w$bold[ix]]) / max(1, sum(nchr)),
               italic_ratio = sum(nchr[w$italic[ix]]) / max(1, sum(nchr)),
               n_spans = length(ix), n_words = sum(!w$superscript[ix]),
               stringsAsFactors = FALSE)
  })
  lines <- do.call(rbind, lines)
  rownames(lines) <- NULL
  list(words = w, lines = lines)
}

# Is a raised word a power, unit exponent or charge rather than a marker?
# `prev` and `prev2` are the words before it on the line.
scientific_superscript <- function(sup, prev, prev2) {
  sup <- trimws(sup)
  if (!grepl("^[-+\u2212]?[0-9]{1,3}[-+\u2212]?$", sup)) return(FALSE)
  # a sign makes it an exponent or a charge, never a citation
  if (grepl("[-+\u2212]", sup)) return(TRUE)
  # power of ten: "x 10", "\u00d710", "\u00b710"
  if (grepl("(^|[x\u00d7\u00b7])10$", prev) && (grepl("[x\u00d7\u00b7]", prev) || grepl("^[x\u00d7\u00b7]$", prev2))) return(TRUE)
  # unit exponent after a number: "7 m", "3.2 kg/m", "120 cm"
  unit <- "^(?:[a-z\u00b5\u03bc]{1,3}|[a-z\u00b5\u03bc]{1,3}/[a-z\u00b5\u03bc]{1,3})$"
  grepl(unit, prev, perl = TRUE) && grepl("[0-9]$", prev2)
}

# Mark running headers, footers and page numbers using all pages.
mark_marginal <- function(lines, page_h, cfg, body_size) {
  lines$marginal <- rep(NA_character_, nrow(lines))
  lines$marginal_status <- rep(NA_character_, nrow(lines))
  if (!nrow(lines) || !cfg$detect_headers) return(lines)
  band_top <- cfg$header_band * page_h[lines$page]
  band_bot <- (1 - cfg$header_band) * page_h[lines$page]
  top <- lines$y1 <= band_top
  bot <- lines$y0 >= band_bot
  key <- gsub("[0-9]+", "#", tolower(normalize_text(lines$text)))
  n_pages <- length(unique(lines$page))
  for (pos in c("header", "footer")) {
    sel <- if (pos == "header") top else bot
    if (!any(sel)) next
    pages_per_key <- tapply(lines$page[sel], key[sel], function(p) length(unique(p)))
    repeated <- names(pages_per_key)[pages_per_key >= max(2, ceiling(0.5 * n_pages))]
    hit <- sel & key %in% repeated
    lines$marginal[hit] <- pos
    lines$marginal_status[hit] <- "extracted"
    # unrepeated small text in the band (single page documents)
    small <- sel & is.na(lines$marginal) & lines$font_size < 0.9 * body_size &
      lines$n_words <= 14
    lines$marginal[small] <- pos
    lines$marginal_status[small] <- "inferred"
  }
  pn <- (top | bot) & grepl("^(page\\s*)?[0-9]{1,4}(\\s*(of|/)\\s*[0-9]{1,4})?$",
                            trimws(tolower(lines$text)))
  lines$marginal[pn] <- "page_number"
  lines$marginal_status[pn] <- ifelse(n_pages > 1, "extracted", "inferred")
  lines
}

# x gaps in the projection of boxes, at least `thr` wide.
x_gaps <- function(b, thr) {
  if (nrow(b) < 2L) return(matrix(numeric(), 0, 2))
  o <- order(b$x0)
  x0 <- b$x0[o]
  x1 <- b$x1[o]
  gaps <- list()
  reach <- x1[1]
  for (i in 2:length(x0)) {
    if (x0[i] - reach >= thr) gaps[[length(gaps) + 1L]] <- c(reach, x0[i])
    reach <- max(reach, x1[i])
  }
  if (!length(gaps)) return(matrix(numeric(), 0, 2))
  do.call(rbind, gaps)
}

# A gutter crossed by only a few full width boxes: an x interval at least
# `thr` wide, in the central part of the region, whose coverage is at most
# a third of the typical coverage, with content on both sides.
find_gutter <- function(b, thr) {
  lo <- floor(min(b$x0))
  hi <- ceiling(max(b$x1))
  if (hi - lo < 3 * thr || nrow(b) < 4L) return(NULL)
  xs <- seq(lo, hi)
  cover <- vapply(xs, function(x) sum(b$x0 <= x & b$x1 >= x), integer(1))
  typical <- stats::quantile(cover, 0.9, names = FALSE)
  span <- hi - lo
  central <- xs >= lo + 0.15 * span & xs <= hi - 0.15 * span
  floor_level <- min(cover[central])
  if (floor_level > typical / 3) return(NULL)
  low <- cover <= floor_level
  rl <- rle(low)
  ends <- cumsum(rl$lengths)
  starts <- ends - rl$lengths + 1L
  best <- NULL
  for (k in which(rl$values & rl$lengths >= thr)) {
    a <- xs[starts[k]]
    z <- xs[ends[k]]
    mid <- (a + z) / 2
    if (mid < lo + 0.15 * (hi - lo) || mid > hi - 0.15 * (hi - lo)) next
    left <- sum(b$x1 <= a + 1)
    right <- sum(b$x0 >= z - 1)
    if (left < 2L || right < 2L) next
    if (is.null(best) || (z - a) > (best[2] - best[1])) best <- c(a, z)
  }
  best
}

# Horizontal bands: clusters of boxes with overlapping y intervals.
y_bands <- function(b) {
  o <- order(b$y0)
  band <- integer(nrow(b))
  cur <- 0L
  reach <- -Inf
  for (i in o) {
    if (b$y0[i] >= reach - 0.5) {
      cur <- cur + 1L
      reach <- b$y1[i]
    } else {
      reach <- max(reach, b$y1[i])
    }
    band[i] <- cur
  }
  band
}

# Recursive XY-cut. Vertical cuts (columns) are taken first when a gutter
# runs through the whole region. Otherwise the region is split into
# horizontal bands, and consecutive bands that do not cross the dominant
# gutter of the narrow boxes are grouped so that columns are read through
# before the next full width element. Returns ids in reading order with the
# leaf (column region) each id belongs to.
xy_order <- function(b, thr, depth = 0L, leaf_start = 0L) {
  if (nrow(b) == 0L) return(data.frame(id = character(), leaf = integer()))
  sorted <- function(bb) {
    bd <- y_bands(bb)
    o <- order(bd, bb$x0)
    data.frame(id = bb$id[o], leaf = leaf_start + 1L, stringsAsFactors = FALSE)
  }
  if (nrow(b) == 1L || depth > 40L) return(sorted(b))
  vg <- x_gaps(b, thr)
  if (nrow(vg)) {
    grp <- findInterval((b$x0 + b$x1) / 2, vg[, 2])
    out <- list()
    lf <- leaf_start
    for (g in sort(unique(grp))) {
      r <- xy_order(b[grp == g, , drop = FALSE], thr, depth + 1L, lf)
      lf <- max(c(lf, r$leaf))
      out[[length(out) + 1L]] <- r
    }
    return(do.call(rbind, out))
  }
  g <- find_gutter(b, thr)
  if (is.null(g)) return(sorted(b))
  bd <- y_bands(b)
  crosses <- tapply(b$x0 < g[2] & b$x1 > g[1], bd, any)
  ub <- sort(unique(bd))
  # the short last line of a full width paragraph belongs to that paragraph
  by0 <- tapply(b$y0, bd, min)
  by1 <- tapply(b$y1, bd, max)
  bn <- tapply(b$y0, bd, length)
  for (k in seq_along(ub)[-1]) {
    kk <- as.character(ub[k])
    pk <- as.character(ub[k - 1L])
    if (!crosses[kk] && crosses[pk] && bn[kk] == 1L &&
        by0[kk] - by1[pk] < 0.5 * (by1[kk] - by0[kk])) crosses[kk] <- TRUE
  }
  grp <- integer(length(ub))
  cur <- 0L
  prev <- NA
  for (k in seq_along(ub)) {
    cr <- crosses[as.character(ub[k])]
    if (is.na(prev) || cr != prev) cur <- cur + 1L
    grp[k] <- cur
    prev <- cr
  }
  if (max(grp) == 1L) return(sorted(b))
  out <- list()
  lf <- leaf_start
  for (k in seq_len(max(grp))) {
    sel <- bd %in% ub[grp == k]
    r <- xy_order(b[sel, , drop = FALSE], thr, depth + 1L, lf)
    lf <- max(c(lf, r$leaf))
    out[[length(out) + 1L]] <- r
  }
  do.call(rbind, out)
}

line_is_caption <- function(text) grepl(caption_regex, trimws(text), perl = TRUE)

line_is_heading <- function(lines, body_size) {
  txt <- trimws(lines$text)
  low <- tolower(sub("^[0-9]+(\\.[0-9]+)*\\.?\\s+", "", txt))
  low <- sub("[:.]$", "", low)
  numbered <- grepl("^[0-9]+(\\.[0-9]+)*\\.?\\s+[A-Z]", txt)
  styled <- lines$bold_ratio >= 0.6 | lines$font_size >= 1.12 * body_size
  short <- lines$n_words <= 14
  ends_ok <- !grepl("[,;]$", txt) & (!grepl("\\.$", txt) | numbered)
  starts_ok <- grepl("^[A-Z0-9]", txt)
  known <- low %in% canonical_headings & lines$n_words <= 4
  (styled & short & ends_ok & starts_ok & !line_is_caption(txt)) |
    (known & (styled | lines$n_words <= 3))
}

math_chars <- "[=\u00b1\u2264\u2265\u2260\u2248\u2211\u220f\u222b\u221a\u03b1-\u03c9\u0391-\u03a9^]"

# Equation rows: a math line with an equation number at the right margin,
# or a short centered math line.
mark_equations <- function(lines, col_left, col_right) {
  lines$equation <- rep(NA_integer_, nrow(lines))
  lab <- which(grepl("^\\([0-9]{1,3}[a-z]?\\)$", trimws(lines$text)))
  eq <- 0L
  for (i in lab) {
    same <- which(lines$row == lines$row[i] & seq_len(nrow(lines)) != i &
                    lines$x1 < lines$x0[i] & grepl(math_chars, lines$text, perl = TRUE))
    if (length(same)) {
      eq <- eq + 1L
      lines$equation[c(i, same)] <- eq
    }
  }
  width <- col_right - col_left
  cand <- which(is.na(lines$equation) & grepl("=", lines$text) &
                  grepl(math_chars, lines$text, perl = TRUE) & lines$n_words <= 12 &
                  lines$x0 - col_left > 0.2 * width & col_right - lines$x1 > 0.2 * width)
  for (i in cand) {
    alone <- sum(lines$row == lines$row[i]) == 1L
    if (alone) {
      eq <- eq + 1L
      lines$equation[i] <- eq
    }
  }
  lines
}

# Join line texts of one block with conservative hyphenation repair.
join_lines <- function(texts, vocab, repair = TRUE, log = NULL, page = NA) {
  if (length(texts) == 1L) return(list(text = texts, repairs = 0L))
  out <- texts[1]
  repairs <- 0L
  for (k in 2:length(texts)) {
    nxt <- texts[k]
    if (repair && grepl("[A-Za-z]-$", out) && grepl("^[a-z]", nxt)) {
      left <- regmatches(out, regexpr("[A-Za-z]+-$", out))
      right <- regmatches(nxt, regexpr("^[a-z]+", nxt))
      joined <- tolower(paste0(sub("-$", "", left), right))
      if (length(left) && length(right) && joined %in% vocab) {
        out <- paste0(sub("-$", "", out), nxt)
        repairs <- repairs + 1L
      } else {
        out <- paste0(out, nxt)
      }
    } else {
      out <- paste(out, nxt)
    }
  }
  list(text = out, repairs = repairs)
}

doc_vocab <- function(texts) {
  tok <- unlist(strsplit(tolower(texts), "[^a-z]+"))
  unique(tok[nchar(tok) > 2])
}

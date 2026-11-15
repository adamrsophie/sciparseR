# Deterministic table detection and structure recovery from line segments,
# optionally helped by ruling lines found in the rendered page.

missing_token_regex <- "^(\u2013|\u2014|-|\u2212|NA|N/A|n/a|NR|nr|ND|nd|NE|\u00b7|\\.\\.\\.|\u2026|\u2020?NA)$"

is_numeric_like <- function(x) {
  grepl("^[-+\u2212\u2013]?\\s*[<>\u2264\u2265]?\\s*[0-9]", trimws(x)) |
    grepl(missing_token_regex, trimws(x))
}

row_summary <- function(lines) {
  if (!nrow(lines)) return(NULL)
  rs <- lapply(split(seq_len(nrow(lines)), lines$row), function(ix) {
    data.frame(row = lines$row[ix[1]], n_seg = length(ix),
               max_words = max(lines$n_words[ix]),
               y0 = min(lines$y0[ix]), y1 = max(lines$y1[ix]),
               x0 = min(lines$x0[ix]), x1 = max(lines$x1[ix]),
               h = stats::median(lines$y1[ix] - lines$y0[ix]))
  })
  rs <- do.call(rbind, rs)
  rs <- rs[order(rs$y0), , drop = FALSE]
  rs$tabular <- rs$n_seg >= 3L | (rs$n_seg == 2L & rs$max_words <= 6L)
  rownames(rs) <- NULL
  rs
}

# Walk rows downward from index `start`, collecting a table region.
walk_rows <- function(rs, start, x_lo, x_hi) {
  if (start > nrow(rs)) return(integer())
  taken <- integer()
  i <- start
  while (i <= nrow(rs)) {
    r <- rs[i, ]
    if (r$x1 < x_lo || r$x0 > x_hi) break
    if (length(taken)) {
      prev <- rs[taken[length(taken)], ]
      if (r$y0 - prev$y1 > 2.2 * max(prev$h, r$h)) break
    }
    if (r$tabular) {
      taken <- c(taken, i)
    } else if (r$max_words <= 8L && i < nrow(rs) && rs$tabular[i + 1L] &&
               rs$y0[i + 1L] - r$y1 <= 2.2 * r$h) {
      taken <- c(taken, i)
    } else if (length(taken) && r$max_words <= 8L && r$n_seg == 1L &&
               r$y0 - rs$y1[taken[length(taken)]] <= 0.8 * r$h) {
      # a wrapped row label directly under a row
      taken <- c(taken, i)
    } else {
      break
    }
    i <- i + 1L
  }
  # trailing non-tabular rows are not part of the table
  while (length(taken) && !rs$tabular[taken[length(taken)]]) taken <- taken[-length(taken)]
  taken
}

seg_overlap <- function(a0, a1, b0, b1) max(0, min(a1, b1) - max(a0, b0))

# Column intervals from the rows with the most segments.
column_intervals <- function(segs) {
  k <- max(table(segs$row))
  full <- names(which(table(segs$row) == k))
  s <- segs[segs$row %in% as.integer(full), , drop = FALSE]
  iv <- s[order(s$x0), c("x0", "x1")]
  merged <- list()
  for (i in seq_len(nrow(iv))) {
    if (length(merged) && iv$x0[i] <= merged[[length(merged)]][2]) {
      merged[[length(merged)]][2] <- max(merged[[length(merged)]][2], iv$x1[i])
    } else {
      merged[[length(merged) + 1L]] <- c(iv$x0[i], iv$x1[i])
    }
  }
  m <- do.call(rbind, merged)
  # extend with segments of other rows that fall into a single interval
  for (i in seq_len(nrow(segs))) {
    ov <- vapply(seq_len(nrow(m)), function(j) seg_overlap(segs$x0[i], segs$x1[i], m[j, 1], m[j, 2]), numeric(1))
    if (sum(ov > 0) == 1L) {
      j <- which(ov > 0)
      lo <- min(m[j, 1], segs$x0[i])
      hi <- max(m[j, 2], segs$x1[i])
      clash <- (j > 1L && lo <= m[j - 1L, 2]) || (j < nrow(m) && hi >= m[j + 1L, 1])
      if (!clash) m[j, ] <- c(lo, hi)
    }
  }
  m
}

assign_columns <- function(x0, x1, m) {
  ov <- vapply(seq_len(nrow(m)), function(j) seg_overlap(x0, x1, m[j, 1], m[j, 2]), numeric(1))
  need <- pmin(4, 0.3 * pmin(x1 - x0, m[, 2] - m[, 1]))
  hit <- which(ov > 0 & ov >= need)
  if (!length(hit)) {
    ctr <- (x0 + x1) / 2
    d <- pmax(m[, 1] - ctr, ctr - m[, 2], 0)
    hit <- which.min(d)
  }
  range(hit)
}

# Detect tables on one page. `lines` carry row/segment ids; `words` carry
# segment ids and superscript flags. Returns entity frames and the segment
# ids consumed by tables.
detect_tables_page <- function(lines, words, rules, page, cfg, body_size,
                               id_offset = 0L, log = NULL) {
  out <- list(tables = NULL, cells = NULL, footnotes = NULL, used = integer(),
              units = NULL, captions_used = integer())
  if (!cfg$tables || !nrow(lines)) return(out)
  elig <- lines[is.na(lines$marginal) & is.na(lines$equation) & !lines$caption, , drop = FALSE]
  rs <- row_summary(elig)
  if (is.null(rs)) return(out)
  cap <- lines[lines$caption & grepl("^(Supplementa(ry|l)\\s+)?(Table|Tab\\.)", trimws(lines$text)), , drop = FALSE]
  cap <- cap[order(cap$y0), , drop = FALSE]
  regions <- list()
  used_rows <- integer()
  for (ci in seq_len(nrow(cap))) {
    below <- which(rs$y0 > cap$y1[ci] & !(seq_len(nrow(rs)) %in% used_rows))
    taken <- integer()
    if (length(below) && rs$y0[below[1]] - cap$y1[ci] <= 3 * max(rs$h[below[1]], 8)) {
      taken <- walk_rows(rs, below[1], cap$x0[ci] - 5, Inf)
    }
    if (length(taken) >= 2L) {
      regions[[length(regions) + 1L]] <- list(rows = taken, caption = cap$segment[ci])
      used_rows <- c(used_rows, taken)
    } else if (!is.null(log)) {
      log$add("tables", "caption_without_table",
              paste0("Table caption found but no aligned rows below it: ", cap$text[ci]),
              page)
    }
  }
  i <- 1L
  while (i <= nrow(rs)) {
    if (i %in% used_rows || !rs$tabular[i]) {
      i <- i + 1L
      next
    }
    taken <- setdiff(walk_rows(rs, i, -Inf, Inf), used_rows)
    if (length(taken) >= cfg$min_table_rows &&
        stats::median(rs$n_seg[taken]) >= 2) {
      regions[[length(regions) + 1L]] <- list(rows = taken, caption = NA_integer_)
      used_rows <- c(used_rows, taken)
      i <- max(taken) + 1L
    } else {
      i <- i + 1L
    }
  }
  if (!length(regions)) return(out)
  tabs <- cells_all <- fns_all <- units <- list()
  for (k in seq_along(regions)) {
    rg <- regions[[k]]
    tid <- make_id("t", page, id_offset + k)
    rows_y <- rs[rg$rows, , drop = FALSE]
    segs <- elig[elig$row %in% rows_y$row, , drop = FALSE]
    m <- column_intervals(segs)
    n_cols <- nrow(m)
    trow <- match(segs$row, rows_y$row)
    cspan <- t(vapply(seq_len(nrow(segs)), function(i) assign_columns(segs$x0[i], segs$x1[i], m), integer(2)))
    # spanning header cells extended by rules lying under them
    if (nrow(rules)) {
      for (i in seq_len(nrow(segs))) {
        under <- rules[rules$y0 >= segs$y1[i] - 1 & rules$y0 <= segs$y1[i] + 6 &
                         rules$x0 <= segs$x1[i] & rules$x1 >= segs$x0[i], , drop = FALSE]
        if (!nrow(under)) next
        ru <- under[1, ]
        if ((ru$x1 - ru$x0) > 0.9 * (max(m[, 2]) - min(m[, 1]))) next
        cov <- vapply(seq_len(n_cols), function(j) seg_overlap(ru$x0, ru$x1, m[j, 1], m[j, 2]) / (m[j, 2] - m[j, 1]), numeric(1))
        hit <- which(cov >= 0.5)
        if (length(hit) >= 2L) cspan[i, ] <- range(c(hit, cspan[i, ]))
      }
    }
    n_rows <- nrow(rows_y)
    grid_seg <- matrix(list(), n_rows, n_cols)
    for (i in seq_len(nrow(segs))) {
      r <- trow[i]
      c0 <- cspan[i, 1]
      grid_seg[[r, c0]] <- c(grid_seg[[r, c0]], i)
    }
    span <- matrix(1L, n_rows, n_cols)
    for (i in seq_len(nrow(segs))) span[trow[i], cspan[i, 1]] <- max(span[trow[i], cspan[i, 1]], cspan[i, 2] - cspan[i, 1] + 1L)
    seg_text <- function(ix) {
      if (!length(ix)) return("")
      ix <- ix[order(segs$x0[ix])]
      paste(segs$text[ix], collapse = " ")
    }
    # conservative merge of wrapped row labels into the row above
    keep <- rep(TRUE, n_rows)
    for (r in seq_len(n_rows)[-1]) {
      filled <- vapply(seq_len(n_cols), function(j) length(grid_seg[[r, j]]) > 0, logical(1))
      prev <- max(which(keep[seq_len(r - 1L)]))
      gap <- rows_y$y0[r] - rows_y$y1[prev]
      if (filled[1] && sum(filled) == 1L && length(grid_seg[[prev, 1]]) &&
          gap <= 0.6 * rows_y$h[r] && grepl("^[a-z(]", seg_text(grid_seg[[r, 1]]))) {
        grid_seg[[prev, 1]] <- c(grid_seg[[prev, 1]], grid_seg[[r, 1]])
        keep[r] <- FALSE
      }
    }
    # a raised marker printed as its own row ("a" above "Diabetes, n (%)")
    # belongs to the label of the next row
    marker_segs <- character()
    for (r in seq_len(n_rows - 1L)) {
      if (!keep[r]) next
      filled <- vapply(seq_len(n_cols), function(j) length(grid_seg[[r, j]]) > 0, logical(1))
      txt <- seg_text(grid_seg[[r, 1]])
      nxt <- r + 1L
      if (filled[1] && sum(filled) == 1L && grepl("^[a-z*\u2020\u2021\u00a7]{1,2}$", txt) &&
          length(grid_seg[[nxt, 1]]) && rows_y$y0[nxt] - rows_y$y1[r] <= 0.6 * rows_y$h[nxt]) {
        grid_seg[[nxt, 1]] <- c(grid_seg[[nxt, 1]], grid_seg[[r, 1]])
        marker_segs <- c(marker_segs, segs$segment[grid_seg[[r, 1]]])
        keep[r] <- FALSE
      }
    }
    if (length(marker_segs)) words$superscript[words$segment %in% marker_segs] <- TRUE
    grid_seg <- grid_seg[keep, , drop = FALSE]
    span <- span[keep, , drop = FALSE]
    rows_y <- rows_y[keep, , drop = FALSE]
    n_rows <- nrow(rows_y)
    # header rows: before the first row with numeric body cells, or above
    # the first full width rule below row 1
    numeric_row <- vapply(seq_len(n_rows), function(r) {
      vals <- vapply(seq_len(n_cols)[-1], function(j) seg_text(grid_seg[[r, j]]), character(1))
      vals <- vals[nzchar(vals)]
      length(vals) > 0 && mean(is_numeric_like(vals)) >= 0.5
    }, logical(1))
    first_num <- which(numeric_row)[1]
    n_header <- if (is.na(first_num)) 0L else as.integer(min(first_num - 1L, 4L))
    tab_x0 <- min(m[, 1])
    tab_x1 <- max(m[, 2])
    ruled <- FALSE
    if (nrow(rules)) {
      inr <- rules[rules$y0 >= min(rows_y$y0) - 12 & rules$y1 <= max(rows_y$y1) + 12 &
                     rules$x0 <= tab_x1 & rules$x1 >= tab_x0, , drop = FALSE]
      ruled <- nrow(inr) >= 2L
      full <- inr[(inr$x1 - inr$x0) >= 0.6 * (tab_x1 - tab_x0), , drop = FALSE]
      mids <- full$y0[full$y0 > rows_y$y1[1] - 1 & full$y0 < max(rows_y$y1) - 1]
      if (length(mids)) {
        nh <- sum(rows_y$y1 <= min(mids) + 1)
        if (nh >= 1L && nh <= 4L) n_header <- as.integer(nh)
      }
    }
    # cells
    cl <- list()
    covered <- matrix(FALSE, n_rows, n_cols)
    for (r in seq_len(n_rows)) {
      for (j in seq_len(n_cols)) {
        if (covered[r, j]) next
        cs <- min(span[r, j], n_cols - j + 1L)
        covered[r, j:(j + cs - 1L)] <- TRUE
        ix <- grid_seg[[r, j]]
        if (length(ix)) {
          # raised markers merged from their own row go after the label
          ix <- ix[order(segs$segment[ix] %in% marker_segs, segs$y0[ix], segs$x0[ix])]
          sw <- words[words$segment %in% segs$segment[ix], , drop = FALSE]
          sw <- sw[order(match(sw$segment, segs$segment[ix]), sw$x0), , drop = FALSE]
          raw <- paste(segs$text[ix], collapse = " ")
          sup <- sw$superscript
          clean <- if (any(sup)) paste(sw$text[!sup], collapse = " ") else raw
          bb <- bbox_union(segs$x0[ix], segs$y0[ix], segs$x1[ix], segs$y1[ix])
          txt <- normalize_text(clean)
          cl[[length(cl) + 1L]] <- list(
            row = r, col = j, row_span = 1L, col_span = cs,
            is_header = r <= n_header, text_raw = raw, text = txt,
            x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
            span_ids = collapse_ids(sw$span_id),
            markers = collapse_ids(sw$text[sup]),
            missing_token = grepl(missing_token_regex, txt),
            status = if (grepl(missing_token_regex, txt)) "missing_token" else if (length(ix) > 1L) "merged_segments" else "extracted")
        } else {
          cl[[length(cl) + 1L]] <- list(
            row = r, col = j, row_span = 1L, col_span = 1L,
            is_header = r <= n_header, text_raw = "", text = "",
            missing_token = FALSE, status = "empty")
        }
      }
    }
    cells <- bind_entity(lapply(cl, as.data.frame, stringsAsFactors = FALSE), "cells")
    cells$table_id <- tid
    cells$cell_id <- sprintf("%s-r%03dc%03d", tid, cells$row, cells$col)
    # footnotes directly below the table
    tab_y1 <- max(rows_y$y1)
    tab_font <- stats::median(segs$font_size)
    below <- elig[elig$y0 > tab_y1 & elig$y0 - tab_y1 < 3 * stats::median(rows_y$h) &
                    !(elig$row %in% rs$row[used_rows]), , drop = FALSE]
    below <- below[order(below$y0), , drop = FALSE]
    fn <- list()
    last_y <- tab_y1
    for (i in seq_len(nrow(below))) {
      b <- below[i, ]
      allowed <- if (!length(fn)) 2.5 * stats::median(rows_y$h) else 1.6 * max(b$y1 - b$y0, 6)
      if (b$y0 - last_y > allowed) break
      looks <- b$font_size < 0.95 * tab_font ||
        grepl("^([*\u2020\u2021\u00a7\u00b6#]|[a-z]\\s|[0-9]\\s|Abbreviation|Note|Data are|Values are)", b$text)
      if (!looks) break
      mk <- regmatches(b$text, regexpr("^([*\u2020\u2021\u00a7\u00b6#]+|[a-z](?=\\s)|[0-9]{1,2}(?=\\s))", b$text, perl = TRUE))
      fn[[length(fn) + 1L]] <- list(
        footnote_id = sprintf("%s-fn%02d", tid, length(fn) + 1L), page = page,
        scope = "table", target_id = tid,
        marker = if (length(mk)) mk else NA_character_, text = normalize_text(b$text),
        x0 = b$x0, y0 = b$y0, x1 = b$x1, y1 = b$y1)
      out$used <- c(out$used, b$segment)
      last_y <- b$y1
    }
    fns <- bind_entity(lapply(fn, as.data.frame, stringsAsFactors = FALSE), "footnotes")
    cap_seg <- rg$caption
    bb <- bbox_union(c(segs$x0, fns$x0), c(segs$y0, fns$y0), c(segs$x1, fns$x1), c(segs$y1, fns$y1))
    tabs[[k]] <- list(
      table_id = tid, page = page, x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
      n_rows = n_rows, n_cols = n_cols, n_header_rows = n_header,
      ruled = ruled,
      method = if (ruled) "aligned_segments+ruling_lines" else "aligned_segments",
      status = if (is.na(cap_seg)) "possible" else "extracted",
      footnote_ids = collapse_ids(fns$footnote_id),
      caption_segment = cap_seg)
    cells_all[[k]] <- cells
    fns_all[[k]] <- fns
    out$used <- c(out$used, segs$segment)
    units[[k]] <- data.frame(id = tid, x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
                             stringsAsFactors = FALSE)
  }
  out$tables <- tabs
  out$cells <- bind_entity(cells_all, "cells")
  out$footnotes <- bind_entity(fns_all, "footnotes")
  out$units <- do.call(rbind, units)
  out
}

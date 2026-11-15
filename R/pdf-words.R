# Word level PDF access through pdftools, page rotation, and rendered
# bitmap analysis (ruling lines, ink regions).

strip_subset <- function(font) sub("^[A-Z]{6}\\+", "", font)

font_is_bold <- function(font) {
  grepl("bold|black|heavy|semibold|demi", font, ignore.case = TRUE)
}

font_is_italic <- function(font) {
  grepl("italic|oblique", font, ignore.case = TRUE)
}

# Words of all pages as one data frame in displayed page space.
pdf_words <- function(path) {
  pages <- pdftools::pdf_data(path, font_info = TRUE)
  out <- vector("list", length(pages))
  for (i in seq_along(pages)) {
    d <- pages[[i]]
    if (!NROW(d)) next
    font <- if (is.null(d$font_name)) rep(NA_character_, nrow(d)) else strip_subset(d$font_name)
    size <- if (is.null(d$font_size)) rep(NA_real_, nrow(d)) else d$font_size
    out[[i]] <- data.frame(
      page = i, stream_order = seq_len(nrow(d)),
      x0 = d$x, y0 = d$y, x1 = d$x + d$width, y1 = d$y + d$height,
      text = enc2utf8(d$text), space = d$space, font_name = font,
      font_size = size, bold = font_is_bold(font %||% ""),
      italic = font_is_italic(font %||% ""), stringsAsFactors = FALSE)
  }
  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    return(data.frame(page = integer(), stream_order = integer(), x0 = double(),
                      y0 = double(), x1 = double(), y1 = double(),
                      text = character(), space = logical(),
                      font_name = character(), font_size = double(),
                      bold = logical(), italic = logical()))
  }
  w <- do.call(rbind, out)
  w[nzchar(trimws(w$text)), , drop = FALSE]
}

# Page geometry: media size from pdf_pagesize, displayed size from a tiny
# rendering (poppler applies /Rotate when rendering). Rotation by 90 or 270
# degrees swaps the displayed dimensions; the direction is read from the
# direction in which consecutive words advance. 0 and 180 degrees cannot be
# told apart this way and are reported as "assumed_0".
pdf_page_geometry <- function(path, words) {
  ps <- pdftools::pdf_pagesize(path)
  n <- nrow(ps)
  out <- data.frame(page = seq_len(n), media_width = ps$width,
                    media_height = ps$height, width = ps$width,
                    height = ps$height, rotation = 0L,
                    rotation_status = "assumed_0", stringsAsFactors = FALSE)
  for (i in seq_len(n)) {
    bm <- tryCatch(pdftools::pdf_render_page(path, page = i, dpi = 4),
                   error = function(e) NULL)
    if (is.null(bm)) {
      out$rotation[i] <- NA_integer_
      out$rotation_status[i] <- "unknown"
      next
    }
    dw <- dim(bm)[2]
    dh <- dim(bm)[3]
    portrait_media <- ps$height[i] >= ps$width[i]
    portrait_disp <- dh >= dw
    if (abs(ps$width[i] - ps$height[i]) > 1 && portrait_media != portrait_disp) {
      out$width[i] <- ps$height[i]
      out$height[i] <- ps$width[i]
      wp <- words[words$page == i, , drop = FALSE]
      dir <- word_direction(wp)
      if (is.na(dir)) {
        out$rotation[i] <- NA_integer_
        out$rotation_status[i] <- "swapped_direction_unknown"
      } else {
        out$rotation[i] <- if (dir > 0) 90L else 270L
        out$rotation_status[i] <- "detected"
      }
    }
  }
  out
}

# +1 when text advances downward (page displayed rotated 90 degrees
# clockwise), -1 when upward, NA when undetermined.
word_direction <- function(w) {
  if (nrow(w) < 2L) return(NA_real_)
  w <- w[order(w$stream_order), ]
  dx <- diff(w$x0)
  dy <- diff(w$y0)
  same_col <- abs(dx) <= 2 & abs(dy) > 0 & abs(dy) < 100
  if (sum(same_col) < 2L) return(NA_real_)
  s <- sign(stats::median(dy[same_col]))
  if (s == 0) NA_real_ else s
}

# Map boxes between displayed space and upright (unrotated content) space.
to_upright <- function(x0, y0, x1, y1, rotation, width, height) {
  if (is.na(rotation) || rotation == 0L) return(list(x0 = x0, y0 = y0, x1 = x1, y1 = y1))
  if (rotation == 90L) {
    list(x0 = y0, x1 = y1, y0 = width - x1, y1 = width - x0)
  } else {
    list(x0 = height - y1, x1 = height - y0, y0 = x0, y1 = x1)
  }
}

from_upright <- function(x0, y0, x1, y1, rotation, width, height) {
  if (is.na(rotation) || rotation == 0L) return(list(x0 = x0, y0 = y0, x1 = x1, y1 = y1))
  if (rotation == 90L) {
    list(x0 = width - y1, x1 = width - y0, y0 = x0, y1 = x1)
  } else {
    list(x0 = y0, x1 = y1, y0 = height - x1, y1 = height - x0)
  }
}

# Render a page to a grayscale ink mask (TRUE = dark pixel) in displayed
# space. Rows are y, columns are x; one pixel is 72 / dpi points.
render_ink <- function(path, page, dpi = 72) {
  bm <- tryCatch(pdftools::pdf_render_page(path, page = page, dpi = dpi,
                                           numeric = TRUE),
                 error = function(e) NULL)
  if (is.null(bm)) return(NULL)
  gray <- 0.299 * bm[, , 1] + 0.587 * bm[, , 2] + 0.114 * bm[, , 3]
  if (dim(bm)[3] >= 4L) gray[bm[, , 4] < 0.5] <- 1
  gray < 0.6
}

# Horizontal ruling lines: long thin runs of ink. Returns boxes in points.
find_rules <- function(ink, dpi = 72, min_len_pt = 40) {
  empty <- data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double())
  if (is.null(ink)) return(empty)
  scale <- 72 / dpi
  min_len <- ceiling(min_len_pt / scale)
  hits <- list()
  for (r in seq_len(nrow(ink))) {
    row <- ink[r, ]
    if (sum(row) < min_len) next
    rl <- rle(row)
    ends <- cumsum(rl$lengths)
    starts <- ends - rl$lengths + 1L
    k <- which(rl$values & rl$lengths >= min_len)
    for (j in k) hits[[length(hits) + 1L]] <- c(r, starts[j], ends[j])
  }
  if (!length(hits)) return(empty)
  h <- do.call(rbind, hits)
  # merge vertically adjacent runs with overlapping x into one rule
  h <- h[order(h[, 1], h[, 2]), , drop = FALSE]
  groups <- integer(nrow(h))
  g <- 0L
  for (i in seq_len(nrow(h))) {
    if (groups[i]) next
    g <- g + 1L
    groups[i] <- g
    last <- h[i, ]
    for (j in seq_len(nrow(h))[-seq_len(i)]) {
      if (groups[j]) next
      if (h[j, 1] - last[1] > 1) break
      if (h[j, 1] - last[1] == 1 && min(h[j, 3], last[3]) - max(h[j, 2], last[2]) > 0) {
        groups[j] <- g
        last <- h[j, ]
      }
    }
  }
  out <- do.call(rbind, lapply(split(seq_len(nrow(h)), groups), function(ix) {
    data.frame(x0 = (min(h[ix, 2]) - 1) * scale, y0 = (min(h[ix, 1]) - 1) * scale,
               x1 = max(h[ix, 3]) * scale, y1 = max(h[ix, 1]) * scale)
  }))
  out <- out[(out$y1 - out$y0) <= 4, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Connected regions of ink that are not covered by words or rules, on a
# coarse grid. Returns boxes in points.
ink_regions <- function(ink, words, rules, dpi = 72, cell_pt = 4,
                        min_area_pt = 900) {
  empty <- data.frame(x0 = double(), y0 = double(), x1 = double(), y1 = double())
  if (is.null(ink)) return(empty)
  scale <- 72 / dpi
  m <- ink
  blank <- function(x0, y0, x1, y1, pad = 1) {
    r0 <- max(1L, floor(y0 / scale) - pad)
    r1 <- min(nrow(m), ceiling(y1 / scale) + pad)
    c0 <- max(1L, floor(x0 / scale) - pad)
    c1 <- min(ncol(m), ceiling(x1 / scale) + pad)
    if (r0 <= r1 && c0 <= c1) m[r0:r1, c0:c1] <<- FALSE
  }
  for (i in seq_len(nrow(words))) blank(words$x0[i], words$y0[i], words$x1[i], words$y1[i])
  for (i in seq_len(nrow(rules))) blank(rules$x0[i], rules$y0[i], rules$x1[i], rules$y1[i])
  step <- max(1L, round(cell_pt / scale))
  gr <- ceiling(nrow(m) / step)
  gc <- ceiling(ncol(m) / step)
  grid <- matrix(FALSE, gr, gc)
  rr <- (row(m) - 1L) %/% step + 1L
  cc <- (col(m) - 1L) %/% step + 1L
  idx <- which(m)
  if (!length(idx)) return(empty)
  grid[cbind(rr[idx], cc[idx])] <- TRUE
  lab <- matrix(0L, gr, gc)
  boxes <- list()
  cur <- 0L
  for (start in which(grid)) {
    if (lab[start]) next
    cur <- cur + 1L
    queue <- start
    lab[start] <- cur
    members <- integer()
    while (length(queue)) {
      p <- queue[1]
      queue <- queue[-1]
      members <- c(members, p)
      pr <- (p - 1L) %% gr + 1L
      pc <- (p - 1L) %/% gr + 1L
      for (dr in -1:1) for (dc in -1:1) {
        nr <- pr + dr
        nc <- pc + dc
        if (nr < 1L || nc < 1L || nr > gr || nc > gc) next
        q <- (nc - 1L) * gr + nr
        if (grid[q] && !lab[q]) {
          lab[q] <- cur
          queue <- c(queue, q)
        }
      }
    }
    pr <- (members - 1L) %% gr + 1L
    pc <- (members - 1L) %/% gr + 1L
    boxes[[cur]] <- c((min(pc) - 1) * step * scale, (min(pr) - 1) * step * scale,
                      max(pc) * step * scale, max(pr) * step * scale)
  }
  b <- do.call(rbind, boxes)
  out <- data.frame(x0 = b[, 1], y0 = b[, 2], x1 = b[, 3], y1 = b[, 4])
  area <- (out$x1 - out$x0) * (out$y1 - out$y0)
  out <- out[area >= min_area_pt & (out$y1 - out$y0) > 6, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Evaluate a parsed document against ground truth
#'
#' Compares a document with a ground truth file in the format of the
#' bundled `*.truth.json` fixtures (blocks in reading order, sections,
#' tables with cell grids, header rows and spans, figures, equations,
#' references). Reports text block recall, reading order agreement
#' (Kendall's tau and the share of correctly ordered pairs), section
#' precision, recall and F1, table structure and cell accuracy, and
#' element counts.
#'
#' @param doc An `sc_document`.
#' @param truth Path to a truth JSON file, or the parsed list.
#' @param threshold Minimum normalized similarity for two texts to match.
#' @return A data frame with columns `metric`, `value` and `n`, with the
#'   per-table detail in attribute `"tables"`.
#' @examples
#' f <- system.file("extdata", "two_column.pdf", package = "sciparseR")
#' sc_evaluate(sc_parse(f), sub("\\.pdf$", ".truth.json", f))
#' @export
sc_evaluate <- function(doc, truth, threshold = 0.9) {
  check_document(doc)
  if (is.character(truth)) {
    if (!file.exists(truth)) abort("Truth file not found: ", truth)
    truth <- jsonlite::fromJSON(truth, simplifyVector = FALSE)
  }
  out <- list()
  add <- function(metric, value, n) out[[length(out) + 1L]] <<- data.frame(metric = metric, value = value, n = n)
  tb <- Filter(function(b) b$type %in% c("paragraph", "heading", "title", "caption", "footnote"), truth$blocks)
  db <- doc$blocks[!is.na(doc$blocks$text), , drop = FALSE]
  db <- db[order(db$reading_order), , drop = FALSE]
  # a paragraph continued on the next page is one truth block
  for (j in rev(which(!is.na(db$continues) & db$continues %in% db$block_id))) {
    i <- match(db$continues[j], db$block_id)
    db$text[i] <- paste(db$text[i], db$text[j])
    db <- db[-j, , drop = FALSE]
  }
  if (length(tb)) {
    pos <- vapply(tb, function(b) best_match(b$text, db$text, threshold), integer(1))
    add("block_recall", mean(!is.na(pos)), length(tb))
    # character error rate over matched blocks; an unmatched truth block
    # counts as wholly wrong
    tt <- norm_cmp(vapply(tb, function(b) b$text, ""))
    edits <- vapply(seq_along(tt), function(k) {
      if (is.na(pos[k])) nchar(tt[k]) else utils::adist(tt[k], norm_cmp(db$text[pos[k]]))[1]
    }, numeric(1))
    add("character_error_rate", sum(edits) / max(1, sum(nchar(tt))), sum(nchar(tt)))
    ok <- !is.na(pos)
    if (sum(ok) >= 2L) {
      r <- db$reading_order[pos[ok]]
      tau <- suppressWarnings(stats::cor(seq_along(r), r, method = "kendall"))
      pairs <- utils::combn(length(r), 2)
      add("reading_order_tau", tau, sum(ok))
      add("reading_order_pairs", mean(r[pairs[1, ]] < r[pairs[2, ]]), ncol(pairs))
    }
  }
  ts <- vapply(truth$sections %||% list(), function(s) norm_cmp(s$title), "")
  ds <- norm_cmp(doc$sections$title)
  if (length(ts) || length(ds)) {
    tp <- sum(ts %in% ds)
    p <- if (length(ds)) sum(ds %in% ts) / length(ds) else NA_real_
    r <- if (length(ts)) tp / length(ts) else NA_real_
    add("section_precision", p, length(ds))
    add("section_recall", r, length(ts))
    add("section_f1", if (isTRUE(p + r > 0)) 2 * p * r / (p + r) else 0, length(ts))
  }
  tdet <- NULL
  for (t in truth$tables %||% list()) {
    lab <- t$label
    cand <- doc$tables[tolower(doc$tables$label %|NA|% "") == tolower(lab %||% ""), , drop = FALSE]
    if (!is.null(t$continued_of) && nrow(cand) > 1L) cand <- cand[!is.na(cand$continuation_of), , drop = FALSE]
    if (is.null(t$continued_of) && nrow(cand) > 1L) cand <- cand[is.na(cand$continuation_of), , drop = FALSE]
    cand <- cand[cand$page == t$page, , drop = FALSE]
    tcells <- t$cells
    nr <- length(tcells)
    nc <- if (nr) length(tcells[[1]]) else 0L
    if (!nrow(cand)) {
      tdet <- rbind(tdet, data.frame(label = lab, page = t$page, found = FALSE, dims_ok = FALSE, header_ok = FALSE,
                                     cell_accuracy = 0, spans_ok = FALSE, grits_con = 0, teds = 0))
      next
    }
    tid <- cand$table_id[1]
    m <- sc_table_to_matrix(doc, tid, join_continuations = FALSE)
    dims_ok <- nrow(m) == nr && ncol(m) == nc
    # position-aware: cells are compared at the same row and column; when
    # the grids differ in size, missing and extra positions count as errors
    tm <- do.call(rbind, lapply(tcells, function(r) {
      v <- vapply(r, function(z) z %||% "", "")
      length(v) <- nc
      v[is.na(v)] <- ""
      v
    }))
    # markers count as part of the cell text in the truth files
    d2 <- doc
    d2$cells$text <- d2$cells$text_raw
    mr <- sc_table_to_matrix(d2, tid, join_continuations = FALSE)
    sq <- function(z) gsub(" ", "", norm_cmp(z))
    rr <- min(nrow(m), nr)
    cc <- min(ncol(m), nc)
    hits <- if (rr && cc) {
      a <- tm[seq_len(rr), seq_len(cc), drop = FALSE]
      sum(norm_cmp(a) == norm_cmp(m[seq_len(rr), seq_len(cc), drop = FALSE]) |
            sq(a) == sq(mr[seq_len(rr), seq_len(cc), drop = FALSE]))
    } else 0
    acc <- hits / (max(nrow(m), nr) * max(ncol(m), nc))
    cl <- doc$cells[doc$cells$table_id == tid & (doc$cells$col_span > 1L | doc$cells$row_span > 1L), , drop = FALSE]
    spans_ok <- all(vapply(t$spans %||% list(), function(s) {
      any(cl$row == s$row & cl$col == s$col & cl$col_span == s$ncol & cl$row_span == (s$nrow %||% 1L))
    }, logical(1)))
    tc <- truth_cells(t)
    pc <- d2$cells[d2$cells$table_id == tid, c("row", "col", "row_span", "col_span", "text", "is_header"), drop = FALSE]
    tdet <- rbind(tdet, data.frame(label = lab, page = t$page, found = TRUE, dims_ok = dims_ok,
                                   header_ok = identical(as.integer(cand$n_header_rows[1]), as.integer(t$header_rows)),
                                   cell_accuracy = acc, spans_ok = spans_ok,
                                   grits_con = grits_con(fill_grid(tc), fill_grid(pc)), teds = teds(tc, pc)))
  }
  if (!is.null(tdet)) {
    add("table_recall", mean(tdet$found), nrow(tdet))
    add("table_structure", mean(tdet$dims_ok), nrow(tdet))
    add("table_header_rows", mean(tdet$header_ok), nrow(tdet))
    add("table_cell_accuracy", mean(tdet$cell_accuracy), nrow(tdet))
    add("table_spans", mean(tdet$spans_ok), nrow(tdet))
    add("table_grits_con", mean(tdet$grits_con), nrow(tdet))
    add("table_teds", mean(tdet$teds), nrow(tdet))
  }
  if (length(truth$tables)) {
    # detection at IoU 0.5 on the same page; truth files without horizontal
    # extents are scored on the vertical extent (top to bottom) only
    iou1 <- function(a0, a1, b0, b1) max(0, min(a1, b1) - max(a0, b0)) / (max(a1, b1) - min(a0, b0))
    det <- doc$tables[!is.na(doc$tables$y0), , drop = FALSE]
    used <- logical(nrow(det))
    hit <- 0L
    for (t in truth$tables) {
      if (is.null(t$top) || is.null(t$bottom)) next
      io <- vapply(seq_len(nrow(det)), function(k) {
        if (used[k] || det$page[k] != t$page) return(0)
        v <- iou1(t$top, t$bottom, det$y0[k], det$y1[k])
        if (!is.null(t$x0) && !is.null(t$x1)) v <- v * iou1(t$x0, t$x1, det$x0[k], det$x1[k])
        v
      }, numeric(1))
      if (length(io) && max(io) >= 0.5) {
        used[which.max(io)] <- TRUE
        hit <- hit + 1L
      }
    }
    add("table_detection_precision", if (nrow(det)) hit / nrow(det) else NA_real_, nrow(det))
    add("table_detection_recall", hit / length(truth$tables), length(truth$tables))
  }
  for (ent in c("figures", "equations", "references")) {
    n_t <- length(truth[[ent]] %||% list())
    if (n_t) add(paste0(ent, "_count_ratio"), nrow(doc[[ent]]) / n_t, n_t)
  }
  if (length(truth$references)) {
    dois <- tolower(unlist(lapply(truth$references, function(r) if (is.list(r)) r$doi else regmatches(r, regexpr("10\\.[0-9]{4,9}/[^ ]+[^ .]", r)))))
    if (length(dois)) add("reference_doi_recall", mean(dois %in% tolower(doc$references$doi)), length(dois))
  }
  if (!is.null(truth$image_only_pages)) {
    add("image_only_detected", as.numeric(truth$image_only_pages %in% doc$pages$page[doc$pages$image_only %in% TRUE]),
        length(truth$image_only_pages))
  }
  res <- do.call(rbind, out)
  attr(res, "tables") <- tdet
  res
}

norm_cmp <- function(x) {
  x <- normalize_text(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("[\u2013\u2014\u2212]", "-", x)
  tolower(gsub("\\s+", " ", trimws(x)))
}

best_match <- function(t, cands, threshold) {
  t <- norm_cmp(t)
  cn <- norm_cmp(cands)
  exact <- which(cn == t)
  if (length(exact)) return(exact[1])
  # containment handles blocks that merge or split differently
  cont <- which(nchar(cn) > 20 & (startsWith(cn, substr(t, 1, 40)) | startsWith(t, substr(cn, 1, 40))))
  sims <- vapply(cont, function(i) 1 - utils::adist(t, cn[i]) / max(nchar(t), nchar(cn[i])), numeric(1))
  if (length(sims) && max(sims) >= threshold) return(cont[which.max(sims)])
  NA_integer_
}

# ---- table similarity metrics ----------------------------------------------

# Content similarity of every cell of `a` with every cell of `b`:
# 2 * LCS / (|a| + |b|), 1 for two empty cells (the GriTS-Con cell score).
lcs_sim <- function(a, b) {
  a <- norm_cmp(a)
  b <- norm_cmp(b)
  d <- utils::adist(a, b, costs = list(ins = 1, del = 1, sub = 2))
  la <- nchar(a)
  lb <- nchar(b)
  tot <- outer(la, lb, "+")
  lcs <- (tot - d) / 2
  s <- ifelse(tot == 0, 1, 2 * lcs / pmax(tot, 1))
  matrix(s, length(a), length(b))
}

# Best order-preserving alignment of two sequences given a score matrix
# (gaps score 0). Returns the total and the matched index pairs.
align_seq <- function(sc) {
  n <- nrow(sc)
  m <- ncol(sc)
  f <- matrix(0, n + 1L, m + 1L)
  for (i in seq_len(n)) for (j in seq_len(m)) {
    f[i + 1L, j + 1L] <- max(f[i, j + 1L], f[i + 1L, j], f[i, j] + sc[i, j])
  }
  pairs <- list()
  i <- n
  j <- m
  while (i > 0L && j > 0L) {
    if (isTRUE(all.equal(f[i + 1L, j + 1L], f[i, j] + sc[i, j])) && sc[i, j] > 0) {
      pairs[[length(pairs) + 1L]] <- c(i, j)
      i <- i - 1L
      j <- j - 1L
    } else if (f[i, j + 1L] >= f[i + 1L, j]) {
      i <- i - 1L
    } else {
      j <- j - 1L
    }
  }
  p <- if (length(pairs)) do.call(rbind, rev(pairs)) else matrix(integer(), 0, 2)
  list(score = f[n + 1L, m + 1L], pairs = p)
}

# GriTS-Con (Smock et al. 2023) with the factored alignment: rows and
# columns of the two grids are aligned separately, then the cell scores of
# the aligned positions are summed. Grids have spanned text in every
# covered position.
grits_con <- function(a, b) {
  if (!length(a) || !length(b)) return(0)
  s <- lcs_sim(as.vector(a), as.vector(b))
  cell <- function(i, k, j, l) s[(k - 1L) * nrow(a) + i, (l - 1L) * nrow(b) + j]
  row_sc <- outer(seq_len(nrow(a)), seq_len(nrow(b)), Vectorize(function(i, j) {
    align_seq(outer(seq_len(ncol(a)), seq_len(ncol(b)), Vectorize(function(k, l) cell(i, k, j, l))))$score
  }))
  col_sc <- outer(seq_len(ncol(a)), seq_len(ncol(b)), Vectorize(function(k, l) {
    align_seq(outer(seq_len(nrow(a)), seq_len(nrow(b)), Vectorize(function(i, j) cell(i, k, j, l))))$score
  }))
  ra <- align_seq(row_sc)$pairs
  ca <- align_seq(col_sc)$pairs
  tot <- 0
  for (x in seq_len(nrow(ra))) for (y in seq_len(nrow(ca))) tot <- tot + cell(ra[x, 1], ca[y, 1], ra[x, 2], ca[y, 2])
  2 * tot / (length(a) + length(b))
}

# Ordered labelled tree as parallel vectors in postorder.
table_tree <- function(cells) {
  # cells: data frame with row, col, row_span, col_span, text, is_header
  lab <- character()
  txt <- character()
  kids <- list()
  add <- function(l, t, ch) {
    lab <<- c(lab, l)
    txt <<- c(txt, t)
    kids[[length(kids) + 1L]] <<- ch
    length(lab)
  }
  parts <- integer()
  for (part in c("thead", "tbody")) {
    sub <- cells[cells$is_header == (part == "thead"), , drop = FALSE]
    if (!nrow(sub)) next
    rows <- integer()
    for (r in sort(unique(sub$row))) {
      rc <- sub[sub$row == r, , drop = FALSE]
      rc <- rc[order(rc$col), , drop = FALSE]
      tds <- vapply(seq_len(nrow(rc)), function(k) {
        add(sprintf("td:%d:%d", rc$row_span[k], rc$col_span[k]), norm_cmp(rc$text[k]), integer())
      }, integer(1))
      rows <- c(rows, add("tr", "", tds))
    }
    parts <- c(parts, add(part, "", rows))
  }
  root <- add("table", "", parts)
  # postorder numbering
  ord <- integer()
  visit <- function(v) {
    for (c in kids[[v]]) visit(c)
    ord <<- c(ord, v)
  }
  visit(root)
  pos <- match(seq_along(lab), ord)
  lml <- integer(length(ord))
  for (k in seq_along(ord)) {
    v <- ord[k]
    lml[k] <- if (length(kids[[v]])) lml[pos[kids[[v]][1]]] else k
  }
  list(label = lab[ord], text = txt[ord], lml = lml)
}

# Tree edit distance (Zhang and Shasha 1989) with unit insert and delete
# costs; renaming a cell costs its normalized Levenshtein distance when the
# spans agree and 1 otherwise, as in TEDS (Zhong et al. 2020).
tree_edit_distance <- function(t1, t2) {
  n <- length(t1$label)
  m <- length(t2$label)
  ren <- function(i, j) {
    if (t1$label[i] != t2$label[j]) return(1)
    if (!startsWith(t1$label[i], "td")) return(0)
    a <- t1$text[i]
    b <- t2$text[j]
    if (!nzchar(a) && !nzchar(b)) return(0)
    utils::adist(a, b)[1] / max(nchar(a), nchar(b))
  }
  # key roots: the highest node of each leftmost leaf
  keyroots <- function(lml) which(!duplicated(lml, fromLast = TRUE))
  td <- matrix(0, n, m)
  for (i in keyroots(t1$lml)) for (j in keyroots(t2$lml)) {
    li <- t1$lml[i]
    lj <- t2$lml[j]
    fd <- matrix(0, i - li + 2L, j - lj + 2L)
    for (x in seq_len(i - li + 1L)) fd[x + 1L, 1] <- fd[x, 1] + 1
    for (y in seq_len(j - lj + 1L)) fd[1, y + 1L] <- fd[1, y] + 1
    for (x in seq_len(i - li + 1L)) for (y in seq_len(j - lj + 1L)) {
      i1 <- li + x - 1L
      j1 <- lj + y - 1L
      if (t1$lml[i1] == li && t2$lml[j1] == lj) {
        fd[x + 1L, y + 1L] <- min(fd[x, y + 1L] + 1, fd[x + 1L, y] + 1, fd[x, y] + ren(i1, j1))
        td[i1, j1] <- fd[x + 1L, y + 1L]
      } else {
        px <- t1$lml[i1] - li
        py <- t2$lml[j1] - lj
        fd[x + 1L, y + 1L] <- min(fd[x, y + 1L] + 1, fd[x + 1L, y] + 1, fd[px + 1L, py + 1L] + td[i1, j1])
      }
    }
  }
  td[n, m]
}

teds <- function(cells_a, cells_b) {
  t1 <- table_tree(cells_a)
  t2 <- table_tree(cells_b)
  1 - tree_edit_distance(t1, t2) / max(length(t1$label), length(t2$label))
}

# Cells of a truth table: covered positions are left out and spans set.
truth_cells <- function(t) {
  g <- t$cells
  nr <- length(g)
  if (!nr) return(data.frame(row = integer(), col = integer(), row_span = integer(), col_span = integer(),
                             text = character(), is_header = logical()))
  nc <- length(g[[1]])
  cov <- matrix(FALSE, nr, nc)
  rs <- cs <- matrix(1L, nr, nc)
  for (s in t$spans %||% list()) {
    nrw <- s$nrow %||% 1L
    rr <- s$row:min(nr, s$row + nrw - 1L)
    cc <- s$col:min(nc, s$col + s$ncol - 1L)
    cov[rr, cc] <- TRUE
    cov[s$row, s$col] <- FALSE
    rs[s$row, s$col] <- nrw
    cs[s$row, s$col] <- s$ncol
  }
  out <- list()
  for (r in seq_len(nr)) for (j in seq_len(nc)) {
    if (cov[r, j]) next
    out[[length(out) + 1L]] <- data.frame(row = r, col = j, row_span = rs[r, j], col_span = cs[r, j],
                                          text = g[[r]][[j]] %||% "", is_header = r <= (t$header_rows %||% 1L),
                                          stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

fill_grid <- function(cells) {
  if (!nrow(cells)) return(matrix(character(), 0, 0))
  m <- matrix("", max(cells$row + cells$row_span - 1L), max(cells$col + cells$col_span - 1L))
  for (k in seq_len(nrow(cells))) {
    m[cells$row[k]:(cells$row[k] + cells$row_span[k] - 1L), cells$col[k]:(cells$col[k] + cells$col_span[k] - 1L)] <- cells$text[k]
  }
  m
}

#' Evaluate an extraction against ground truth
#'
#' Scores extracted values against the `extraction` list of a truth file:
#' one entry per value with its field, statistic, number, source (`text` or
#' `table`), page and the context that should be bound (outcome, arm,
#' comparator, time point, analysis set, effect measure). Context fields
#' left out of a truth entry are not scored.
#'
#' @param x An `sc_extraction`.
#' @param truth Path to a truth JSON file, or the parsed list.
#' @return A data frame with columns `metric`, `value` and `n`:
#'   `value_recall` (truth values found), `value_precision` (extracted
#'   values of the scored fields that are in the truth), `binding_accuracy`
#'   (found values whose every scored context field is right),
#'   `binding_field_accuracy` (share of scored context fields that are
#'   right), `evidence_page_accuracy` and `unsupported_rate`. The matching
#'   of each truth entry is in attribute `"matches"`.
#' @examples
#' f <- system.file("extdata", "trial_tables.pdf", package = "sciparseR")
#' x <- sc_extract(sc_parse(f), "clinical_trial")
#' sc_evaluate_extraction(x, sub("\\.pdf$", ".truth.json", f))
#' @export
sc_evaluate_extraction <- function(x, truth) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  if (is.character(truth)) {
    if (!file.exists(truth)) abort("Truth file not found: ", truth)
    truth <- jsonlite::fromJSON(truth, simplifyVector = FALSE)
  }
  tr <- truth$extraction %||% list()
  if (!length(tr)) abort("The truth has no `extraction` entries.")
  r <- x$records[x$records$status != "superseded" & x$records$data_origin != "derived" &
                   !is.na(x$records$value_num), , drop = FALSE]
  r$source <- ifelse(grepl("table", r$method), "table", "text")
  ev <- x$evidence
  r$page <- ev$page[match(r$evidence_id, ev$evidence_id)]
  ctx <- c("outcome", "arm", "comparator", "timepoint", "analysis_set", "effect_measure")
  same <- function(k, a, b) {
    if (is.na(b)) return(FALSE)
    if (k == "outcome") {
      a <- norm_cmp(a)
      b <- norm_cmp(b)
      return(grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE))
    }
    tolower(a) == tolower(b)
  }
  used <- logical(nrow(r))
  rows <- lapply(tr, function(t) {
    cand <- which(!used & r$field == t$field & r$statistic %in% t$statistic &
                    abs(r$value_num - as.numeric(t$value)) < 1e-9 & r$source == (t$source %||% r$source))
    keys <- intersect(ctx, names(t))
    score <- vapply(cand, function(k) sum(vapply(keys, function(f) same(f, t[[f]], r[[f]][k]), logical(1))), numeric(1))
    if (!length(cand)) {
      return(data.frame(field = t$field, statistic = t$statistic, value = as.numeric(t$value), found = FALSE,
                        record_id = NA_character_, n_context = length(keys), context_ok = NA_integer_,
                        page_ok = NA, stringsAsFactors = FALSE))
    }
    k <- cand[which.max(score)]
    used[k] <<- TRUE
    data.frame(field = t$field, statistic = t$statistic, value = as.numeric(t$value), found = TRUE,
               record_id = r$record_id[k], n_context = length(keys), context_ok = as.integer(max(score)),
               page_ok = if (is.null(t$page)) NA else isTRUE(r$page[k] == t$page), stringsAsFactors = FALSE)
  })
  m <- do.call(rbind, rows)
  fields <- unique(vapply(tr, function(t) t$field, ""))
  scored <- r[r$field %in% fields & r$status %in% c("reported", "ambiguous", "inconsistent"), , drop = FALSE]
  f <- m[m$found, , drop = FALSE]
  all_r <- x$records[x$records$field %in% fields & !x$records$status %in% c("superseded", "not_reported"), , drop = FALSE]
  out <- data.frame(
    metric = c("value_recall", "value_precision", "binding_accuracy", "binding_field_accuracy",
               "evidence_page_accuracy", "unsupported_rate"),
    value = c(mean(m$found),
              if (nrow(scored)) sum(scored$record_id %in% f$record_id) / nrow(scored) else NA_real_,
              if (nrow(f)) mean(f$context_ok == f$n_context) else NA_real_,
              if (sum(f$n_context)) sum(f$context_ok) / sum(f$n_context) else NA_real_,
              if (any(!is.na(f$page_ok))) mean(f$page_ok, na.rm = TRUE) else NA_real_,
              if (nrow(all_r)) mean(all_r$status == "unsupported") else NA_real_),
    n = c(nrow(m), nrow(scored), nrow(f), sum(f$n_context), sum(!is.na(f$page_ok)), nrow(all_r)),
    stringsAsFactors = FALSE)
  attr(out, "matches") <- m
  out
}

#' Time and memory of parsing
#'
#' Parses each file `times` times and reports the median elapsed time and
#' the peak R heap above its starting level during one parse (from [gc()], so memory held
#' by external libraries such as poppler is not counted).
#'
#' @param paths Files to parse.
#' @param config An [sc_config()] object.
#' @param times Number of runs per file.
#' @return A data frame with file, pages, median seconds, seconds per page
#'   and peak memory in MB.
#' @examples
#' sc_benchmark(system.file("extdata", "single_column.pdf", package = "sciparseR"), times = 1)
#' @export
sc_benchmark <- function(paths, config = sc_config(), times = 3L) {
  rows <- lapply(paths, function(p) {
    secs <- numeric(times)
    peak <- NA_real_
    for (k in seq_len(times)) {
      g0 <- gc(reset = TRUE)
      t0 <- proc.time()[["elapsed"]]
      d <- sc_parse(p, config = config)
      secs[k] <- proc.time()[["elapsed"]] - t0
      g1 <- gc()
      # the MB column right after "max used", for cons and vector cells
      mb <- which(colnames(g1) == "max used") + 1L
      peak <- max(peak, sum(g1[, mb]) - sum(g0[, 2]), na.rm = TRUE)
    }
    np <- nrow(d$pages)
    data.frame(file = basename(p), pages = np, seconds = stats::median(secs),
               seconds_per_page = stats::median(secs) / max(1L, np), peak_mb = round(peak, 1),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

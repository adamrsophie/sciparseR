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
  if (is.character(truth)) truth <- jsonlite::fromJSON(truth, simplifyVector = FALSE)
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
                                     cell_accuracy = 0, spans_ok = FALSE))
      next
    }
    tid <- cand$table_id[1]
    m <- sc_table_to_matrix(doc, tid, join_continuations = FALSE)
    dims_ok <- nrow(m) == nr && ncol(m) == nc
    acc <- if (dims_ok) {
      tm <- matrix(unlist(lapply(tcells, function(r) vapply(r, function(z) z %||% "", ""))), nr, nc, byrow = TRUE)
      # markers count as part of the cell text in the truth files
      d2 <- doc
      d2$cells$text <- d2$cells$text_raw
      mr <- sc_table_to_matrix(d2, tid, join_continuations = FALSE)
      sq <- function(z) gsub(" ", "", norm_cmp(z))
      mean(norm_cmp(tm) == norm_cmp(m) | sq(tm) == sq(mr))
    } else {
      tv <- norm_cmp(unlist(tcells))
      tv <- tv[nzchar(tv)]
      mean(tv %in% norm_cmp(m))
    }
    cl <- doc$cells[doc$cells$table_id == tid & doc$cells$col_span > 1L, , drop = FALSE]
    spans_ok <- all(vapply(t$spans %||% list(), function(s) any(cl$row == s$row & cl$col == s$col & cl$col_span == s$ncol), logical(1)))
    tdet <- rbind(tdet, data.frame(label = lab, page = t$page, found = TRUE, dims_ok = dims_ok,
                                   header_ok = identical(as.integer(cand$n_header_rows[1]), as.integer(t$header_rows)),
                                   cell_accuracy = acc, spans_ok = spans_ok))
  }
  if (!is.null(tdet)) {
    add("table_recall", mean(tdet$found), nrow(tdet))
    add("table_structure", mean(tdet$dims_ok), nrow(tdet))
    add("table_header_rows", mean(tdet$header_ok), nrow(tdet))
    add("table_cell_accuracy", mean(tdet$cell_accuracy), nrow(tdet))
    add("table_spans", mean(tdet$spans_ok), nrow(tdet))
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

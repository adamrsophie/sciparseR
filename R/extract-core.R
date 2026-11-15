# Shared machinery of the extraction engines: text units with section
# labels, sentence splitting with offsets, evidence records and extraction
# records.

num_re <- "[-+]?(?:[0-9]{1,3}(?:,[0-9]{3})+|[0-9]+)(?:\\.[0-9]+)?|[-+]?\\.[0-9]+"

as_num <- function(x) {
  x <- gsub(",(?=[0-9]{3}\\b)", "", x, perl = TRUE)
  x <- gsub("[\u2212\u2013]", "-", x)
  suppressWarnings(as.numeric(x))
}

# Body text units (one per block) with the top level section label.
text_units <- function(doc) {
  b <- sc_blocks(doc)
  b <- b[b$type %in% c("paragraph", "caption", "footnote", "title") & !is.na(b$text), , drop = FALSE]
  lab <- doc$sections$label[match(b$section_id, doc$sections$section_id)]
  b$section_label <- ifelse(b$type == "title", "title", lab)
  if (!is.na(doc$metadata$abstract %||% NA) && doc$source$format == "jats") {
    b$section_label[b$section_label %in% NA & grepl("abs", b$xpath)] <- "abstract"
  }
  b[!b$section_label %in% c("references", "acknowledgments", "funding"), , drop = FALSE]
}

abbrev_re <- "\\b(et al|e\\.g|i\\.e|vs|Fig|Figs|No|approx|ca|Dr|St|Inc|Ltd|Eq|Ref|cf|resp|Suppl|Vol)\\."

# Sentence boundaries with 1-based offsets into `text`.
split_sentences <- function(text) {
  if (is.na(text) || !nzchar(text)) return(data.frame(start = integer(), end = integer(), text = character()))
  masked <- text
  m <- gregexpr(abbrev_re, masked, perl = TRUE)[[1]]
  if (m[1] > 0) {
    for (j in seq_along(m)) {
      e <- m[j] + attr(m, "match.length")[j] - 1L
      substr(masked, e, e) <- "\u0001"
    }
  }
  b <- gregexpr("(?<=[.!?])[\")\\]]?\\s+(?=[A-Z(\\[])", masked, perl = TRUE)[[1]]
  starts <- 1L
  ends <- integer()
  if (b[1] > 0) {
    for (j in seq_along(b)) {
      ends <- c(ends, b[j] - 1L)
      starts <- c(starts, b[j] + attr(b, "match.length")[j])
    }
  }
  ends <- c(ends, nchar(text))
  out <- data.frame(start = starts, end = ends, stringsAsFactors = FALSE)
  out$text <- substring(text, out$start, out$end)
  out[nzchar(trimws(out$text)), , drop = FALSE]
}

# Accumulator for records and evidence during one extraction run.
new_collector <- function(doc, schema_name, engine) {
  env <- new.env(parent = emptyenv())
  env$doc <- doc
  env$schema <- schema_name
  env$engine <- engine
  env$records <- list()
  env$evidence <- list()
  env$obs <- 0L
  env
}

next_observation <- function(col) {
  col$obs <- col$obs + 1L
  sprintf("obs-%04d", col$obs)
}

# Evidence for a value found in a block (char offsets into the block text)
# or in a table cell (offsets into the cell text).
add_evidence <- function(col, block_id = NA_character_, cell_id = NA_character_,
                         char_start = NA_integer_, char_end = NA_integer_,
                         value_text = NA_character_, method = "rules",
                         scope_text = NULL) {
  doc <- col$doc
  ev <- list(doc_id = doc$doc_id, sha256 = doc$source$sha256, block_id = block_id,
             cell_id = cell_id, char_start = char_start, char_end = char_end,
             value_text = value_text, method = method)
  if (!is.na(cell_id)) {
    cl <- doc$cells[match(cell_id, doc$cells$cell_id), ]
    tb <- doc$tables[match(cl$table_id, doc$tables$table_id), ]
    blk <- doc$blocks[doc$blocks$parent_id %in% tb$table_id, , drop = FALSE]
    ev <- c(ev, list(table_id = tb$table_id, page = tb$page, text = cl$text,
                     x0 = cl$x0, y0 = cl$y0, x1 = cl$x1, y1 = cl$y1,
                     bbox_scope = if (is.na(cl$x0)) NA_character_ else "cell",
                     xpath = cl$xpath,
                     section_id = if (nrow(blk)) blk$section_id[1] else NA_character_))
  } else {
    bl <- doc$blocks[match(block_id, doc$blocks$block_id), ]
    txt <- scope_text %||% bl$text
    bb <- c(bl$x0, bl$y0, bl$x1, bl$y1)
    scope <- if (is.na(bl$x0)) NA_character_ else "block"
    if (!is.na(value_text) && nrow(doc$spans)) {
      sp <- doc$spans[doc$spans$block_id %in% block_id, , drop = FALSE]
      ix <- span_run(sp$text, value_text, bl$text, char_start)
      if (length(ix)) {
        bb <- bbox_union(sp$x0[ix], sp$y0[ix], sp$x1[ix], sp$y1[ix])
        scope <- "span"
      }
    }
    ev <- c(ev, list(page = bl$page, text = txt, x0 = bb[1], y0 = bb[2], x1 = bb[3], y1 = bb[4],
                     bbox_scope = scope, xpath = bl$xpath, section_id = bl$section_id))
  }
  ev$section_label <- col$doc$sections$label[match(ev$section_id, col$doc$sections$section_id)]
  ev$evidence_id <- sprintf("ev-%05d", length(col$evidence) + 1L)
  col$evidence[[length(col$evidence) + 1L]] <- ev
  ev$evidence_id
}

add_record <- function(col, field, value, statistic = NA_character_, value_num = NA_real_,
                       status = "reported", data_origin = "reported", evidence_id = NA_character_,
                       method = "rules", ...) {
  extra <- list(...)
  rec <- c(list(field = field, value = as.character(value), statistic = statistic,
                value_num = value_num, status = status, data_origin = data_origin,
                evidence_id = evidence_id, method = method, engine = col$engine,
                doc_id = col$doc$doc_id, schema = col$schema), extra)
  col$records[[length(col$records) + 1L]] <- rec
  invisible(length(col$records))
}

collector_frames <- function(col) {
  recs <- bind_entity(lapply(col$records, function(r) as.data.frame(r[!vapply(r, is.null, logical(1))], stringsAsFactors = FALSE)), "records")
  if (nrow(recs)) recs$record_id <- sprintf("rec-%05d", seq_len(nrow(recs)))
  ev <- bind_entity(lapply(col$evidence, function(e) as.data.frame(e[!vapply(e, is.null, logical(1))], stringsAsFactors = FALSE)), "evidence")
  list(records = recs, evidence = ev)
}

match_vocab <- function(text, vocab) {
  hits <- character()
  pos <- integer()
  len <- integer()
  for (nm in names(vocab)) {
    m <- regexpr(vocab[[nm]], text, perl = TRUE, ignore.case = TRUE)
    if (m > 0) {
      hits <- c(hits, nm)
      pos <- c(pos, m)
      len <- c(len, attr(m, "match.length"))
    }
  }
  data.frame(term = hits, start = pos, length = len, stringsAsFactors = FALSE)
}

timepoint_re <- paste0(
  "\\b(?:at|after|by|through|over|to)\\s+(?:week|month|year|day)\\s+[0-9]+\\b|",
  "\\b(?:at|after|by|through|over|to)\\s+[0-9]+(?:\\.[0-9]+)?\\s+(?:days?|weeks?|months?|years?)\\b|",
  "\\b[0-9]+[- ](?:day|week|month|year)\\b")

normalize_timepoint <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- tolower(x)
  n <- regmatches(x, regexpr("[0-9]+(\\.[0-9]+)?", x))
  u <- regmatches(x, regexpr("day|week|month|year", x))
  if (!length(n) || !length(u)) return(NA_character_)
  paste0(n, " ", u, if (as.numeric(n) != 1) "s" else "")
}

find_timepoint <- function(text) {
  m <- regmatches(text, regexpr(timepoint_re, text, perl = TRUE, ignore.case = TRUE))
  if (!length(m)) NA_character_ else normalize_timepoint(m)
}

find_analysis_set <- function(text) {
  v <- match_vocab(text, analysis_set_vocabulary)
  if (!nrow(v)) return(NA_character_)
  v$term[1]
}

# Indices of the spans (words) that hold `value_text`. Tokens are compared
# without punctuation; when the value occurs more than once in the block,
# the occurrence nearest to `char_start` is used.
span_run <- function(span_text, value_text, block_text, char_start) {
  key <- function(z) gsub("[^[:alnum:]]", "", tolower(normalize_text(z)))
  all_st <- key(span_text)
  keep <- which(nzchar(all_st))
  st <- all_st[keep]
  toks <- key(strsplit(trimws(value_text), "\\s+")[[1]])
  toks <- toks[nzchar(toks)]
  k <- length(toks)
  if (!k || length(st) < k) return(integer())
  hit <- function(a, b) a == b || (nzchar(a) && (startsWith(a, b) || endsWith(a, b)))
  starts <- which(vapply(seq_len(length(st) - k + 1L), function(i) {
    all(mapply(hit, st[i:(i + k - 1L)], toks))
  }, logical(1)))
  if (!length(starts)) return(integer())
  if (length(starts) > 1L) {
    if (is.na(char_start) || is.na(block_text)) return(integer())
    # approximate the character position of each span in the block text
    pos <- cumsum(c(1L, nchar(normalize_text(span_text[-length(span_text)])) + 1L))[keep]
    starts <- starts[which.min(abs(pos[starts] - char_start))]
  }
  keep[starts]:keep[starts + k - 1L]
}

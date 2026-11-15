#' Export documents and extractions
#'
#' * `sc_export_json()` writes the complete document or extraction model,
#'   versioned by `schema_version`; [sc_read_json()] reads it back with
#'   column types restored. The JSON Schema is in
#'   `system.file("schema", package = "sciparseR")`.
#' * `sc_export_markdown()` writes readable Markdown: headings, paragraphs,
#'   tables as pipe tables, captions, equations and references.
#' * `sc_export_csv()` and `sc_export_parquet()` write one file per entity
#'   table (Parquet needs the arrow package).
#' * `sc_export_jats()` writes JATS XML from the document model.
#' * `sc_export_evidence()` writes records joined to their evidence as CSV
#'   or JSON, one row per record.
#'
#' The absolute source path is not written unless `include_path = TRUE`;
#' the file name and SHA-256 checksum always are.
#'
#' @param x An `sc_document` or `sc_extraction` (only an `sc_document` for
#'   Markdown and JATS; only an `sc_extraction` for evidence).
#' @param file Output file.
#' @param dir Output directory.
#' @param include_path Whether to keep the absolute source path.
#' @param pretty Indent JSON.
#' @param format `"csv"` or `"json"`.
#' @return The output path(s), invisibly.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' f <- sc_export_json(doc, tempfile(fileext = ".json"))
#' identical(sc_read_json(f)$cells$text, doc$cells$text)
#' md <- sc_export_markdown(doc, tempfile(fileext = ".md"))
#' cat(head(readLines(md), 12), sep = "\n")
#' @name export
NULL

strip_path <- function(x, include_path) {
  if (!include_path && is.list(x$source)) x$source$path <- NULL
  x
}

#' @rdname export
#' @export
sc_export_json <- function(x, file, include_path = FALSE, pretty = TRUE) {
  if (!inherits(x, c("sc_document", "sc_extraction"))) abort("`x` must be an sc_document or sc_extraction.")
  obj <- strip_path(unclass(x), include_path)
  obj$object_type <- class(x)[1]
  obj <- obj[c("object_type", setdiff(names(obj), "object_type"))]
  json <- jsonlite::toJSON(obj, dataframe = "rows", na = "null", null = "null", auto_unbox = TRUE,
                           digits = NA, pretty = pretty)
  writeLines(json, file, useBytes = TRUE)
  invisible(file)
}

#' Read a sciparseR JSON export
#'
#' @param file A file written by [sc_export_json()].
#' @return An `sc_document` or `sc_extraction`.
#' @examples
#' doc <- sc_parse(system.file("extdata", "unicode.pdf", package = "sciparseR"))
#' d2 <- sc_read_json(sc_export_json(doc, tempfile(fileext = ".json")))
#' class(d2)
#' @export
sc_read_json <- function(file) {
  obj <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE, simplifyVector = TRUE, simplifyMatrix = FALSE)
  type <- obj$object_type
  if (!type %in% c("sc_document", "sc_extraction")) abort("Not a sciparseR export.")
  if (!identical(obj$schema_version, sc_schema_version)) {
    warning("File schema version ", obj$schema_version, " differs from ", sc_schema_version, ".", call. = FALSE)
  }
  obj$object_type <- NULL
  ents <- if (type == "sc_document") names(.entity_specs) else names(.extraction_specs)
  for (e in ents) {
    v <- obj[[e]]
    obj[[e]] <- if (is.null(v) || !length(v) || (is.list(v) && !is.data.frame(v))) empty_entity(e) else as_entity(v, e)
  }
  structure(obj, class = type)
}

#' @rdname export
#' @export
sc_export_csv <- function(x, dir) {
  ents <- export_entities(x)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- file.path(dir, paste0(ents, ".csv"))
  for (k in seq_along(ents)) utils::write.csv(x[[ents[k]]], files[k], row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(files)
}

#' @rdname export
#' @export
sc_export_parquet <- function(x, dir) {
  if (!is_installed("arrow")) abort("The arrow package is needed for Parquet export.")
  ents <- export_entities(x)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- file.path(dir, paste0(ents, ".parquet"))
  for (k in seq_along(ents)) arrow::write_parquet(x[[ents[k]]], files[k])
  invisible(files)
}

#' @rdname export
#' @param doc For `sc_export_tables()`, an `sc_document`.
#' @export
sc_export_tables <- function(doc, dir) {
  check_document(doc)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  for (tid in doc$tables$table_id[!doc$tables$continuation_status %in% "joined"]) {
    f <- file.path(dir, paste0(tid, ".csv"))
    utils::write.csv(sc_table_to_dataframe(doc, tid), f, row.names = FALSE, fileEncoding = "UTF-8")
    files <- c(files, f)
  }
  # the long cell table keeps spans, header flags, markers and boxes
  f <- file.path(dir, "cells.csv")
  utils::write.csv(doc$cells, f, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(c(files, f))
}

export_entities <- function(x) {
  if (inherits(x, "sc_document")) return(names(.entity_specs))
  if (inherits(x, "sc_extraction")) return(names(.extraction_specs))
  abort("`x` must be an sc_document or sc_extraction.")
}

#' @rdname export
#' @export
sc_export_evidence <- function(x, file, format = c("csv", "json")) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  format <- match.arg(format)
  ev <- x$evidence
  r <- x$records
  e <- ev[match(r$evidence_id, ev$evidence_id), setdiff(names(ev), c("doc_id", "method")), drop = FALSE]
  names(e) <- paste0("evidence_", names(e))
  out <- cbind(r, e)
  out$source <- x$source
  if (format == "csv") utils::write.csv(out, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  else writeLines(jsonlite::toJSON(out, dataframe = "rows", na = "null", digits = NA, pretty = TRUE), file, useBytes = TRUE)
  invisible(file)
}

md_escape <- function(x) gsub("|", "\\|", gsub("\n", " ", x), fixed = TRUE)

md_table <- function(doc, tid) {
  m <- tryCatch(sc_table_to_matrix(doc, tid), error = function(e) NULL)
  if (is.null(m) || !length(m)) return(character())
  nh <- max(1L, attr(m, "n_header") %||% 0L)
  hdr <- vapply(seq_len(ncol(m)), function(j) paste(unique(m[seq_len(nh), j][nzchar(m[seq_len(nh), j])]), collapse = " / "), "")
  body <- if (nrow(m) > nh) m[-seq_len(nh), , drop = FALSE] else matrix("", 0, ncol(m))
  c(paste0("| ", paste(md_escape(hdr), collapse = " | "), " |"),
    paste0("|", paste(rep(" --- ", ncol(m)), collapse = "|"), "|"),
    if (nrow(body)) apply(body, 1, function(r) paste0("| ", paste(md_escape(r), collapse = " | "), " |")))
}

#' @rdname export
#' @export
sc_export_markdown <- function(x, file) {
  check_document(x, "x")
  doc <- x
  out <- character()
  md <- doc$metadata
  if (!is.na(md$title %||% NA)) out <- c(out, paste("#", md$title), "")
  b <- doc$blocks
  b <- b[!b$type %in% c("header", "footer", "title", "page_number"), , drop = FALSE]
  b <- b[order(b$reading_order), , drop = FALSE]
  done_tables <- character()
  for (i in seq_len(nrow(b))) {
    bl <- b[i, ]
    if (bl$type %in% "heading") {
      lvl <- doc$sections$level[match(bl$block_id, doc$sections$block_id)] %|NA|% 1L
      out <- c(out, paste(strrep("#", min(6L, lvl + 1L)), bl$text), "")
    } else if (bl$type %in% "table") {
      tid <- bl$parent_id
      if (!is.na(tid) && !tid %in% done_tables) {
        done_tables <- c(done_tables, tid)
        st <- doc$tables$continuation_status[match(tid, doc$tables$table_id)]
        if (!st %in% "joined") out <- c(out, md_table(doc, tid), "")
      }
    } else if (bl$type %in% "equation") {
      eq <- doc$equations[doc$equations$block_id %in% bl$block_id, , drop = FALSE]
      tex <- if (nrow(eq) && !is.na(eq$latex[1])) eq$latex[1] else NA
      out <- c(out, if (!is.na(tex)) c("$$", tex, "$$") else paste0("    ", bl$text), "")
    } else if (bl$type %in% "caption") {
      out <- c(out, paste0("*", md_escape(bl$text), "*"), "")
    } else if (bl$type %in% "reference") {
      next
    } else if (!is.na(bl$text) && nzchar(bl$text)) {
      out <- c(out, bl$text, "")
    }
  }
  # JATS documents have tables without table blocks
  rest <- setdiff(doc$tables$table_id[!doc$tables$continuation_status %in% "joined"], done_tables)
  for (tid in rest) {
    cap <- doc$captions$text[match(doc$tables$caption_id[match(tid, doc$tables$table_id)], doc$captions$caption_id)]
    if (!is.na(cap) && !any(grepl(cap, out, fixed = TRUE))) out <- c(out, paste0("*", md_escape(cap), "*"), "")
    out <- c(out, md_table(doc, tid), "")
  }
  if (nrow(doc$references)) {
    out <- c(out, "## References", "")
    lab <- ifelse(is.na(doc$references$label), "", paste0(doc$references$label, ". "))
    out <- c(out, paste0(lab, doc$references$text), "")
  }
  writeLines(enc2utf8(out), file, useBytes = TRUE)
  invisible(file)
}

#' @rdname export
#' @export
sc_export_jats <- function(x, file) {
  check_document(x, "x")
  doc <- x
  md <- doc$metadata
  root <- xml2::xml_new_root("article", "article-type" = "research-article", "dtd-version" = "1.3",
                             "xmlns:xlink" = "http://www.w3.org/1999/xlink",
                             "xmlns:mml" = "http://www.w3.org/1998/Math/MathML")
  front <- xml2::xml_add_child(root, "front")
  am <- xml2::xml_add_child(front, "article-meta")
  if (!is.na(md$doi %||% NA)) xml2::xml_add_child(am, "article-id", md$doi, "pub-id-type" = "doi")
  tg <- xml2::xml_add_child(am, "title-group")
  xml2::xml_add_child(tg, "article-title", md$title %|NA|% "")
  auth <- md$authors %||% character()
  if (length(auth) && !all(is.na(auth))) {
    cg <- xml2::xml_add_child(am, "contrib-group")
    for (a in auth[!is.na(auth)]) {
      ct <- xml2::xml_add_child(cg, "contrib", "contrib-type" = "author")
      xml2::xml_add_child(ct, "string-name", a)
    }
  }
  if (!is.na(md$abstract %||% NA)) {
    ab <- xml2::xml_add_child(am, "abstract")
    xml2::xml_add_child(ab, "p", md$abstract)
  }
  body <- xml2::xml_add_child(root, "body")
  secs <- doc$sections[doc$sections$label != "references" | is.na(doc$sections$label), , drop = FALSE]
  node_of <- list()
  b <- doc$blocks[order(doc$blocks$reading_order), , drop = FALSE]
  b <- b[!b$type %in% c("header", "footer", "title", "reference", "heading", "page_number"), , drop = FALSE]
  parent_node <- function(sid) {
    if (is.na(sid) || !sid %in% secs$section_id) return(body)
    if (!is.null(node_of[[sid]])) return(node_of[[sid]])
    s <- secs[secs$section_id == sid, ]
    par <- parent_node(s$parent_id)
    n <- xml2::xml_add_child(par, "sec", id = sid)
    xml2::xml_add_child(n, "title", s$title)
    node_of[[sid]] <<- n
    n
  }
  for (s in secs$section_id) parent_node(s)
  emitted <- character()
  for (i in seq_len(nrow(b))) {
    bl <- b[i, ]
    par <- parent_node(bl$section_id)
    if (bl$type %in% "table") {
      tid <- bl$parent_id
      if (is.na(tid) || tid %in% emitted) next
      emitted <- c(emitted, tid)
      add_jats_table(par, doc, tid)
    } else if (bl$type %in% "equation") {
      eq <- doc$equations[doc$equations$block_id %in% bl$block_id, , drop = FALSE]
      df <- xml2::xml_add_child(par, "disp-formula")
      if (nrow(eq) && !is.na(eq$label[1])) xml2::xml_add_child(df, "label", eq$label[1])
      if (nrow(eq) && !is.na(eq$latex[1])) xml2::xml_add_child(df, "tex-math", eq$latex[1]) else xml2::xml_add_child(df, "alternatives", bl$text)
    } else if (bl$type %in% "caption") {
      next
    } else if (bl$type %in% "figure") {
      f <- doc$figures[doc$figures$figure_id %in% bl$parent_id, , drop = FALSE]
      fg <- xml2::xml_add_child(par, "fig", id = f$figure_id[1] %|NA|% bl$block_id)
      if (nrow(f) && !is.na(f$label[1])) xml2::xml_add_child(fg, "label", f$label[1])
      cap <- doc$captions$text[match(f$caption_id[1], doc$captions$caption_id)]
      if (!is.na(cap %|NA|% NA)) xml2::xml_add_child(xml2::xml_add_child(fg, "caption"), "p", cap)
    } else if (!is.na(bl$text) && nzchar(bl$text)) {
      xml2::xml_add_child(par, "p", bl$text)
    }
  }
  for (tid in setdiff(doc$tables$table_id[!doc$tables$continuation_status %in% "joined"], emitted)) add_jats_table(body, doc, tid)
  if (nrow(doc$references)) {
    back <- xml2::xml_add_child(root, "back")
    rl <- xml2::xml_add_child(back, "ref-list")
    for (k in seq_len(nrow(doc$references))) {
      r <- doc$references[k, ]
      rn <- xml2::xml_add_child(rl, "ref", id = r$ref_id)
      if (!is.na(r$label)) xml2::xml_add_child(rn, "label", r$label)
      mc <- xml2::xml_add_child(rn, "mixed-citation", r$text)
      if (!is.na(r$doi)) xml2::xml_add_child(mc, "pub-id", r$doi, "pub-id-type" = "doi")
      if (!is.na(r$pmid)) xml2::xml_add_child(mc, "pub-id", r$pmid, "pub-id-type" = "pmid")
    }
  }
  xml2::write_xml(root, file, options = c("format", "as_xml"))
  invisible(file)
}

add_jats_table <- function(parent, doc, tid) {
  tb <- doc$tables[doc$tables$table_id == tid, ]
  tw <- xml2::xml_add_child(parent, "table-wrap", id = tid)
  if (!is.na(tb$label)) xml2::xml_add_child(tw, "label", tb$label)
  cap <- doc$captions$text[match(tb$caption_id, doc$captions$caption_id)]
  if (!is.na(cap %|NA|% NA)) xml2::xml_add_child(xml2::xml_add_child(tw, "caption"), "p", cap)
  tab <- xml2::xml_add_child(tw, "table")
  cl <- doc$cells[doc$cells$table_id == tid, , drop = FALSE]
  cl <- cl[order(cl$row, cl$col), , drop = FALSE]
  covered <- character()
  for (part in c("thead", "tbody")) {
    sub <- cl[cl$is_header == (part == "thead"), , drop = FALSE]
    if (!nrow(sub)) next
    pn <- xml2::xml_add_child(tab, part)
    for (r in unique(sub$row)) {
      tr <- xml2::xml_add_child(pn, "tr")
      rc <- sub[sub$row == r, , drop = FALSE]
      for (k in seq_len(nrow(rc))) {
        c1 <- rc[k, ]
        if (paste(c1$row, c1$col) %in% covered) next
        attrs <- list()
        if (c1$col_span > 1L) attrs$colspan <- c1$col_span
        if (c1$row_span > 1L) attrs$rowspan <- c1$row_span
        td <- do.call(xml2::xml_add_child, c(list(tr, if (part == "thead") "th" else "td", c1$text), attrs))
        if (!is.na(c1$markers)) for (mk in split_ids(c1$markers)) xml2::xml_add_child(td, "sup", mk)
      }
    }
  }
  fn <- doc$footnotes[doc$footnotes$target_id %in% tid, , drop = FALSE]
  if (nrow(fn)) {
    fg <- xml2::xml_add_child(tw, "table-wrap-foot")
    for (k in seq_len(nrow(fn))) {
      f <- xml2::xml_add_child(fg, "fn")
      if (!is.na(fn$marker[k])) xml2::xml_add_child(f, "label", fn$marker[k])
      xml2::xml_add_child(f, "p", fn$text[k])
    }
  }
  invisible(tw)
}

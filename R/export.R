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
#' Every entity array must be present. Identifiers must be unique,
#' references between tables (blocks to sections, cells to tables,
#' records to evidence, corrections to the records they supersede) must
#' resolve, and section parents and supersedes chains must not form cycles;
#' otherwise the file is refused.
#'
#' @param file A file written by [sc_export_json()].
#' @return An `sc_document` or `sc_extraction`.
#' @examples
#' doc <- sc_parse(system.file("extdata", "unicode.pdf", package = "sciparseR"))
#' d2 <- sc_read_json(sc_export_json(doc, tempfile(fileext = ".json")))
#' class(d2)
#' @export
sc_read_json <- function(file) {
  check_string(file, "file")
  if (!file.exists(file)) abort("File not found: ", file)
  txt <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  if (!grepl("^\\s*\\{", txt)) abort("Not a sciparseR export: the file must hold a JSON object.")
  obj <- tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE, simplifyVector = TRUE, simplifyMatrix = FALSE),
                  error = function(e) abort("Not valid JSON: ", conditionMessage(e)))
  type <- obj$object_type
  if (!is.character(type) || length(type) != 1L || !type %in% c("sc_document", "sc_extraction")) abort("Not a sciparseR export.")
  ver <- obj$schema_version
  if (!is.character(ver) || length(ver) != 1L) abort("The export has no schema version.")
  if (!identical(sub("\\..*$", "", ver), sub("\\..*$", "", sc_schema_version))) {
    abort("Schema version ", ver, " is not supported (this version reads ", sc_schema_version, ").")
  }
  if (!identical(ver, sc_schema_version)) {
    warning("File schema version ", ver, " differs from ", sc_schema_version, ".", call. = FALSE)
  }
  id_ok <- function(v) is.character(v) && length(v) == 1L && grepl("^[A-Za-z0-9_.:-]+$", v)
  if (!id_ok(obj$doc_id)) abort("The export has no valid `doc_id`.")
  if (type == "sc_document") {
    src <- obj$source
    if (!is.list(src) || !is.character(src$sha256) || !grepl("^[0-9a-f]{64}$", src$sha256 %||% "") ||
        !is.character(src$format) || !is.list(obj$processing)) {
      abort("The export lacks the source and processing records of a document.")
    }
  } else if (!is.character(obj$sha256) || !is.list(obj$schema)) {
    abort("The export lacks the source checksum or schema of an extraction.")
  }
  obj$object_type <- NULL
  if (type == "sc_document") obj$metadata <- restore_metadata(obj$metadata)
  ents <- if (type == "sc_document") names(.entity_specs) else names(.extraction_specs)
  # every entity table is required, as an array (empty when it has no rows)
  missing <- ents[!ents %in% names(obj) | vapply(ents, function(e) is.null(obj[[e]]), logical(1))]
  if (length(missing)) abort("The export lacks the entity arrays: ", paste(missing, collapse = ", "), ".")
  for (e in ents) {
    v <- obj[[e]]
    if (is.list(v) && !is.data.frame(v) && length(v)) abort("Entity `", e, "` must be an array of records.")
    obj[[e]] <- if (!length(v)) empty_entity(e) else as_entity(v, e)
    # identifiers become file names and keys: letters, digits and . _ : - only
    for (idc in grep("_id$", names(obj[[e]]), value = TRUE)) {
      iv <- obj[[e]][[idc]]
      bad <- !is.na(iv) & !grepl("^[A-Za-z0-9_.:-]+$", iv)
      if (any(bad)) abort("Invalid identifier in ", e, "$", idc, ": ", iv[bad][1])
    }
  }
  check_graph(obj, type)
  structure(obj, class = type)
}

# Graph invariants of an imported object: unique primary ids, references
# that resolve, and parent and supersedes chains without cycles.
check_graph <- function(obj, type) {
  pk <- if (type == "sc_document") {
    c(spans = "span_id", lines = "line_id", blocks = "block_id", sections = "section_id", tables = "table_id",
      cells = "cell_id", figures = "figure_id", equations = "equation_id", captions = "caption_id",
      footnotes = "footnote_id", references = "ref_id", citations = "citation_id", supplementary = "supp_id")
  } else c(records = "record_id", evidence = "evidence_id")
  for (e in names(pk)) {
    id <- obj[[e]][[pk[[e]]]]
    if (any(is.na(id))) abort("Missing identifier in ", e, "$", pk[[e]], ".")
    if (anyDuplicated(id)) abort("Duplicate identifier in ", e, "$", pk[[e]], ": ", id[duplicated(id)][1])
  }
  fk <- if (type == "sc_document") {
    list(c("blocks", "section_id", "sections", "section_id"), c("sections", "parent_id", "sections", "section_id"),
         c("sections", "block_id", "blocks", "block_id"), c("cells", "table_id", "tables", "table_id"),
         c("captions", "block_id", "blocks", "block_id"), c("citations", "ref_id", "references", "ref_id"),
         c("citations", "block_id", "blocks", "block_id"), c("tables", "caption_id", "captions", "caption_id"),
         c("spans", "line_id", "lines", "line_id"), c("lines", "block_id", "blocks", "block_id"))
  } else {
    list(c("records", "evidence_id", "evidence", "evidence_id"), c("records", "supersedes", "records", "record_id"))
  }
  for (f in fk) {
    v <- obj[[f[1]]][[f[2]]]
    bad <- !is.na(v) & !v %in% obj[[f[3]]][[f[4]]]
    if (any(bad)) abort("Unresolved reference ", f[1], "$", f[2], " = ", v[bad][1], " (no such ", f[3], "$", f[4], ").")
  }
  chains <- if (type == "sc_document") list(c("sections", "section_id", "parent_id")) else list(c("records", "record_id", "supersedes"))
  for (ch in chains) {
    id <- obj[[ch[1]]][[ch[2]]]
    up <- obj[[ch[1]]][[ch[3]]]
    for (k in seq_along(id)) {
      cur <- up[k]
      steps <- 0L
      while (!is.na(cur)) {
        if (identical(cur, id[k]) || steps > length(id)) abort("Cycle in ", ch[1], "$", ch[3], " at ", id[k], ".")
        cur <- up[match(cur, id)]
        steps <- steps + 1L
      }
    }
  }
  invisible(TRUE)
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
    f <- file.path(dir, paste0(safe_file_name(tid), ".csv"))
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
      if (!is.na(tex)) out <- c(out, "$$", tex, "$$", "")
      else if (!is.na(bl$text) && nzchar(bl$text)) out <- c(out, paste0("    ", bl$text), "")
    } else if (bl$type %in% "caption") {
      if (!is.na(bl$text) && nzchar(bl$text)) out <- c(out, paste0("*", md_escape(bl$text), "*"), "")
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
  if (!is.na(md$journal %||% NA)) {
    xml2::xml_add_child(xml2::xml_add_child(xml2::xml_add_child(front, "journal-meta"), "journal-title-group"),
                        "journal-title", md$journal)
  }
  am <- xml2::xml_add_child(front, "article-meta")
  for (idt in c("doi", "pmid", "pmcid")) {
    if (!is.na(md[[idt]] %||% NA)) xml2::xml_add_child(am, "article-id", md[[idt]], "pub-id-type" = idt)
  }
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
  if (!is.na(md$year %||% NA)) xml2::xml_add_child(xml2::xml_add_child(am, "pub-date", "pub-type" = "epub"), "year", md$year)
  # the abstract goes to the front matter once, never also into the body,
  # with its subsections ("Methods", "Results") kept
  sec_lab <- doc$sections$label[match(doc$blocks$section_id, doc$sections$section_id)]
  in_abs <- sec_lab %in% "abstract" | grepl("/abstract", doc$blocks$xpath %|NA|% "")
  abs_blocks <- doc$blocks[in_abs & doc$blocks$type %in% "paragraph", , drop = FALSE]
  abs_blocks <- abs_blocks[order(abs_blocks$reading_order), , drop = FALSE]
  if (nrow(abs_blocks) || !is.na(md$abstract %||% NA)) {
    ab <- xml2::xml_add_child(am, "abstract")
    abs_root <- doc$sections$section_id[doc$sections$label %in% "abstract" & is.na(doc$sections$parent_id)]
    abs_node <- list()
    abs_parent <- function(sid) {
      if (is.na(sid) || sid %in% abs_root || !sid %in% doc$sections$section_id) return(ab)
      if (!is.null(abs_node[[sid]])) return(abs_node[[sid]])
      s <- doc$sections[doc$sections$section_id == sid, ]
      n <- xml2::xml_add_child(abs_parent(s$parent_id), "sec")
      if (!is.na(s$title)) xml2::xml_add_child(n, "title", s$title)
      abs_node[[sid]] <<- n
      n
    }
    if (nrow(abs_blocks)) for (k in seq_len(nrow(abs_blocks))) add_jats_paragraph(abs_parent(abs_blocks$section_id[k]), abs_blocks[k, ], doc$citations)
    else xml2::xml_add_child(ab, "p", md$abstract)
  }
  kw <- md$keywords %||% character()
  if (length(kw)) {
    kg <- xml2::xml_add_child(am, "kwd-group")
    for (k in kw) xml2::xml_add_child(kg, "kwd", k)
  }
  if (!is.na(md$license %||% NA) || !is.na(md$license_text %||% NA)) {
    lic <- xml2::xml_add_child(xml2::xml_add_child(am, "permissions"), "license")
    if (grepl("^https?://", md$license %|NA|% "")) xml2::xml_set_attr(lic, "xlink:href", md$license)
    lt <- md$license_text %|NA|% md$license
    if (!is.na(lt)) xml2::xml_add_child(lic, "license-p", lt)
  }
  body <- xml2::xml_add_child(root, "body")
  secs <- doc$sections[!doc$sections$label %in% c("references", "abstract"), , drop = FALSE]
  node_of <- list()
  b <- doc$blocks[!in_abs, , drop = FALSE]
  b <- b[order(b$reading_order), , drop = FALSE]
  b <- b[!b$type %in% c("header", "footer", "title", "reference", "page_number"), , drop = FALSE]
  parent_node <- function(sid) {
    if (is.na(sid) || !sid %in% secs$section_id) return(body)
    if (!is.null(node_of[[sid]])) return(node_of[[sid]])
    s <- secs[secs$section_id == sid, ]
    par <- parent_node(s$parent_id)
    n <- xml2::xml_add_child(par, "sec", id = sid)
    if (!is.na(s$title) && nzchar(s$title)) xml2::xml_add_child(n, "title", s$title)
    node_of[[sid]] <<- n
    n
  }
  # sections are opened in reading order, so a paragraph before a section,
  # or a parent paragraph after a child section, keeps its place
  emitted <- character()
  for (i in seq_len(nrow(b))) {
    bl <- b[i, ]
    par <- parent_node(bl$section_id)
    if (bl$type %in% "heading") {
      next
    } else if (bl$type %in% "table") {
      tid <- bl$parent_id
      if (is.na(tid) || tid %in% emitted) next
      emitted <- c(emitted, tid)
      add_jats_table(par, doc, tid)
    } else if (bl$type %in% "equation") {
      eq <- doc$equations[doc$equations$block_id %in% bl$block_id, , drop = FALSE]
      df <- xml2::xml_add_child(par, "disp-formula")
      if (nrow(eq) && !is.na(eq$label[1])) xml2::xml_add_child(df, "label", eq$label[1])
      mm <- if (nrow(eq) && !is.na(eq$mathml[1])) tryCatch(xml2::read_xml(eq$mathml[1]), error = function(e) NULL) else NULL
      if (!is.null(mm)) {
        xml2::xml_set_attr(mm, "xmlns", "http://www.w3.org/1998/Math/MathML")
        xml2::xml_add_child(df, mm)
      }
      if (nrow(eq) && !is.na(eq$latex[1])) xml2::xml_add_child(df, "tex-math", eq$latex[1]) else if (is.null(mm) && !is.na(bl$text)) xml2::xml_text(df) <- bl$text
    } else if (bl$type %in% "caption") {
      next
    } else if (bl$type %in% "figure") {
      f <- doc$figures[doc$figures$figure_id %in% bl$parent_id, , drop = FALSE]
      fg <- xml2::xml_add_child(par, "fig", id = f$figure_id[1] %|NA|% bl$block_id)
      if (nrow(f) && !is.na(f$label[1])) xml2::xml_add_child(fg, "label", f$label[1])
      cap <- doc$captions$text[match(f$caption_id[1], doc$captions$caption_id)]
      if (!is.na(cap %|NA|% NA)) xml2::xml_add_child(xml2::xml_add_child(fg, "caption"), "p", cap)
      g <- if (nrow(f) && "graphic" %in% names(f)) f$graphic[1] else NA_character_
      if (!is.na(g)) xml2::xml_add_child(fg, "graphic", "xlink:href" = g)
    } else if (!is.na(bl$text) && nzchar(bl$text)) {
      add_jats_paragraph(par, bl, doc$citations)
    }
  }
  for (s in setdiff(secs$section_id, names(node_of))) parent_node(s)
  for (tid in setdiff(doc$tables$table_id[!doc$tables$continuation_status %in% "joined"], emitted)) add_jats_table(body, doc, tid)
  dfn <- doc$footnotes[doc$footnotes$scope %in% "document", , drop = FALSE]
  if (nrow(doc$references) || nrow(doc$supplementary) || nrow(dfn)) back <- xml2::xml_add_child(root, "back")
  if (nrow(dfn)) {
    fg <- xml2::xml_add_child(back, "fn-group")
    for (k in seq_len(nrow(dfn))) {
      fn <- xml2::xml_add_child(fg, "fn")
      if (!is.na(dfn$target_id[k])) xml2::xml_set_attr(fn, "id", dfn$target_id[k])
      if (!is.na(dfn$marker[k])) xml2::xml_add_child(fn, "label", dfn$marker[k])
      xml2::xml_add_child(fn, "p", dfn$text[k] %|NA|% "")
    }
  }
  for (k in seq_len(nrow(doc$supplementary))) {
    sm <- doc$supplementary[k, ]
    sn <- xml2::xml_add_child(back, "supplementary-material", id = sm$supp_id)
    if (!is.na(sm$href)) xml2::xml_set_attr(sn, "xlink:href", sm$href)
    if (!is.na(sm$label)) xml2::xml_add_child(sn, "label", sm$label)
    if (!is.na(sm$caption)) xml2::xml_add_child(xml2::xml_add_child(sn, "caption"), "p", sm$caption)
  }
  if (nrow(doc$references)) {
    rl <- xml2::xml_add_child(back, "ref-list")
    for (k in seq_len(nrow(doc$references))) {
      r <- doc$references[k, ]
      rn <- xml2::xml_add_child(rl, "ref", id = r$ref_id)
      if (!is.na(r$label)) xml2::xml_add_child(rn, "label", r$label)
      # the citation text as printed, and the parsed fields beside it: the
      # text is never extended, so repeated round trips are stable
      xml2::xml_add_child(rn, "mixed-citation", r$text %|NA|% "")
      ec <- xml2::xml_add_child(rn, "element-citation")
      au <- if (is.na(r$authors)) character() else trimws(strsplit(r$authors, ",\\s*")[[1]])
      if (length(au)) {
        pg <- xml2::xml_add_child(ec, "person-group", "person-group-type" = "author")
        for (a in au) xml2::xml_add_child(pg, "string-name", a)
      }
      if (!is.na(r$title)) xml2::xml_add_child(ec, "article-title", r$title)
      if (!is.na(r$journal)) xml2::xml_add_child(ec, "source", r$journal)
      if (!is.na(r$year)) xml2::xml_add_child(ec, "year", r$year)
      if (!is.na(r$volume)) xml2::xml_add_child(ec, "volume", r$volume)
      if (!is.na(r$pages)) xml2::xml_add_child(ec, "elocation-id", r$pages)
      for (idt in c("doi", "pmid", "pmcid")) if (!is.na(r[[idt]])) xml2::xml_add_child(ec, "pub-id", r[[idt]], "pub-id-type" = idt)
    }
  }
  xml2::write_xml(root, file, options = c("format", "as_xml"))
  invisible(file)
}

# A paragraph with its citations as <xref> elements at their offsets.
add_jats_paragraph <- function(parent, bl, citations) {
  ci <- citations[citations$block_id %in% bl$block_id & !is.na(citations$char_start), , drop = FALSE]
  ci <- ci[!duplicated(ci$char_start), , drop = FALSE]
  ci <- ci[order(ci$char_start), , drop = FALSE]
  if (!nrow(ci)) return(invisible(xml2::xml_add_child(parent, "p", bl$text)))
  esc <- function(z) gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", z, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  parts <- character()
  pos <- 1L
  for (k in seq_len(nrow(ci))) {
    if (ci$char_start[k] < pos) next
    rids <- citations$ref_id[citations$block_id %in% bl$block_id & citations$char_start %in% ci$char_start[k]]
    rids <- rids[!is.na(rids)]
    parts <- c(parts, esc(substr(bl$text, pos, ci$char_start[k] - 1L)),
               sprintf('<xref ref-type="bibr"%s>%s</xref>',
                       if (length(rids)) sprintf(' rid="%s"', paste(unique(rids), collapse = " ")) else "",
                       esc(substr(bl$text, ci$char_start[k], ci$char_end[k]))))
    pos <- ci$char_end[k] + 1L
  }
  parts <- c(parts, esc(substring(bl$text, pos)))
  p <- xml2::read_xml(paste0("<p>", paste(parts, collapse = ""), "</p>"))
  invisible(xml2::xml_add_child(parent, p))
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
  fn <- doc$footnotes[doc$footnotes$target_id %in% tid, , drop = FALSE]
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
        # markers as footnote links, so a parser can tell them from the text
        if (!is.na(c1$markers)) for (mk in split_ids(c1$markers)) {
          rid <- fn$footnote_id[match(mk, fn$marker)]
          xr <- xml2::xml_add_child(td, "xref", mk, "ref-type" = "table-fn")
          if (!is.na(rid)) xml2::xml_set_attr(xr, "rid", rid)
        }
      }
    }
  }
  if (nrow(fn)) {
    fg <- xml2::xml_add_child(tw, "table-wrap-foot")
    for (k in seq_len(nrow(fn))) {
      f <- xml2::xml_add_child(fg, "fn", id = fn$footnote_id[k])
      if (!is.na(fn$marker[k])) xml2::xml_add_child(f, "label", fn$marker[k])
      xml2::xml_add_child(f, "p", fn$text[k])
    }
  }
  invisible(tw)
}

# File name from an identifier: only letters, digits, "-", "_" and ".",
# never a path, never hidden. Identifiers come from documents that may have
# been read from untrusted JSON.
safe_file_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_.-]", "_", x)
  x <- sub("^[.]+", "_", x)
  ifelse(nzchar(x), x, "_")
}

# JSON turns NA scalars into null and empty vectors into []: give every
# metadata field back its R type.
restore_metadata <- function(md) {
  md <- if (is.list(md)) md else list()
  chr <- function(v) if (is.null(v) || !length(v)) NA_character_ else as.character(unlist(v))[1]
  vec <- function(v) if (is.null(v) || !length(v)) character() else as.character(unlist(v))
  int <- function(v) if (is.null(v) || !length(v)) NA_integer_ else suppressWarnings(as.integer(unlist(v)[1]))
  out <- md
  for (k in intersect(c("title", "doi", "pmid", "pmcid", "journal", "abstract", "license", "license_text"), names(md))) out[[k]] <- chr(md[[k]])
  for (k in intersect(c("authors", "keywords"), names(md))) out[[k]] <- vec(md[[k]])
  for (k in intersect(c("year", "n_pages"), names(md))) out[[k]] <- int(md[[k]])
  out$sources <- if (is.list(md$sources)) lapply(md$sources, chr) else list()
  out
}

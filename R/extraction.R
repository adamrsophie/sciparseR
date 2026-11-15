#' Extract scientific information with evidence
#'
#' Runs a schema against a parsed document and returns an `sc_extraction`
#' with one row per extracted value (`records`) and one row per supporting
#' source location (`evidence`). Every record links to its evidence: page,
#' section, block or table cell, the exact supporting text, character
#' offsets and the bounding box when the source provides one.
#'
#' The `"rules"` engine is deterministic and needs no network or model. The
#' `"llm"` engine sends text passages (never the PDF file) to a chat model
#' that the caller supplies explicitly, then checks every returned value
#' against the quoted passage; values whose quote cannot be found are kept
#' with status `"unsupported"`. See [sc_llm_engine()].
#'
#' Statuses: `reported` (found as stated), `missing` (the source marks the
#' value as missing, for example with a dash), `not_reported` (expected but
#' absent: a required field or an empty table cell), `ambiguous` (found but
#' its meaning is not stated, such as a value after a plus-minus sign
#' without SD or SE), `unsupported` (model output without verifiable
#' evidence). Data origins: `reported`, `derived` ([sc_derive()]),
#' `model_inferred` and `human_corrected` ([sc_correct()]).
#'
#' @param doc An `sc_document`.
#' @param schema An [sc_schema()] or the name of a built-in schema.
#' @param engine `"rules"` or `"llm"`.
#' @param llm For `engine = "llm"`, an engine from [sc_llm_engine()].
#' @param ... Passed to the engine.
#' @return An object of class `sc_extraction`.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "clinical_trial")
#' x
#' head(sc_observations(x)[, c("outcome", "arm", "statistic", "value_num")])
#' @export
sc_extract <- function(doc, schema = "clinical_trial", engine = c("rules", "llm"),
                       llm = NULL, ...) {
  check_document(doc)
  engine <- match.arg(engine)
  if (is.character(schema)) schema <- sc_schema(schema)
  if (!inherits(schema, "sc_schema")) abort("`schema` must be an sc_schema or a schema name.")
  if (engine == "llm") {
    if (is.null(llm)) abort("engine = \"llm\" needs an engine from sc_llm_engine(); none is used by default.")
    return(llm_extract(doc, schema, llm, ...))
  }
  col <- new_collector(doc, schema$name, "rules")
  units <- text_units(doc)
  fields <- names(schema$fields)
  if (schema$builtin) {
    rule_design(col, units, fields)
    arms <- rule_methods(col, units, fields)
    if (any(c("result", "baseline") %in% fields)) {
      arms <- c(arms, table_arms(doc))
      arms <- arms[!duplicated(tolower(arms))]
      rule_text_statistics(col, units, arms)
      rule_table_statistics(col, doc, arms)
      if (!"baseline" %in% fields) col$records <- Filter(function(r) r$field != "baseline", col$records)
      if (!"result" %in% fields) col$records <- Filter(function(r) r$field != "result", col$records)
      if (!"arm_n" %in% fields) col$records <- Filter(function(r) r$field != "arm_n", col$records)
    }
  } else {
    for (f in schema$fields) {
      if (is.null(f$pattern) && f$type != "categorical") next
      rule_user_field(col, units, f)
    }
  }
  fr <- collector_frames(col)
  fr$records <- add_not_reported(fr$records, schema, doc, "rules")
  new_extraction(doc, schema, fr$records, fr$evidence, engine = list(name = "rules", version = pkg_version("sciparseR")))
}

table_arms <- function(doc) {
  out <- character()
  for (tid in doc$tables$table_id) {
    cells <- doc$cells[doc$cells$table_id == tid, , drop = FALSE]
    hdr <- column_headers(cells, max(c(0L, cells$col)))
    for (h in hdr[-1]) {
      r <- column_role(h, character())
      if (r$role == "arm" && !identical(r$arm, "total")) out <- c(out, r$arm)
    }
  }
  unique(out)
}

add_not_reported <- function(records, schema, doc, engine) {
  have <- unique(records$field)
  miss <- Filter(function(f) f$required && !f$name %in% have, schema$fields)
  if (!length(miss)) return(records)
  extra <- as_entity(list(
    field = vapply(miss, function(f) f$name, character(1)), status = "not_reported",
    data_origin = if (engine == "llm") "model_inferred" else "reported",
    method = paste0(engine, ":required_field_check"), engine = engine,
    doc_id = doc$doc_id, schema = schema$name,
    notes = "required field: no supporting text found"), "records")
  out <- rbind(records, extra)
  out$record_id <- sprintf("rec-%05d", seq_len(nrow(out)))
  out
}

new_extraction <- function(doc, schema, records, evidence, engine, llm = NULL) {
  structure(list(
    schema_version = sc_schema_version,
    doc_id = doc$doc_id, sha256 = doc$source$sha256, source = doc$source$file_name,
    schema = list(name = schema$name, version = schema$version,
                  fields = lapply(schema$fields, function(f) {
                    c(unclass(f)[c("name", "type", "required", "multiple", "description")],
                      list(values = names(f$vocabulary) %||% character(), pattern = f$pattern %||% NA_character_,
                           sections = f$sections %||% character(), unit = f$unit %||% NA_character_))
                  })),
    engine = engine, llm = llm, created_at = now_utc(),
    package_version = pkg_version("sciparseR"),
    records = as_entity(records, "records"), evidence = as_entity(evidence, "evidence"),
    validation = empty_entity("validation")),
    class = "sc_extraction")
}

#' @export
print.sc_extraction <- function(x, ...) {
  r <- x$records
  cat("<sc_extraction> ", x$schema$name, " from ", x$source, " (", x$doc_id, ")\n", sep = "")
  cat("  engine: ", x$engine$name, if (!is.null(x$llm$model)) paste0(" (", x$llm$model, ")"), "\n", sep = "")
  cat("  records: ", nrow(r), "   evidence rows: ", nrow(x$evidence), "\n", sep = "")
  if (nrow(r)) {
    st <- table(r$status)
    cat("  status: ", paste(names(st), st, sep = " ", collapse = ", "), "\n", sep = "")
    og <- table(r$data_origin)
    cat("  origin: ", paste(names(og), og, sep = " ", collapse = ", "), "\n", sep = "")
  }
  if (nrow(x$validation)) {
    v <- table(x$validation$result)
    cat("  validation: ", paste(names(v), v, sep = " ", collapse = ", "), "\n", sep = "")
  } else {
    cat("  validation: not run (see sc_validate())\n")
  }
  invisible(x)
}

#' @export
as.data.frame.sc_extraction <- function(x, ...) {
  ev <- x$evidence
  r <- x$records
  cbind(r, ev[match(r$evidence_id, ev$evidence_id), c("page", "section_label", "block_id", "table_id", "cell_id", "text", "char_start", "char_end"), drop = FALSE])
}

#' Evidence of extracted values
#'
#' Returns the evidence rows behind records: the document checksum, page,
#' section, block or table cell, exact supporting text, character offsets,
#' bounding box (with its scope: span, cell or block) and XPath for JATS.
#'
#' @param x An `sc_extraction`.
#' @param record_id Optional record identifiers; all evidence by default.
#' @return A data frame of evidence joined to the record identifiers.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "trial_details")
#' sc_evidence(x)[1:3, c("record_id", "page", "text", "char_start", "char_end")]
#' @export
sc_evidence <- function(x, record_id = NULL) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  r <- x$records
  if (!is.null(record_id)) r <- r[r$record_id %in% record_id, , drop = FALSE]
  ev <- x$evidence[match(r$evidence_id, x$evidence$evidence_id), , drop = FALSE]
  out <- cbind(data.frame(record_id = r$record_id, field = r$field, value = r$value, stringsAsFactors = FALSE), ev)
  rownames(out) <- NULL
  out
}

#' One row per observation
#'
#' Pivots atomic records that share an observation id (an estimate with its
#' confidence limits and p-value, events with their denominator, a mean
#' with its dispersion) into one row per observation and statistic, keeping
#' the binding context.
#'
#' @param x An `sc_extraction`.
#' @param wide If `TRUE`, one row per observation with one column per
#'   statistic.
#' @return A data frame.
#' @examples
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "numerical_results")
#' head(sc_observations(x, wide = TRUE))
#' @export
sc_observations <- function(x, wide = FALSE) {
  if (!inherits(x, "sc_extraction")) abort("`x` must be an sc_extraction.")
  r <- x$records[!is.na(x$records$observation_id) & x$records$status != "superseded", , drop = FALSE]
  keep <- c("observation_id", "record_id", "field", "outcome", "arm", "comparator", "timepoint",
            "analysis_set", "population", "effect_measure", "adjusted", "statistic", "value", "value_num", "unit",
            "status", "data_origin", "binding", "evidence_id")
  if (!wide) return(r[, keep, drop = FALSE])
  ctx <- c("observation_id", "field", "outcome", "arm", "comparator", "timepoint", "analysis_set",
           "population", "effect_measure", "adjusted", "unit", "binding")
  ids <- unique(r$observation_id)
  if (!length(ids)) {
    out <- r[0, ctx, drop = FALSE]
    out$status <- character()
    return(out)
  }
  rows <- lapply(ids, function(o) {
    ro <- r[r$observation_id == o, , drop = FALSE]
    base <- ro[1, ctx, drop = FALSE]
    base$unit <- ro$unit[!is.na(ro$unit) & ro$unit != "%"][1] %|NA|% ro$unit[1]
    base$status <- paste(unique(ro$status), collapse = ",")
    for (k in seq_len(nrow(ro))) base[[ro$statistic[k]]] <- ro$value_num[k]
    base
  })
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(z) {
    for (m in setdiff(all_cols, names(z))) z[[m]] <- NA
    z[all_cols]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' @rdname sc_extract
#' @export
sc_extract_pico <- function(doc, ...) sc_extract(doc, "pico", ...)

#' @rdname sc_extract
#' @export
sc_extract_methods <- function(doc, ...) sc_extract(doc, "trial_details", ...)

#' @rdname sc_extract
#' @export
sc_extract_outcomes <- function(doc, ...) {
  s <- sc_schema("trial_details")
  s$fields <- s$fields[c("primary_outcome", "secondary_outcomes", "assessment_times", "follow_up")]
  s$name <- rep("outcomes", nrow(s))
  s$builtin <- rep(TRUE, nrow(s))
  sc_extract(doc, s, ...)
}

#' @rdname sc_extract
#' @export
sc_extract_statistics <- function(doc, ...) sc_extract(doc, "numerical_results", ...)

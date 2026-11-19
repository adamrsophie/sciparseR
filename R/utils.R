`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

`%|NA|%` <- function(x, y) if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x

sc_schema_version <- "1.0.0"

# Column specifications of every entity table in an sc_document. Types are
# "c" character, "i" integer, "d" double, "l" logical.
.entity_specs <- list(
  pages = c(page = "i", width = "d", height = "d", media_width = "d",
            media_height = "d", rotation = "i", rotation_status = "c",
            n_words = "i", has_text_layer = "l", image_only = "l",
            ink_ratio = "d", ocr_status = "c", n_columns = "i",
            media_x0 = "d", media_y0 = "d", rotation_source = "c"),
  spans = c(span_id = "c", page = "i", x0 = "d", y0 = "d", x1 = "d", y1 = "d",
            text = "c", font_name = "c", font_size = "d", bold = "l",
            italic = "l", superscript = "l", line_id = "c", block_id = "c",
            source = "c", confidence = "d"),
  lines = c(line_id = "c", page = "i", x0 = "d", y0 = "d", x1 = "d", y1 = "d",
            text = "c", font_size = "d", bold_ratio = "d", n_spans = "i",
            block_id = "c"),
  blocks = c(block_id = "c", page = "i", type = "c", x0 = "d", y0 = "d",
             x1 = "d", y1 = "d", text_raw = "c", text = "c",
             section_id = "c", reading_order = "i", column = "i",
             font_size = "d", bold = "l", parent_id = "c", continues = "c",
             method = "c", status = "c", xpath = "c"),
  sections = c(section_id = "c", parent_id = "c", level = "i", number = "c",
               title = "c", label = "c", block_id = "c", page_start = "i",
               page_end = "i", reading_order = "i", method = "c",
               xpath = "c"),
  tables = c(table_id = "c", page = "i", label = "c", caption_id = "c",
             x0 = "d", y0 = "d", x1 = "d", y1 = "d", n_rows = "i",
             n_cols = "i", n_header_rows = "i", ruled = "l", method = "c",
             status = "c", continuation_of = "c",
             continuation_status = "c", footnote_ids = "c", xpath = "c"),
  cells = c(cell_id = "c", table_id = "c", row = "i", col = "i",
            row_span = "i", col_span = "i", is_header = "l",
            text_raw = "c", text = "c", x0 = "d", y0 = "d", x1 = "d",
            y1 = "d", span_ids = "c", markers = "c", missing_token = "l",
            status = "c", xpath = "c"),
  figures = c(figure_id = "c", page = "i", label = "c", caption_id = "c",
              x0 = "d", y0 = "d", x1 = "d", y1 = "d", method = "c",
              status = "c", graphic = "c", xpath = "c"),
  equations = c(equation_id = "c", page = "i", label = "c", text = "c",
                latex = "c", mathml = "c", latex_status = "c", x0 = "d",
                y0 = "d", x1 = "d", y1 = "d", block_id = "c", method = "c",
                xpath = "c"),
  captions = c(caption_id = "c", target_type = "c", target_id = "c",
               label = "c", text = "c", page = "i", x0 = "d", y0 = "d",
               x1 = "d", y1 = "d", block_id = "c", xpath = "c"),
  footnotes = c(footnote_id = "c", page = "i", scope = "c", target_id = "c",
                marker = "c", text = "c", x0 = "d", y0 = "d", x1 = "d",
                y1 = "d", block_id = "c", xpath = "c"),
  references = c(ref_id = "c", label = "c", text = "c", authors = "c",
                 title = "c", journal = "c", year = "i", volume = "c",
                 pages = "c", doi = "c", pmid = "c", pmcid = "c", page = "i",
                 block_id = "c", method = "c", xpath = "c"),
  citations = c(citation_id = "c", block_id = "c", ref_id = "c",
                marker = "c", char_start = "i", char_end = "i", page = "i",
                style = "c", status = "c", xpath = "c"),
  supplementary = c(supp_id = "c", label = "c", caption = "c", href = "c",
                    mimetype = "c", xpath = "c"),
  warnings = c(stage = "c", code = "c", message = "c", page = "i",
               element_id = "c")
)

# Extraction layer tables (not part of the document).
.extraction_specs <- list(
  records = c(record_id = "c", doc_id = "c", schema = "c", field = "c",
              statistic = "c", value = "c", value_num = "d", unit = "c",
              status = "c", data_origin = "c", observation_id = "c",
              population = "c", arm = "c", comparator = "c", outcome = "c",
              timepoint = "c", analysis_set = "c", effect_measure = "c",
              adjusted = "l", ci_level = "d", interval_type = "c",
              p_operator = "c", test_role = "c",
              dispersion_type = "c", binding = "c", evidence_id = "c",
              method = "c", engine = "c", validation_status = "c",
              source_quality = "c", inputs = "c", formula = "c",
              assumptions = "c", software = "c",
              supersedes = "c", reviewer = "c", corrected_at = "c", notes = "c"),
  evidence = c(evidence_id = "c", doc_id = "c", sha256 = "c", page = "i",
               section_id = "c", section_label = "c", block_id = "c",
               table_id = "c", cell_id = "c", figure_id = "c", xpath = "c",
               text = "c", char_start = "i", char_end = "i",
               value_text = "c", x0 = "d", y0 = "d", x1 = "d", y1 = "d",
               bbox_scope = "c", method = "c"),
  validation = c(rule = "c", severity = "c", result = "c",
                 record_ids = "c", observation_id = "c", message = "c",
                 justification = "c")
)
.entity_specs_all <- function() c(.entity_specs, .extraction_specs)

.na_of <- function(type) {
  switch(type, c = NA_character_, i = NA_integer_, d = NA_real_, l = NA)
}

.cast <- function(x, type) {
  switch(type, c = as.character(x), i = as.integer(x), d = as.double(x),
         l = as.logical(x))
}

# Empty data frame for an entity.
empty_entity <- function(entity) {
  spec <- .entity_specs_all()[[entity]]
  cols <- lapply(spec, function(t) .cast(logical(0), t))
  as.data.frame(cols, stringsAsFactors = FALSE)
}

# Coerce a list of columns (possibly missing some) to the entity spec.
# Missing columns are filled with NA; extra columns are dropped.
as_entity <- function(x, entity) {
  spec <- .entity_specs_all()[[entity]]
  if (is.null(x)) return(empty_entity(entity))
  if (is.data.frame(x)) x <- as.list(x)
  n <- max(c(0L, vapply(x, length, integer(1))))
  if (n == 0L) return(empty_entity(entity))
  cols <- lapply(names(spec), function(nm) {
    v <- x[[nm]]
    if (is.null(v) || !length(v)) v <- rep(.na_of(spec[[nm]]), n)
    if (length(v) == 1L && n > 1L) v <- rep(v, n)
    if (length(v) != n) abort("Column `", nm, "` of ", entity, " has ", length(v), " values for ", n, " rows.")
    unname(.cast(v, spec[[nm]]))
  })
  names(cols) <- names(spec)
  # built directly: as.data.frame() per row dominated extraction and JATS
  # table parsing
  structure(cols, class = "data.frame", row.names = .set_row_names(n))
}

bind_entity <- function(lst, entity) {
  lst <- Filter(function(z) !is.null(z) && NROW(z) > 0L, lst)
  if (!length(lst)) return(empty_entity(entity))
  parts <- lapply(lst, as_entity, entity = entity)
  spec <- .entity_specs_all()[[entity]]
  cols <- lapply(stats::setNames(names(spec), names(spec)), function(nm) unlist(lapply(parts, `[[`, nm), use.names = FALSE))
  structure(cols, class = "data.frame", row.names = .set_row_names(sum(vapply(parts, nrow, integer(1)))))
}

make_id <- function(prefix, page, i) {
  pg <- ifelse(is.na(page), "xml", sprintf("p%03d", as.integer(page)))
  sprintf("%s-%s-%04d", prefix, pg, as.integer(i))
}

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

hash_object <- function(x) {
  digest::digest(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA,
                                  null = "null"),
                 algo = "sha256", serialize = FALSE)
}

# Whitespace and typographic normalization used for the `text` columns.
# Raw text is always kept alongside. Hyphens and dashes are not altered
# except the Unicode minus sign, which becomes an ASCII hyphen-minus.
normalize_text <- function(x) {
  if (!length(x)) return(character())
  x <- enc2utf8(as.character(x))
  rep <- c("\ufb00" = "ff", "\ufb01" = "fi", "\ufb02" = "fl",
           "\ufb03" = "ffi", "\ufb04" = "ffl", "\u00a0" = " ",
           "\u2009" = " ", "\u202f" = " ", "\u2212" = "-", "\u00ad" = "")
  for (k in names(rep)) x <- gsub(k, rep[[k]], x, fixed = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

# Warning log shared by the parsing stages.
new_log <- function() {
  env <- new.env(parent = emptyenv())
  env$rows <- list()
  env$add <- function(stage, code, message, page = NA_integer_,
                      element_id = NA_character_) {
    env$rows[[length(env$rows) + 1L]] <- list(
      stage = stage, code = code, message = message, page = page,
      element_id = element_id)
    invisible(NULL)
  }
  env$frame <- function() {
    if (!length(env$rows)) return(empty_entity("warnings"))
    bind_entity(lapply(env$rows, as.data.frame, stringsAsFactors = FALSE),
                "warnings")
  }
  env
}

bbox_union <- function(x0, y0, x1, y1) {
  if (!length(x0) || all(is.na(x0))) return(c(NA_real_, NA_real_, NA_real_, NA_real_))
  c(min(x0, na.rm = TRUE), min(y0, na.rm = TRUE),
    max(x1, na.rm = TRUE), max(y1, na.rm = TRUE))
}

bbox_iou <- function(a, b) {
  if (anyNA(a) || anyNA(b)) return(NA_real_)
  iw <- max(0, min(a[3], b[3]) - max(a[1], b[1]))
  ih <- max(0, min(a[4], b[4]) - max(a[2], b[2]))
  inter <- iw * ih
  ua <- (a[3] - a[1]) * (a[4] - a[2]) + (b[3] - b[1]) * (b[4] - b[2]) - inter
  if (ua <= 0) return(0)
  inter / ua
}

pkg_version <- function(pkg) {
  tryCatch(as.character(utils::packageVersion(pkg)),
           error = function(e) NA_character_)
}

now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

abort <- function(..., call. = FALSE) stop(paste0(...), call. = call.)

check_document <- function(doc, arg = "doc") {
  if (!inherits(doc, "sc_document")) {
    abort("`", arg, "` must be an sc_document created by sc_parse().")
  }
  invisible(doc)
}

check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    abort("`", arg, "` must be a single string.")
  }
  invisible(x)
}

median_or <- function(x, default) {
  x <- x[is.finite(x)]
  if (!length(x)) default else stats::median(x)
}

is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# Collapse a character vector to a comma separated string (NA if empty).
collapse_ids <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) NA_character_ else paste(x, collapse = ",")
}

split_ids <- function(x) {
  if (is.na(x) || !nzchar(x)) character() else strsplit(x, ",", fixed = TRUE)[[1]]
}

# Escape every regular expression metacharacter of a literal string.
regex_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

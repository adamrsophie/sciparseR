# Writes the JSON Schema of sciparseR exports from the entity specifications.
# Run from the package root: Rscript data-raw/make-schema.R
pkgload::load_all(quiet = TRUE)
jtype <- c(c = "string", i = "integer", d = "number", l = "boolean")
table_schema <- function(spec) {
  list(type = "array", items = list(
    type = "object", additionalProperties = FALSE,
    properties = lapply(spec, function(t) list(type = list(jtype[[t]], "null")))))
}
make <- function(kind, specs, extra) {
  props <- c(list(object_type = list(const = kind), schema_version = list(type = "string")),
             extra, lapply(specs, table_schema))
  list(`$schema` = "https://json-schema.org/draft/2020-12/schema",
       `$id` = paste0("https://adamrsophie.github.io/sciparseR/schema/", kind, "-", sc_schema_version, ".json"),
       title = paste("sciparseR", kind, "export, schema version", sc_schema_version),
       type = "object", required = c("object_type", "schema_version", names(specs)),
       properties = props)
}
doc <- make("sc_document", .entity_specs, list(
  doc_id = list(type = "string"), source = list(type = "object"),
  processing = list(type = "object"), metadata = list(type = "object")))
ext <- make("sc_extraction", .extraction_specs, list(
  doc_id = list(type = "string"), sha256 = list(type = "string"), source = list(type = "string"),
  schema = list(type = "object"), engine = list(type = "object"),
  llm = list(type = list("object", "null")), created_at = list(type = "string"),
  package_version = list(type = "string")))
for (x in list(list("sc_document", doc), list("sc_extraction", ext))) {
  writeLines(jsonlite::toJSON(x[[2]], auto_unbox = TRUE, pretty = TRUE),
             file.path("inst", "schema", paste0(x[[1]], "-", sc_schema_version, ".json")))
}

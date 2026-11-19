#' Optional ONNX models
#'
#' sciparseR never downloads models on its own. These functions manage
#' models you place on disk yourself: `sc_model_register()` records a local
#' ONNX file with its task, license and SHA-256 checksum in a registry;
#' `sc_models()` lists the registry; `sc_model_remove()` removes an entry;
#' `sc_onnx_status()` reports whether the optional nativeORT runtime is
#' usable; `sc_onnx_session()` verifies the checksum and opens an ONNX
#' Runtime session; `sc_onnx_infer()` runs one raw inference.
#'
#' The checksum is verified every time a session is opened, and a model
#' without an accepted license is refused.
#'
#' Status in this version: model management and raw inference are
#' implemented. Learned layout detection, table structure recognition and
#' formula recognition are not wired into [sc_parse()]: detection models
#' such as Table Transformer (DETR) return several output tensors and
#' `nativeORT::ort_infer_raw()` returns only the first, so their boxes cannot be decoded.
#' Parsing therefore always uses the deterministic geometric methods.
#'
#' @param name Model name.
#' @param path Path to the local `.onnx` file.
#' @param task One of `"layout"`, `"table_detection"`, `"table_structure"`,
#'   `"formula"`, `"reading_order"`, `"other"`.
#' @param license License identifier of the model weights (for example
#'   `"MIT"`).
#' @param sha256 Expected SHA-256 checksum; the file is refused if it
#'   differs. When `NULL`, the checksum of the file is computed and stored.
#' @param accept_license Set to `TRUE` to confirm the license allows your
#'   use.
#' @param source Optional URL or description of where the file came from.
#' @param registry Registry directory; defaults to the user cache
#'   directory of the package.
#' @param input Input array for `sc_onnx_infer()`.
#' @param session A session from `sc_onnx_session()`. For many inferences,
#'   open the session once and pass it here: the checksum is then verified
#'   once and the model is loaded once, instead of on every call.
#' @param ... Passed to `nativeORT::ort_session()`.
#' @return `sc_models()` a data frame; `sc_onnx_status()` a list;
#'   `sc_onnx_session()` a nativeORT session; `sc_onnx_infer()` the raw
#'   output; the others the registry invisibly.
#' @examples
#' reg <- file.path(tempdir(), "sciparseR-models")
#' f <- tempfile(fileext = ".onnx")
#' writeBin(as.raw(1:10), f)
#' sc_model_register("demo", f, task = "other", license = "MIT",
#'                   accept_license = TRUE, registry = reg)
#' sc_models(reg)[, c("name", "task", "license", "sha256")]
#' sc_model_remove("demo", registry = reg)
#' sc_onnx_status()$available
#' @name models
NULL

model_registry_dir <- function(registry = NULL) {
  registry %||% file.path(tools::R_user_dir("sciparseR", "cache"), "models")
}

read_registry <- function(dir) {
  f <- file.path(dir, "registry.json")
  if (!file.exists(f)) {
    return(data.frame(name = character(), task = character(), path = character(), sha256 = character(),
                      license = character(), license_accepted = logical(), source = character(),
                      registered_at = character(), stringsAsFactors = FALSE))
  }
  r <- jsonlite::fromJSON(f)
  if (!length(r)) return(read_registry(tempfile()))
  as.data.frame(r, stringsAsFactors = FALSE)
}

write_registry <- function(r, dir) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  writeLines(jsonlite::toJSON(r, dataframe = "rows", pretty = TRUE, na = "null"), file.path(dir, "registry.json"))
  invisible(r)
}

#' @rdname models
#' @export
sc_model_register <- function(name, path, task = c("layout", "table_detection", "table_structure", "formula", "reading_order", "other"),
                              license, sha256 = NULL, accept_license = FALSE, source = NA_character_, registry = NULL) {
  check_string(name, "name")
  check_string(path, "path")
  check_string(license, "license")
  task <- match.arg(task)
  if (!file.exists(path)) abort("Model file not found: ", path)
  if (!isTRUE(accept_license)) abort("Confirm the model license with accept_license = TRUE.")
  actual <- sha256_file(path)
  if (!is.null(sha256) && !identical(tolower(sha256), actual)) abort("Checksum mismatch for ", basename(path), ": expected ", sha256, ", found ", actual, ".")
  dir <- model_registry_dir(registry)
  r <- read_registry(dir)
  r <- r[r$name != name, , drop = FALSE]
  r <- rbind(r, data.frame(name = name, task = task, path = normalizePath(path, winslash = "/"), sha256 = actual,
                           license = license, license_accepted = TRUE, source = source,
                           registered_at = now_utc(), stringsAsFactors = FALSE))
  write_registry(r, dir)
}

#' @rdname models
#' @export
sc_models <- function(registry = NULL) {
  read_registry(model_registry_dir(registry))
}

#' @rdname models
#' @export
sc_model_remove <- function(name, registry = NULL) {
  dir <- model_registry_dir(registry)
  r <- read_registry(dir)
  if (!name %in% r$name) abort("No registered model '", name, "'.")
  write_registry(r[r$name != name, , drop = FALSE], dir)
}

#' @rdname models
#' @export
sc_onnx_status <- function() {
  pkg <- is_installed("nativeORT")
  rt <- pkg && isTRUE(tryCatch(nativeORT::ort_is_installed(), error = function(e) FALSE))
  list(available = pkg && rt, nativeORT = if (pkg) pkg_version("nativeORT") else NA_character_,
       runtime_installed = rt,
       integrated_tasks = character(),
       note = paste("Model management and raw inference only. Learned layout, table and formula",
                    "recognition are not used by sc_parse() in this version: multi-output detection",
                    "models cannot be decoded with nativeORT, which returns only the first output."))
}

#' @rdname models
#' @export
sc_onnx_session <- function(name, registry = NULL, ...) {
  r <- sc_models(registry)
  m <- r[r$name == name, , drop = FALSE]
  if (!nrow(m)) abort("No registered model '", name, "'.")
  if (!isTRUE(m$license_accepted)) abort("The license of model '", name, "' has not been accepted.")
  if (!file.exists(m$path)) abort("Model file missing: ", m$path)
  if (!identical(sha256_file(m$path), m$sha256)) abort("Checksum of model '", name, "' no longer matches the registry.")
  st <- sc_onnx_status()
  if (!st$available) abort("ONNX Runtime is not available: install nativeORT and run nativeORT::ort_install().")
  nativeORT::ort_session(m$path, ...)
}

#' @rdname models
#' @export
sc_onnx_infer <- function(name, input, registry = NULL, session = NULL, ...) {
  s <- session %||% sc_onnx_session(name, registry = registry, ...)
  nativeORT::ort_infer_raw(s, input)
}

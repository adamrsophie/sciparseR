#' Parse many documents with caching
#'
#' Parses each file and returns a list of documents with a status table.
#' Failures are recorded and never stop the batch. With a cache directory,
#' each result is stored under a key built from the file's SHA-256, the
#' configuration hash and the package and schema versions; a file is
#' parsed again only when one of these changes.
#'
#' @param paths File paths.
#' @param config An [sc_config()] object.
#' @param cache_dir Optional cache directory.
#' @param workers Number of parallel workers (uses `parallel::mclapply`, so
#'   values above 1 have effect only on Unix-alike systems).
#' @param progress Print one line per file.
#' @return An object of class `sc_batch`: a list with `documents` (named
#'   list, `NULL` for failures) and `status` (data frame with file, status,
#'   cached, seconds, doc_id and error message).
#' @examples
#' files <- system.file("extdata", c("single_column.pdf", "trial_article.xml"),
#'                      package = "sciparseR")
#' cache <- file.path(tempdir(), "sciparseR-cache")
#' b <- sc_parse_batch(files, cache_dir = cache)
#' b$status
#' sc_parse_batch(files, cache_dir = cache)$status$cached
#' sc_cache_info(cache)
#' @export
sc_parse_batch <- function(paths, config = sc_config(), cache_dir = NULL, workers = 1L,
                           progress = FALSE) {
  if (!is.character(paths) || !length(paths)) abort("`paths` must be a character vector of files.")
  if (!is.null(cache_dir)) dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  one <- function(p) {
    t0 <- Sys.time()
    res <- tryCatch({
      if (!file.exists(p)) stop("file not found")
      key <- cache_key(p, config)
      cf <- if (!is.null(cache_dir)) file.path(cache_dir, paste0(key, ".rds")) else NULL
      # an unreadable cache entry is parsed again and replaced
      d <- if (!is.null(cf) && file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL) else NULL
      if (!inherits(d, "sc_document")) d <- NULL
      if (!is.null(d)) {
        # identical bytes may live at another path: point to this file
        d$source$path <- normalizePath(p, mustWork = TRUE)
        d$source$file_name <- basename(p)
        list(doc = d, cached = TRUE, error = NA_character_)
      } else {
        d <- sc_parse(p, config = config)
        if (!is.null(cf)) {
          tmp <- paste0(cf, ".tmp")
          saveRDS(d, tmp)
          file.rename(tmp, cf)
        }
        list(doc = d, cached = FALSE, error = NA_character_)
      }
    }, error = function(e) list(doc = NULL, cached = FALSE, error = conditionMessage(e)))
    res$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (progress) message(basename(p), ": ", if (is.null(res$doc)) paste("failed:", res$error) else if (res$cached) "cached" else "parsed")
    res
  }
  res <- if (workers > 1L && .Platform$OS.type == "unix") parallel::mclapply(paths, one, mc.cores = workers) else lapply(paths, one)
  docs <- lapply(res, `[[`, "doc")
  names(docs) <- make.unique(basename(paths), sep = "_")
  status <- data.frame(
    file = names(docs),
    status = vapply(res, function(r) if (is.null(r$doc)) "failed" else "ok", ""),
    cached = vapply(res, `[[`, logical(1), "cached"),
    seconds = round(vapply(res, `[[`, numeric(1), "seconds"), 3),
    doc_id = vapply(docs, function(d) if (is.null(d)) NA_character_ else d$doc_id, ""),
    n_warnings = vapply(docs, function(d) if (is.null(d)) NA_integer_ else nrow(d$warnings), integer(1)),
    error = vapply(res, `[[`, character(1), "error"),
    stringsAsFactors = FALSE)
  rownames(status) <- NULL
  structure(list(documents = docs, status = status), class = "sc_batch")
}

#' @export
print.sc_batch <- function(x, ...) {
  s <- x$status
  cat("<sc_batch> ", nrow(s), " files: ", sum(s$status == "ok"), " ok (", sum(s$cached), " from cache), ",
      sum(s$status == "failed"), " failed\n", sep = "")
  invisible(x)
}

cache_key <- function(path, config) {
  # every backend whose version can change the parse is part of the key
  ocr <- !identical(config$ocr, "never") && is_installed("tesseract")
  substr(hash_object(list(sha256_file(path), config_hash(config), pkg_version("sciparseR"),
                          sc_schema_version, pkg_version("pdftools"), pkg_version("xml2"),
                          if (ocr) pkg_version("tesseract") else NA_character_)), 1L, 32L)
}

#' Inspect or clear a parse cache
#'
#' @param cache_dir Cache directory used with [sc_parse_batch()].
#' @param clear If `TRUE`, delete the cached documents.
#' @return A data frame with one row per cached document (invisibly empty
#'   after clearing).
#' @examples
#' sc_cache_info(file.path(tempdir(), "no-cache-here"))
#' @export
sc_cache_info <- function(cache_dir, clear = FALSE) {
  check_string(cache_dir, "cache_dir")
  files <- if (dir.exists(cache_dir)) list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE) else character()
  if (clear) {
    unlink(files)
    return(invisible(data.frame(key = character(), source = character(), doc_id = character(),
                                package_version = character(), size_bytes = numeric(), modified = as.POSIXct(character()))))
  }
  rows <- lapply(files, function(f) {
    d <- tryCatch(readRDS(f), error = function(e) NULL)
    data.frame(key = sub("\\.rds$", "", basename(f)),
               source = if (is.null(d)) NA_character_ else d$source$file_name,
               doc_id = if (is.null(d)) NA_character_ else d$doc_id,
               package_version = if (is.null(d)) NA_character_ else d$processing$package_version,
               size_bytes = file.size(f), modified = file.mtime(f), stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame(key = character(), source = character(), doc_id = character(),
                                       package_version = character(), size_bytes = numeric(), modified = as.POSIXct(character())))
  do.call(rbind, rows)
}

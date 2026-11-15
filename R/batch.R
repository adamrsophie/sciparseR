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
#' @param keep If `FALSE`, documents are not kept in memory: each one is
#'   written to the cache (which `cache_dir` must then name) and read back
#'   on demand with [sc_batch_document()]. Use it for large corpora.
#' @return An object of class `sc_batch`: a list with `documents` (named
#'   list, `NULL` for failures and for every file when `keep = FALSE`) and
#'   `status` (data frame with file, path, status, cached, seconds, doc_id,
#'   number of warnings, cache file and error message).
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
                           progress = FALSE, keep = TRUE) {
  if (!is.character(paths) || !length(paths)) abort("`paths` must be a character vector of files.")
  if (!isTRUE(keep) && is.null(cache_dir)) abort("`keep = FALSE` needs a `cache_dir` to hold the documents.")
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
        list(doc = d, cached = TRUE, error = NA_character_, cache_file = cf)
      } else {
        d <- sc_parse(p, config = config)
        if (!is.null(cf)) {
          tmp <- paste0(cf, ".tmp")
          saveRDS(d, tmp)
          file.rename(tmp, cf)
        }
        list(doc = d, cached = FALSE, error = NA_character_, cache_file = cf %||% NA_character_)
      }
    }, error = function(e) list(doc = NULL, cached = FALSE, error = conditionMessage(e), cache_file = NA_character_))
    res$doc_id <- if (is.null(res$doc)) NA_character_ else res$doc$doc_id
    res$n_warnings <- if (is.null(res$doc)) NA_integer_ else nrow(res$doc$warnings)
    res$ok <- !is.null(res$doc)
    if (!isTRUE(keep)) res$doc <- NULL
    res$seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (progress) message(basename(p), ": ", if (!res$ok) paste("failed:", res$error) else if (res$cached) "cached" else "parsed")
    res
  }
  res <- if (workers > 1L && .Platform$OS.type == "unix") parallel::mclapply(paths, one, mc.cores = workers) else lapply(paths, one)
  docs <- lapply(res, `[[`, "doc")
  names(docs) <- make.unique(basename(paths), sep = "_")
  status <- data.frame(
    file = names(docs),
    path = paths,
    status = vapply(res, function(r) if (r$ok) "ok" else "failed", ""),
    cached = vapply(res, `[[`, logical(1), "cached"),
    seconds = round(vapply(res, `[[`, numeric(1), "seconds"), 3),
    doc_id = vapply(res, `[[`, character(1), "doc_id"),
    n_warnings = vapply(res, `[[`, integer(1), "n_warnings"),
    cache_file = vapply(res, function(r) r$cache_file %||% NA_character_, character(1)),
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

#' Get one document of a batch
#'
#' Returns a document of an [sc_parse_batch()] result, from memory or, for
#' a batch run with `keep = FALSE`, from the cache.
#'
#' @param batch An `sc_batch`.
#' @param file A file name as in `batch$status$file`, or a row number.
#' @return An `sc_document`.
#' @examples
#' files <- system.file("extdata", c("single_column.pdf", "trial_article.xml"),
#'                      package = "sciparseR")
#' b <- sc_parse_batch(files, cache_dir = file.path(tempdir(), "sc-cache"), keep = FALSE)
#' sc_batch_document(b, "trial_article.xml")
#' @export
sc_batch_document <- function(batch, file) {
  if (!inherits(batch, "sc_batch")) abort("`batch` must come from sc_parse_batch().")
  st <- batch$status
  i <- if (is.numeric(file)) as.integer(file) else match(file, st$file)
  if (length(i) != 1L || is.na(i) || i < 1L || i > nrow(st)) abort("No such file in the batch: ", file)
  if (st$status[i] != "ok") abort("File ", st$file[i], " failed: ", st$error[i])
  d <- batch$documents[[i]]
  if (!is.null(d)) return(d)
  cf <- st$cache_file[i]
  d <- if (!is.na(cf) && file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL) else NULL
  if (!inherits(d, "sc_document")) abort("The cached document of ", st$file[i], " is gone; run the batch again.")
  d$source$path <- normalizePath(st$path[i], mustWork = FALSE)
  d$source$file_name <- basename(st$path[i])
  d
}

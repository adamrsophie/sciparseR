#' Language model extraction engine
#'
#' Creates an engine for `sc_extract(engine = "llm")`. Nothing is sent
#' anywhere unless you create an engine and pass it explicitly. Only text
#' passages (block text with block identifiers) are sent, never the PDF.
#'
#' Three ways to choose the model:
#' * `provider`: any provider supported by
#'   [ellmer](https://ellmer.tidyverse.org) (`"anthropic"`, `"openai"`,
#'   `"openrouter"`, `"google_gemini"`, `"ollama"`, `"azure_openai"`,
#'   `"aws_bedrock"`, `"mistral"`, `"groq"`, `"deepseek"` and the rest; see
#'   [sc_llm_providers()]), or `"codex"` (alias `"chatgpt"`) to use a
#'   ChatGPT plan through
#'   [ellmercodex](https://github.com/simxnherrera/ellmercodex) without an
#'   API key. Arguments in `...` go to the provider constructor (for example
#'   `credentials`, `base_url`, `params` or, for Codex, `effort`). API keys are
#'   read by ellmer from the usual environment variables
#'   (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `OPENROUTER_API_KEY`, ...);
#'   sciparseR never stores them.
#' * `chat`: an ellmer `Chat` object you created yourself.
#' * `fun`: a function that receives the prompt string and returns the
#'   model's answer as a JSON string or a data frame (local models,
#'   testing).
#'
#' The model must return items with `field`, `value`, `quote` (an exact
#' quotation from the passage) and `block_id`, plus optional `statistic`,
#' `arm`, `comparator`, `outcome`, `timepoint`, `analysis_set`, `unit`.
#' Every item is verified: the quote must occur in the cited block (after
#' whitespace and Unicode normalization), and a numeric value must occur
#' in the quote. Verified items get status `"reported"`; others are kept
#' with status `"unsupported"` and a note. All items have data origin
#' `"model_inferred"`. Requests use ellmer structured output; with
#' `structured = "auto"`, a provider that rejects structured output is
#' asked for plain JSON instead.
#'
#' @param provider Provider name (see [sc_llm_providers()]).
#' @param model Model name passed to the provider (provider default when
#'   `NULL`); with `chat` or `fun`, a label recorded in the extraction.
#' @param ... Further arguments for the provider constructor.
#' @param chat An ellmer `Chat` object.
#' @param fun A function `function(prompt)` returning JSON text or a data
#'   frame.
#' @param structured `"auto"`, `"always"` or `"never"`: whether to request
#'   ellmer structured output.
#' @param max_chars Maximum characters of passage text per request.
#' @param system_prompt Optional replacement for the system instructions.
#' @return An object of class `sc_llm_engine`.
#' @seealso [sc_llm_providers()]
#' @examples
#' # A deterministic stand-in for a model, useful for testing pipelines
#' fake <- function(prompt) {
#'   '[{"field": "randomized_n", "value": "238", "block_id": "b-p001-0004",
#'      "quote": "A total of 238 patients were randomly assigned"}]'
#' }
#' eng <- sc_llm_engine(fun = fake, model = "fixture")
#' doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
#' x <- sc_extract(doc, "trial_details", engine = "llm", llm = eng)
#' x$records[, c("field", "value", "status", "data_origin")]
#' \dontrun{
#' # API providers; keys come from environment variables
#' sc_llm_engine("anthropic", model = "claude-sonnet-5-5")
#' sc_llm_engine("openai", model = "gpt-6")
#' sc_llm_engine("openrouter", model = "anthropic/claude-opus-5-5")
#' sc_llm_engine("ollama", model = "llama4")
#' # ChatGPT plan, after ellmercodex::codex_login()
#' sc_llm_engine("chatgpt", model = "gpt-6-luna", effort = "low")
#' }
#' @export
sc_llm_engine <- function(provider = NULL, model = NULL, ..., chat = NULL, fun = NULL,
                          structured = c("auto", "always", "never"),
                          max_chars = 12000L, system_prompt = NULL) {
  structured <- match.arg(structured)
  n_set <- (!is.null(provider)) + (!is.null(chat)) + (!is.null(fun))
  if (n_set != 1L) abort("Supply exactly one of `provider`, `chat` or `fun`.")
  if (!is.null(fun) && !is.function(fun)) abort("`fun` must be a function.")
  prov <- NA_character_
  if (!is.null(provider)) {
    check_string(provider, "provider")
    p <- resolve_provider(provider)
    args <- list(...)
    if (!is.null(model)) args$model <- model
    chat <- tryCatch(do.call(getExportedValue(p$package, p$fun), args),
                     error = function(e) abort("Could not create the ", p$provider, " chat: ", conditionMessage(e)))
    prov <- p$provider
  } else if (length(list(...))) {
    abort("Arguments in `...` are used only with `provider`.")
  }
  if (!is.null(chat)) {
    if (!is_installed("ellmer")) abort("The ellmer package is needed for `chat`.")
    if (!is.environment(chat) || !is.function(chat$chat_structured)) abort("`chat` must be an ellmer Chat object.")
    label <- tryCatch(paste(chat$get_provider()@name, chat$get_model()), error = function(e) NULL)
    model <- label %||% model %||% "ellmer chat"
  }
  structure(list(chat = chat, fun = fun, provider = prov, model = model %||% "user function",
                 structured = structured, max_chars = as.integer(max_chars),
                 system_prompt = system_prompt %||% llm_system_prompt),
            class = "sc_llm_engine")
}

provider_aliases <- c(chatgpt = "codex", claude = "anthropic", gemini = "google_gemini",
                      bedrock = "aws_bedrock", azure = "azure_openai", vertex = "google_vertex")

#' Language model providers
#'
#' Lists the providers that [sc_llm_engine()] accepts: every `chat_*`
#' constructor exported by the installed ellmer version, plus `"codex"`
#' (a ChatGPT plan through ellmercodex). Aliases: `"chatgpt"` (codex),
#' `"claude"` (anthropic), `"gemini"` (google_gemini), `"bedrock"`,
#' `"azure"`, `"vertex"`.
#'
#' @return A data frame with the provider name, package, constructor and
#'   whether that package is installed.
#' @examples
#' sc_llm_providers()
#' @export
sc_llm_providers <- function() {
  ell <- if (is_installed("ellmer")) {
    f <- grep("^chat_", getNamespaceExports("ellmer"), value = TRUE)
    sort(setdiff(f, "chat_claude"))
  } else character()
  out <- data.frame(provider = sub("^chat_", "", ell), package = rep("ellmer", length(ell)),
                    constructor = paste0("ellmer::", ell), installed = rep(length(ell) > 0, length(ell)),
                    stringsAsFactors = FALSE)
  rbind(out, data.frame(provider = "codex", package = "ellmercodex", constructor = "ellmercodex::chat_codex",
                        installed = is_installed("ellmercodex"), stringsAsFactors = FALSE))
}

resolve_provider <- function(provider) {
  p <- tolower(gsub("[- ]", "_", provider))
  p <- unname(c(provider_aliases, stats::setNames(p, p))[p])
  if (p == "codex") {
    if (!is_installed("ellmercodex")) abort("The ellmercodex package is needed for ChatGPT plans: install.packages(\"ellmercodex\"), then ellmercodex::codex_login().")
    return(list(provider = "codex", package = "ellmercodex", fun = "chat_codex"))
  }
  if (!is_installed("ellmer")) abort("The ellmer package is needed for language model providers.")
  fn <- paste0("chat_", p)
  if (!fn %in% getNamespaceExports("ellmer")) {
    abort("Unknown provider '", provider, "'. See sc_llm_providers() for the available providers.")
  }
  list(provider = p, package = "ellmer", fun = fn)
}

#' @export
print.sc_llm_engine <- function(x, ...) {
  cat("<sc_llm_engine> ", x$model, if (!is.na(x$provider %||% NA)) paste0(" via ", x$provider) else if (!is.null(x$chat)) " (ellmer)" else " (function)",
      ", max ", x$max_chars, " characters per request\n", sep = "")
  invisible(x)
}

llm_system_prompt <- paste(
  "You extract information from scientific articles.",
  "Use only the passages given. Each passage starts with its block id in square brackets.",
  "For every value, copy the exact supporting text from the passage into `quote` and give the passage's `block_id`.",
  "Do not compute, convert, round or infer values. If a field is not stated, omit it.",
  "Return a JSON array of objects with keys: field, value, quote, block_id, and optionally",
  "statistic, arm, comparator, outcome, timepoint, analysis_set, unit.")

llm_prompt <- function(schema, passages) {
  fl <- vapply(schema$fields, function(f) {
    voc <- if (!is.null(f$vocabulary)) paste0(" Allowed values: ", paste(names(f$vocabulary), collapse = ", "), ".") else ""
    paste0("- ", f$name, " (", f$type, if (f$multiple) ", may repeat", "): ", f$description, ".", voc)
  }, character(1))
  paste0("Fields to extract:\n", paste(fl, collapse = "\n"), "\n\nPassages:\n",
         paste0("[", passages$block_id, "] ", passages$text, collapse = "\n\n"))
}

chunk_passages <- function(units, max_chars) {
  size <- nchar(units$text) + nchar(units$block_id) + 4L
  grp <- integer(nrow(units))
  cur <- 1L
  acc <- 0L
  for (i in seq_len(nrow(units))) {
    if (acc > 0L && acc + size[i] > max_chars) {
      cur <- cur + 1L
      acc <- 0L
    }
    grp[i] <- cur
    acc <- acc + size[i]
  }
  split(units, grp)
}

llm_type <- function() {
  ellmer::type_array(ellmer::type_object(
    field = ellmer::type_string(), value = ellmer::type_string(),
    quote = ellmer::type_string(), block_id = ellmer::type_string(),
    statistic = ellmer::type_string(required = FALSE), arm = ellmer::type_string(required = FALSE),
    comparator = ellmer::type_string(required = FALSE), outcome = ellmer::type_string(required = FALSE),
    timepoint = ellmer::type_string(required = FALSE), analysis_set = ellmer::type_string(required = FALSE),
    unit = ellmer::type_string(required = FALSE)))
}

as_items <- function(ans) {
  if (is.character(ans)) {
    txt <- paste(ans, collapse = "\n")
    txt <- sub("^\\s*```(json)?", "", sub("```\\s*$", "", txt))
    ans <- tryCatch(jsonlite::fromJSON(txt, simplifyDataFrame = TRUE),
                    error = function(e) abort("The model answer is not valid JSON: ", conditionMessage(e)))
  }
  if (is.list(ans) && !is.data.frame(ans)) {
    ans <- if (length(ans)) do.call(rbind, lapply(ans, function(z) as.data.frame(z[!vapply(z, is.null, logical(1))], stringsAsFactors = FALSE))) else data.frame()
  }
  if (!is.data.frame(ans)) abort("The model answer must be a JSON array or a data frame.")
  if (!nrow(ans)) return(ans)
  miss <- setdiff(c("field", "value", "quote", "block_id"), names(ans))
  for (m in miss) ans[[m]] <- NA_character_
  ans[] <- lapply(ans, function(v) if (is.list(v)) vapply(v, function(z) paste(z, collapse = " "), "") else v)
  ans
}

llm_extract <- function(doc, schema, llm, ...) {
  if (!inherits(llm, "sc_llm_engine")) abort("`llm` must come from sc_llm_engine().")
  units <- text_units(doc)
  if (!nrow(units)) abort("The document has no text passages to send.")
  col <- new_collector(doc, schema$name, "llm")
  chunks <- chunk_passages(units[, c("block_id", "text")], llm$max_chars)
  calls <- list()
  for (ch in chunks) {
    prompt <- llm_prompt(schema, ch)
    t0 <- Sys.time()
    if (!is.null(llm$chat)) {
      chat <- llm$chat$clone()
      chat$set_system_prompt(llm$system_prompt)
      ans <- ask_chat(chat, prompt, llm$structured)
      tok <- tryCatch(chat$get_tokens(), error = function(e) NULL)
      cost <- tryCatch(as.numeric(chat$get_cost()), error = function(e) NA_real_)
    } else {
      ans <- llm$fun(paste0(llm$system_prompt, "\n\n", prompt))
      tok <- NULL
      cost <- NA_real_
    }
    calls[[length(calls) + 1L]] <- list(prompt_sha256 = digest::digest(prompt, algo = "sha256", serialize = FALSE),
                                        n_passages = nrow(ch), seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                                        input_tokens = sum_tokens(tok, "input"), output_tokens = sum_tokens(tok, "output"),
                                        cost = cost)
    items <- as_items(ans)
    for (i in seq_len(nrow(items))) verify_item(col, items[i, , drop = FALSE], schema, ch$block_id)
  }
  fr <- collector_frames(col)
  fr$records <- add_not_reported(fr$records, schema, doc, "llm")
  llm_info <- list(model = llm$model, provider = llm$provider %||% NA_character_, system_prompt_sha256 = digest::digest(llm$system_prompt, algo = "sha256", serialize = FALSE),
                   calls = do.call(rbind, lapply(calls, as.data.frame)))
  new_extraction(doc, schema, fr$records, fr$evidence,
                 engine = list(name = "llm", version = pkg_version("sciparseR")), llm = llm_info)
}

sum_tokens <- function(tok, which) {
  if (is.null(tok) || !is.data.frame(tok)) return(NA_real_)
  nm <- grep(which, names(tok), value = TRUE)
  if (!length(nm)) return(NA_real_)
  sum(as.numeric(tok[[nm[1]]]), na.rm = TRUE)
}

squash <- function(x) gsub("\\s+", " ", trimws(normalize_text(x)))

verify_item <- function(col, it, schema, sent_ids) {
  g <- function(k) {
    v <- it[[k]]
    if (is.null(v) || !length(v) || is.na(v[1]) || !nzchar(v[1])) NA_character_ else as.character(v[1])
  }
  field <- g("field")
  value <- g("value")
  quote <- g("quote")
  bid <- g("block_id")
  notes <- character()
  fdef <- if (!is.na(field)) schema$fields[[field]] else NULL
  status <- "reported"
  if (is.null(fdef)) {
    notes <- c(notes, "field not in schema")
    status <- "unsupported"
  }
  ev <- NA_character_
  doc <- col$doc
  if (status == "unsupported") {
    # nothing to verify for a field outside the schema
  } else if (is.na(bid) || !bid %in% sent_ids) {
    status <- "unsupported"
    notes <- c(notes, "block id missing or not among the passages sent")
  } else if (is.na(quote)) {
    status <- "unsupported"
    notes <- c(notes, "no quote given")
  } else {
    btxt <- doc$blocks$text[match(bid, doc$blocks$block_id)]
    pos <- regexpr(squash(quote), squash(btxt), fixed = TRUE)
    if (pos < 0) {
      status <- "unsupported"
      notes <- c(notes, "quote not found in the cited block")
    } else {
      # offsets refer to the whitespace-squashed block text, which equals
      # the stored block text for normalized PDF and JATS blocks
      ev <- add_evidence(col, block_id = bid, char_start = as.integer(pos),
                         char_end = as.integer(pos) + nchar(squash(quote)) - 1L, value_text = quote,
                         method = "llm_quote")
      if (!is.null(fdef) && fdef$type %in% c("numeric", "integer") && !is.na(value) &&
          !grepl(value, quote, fixed = TRUE)) {
        status <- "unsupported"
        notes <- c(notes, "numeric value does not occur in the quote")
      }
    }
  }
  if (!is.null(fdef) && fdef$type == "categorical" && !is.na(value) && !value %in% names(fdef$vocabulary)) {
    notes <- c(notes, "value outside the field vocabulary")
    if (status == "reported") status <- "ambiguous"
  }
  num <- if (!is.null(fdef) && fdef$type %in% c("numeric", "integer")) as_num(value) else NA_real_
  add_record(col, field %|NA|% "unknown", value, statistic = g("statistic"), value_num = num,
             status = status, data_origin = "model_inferred", evidence_id = ev, method = "llm",
             arm = g("arm"), comparator = g("comparator"), outcome = g("outcome"),
             timepoint = g("timepoint"), analysis_set = g("analysis_set"), unit = g("unit"),
             observation_id = if (field %in% c("result", "baseline")) next_observation(col) else NA_character_,
             binding = "model_output",
             notes = if (length(notes)) paste(notes, collapse = "; ") else NA_character_)
}

# Structured output when the provider supports it; otherwise plain JSON.
ask_chat <- function(chat, prompt, structured) {
  plain <- function() chat$chat(paste0(prompt, "\n\nAnswer with the JSON array only."), echo = "none")
  if (structured == "never") return(plain())
  res <- tryCatch(chat$chat_structured(prompt, type = llm_type(), echo = "none"), error = function(e) e)
  if (!inherits(res, "error")) return(res)
  if (structured == "always") abort("Structured output failed: ", conditionMessage(res))
  plain()
}

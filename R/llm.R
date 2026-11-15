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
                    constructor = if (length(ell)) paste0("ellmer::", ell) else character(),
                    installed = rep(TRUE, length(ell)),
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
  "statistic, arm, comparator, outcome, timepoint, analysis_set, unit, effect_measure, ci_level,",
  "adjusted and observation.",
  "For numerical results, give one object per number: statistic is one of estimate, ci_lower, ci_upper,",
  "p_value, events, denominator, percent, mean, sd, se, median, iqr_lower, iqr_upper, n.",
  "Numbers that belong to the same result (an estimate with its interval and p-value, events with their",
  "denominator) share the same observation label, for example \"1\".",
  "Passages whose id starts with t- are tables, one line per row as `row label | column: value`.")

llm_prompt <- function(schema, passages) {
  fl <- vapply(schema$fields, function(f) {
    voc <- if (!is.null(f$vocabulary)) paste0(" Allowed values: ", paste(names(f$vocabulary), collapse = ", "), ".") else ""
    paste0("- ", f$name, " (", f$type, if (f$multiple) ", may repeat", "): ", f$description, ".", voc)
  }, character(1))
  paste0("Fields to extract:\n", paste(fl, collapse = "\n"), "\n\nPassages:\n",
         paste0("[", passages$block_id, "] ", passages$text, collapse = "\n\n"))
}

chunk_passages <- function(units, max_chars) {
  units <- split_long_passages(units, max_chars)
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

# A passage longer than the budget is sent in pieces, cut at sentence ends
# where possible, under the same block id: quotes are verified against the
# whole block, so offsets are unaffected.
split_long_passages <- function(units, max_chars) {
  room <- max(200L, max_chars - 50L)
  pieces <- lapply(seq_len(nrow(units)), function(i) {
    t <- units$text[i]
    if (nchar(t) <= room) return(data.frame(block_id = units$block_id[i], text = t, stringsAsFactors = FALSE))
    parts <- character()
    while (nchar(t) > room) {
      head <- substr(t, 1L, room)
      cut <- max(c(gregexpr("[.;]\\s", head)[[1]], gregexpr("\\s", head)[[1]][1]))
      if (cut < room / 2) cut <- max(gregexpr("\\s", head)[[1]])
      if (cut < 1L) cut <- room
      parts <- c(parts, trimws(substr(t, 1L, cut)))
      t <- trimws(substr(t, cut + 1L, nchar(t)))
    }
    data.frame(block_id = units$block_id[i], text = c(parts, t), stringsAsFactors = FALSE)
  })
  do.call(rbind, pieces)
}

llm_type <- function() {
  ellmer::type_array(ellmer::type_object(
    field = ellmer::type_string(), value = ellmer::type_string(),
    quote = ellmer::type_string(), block_id = ellmer::type_string(),
    statistic = ellmer::type_string(required = FALSE), arm = ellmer::type_string(required = FALSE),
    comparator = ellmer::type_string(required = FALSE), outcome = ellmer::type_string(required = FALSE),
    timepoint = ellmer::type_string(required = FALSE), analysis_set = ellmer::type_string(required = FALSE),
    unit = ellmer::type_string(required = FALSE), effect_measure = ellmer::type_string(required = FALSE),
    ci_level = ellmer::type_string(required = FALSE), adjusted = ellmer::type_string(required = FALSE),
    observation = ellmer::type_string(required = FALSE)))
}

# Tables as passages: one line per body row, "label | header: value".
table_passages <- function(doc) {
  tabs <- doc$tables[!doc$tables$continuation_status %in% "joined", , drop = FALSE]
  if (!nrow(tabs)) return(data.frame(block_id = character(), text = character(), stringsAsFactors = FALSE))
  txt <- vapply(tabs$table_id, function(tid) {
    df <- tryCatch(sc_table_to_dataframe(doc, tid), error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return("")
    cap <- sc_table_caption(doc, tid) %||% ""
    rows <- vapply(seq_len(nrow(df)), function(i) {
      v <- as.character(unlist(df[i, ]))
      paste0(v[1], " | ", paste(paste0(names(df)[-1], ": ", v[-1]), collapse = " | "))
    }, character(1))
    paste(c(cap, rows), collapse = "\n")
  }, character(1))
  out <- data.frame(block_id = tabs$table_id, text = unname(txt), stringsAsFactors = FALSE)
  out[nzchar(out$text), , drop = FALSE]
}

as_items <- function(ans) {
  if (is.character(ans)) {
    txt <- paste(ans, collapse = "\n")
    txt <- sub("^\\s*```(json)?", "", sub("```\\s*$", "", txt))
    # parse the text itself: jsonlite::fromJSON() would also accept a URL or
    # a file name, which model output must never be able to trigger
    # models often wrap the JSON in prose ("Here is the JSON: [...]"): keep
    # the text from the first bracket to the last matching one
    st <- regexpr("[\\[{]", txt)
    if (st < 0) abort("The model answer is not valid JSON: it must be an array or an object.")
    close <- if (substr(txt, st, st) == "[") "]" else "}"
    ends <- gregexpr(close, txt, fixed = TRUE)[[1]]
    ends <- ends[ends > st]
    if (!length(ends)) abort("The model answer is not valid JSON: the array or object is not closed.")
    txt <- substr(txt, st, max(ends))
    ans <- tryCatch(jsonlite::parse_json(txt, simplifyVector = TRUE, simplifyDataFrame = TRUE),
                    error = function(e) abort("The model answer is not valid JSON: ", conditionMessage(e)))
  }
  if (is.list(ans) && !is.data.frame(ans) && !is.null(names(ans))) {
    # a single object, or an object wrapping the array
    ans <- if ("field" %in% names(ans)) list(ans) else if (length(ans) == 1L) ans[[1]] else ans
  }
  if (is.list(ans) && !is.data.frame(ans)) {
    rows <- lapply(ans, function(z) {
      if (!is.list(z)) abort("Each element of the model answer must be an object.")
      z <- z[!vapply(z, is.null, logical(1))]
      lapply(z, function(v) paste(as.character(unlist(v)), collapse = " "))
    })
    keys <- unique(unlist(lapply(rows, names)))
    ans <- if (length(rows)) {
      as.data.frame(lapply(stats::setNames(keys, keys), function(k) {
        vapply(rows, function(z) if (is.null(z[[k]])) NA_character_ else z[[k]], character(1))
      }), stringsAsFactors = FALSE)
    } else data.frame()
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
  units <- rbind(text_units(doc)[, c("block_id", "text")], table_passages(doc))
  if (!nrow(units)) abort("The document has no text passages to send.")
  col <- new_collector(doc, schema$name, "llm")
  chunks <- chunk_passages(units, llm$max_chars)
  calls <- list()
  for (ch in chunks) {
    prompt <- llm_prompt(schema, ch)
    t0 <- Sys.time()
    if (!is.null(llm$chat)) {
      chat <- llm$chat$clone()
      # each chunk starts from an empty conversation with the package system
      # prompt, so earlier turns are neither resent nor billed again
      tryCatch(chat$set_turns(list()), error = function(e) NULL)
      chat$set_system_prompt(llm$system_prompt)
      # usage of this request only: the difference to any earlier history
      tok0 <- tryCatch(chat$get_tokens(), error = function(e) NULL)
      cost0 <- tryCatch(as.numeric(chat$get_cost()), error = function(e) NA_real_)
      ans <- ask_chat(chat, prompt, llm$structured)
      tok <- tryCatch(chat$get_tokens(), error = function(e) NULL)
      cost <- tryCatch(as.numeric(chat$get_cost()), error = function(e) NA_real_) - (if (length(cost0) == 1L && !is.na(cost0)) cost0 else 0)
    } else {
      ans <- llm$fun(paste0(llm$system_prompt, "\n\n", prompt))
      tok <- tok0 <- NULL
      cost <- NA_real_
    }
    calls[[length(calls) + 1L]] <- list(prompt_sha256 = digest::digest(prompt, algo = "sha256", serialize = FALSE),
                                        n_passages = nrow(ch), seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
                                        input_tokens = sum_tokens(tok, "input") - sum_tokens0(tok0, "input"),
                                        output_tokens = sum_tokens(tok, "output") - sum_tokens0(tok0, "output"),
                                        cost = cost, error = NA_character_)
    # one malformed answer must not lose the results of the other chunks
    items <- tryCatch(as_items(ans), error = function(e) {
      calls[[length(calls)]]$error <<- conditionMessage(e)
      NULL
    })
    if (is.null(items)) {
      warning(paste0("The answer to chunk ", length(calls), " could not be read and was skipped: ", calls[[length(calls)]]$error), call. = FALSE)
      next
    }
    obs_map <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(items))) verify_item(col, items[i, , drop = FALSE], schema, unique(ch$block_id), obs_map)
  }
  fr <- collector_frames(col)
  fr$records <- add_not_reported(fr$records, schema, doc, "llm")
  llm_info <- list(model = llm$model, provider = llm$provider %||% NA_character_, system_prompt_sha256 = digest::digest(llm$system_prompt, algo = "sha256", serialize = FALSE),
                   calls = do.call(rbind, lapply(calls, as.data.frame)))
  new_extraction(doc, schema, fr$records, fr$evidence,
                 engine = list(name = "llm", version = pkg_version("sciparseR")), llm = llm_info)
}

sum_tokens0 <- function(tok, which) {
  v <- sum_tokens(tok, which)
  if (is.na(v)) 0 else v
}

sum_tokens <- function(tok, which) {
  if (is.null(tok) || !is.data.frame(tok)) return(NA_real_)
  nm <- grep(which, names(tok), value = TRUE)
  if (!length(nm)) return(NA_real_)
  sum(as.numeric(tok[[nm[1]]]), na.rm = TRUE)
}

squash <- function(x) gsub("\\s+", " ", trimws(normalize_text(x)))

verify_item <- function(col, it, schema, sent_ids, obs_map = new.env(parent = emptyenv())) {
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
  is_num <- !is.null(fdef) && fdef$type %in% c("numeric", "integer")
  src_text <- NA_character_
  if (status == "unsupported") {
    # nothing to verify for a field outside the schema
  } else if (is.na(bid) || !bid %in% sent_ids) {
    status <- "unsupported"
    notes <- c(notes, "block id missing or not among the passages sent")
  } else if (is.na(value)) {
    status <- "unsupported"
    notes <- c(notes, "no value given")
  } else if (is.na(quote)) {
    status <- "unsupported"
    notes <- c(notes, "no quote given")
  } else if (bid %in% doc$tables$table_id) {
    # a table passage: the quote must be the text of one cell of that table
    cl <- doc$cells[doc$cells$table_id %in% bid, , drop = FALSE]
    hit <- which(squash(cl$text) == squash(quote))
    if (!length(hit)) hit <- which(nzchar(cl$text) & vapply(cl$text, function(t) grepl(squash(quote), squash(t), fixed = TRUE), logical(1)))
    if (!length(hit)) {
      status <- "unsupported"
      notes <- c(notes, "quote is not the text of a cell of the cited table")
    } else {
      cid <- cl$cell_id[hit[1]]
      src_text <- paste(cl$text, collapse = " ")
      ev <- add_evidence(col, cell_id = cid, char_start = 1L, char_end = nchar(cl$text[hit[1]]),
                         value_text = cl$text[hit[1]], method = "llm_quote")
    }
  } else {
    btxt <- doc$blocks$text[match(bid, doc$blocks$block_id)]
    pos <- regexpr(squash(quote), squash(btxt), fixed = TRUE)
    if (pos < 0) {
      status <- "unsupported"
      notes <- c(notes, "quote not found in the cited block")
    } else {
      src_text <- btxt
      # offsets refer to the whitespace-squashed block text, which equals
      # the stored block text for normalized PDF and JATS blocks
      ev <- add_evidence(col, block_id = bid, char_start = as.integer(pos),
                         char_end = as.integer(pos) + nchar(squash(quote)) - 1L, value_text = quote,
                         method = "llm_quote")
    }
  }
  if (status == "reported" && !is.na(value)) {
    if (is_num) {
      # the value must be one of the complete numbers written in the quote
      toks <- regmatches(quote, gregexpr(num_re, quote, perl = TRUE))[[1]]
      vn <- as_num(value)
      if (is.na(vn) || !any(abs(as_num(toks) - vn) < 1e-12 * max(1, abs(vn)), na.rm = TRUE)) {
        status <- "unsupported"
        notes <- c(notes, "numeric value is not a number written in the quote")
      }
    } else if (fdef$type == "categorical" && !is.null(fdef$vocabulary)) {
      if (!value %in% names(fdef$vocabulary)) {
        notes <- c(notes, "value outside the field vocabulary")
        status <- "ambiguous"
      } else if (!value %in% match_vocab(quote, fdef$vocabulary)$term) {
        status <- "unsupported"
        notes <- c(notes, "the quote does not state this category")
      }
    } else if (fdef$type == "character" && !grepl(squash(tolower(value)), squash(tolower(quote)), fixed = TRUE)) {
      status <- "unsupported"
      notes <- c(notes, "value does not occur in the quote")
    }
  }
  # context the model attached must be written in the cited passage
  if (status == "reported" && !is.na(src_text)) {
    for (k in c("arm", "comparator", "outcome", "timepoint", "analysis_set")) {
      v <- g(k)
      if (!is.na(v) && !grepl(squash(tolower(v)), squash(tolower(src_text)), fixed = TRUE)) {
        notes <- c(notes, paste0(k, " '", v, "' is not written in the cited passage"))
        status <- "ambiguous"
      }
    }
  }
  num <- if (is_num) as_num(value) else NA_real_
  obs <- NA_character_
  if (field %in% c("result", "baseline")) {
    lab <- g("observation")
    key <- paste(bid, lab)
    if (is.na(lab)) {
      obs <- next_observation(col)
    } else {
      if (is.null(obs_map[[key]])) obs_map[[key]] <- next_observation(col)
      obs <- obs_map[[key]]
    }
  }
  lvl <- suppressWarnings(as.numeric(sub("%", "", g("ci_level"))))
  adj <- tolower(g("adjusted"))
  add_record(col, field %|NA|% "unknown", value, statistic = g("statistic"), value_num = num,
             status = status, data_origin = "model_inferred", evidence_id = ev, method = "llm",
             arm = g("arm"), comparator = g("comparator"), outcome = g("outcome"),
             timepoint = g("timepoint"), analysis_set = g("analysis_set"), unit = g("unit"),
             effect_measure = g("effect_measure"),
             ci_level = if (is.finite(lvl) && lvl > 0 && lvl < 100) lvl else NA_real_,
             adjusted = if (adj %in% c("true", "yes", "adjusted")) TRUE else if (adj %in% c("false", "no", "unadjusted")) FALSE else NA,
             observation_id = obs, binding = "model_output",
             notes = if (length(notes)) paste(notes, collapse = "; ") else NA_character_)
}

# Structured output when the provider supports it; otherwise plain JSON.
ask_chat <- function(chat, prompt, structured) {
  plain <- function() chat$chat(paste0(prompt, "\n\nAnswer with the JSON array only."), echo = "none")
  if (structured == "never") return(plain())
  res <- tryCatch(chat$chat_structured(prompt, type = llm_type(), echo = "none"), error = function(e) e)
  if (!inherits(res, "error")) return(res)
  if (structured == "always") abort("Structured output failed: ", conditionMessage(res))
  # fall back only when the provider cannot do structured output; network,
  # authentication and quota errors are reported as they are
  if (!grepl("structured|schema|json|response_format|tool|function call|not support|unsupported|parse",
             conditionMessage(res), ignore.case = TRUE)) {
    abort("The language model request failed: ", conditionMessage(res))
  }
  plain()
}

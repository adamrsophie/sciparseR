test_that("derived values record inputs and formula", {
  x <- sc_derive(sc_extract(sc_parse(fixture("trial_tables.pdf"))), c("se_log_ratio", "sd_from_se", "log_ratio"))
  d <- x$records[x$records$data_origin == "derived", ]
  se <- d[d$statistic == "se_log" & d$outcome %in% "primary outcome", ][1, ]
  expect_equal(se$value_num, (log(0.94) - log(0.41)) / (2 * qnorm(0.975)), tolerance = 1e-9)
  expect_false(is.na(se$inputs))
  expect_match(se$formula, "qnorm")
  expect_match(se$software, "^R ")
  sd <- d[d$statistic == "sd" & d$arm %in% "Drug A", ]
  expect_equal(sd$value_num, 1.2 * sqrt(120), tolerance = 1e-9)
  expect_true(all(is.na(d$evidence_id)))
  v <- sc_validate(x)
  expect_false(any(unlist(strsplit(v$validation$record_ids, ",")) %in% d$record_id))
})

test_that("unstated confidence levels are not assumed", {
  x <- sc_extract(sc_parse(fixture("trial_tables.pdf")))
  x$records$ci_level <- NA_real_
  expect_false(any(sc_derive(x, "se_log_ratio")$records$statistic %in% "se_log"))
  y <- sc_derive(x, "se_log_ratio", assume_level = 95)
  se <- y$records[y$records$statistic %in% "se_log", ]
  expect_gt(nrow(se), 0)
  expect_true(all(grepl("assumed", se$notes)))
})

test_that("LLM engine verifies quotes and never runs by default", {
  doc <- sc_parse(fixture("trial_tables.pdf"))
  expect_error(sc_extract(doc, engine = "llm"), "sc_llm_engine")
  expect_error(sc_llm_engine(), "exactly one")
  b <- sc_blocks(doc, type = "paragraph")
  bid <- b$block_id[grepl("total of 238", b$text)]
  seen <- NULL
  fake <- function(prompt) {
    seen <<- prompt
    jsonlite::toJSON(data.frame(
      field = c("randomized_n", "randomized_n", "comparator", "study_design"),
      value = c("238", "240", "sham", "cohort"),
      quote = c("A total of 238 patients were randomly assigned", "A total of 238 patients", "sham", "randomized"),
      block_id = c(bid, bid, "nope", bid)))
  }
  x <- sc_extract(doc, "trial_details", engine = "llm", llm = sc_llm_engine(fun = fake, model = "fixture"))
  r <- x$records
  expect_identical(r$status[r$value %in% "238"], "reported")
  expect_identical(r$status[r$value %in% "240"], "unsupported")
  expect_identical(r$status[r$value %in% "sham"], "unsupported")
  expect_true(all(r$data_origin[!r$status %in% "not_reported"] == "model_inferred"))
  expect_match(seen, bid, fixed = TRUE)
  expect_match(x$llm$calls$prompt_sha256[1], "^[0-9a-f]{64}$")
  expect_identical(x$llm$model, "fixture")
  bad <- function(prompt) "not json"
  expect_error(sc_extract(doc, engine = "llm", llm = sc_llm_engine(fun = bad)), "JSON")
})

fake_chat <- function(structured_ok) {
  env <- new.env()
  env$calls <- character()
  env$clone <- function() env
  env$set_system_prompt <- function(x) invisible(env)
  env$get_provider <- function() stop("no provider")
  env$get_tokens <- function() data.frame(input = 10, output = 5)
  env$get_cost <- function() 0.01
  env$chat_structured <- function(prompt, type, echo) {
    env$calls <- c(env$calls, "structured")
    if (!structured_ok) stop("structured output unsupported")
    data.frame(field = "allocation_ratio", value = "1:1", quote = "1:1 ratio", block_id = env$bid)
  }
  env$chat <- function(prompt, echo) {
    env$calls <- c(env$calls, "plain")
    sprintf('[{"field":"allocation_ratio","value":"1:1","quote":"1:1 ratio","block_id":"%s"}]', env$bid)
  }
  env
}

test_that("ellmer chats use structured output with a plain JSON fallback", {
  skip_if_not_installed("ellmer")
  doc <- sc_parse(fixture("trial_tables.pdf"))
  b <- sc_blocks(doc, type = "paragraph")
  bid <- b$block_id[grepl("1:1 ratio", b$text)]
  for (ok in c(TRUE, FALSE)) {
    ch <- fake_chat(ok)
    ch$bid <- bid
    eng <- sc_llm_engine(chat = ch, model = "fake")
    x <- sc_extract(doc, "trial_details", engine = "llm", llm = eng)
    expect_identical(x$records$status[x$records$field == "allocation_ratio"], "reported")
    expect_identical(ch$calls, if (ok) "structured" else c("structured", "plain"))
    expect_equal(x$llm$calls$input_tokens, 10)
  }
  ch <- fake_chat(FALSE)
  ch$bid <- bid
  expect_error(sc_extract(doc, engine = "llm", llm = sc_llm_engine(chat = ch, structured = "always")), "Structured")
})

test_that("providers resolve through ellmer and ellmercodex", {
  skip_if_not_installed("ellmer")
  p <- sc_llm_providers()
  expect_true(all(c("anthropic", "openai", "openrouter", "codex") %in% p$provider))
  expect_error(sc_llm_engine("no_such_provider"), "Unknown provider")
  expect_error(sc_llm_engine("openai", chat = fake_chat(TRUE)), "exactly one")
  expect_error(sc_llm_engine(fun = identity, api_key = "x"), "only with `provider`")
  expect_identical(sciparseR:::resolve_provider("Claude")$fun, "chat_anthropic")
  expect_identical(sciparseR:::resolve_provider("gemini")$fun, "chat_google_gemini")
})

test_that("derived values record inputs and formula", {
  ex <- sc_extract(sc_parse(fixture("trial_tables.pdf")))
  # the allocated arm size is not the number analyzed: no SD by default
  d0 <- sc_derive(ex, "sd_from_se")$records
  expect_false(any(d0$statistic %in% "sd" & d0$data_origin == "derived"))
  x <- sc_derive(ex, c("se_log_ratio", "sd_from_se", "log_ratio"), allow_randomized_n = TRUE)
  d <- x$records[x$records$data_origin == "derived", ]
  se <- d[d$statistic == "se_log" & d$outcome %in% "primary outcome", ][1, ]
  expect_equal(se$value_num, (log(0.94) - log(0.41)) / (2 * qnorm(0.975)), tolerance = 1e-9)
  expect_false(is.na(se$inputs))
  expect_match(se$formula, "qnorm")
  expect_match(se$software, "^R ")
  sd <- d[d$statistic == "sd" & d$arm %in% "Drug A", ]
  expect_equal(sd$value_num, 1.2 * sqrt(120), tolerance = 1e-9)
  expect_match(sd$notes, "not a reported number analyzed")
  sup <- sc_derive(ex, "sd_from_se", n = c("Drug A" = 100))$records
  expect_equal(sup$value_num[sup$statistic %in% "sd" & sup$arm %in% "Drug A" & sup$data_origin == "derived"], 1.2 * sqrt(100))
  expect_error(sc_derive(ex, "se_log_ratio", assume_level = 0), "strictly between")
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
  # an unreadable answer skips its chunk with a warning and is logged
  expect_warning(xb <- sc_extract(doc, engine = "llm", llm = sc_llm_engine(fun = bad)), "could not be read")
  expect_match(xb$llm$calls$error[1], "JSON")
  # prose around the JSON is tolerated
  wrapped <- function(prompt) paste0("Here is the JSON:\n", sprintf('[{"field":"randomized_n","value":"238","quote":"238","block_id":"%s"}]', bid))
  xw <- sc_extract(doc, engine = "llm", llm = sc_llm_engine(fun = wrapped))
  expect_true(all(is.na(xw$llm$calls$error)))
  # an item without a value is never reported
  novalue <- function(prompt) sprintf('[{"field":"primary_outcome","quote":"patients","block_id":"%s"}]', bid)
  xn <- sc_extract(doc, engine = "llm", llm = sc_llm_engine(fun = novalue))
  expect_false(any(xn$records$status == "reported" & xn$records$field == "primary_outcome" & xn$records$data_origin != "model_inferred"))
})

fake_chat <- function(structured_ok) {
  env <- new.env()
  env$calls <- character()
  env$clone <- function() env
  env$set_system_prompt <- function(x) invisible(env)
  env$get_provider <- function() stop("no provider")
  # earlier history (100/50 tokens) plus one row per new request
  env$get_tokens <- function() data.frame(input = c(100, rep(10, length(env$calls))), output = c(50, rep(5, length(env$calls))))
  env$get_cost <- function() 0.5 + 0.01 * length(env$calls)
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
    expect_equal(x$llm$calls$input_tokens, if (ok) 10 else 20)
    expect_equal(x$llm$calls$cost, if (ok) 0.01 else 0.02)
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

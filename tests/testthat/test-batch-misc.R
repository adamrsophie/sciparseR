test_that("batch parsing caches and isolates failures", {
  cache <- file.path(tempdir(), paste0("cache-", Sys.getpid()))
  on.exit(unlink(cache, recursive = TRUE))
  files <- c(fixture("rotated.pdf"), file.path(tempdir(), "missing.pdf"))
  b <- sc_parse_batch(files, cache_dir = cache)
  expect_identical(b$status$status, c("ok", "failed"))
  expect_null(b$documents[[2]])
  expect_true(sc_parse_batch(files[1], cache_dir = cache)$status$cached)
  expect_false(sc_parse_batch(files[1], config = sc_config(column_gap = 2), cache_dir = cache)$status$cached)
  expect_equal(nrow(sc_cache_info(cache)), 2L)
  sc_cache_info(cache, clear = TRUE)
  expect_equal(nrow(sc_cache_info(cache)), 0L)
})

test_that("OCR is optional and reports per page status", {
  doc <- sc_parse(fixture("scanned.pdf"), config = sc_config(ocr = "never"))
  expect_identical(doc$pages$ocr_status[2], "needed")
  skip_if_not_installed("tesseract")
  skip_if_not("eng" %in% tesseract::tesseract_info()$available)
  o <- sc_parse(fixture("scanned.pdf"), config = sc_config(ocr = "auto"))
  expect_identical(o$pages$ocr_status[2], "applied")
  sp <- o$spans[o$spans$source == "ocr_tesseract", ]
  expect_gt(nrow(sp), 3)
  expect_true(all(sp$confidence >= 0 & sp$confidence <= 100))
  expect_match(paste(sc_blocks(o, page = 2)$text, collapse = " "), "no text layer")
})

test_that("figure crop and highlight write PNG files", {
  skip_if_not_installed("png")
  fe <- sc_parse(fixture("figure_equation.pdf"))
  p <- sc_figure_crop(fe, sc_figures(fe)$figure_id[1], tempfile(fileext = ".png"))
  expect_true(file.exists(p))
  d <- sc_parse(fixture("trial_tables.pdf"))
  h <- sc_highlight(d, sc_extract(d, "trial_details"), dir = file.path(tempdir(), "hl"))
  expect_true(all(file.exists(h)))
  expect_error(sc_figure_crop(sc_parse(fixture("trial_article.xml")), "x", tempfile()), "PDF")
})

test_that("model registry checks licenses and checksums", {
  reg <- file.path(tempdir(), paste0("models-", Sys.getpid()))
  on.exit(unlink(reg, recursive = TRUE))
  f <- tempfile(fileext = ".onnx")
  writeBin(as.raw(1:20), f)
  expect_error(sc_model_register("m", f, task = "layout", license = "MIT", registry = reg), "accept_license")
  expect_error(sc_model_register("m", f, task = "layout", license = "MIT", sha256 = strrep("0", 64), accept_license = TRUE, registry = reg), "Checksum")
  sc_model_register("m", f, task = "layout", license = "MIT", accept_license = TRUE, registry = reg)
  expect_equal(nrow(sc_models(reg)), 1L)
  writeBin(as.raw(20:1), f)
  expect_error(sc_onnx_session("m", registry = reg), "Checksum")
  sc_model_remove("m", registry = reg)
  expect_equal(nrow(sc_models(reg)), 0L)
  st <- sc_onnx_status()
  expect_length(st$integrated_tasks, 0L)
})

test_that("GROBID TEI comparison", {
  tei <- tempfile(fileext = ".xml")
  writeLines(c('<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc><titleStmt>',
               '<title level="a" type="main">Effect of Drug A on Blood Pressure: a Synthetic Example</title>',
               '</titleStmt></fileDesc></teiHeader><text><body>',
               '<div><head n="1.">Introduction</head></div><div><head>Results</head></div></body>',
               '<back><listBibl><biblStruct xml:id="b0"><analytic><title level="a">Global burden</title>',
               '<idno type="DOI">10.1000/jsm.2020.001</idno></analytic></biblStruct></listBibl></back></text></TEI>'), tei)
  g <- sc_read_grobid(tei)
  expect_equal(nrow(g$sections), 2L)
  expect_identical(g$references$doi, "10.1000/jsm.2020.001")
  cmp <- sc_compare_grobid(sc_parse(fixture("single_column.pdf")), tei)
  expect_equal(cmp$value[cmp$measure == "title_match"], 1)
  expect_equal(cmp$value[cmp$measure == "dois_shared"], 1)
})

test_that("configuration validates its arguments", {
  expect_error(sc_config(render_dpi = -1))
  expect_s3_class(sc_config(), "sc_config")
  expect_output(print(sc_schema("pico")), "sc_schema")
})

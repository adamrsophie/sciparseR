jats_file <- function(body, front = "", after = "") {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0('<article xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:mml="http://www.w3.org/1998/Math/MathML"><front><journal-meta><journal-title>Test Journal</journal-title></journal-meta><article-meta><title-group><article-title>Audit trial</article-title></title-group>',
                    front, "</article-meta></front><body>", body, "</body>", after, "</article>"), f, useBytes = TRUE)
  f
}
round_trip <- function(d) sc_parse(sc_export_jats(d, tempfile(fileext = ".xml")))

test_that("floats-group objects and appendices are kept", {
  d <- sc_parse(jats_file('<sec><title>Results</title><p>See Table 1.</p></sec>',
                          after = '<back><app-group><app><title>Appendix</title><p>APPENDIX RESULT 123</p></app></app-group></back><floats-group><table-wrap id="T1"><label>Table 1</label><table><tbody><tr><td>Drug A</td><td>12</td></tr></tbody></table></table-wrap><fig id="F1"><label>Figure 1</label><graphic xlink:href="fig1.png"/></fig></floats-group>'))
  expect_equal(nrow(d$tables), 1L)
  expect_equal(nrow(d$figures), 1L)
  ap <- d$blocks[grepl("APPENDIX RESULT", d$blocks$text), ]
  expect_equal(d$sections$label[match(ap$section_id, d$sections$section_id)], "appendix")
  expect_match(ap$xpath, "/app/")
})

test_that("structured abstracts keep their subsections", {
  d <- sc_parse(jats_file("<p>Body.</p>", front = "<abstract><sec><title>Methods</title><p>We randomized 120 participants.</p></sec><sec><title>Results</title><p>We found 12 events.</p></sec></abstract>"))
  s <- d$sections
  expect_true(all(c("Methods", "Results") %in% s$title))
  m <- d$blocks$section_id[d$blocks$text == "We randomized 120 participants."]
  expect_equal(s$title[s$section_id == m], "Methods")
  expect_equal(s$label[s$section_id == m], "abstract")
  r <- round_trip(d)
  expect_true(all(c("Methods", "Results") %in% r$sections$title))
  # bare abstract text without a paragraph still reaches the text
  t <- sc_parse(jats_file("<p>Body.</p>", front = "<abstract>We randomized 120 participants to Drug A.</abstract>"))
  expect_match(sc_text(t), "120 participants")
})

test_that("CDATA is literal and author and license fallbacks hold", {
  d <- sc_parse(jats_file('<p><![CDATA[The literal string &minus; is part of this description.]]></p>'))
  expect_equal(d$blocks$text[d$blocks$type == "paragraph"], "The literal string &minus; is part of this description.")
  a <- sc_parse(jats_file("<p>Text.</p>", front = '<contrib-group><contrib contrib-type="author"><name><surname>Smith</surname></name></contrib></contrib-group><permissions><license><license-p>Public domain</license-p></license></permissions>'))
  expect_equal(a$metadata$authors, "Smith")
  expect_equal(a$metadata$license, "Public domain")
})

test_that("JATS export keeps reading order, MathML, metadata, footnotes and references", {
  d <- sc_parse(jats_file('<p>BODY BEFORE SECTION</p><sec><title>Results</title><p>PARENT BEFORE CHILD</p><sec><title>Subgroup</title><p>CHILD PARAGRAPH</p></sec><p>PARENT AFTER CHILD</p></sec><sec><title>Methods</title><disp-formula id="e1"><label>(1)</label><mml:math><mml:mfrac><mml:mn>1</mml:mn><mml:mn>2</mml:mn></mml:mfrac></mml:math></disp-formula></sec>',
                          front = '<article-id pub-id-type="pmid">12345</article-id><article-id pub-id-type="pmcid">PMC12345</article-id><pub-date><year>2024</year></pub-date><kwd-group><kwd>hypertension</kwd></kwd-group><permissions><license xlink:href="https://creativecommons.org/licenses/by/4.0/"><license-p>CC BY</license-p></license></permissions>',
                          after = '<back><fn-group><fn id="fn1"><label>a</label><p>Important analysis restriction.</p></fn></fn-group><ref-list><ref id="r1"><label>1</label><mixed-citation><person-group person-group-type="author"><name><surname>Smith</surname><given-names>J</given-names></name></person-group>. A trial. <source>Lancet</source>. <year>2020</year>. <pub-id pub-id-type="doi">10.1000/xyz</pub-id></mixed-citation></ref></ref-list></back>'))
  r <- round_trip(d)
  para <- function(z) z$blocks$text[z$blocks$type == "paragraph"]
  expect_equal(para(r), para(d))
  expect_equal(r$equations$text, d$equations$text)
  expect_false(is.na(r$equations$mathml))
  for (f in c("journal", "year", "pmid", "pmcid", "keywords", "license")) expect_equal(r$metadata[[f]], d$metadata[[f]], info = f)
  expect_equal(r$footnotes$text[r$footnotes$scope == "document"], "Important analysis restriction.")
  r2 <- round_trip(r)
  expect_equal(r2$references$text, r$references$text)
  expect_equal(r$references$text, d$references$text)
  for (f in c("authors", "year", "journal", "doi")) expect_equal(r2$references[[f]], d$references[[f]], info = f)
})

test_that("evidence from another document is refused", {
  d1 <- sc_parse(jats_file("<p>We randomized 120 participants.</p>"))
  d2 <- sc_parse(jats_file("<p>We randomized 999 participants.</p>"))
  x1 <- sc_extract(d1, "trial_details")
  expect_error(sc_source(d2, x1$evidence), "different document")
  expect_equal(unique(sc_source(d1, x1$evidence)$sha256), d1$source$sha256)
})

test_that("case-insensitive fixed search works", {
  j <- sc_parse(fixture("trial_article.xml"))
  expect_silent(lo <- sc_find(j, "drug", fixed = TRUE, ignore_case = TRUE))
  expect_gte(nrow(lo), nrow(sc_find(j, "Drug", fixed = TRUE)))
  expect_gt(nrow(sc_find(j, "a.", fixed = TRUE, ignore_case = TRUE)) + 1L, 0L)
})

test_that("failed cache writes are not reported as cached", {
  f <- jats_file("<p>Cache test.</p>")
  cache <- tempfile()
  dir.create(cache)
  cf <- file.path(cache, paste0(cache_key(f, sc_config()), ".rds"))
  dir.create(cf)
  b <- sc_parse_batch(f, cache_dir = cache, keep = FALSE)
  expect_equal(b$status$status, "failed")
  expect_match(b$status$error, "could not write")
  b2 <- sc_parse_batch(f, cache_dir = cache, keep = TRUE)
  expect_equal(b2$status$status, "ok")
  expect_true(is.na(b2$status$cache_file))
  expect_false(any(grepl("\\.tmp$", list.files(cache))))
  expect_true(all(c("poppler", "libxml2") %in% names(native_versions())))
})

test_that("JSON import rejects missing arrays, dangling references and cycles", {
  f <- tempfile(fileext = ".json")
  writeLines(paste0('{"object_type":"sc_document","schema_version":"1.0.0","doc_id":"doc-a","source":{"sha256":"', strrep("a", 64), '","format":"jats"},"processing":{},"metadata":{}}'), f)
  expect_error(sc_read_json(f), "lacks the entity arrays")
  d <- sc_parse(fixture("trial_article.xml"))
  bad <- d
  bad$blocks$section_id[2] <- "s-999"
  expect_error(sc_read_json(sc_export_json(bad, tempfile(fileext = ".json"))), "Unresolved reference")
  cyc <- d
  cyc$sections$parent_id[1] <- cyc$sections$section_id[2]
  cyc$sections$parent_id[2] <- cyc$sections$section_id[1]
  expect_error(sc_read_json(sc_export_json(cyc, tempfile(fileext = ".json"))), "Cycle")
  dup <- d
  dup$blocks$block_id[2] <- dup$blocks$block_id[1]
  expect_error(sc_read_json(sc_export_json(dup, tempfile(fileext = ".json"))), "Duplicate identifier")
  expect_s3_class(sc_read_json(sc_export_json(d, tempfile(fileext = ".json"))), "sc_document")
})

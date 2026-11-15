#' Read GROBID TEI output for comparison
#'
#' GROBID is an optional external tool: sciparseR never calls it. If you
#' process a PDF with GROBID yourself, `sc_read_grobid()` reads its TEI XML
#' (title, DOI, abstract, section headings and bibliography) and
#' `sc_compare_grobid()` compares it with a document parsed by sciparseR,
#' which is useful for spotting missed sections or references.
#'
#' @param tei Path to a GROBID TEI XML file.
#' @param doc An `sc_document`.
#' @return `sc_read_grobid()` a list with `metadata`, `sections` and
#'   `references`; `sc_compare_grobid()` a data frame of agreement
#'   measures.
#' @examples
#' tei <- tempfile(fileext = ".tei.xml")
#' writeLines(c('<TEI xmlns="http://www.tei-c.org/ns/1.0"><teiHeader><fileDesc>',
#'   '<titleStmt><title level="a" type="main">A Trial</title></titleStmt>',
#'   '</fileDesc></teiHeader><text><body><div><head>Methods</head><p>x</p></div>',
#'   '</body></text></TEI>'), tei)
#' g <- sc_read_grobid(tei)
#' g$sections
#' @export
sc_read_grobid <- function(tei) {
  check_string(tei, "tei")
  x <- xml2::read_xml(tei, options = c("NOBLANKS", "NONET"))
  ns <- c(t = "http://www.tei-c.org/ns/1.0")
  txt <- function(xp, node = x) {
    n <- xml2::xml_find_first(node, xp, ns)
    if (inherits(n, "xml_missing")) NA_character_ else normalize_text(xml2::xml_text(n))
  }
  md <- list(title = txt(".//t:teiHeader//t:titleStmt/t:title"),
             doi = txt(".//t:teiHeader//t:idno[@type='DOI']"),
             abstract = txt(".//t:teiHeader//t:abstract"))
  heads <- xml2::xml_find_all(x, ".//t:text/t:body//t:div/t:head", ns)
  sections <- data.frame(number = xml2::xml_attr(heads, "n"), title = normalize_text(xml2::xml_text(heads)), stringsAsFactors = FALSE)
  bib <- xml2::xml_find_all(x, ".//t:listBibl/t:biblStruct", ns)
  refs <- data.frame(
    ref_id = xml2::xml_attr(bib, "id"),
    title = vapply(bib, function(b) txt(".//t:title[@level='a']", b), ""),
    journal = vapply(bib, function(b) txt(".//t:title[@level='j']", b), ""),
    year = vapply(bib, function(b) {
      d <- xml2::xml_find_first(b, ".//t:date[@type='published']", ns)
      w <- xml2::xml_attr(d, "when")
      if (is.na(w)) NA_character_ else substr(w, 1, 4)
    }, ""),
    doi = vapply(bib, function(b) txt(".//t:idno[@type='DOI']", b), ""),
    pmid = vapply(bib, function(b) txt(".//t:idno[@type='PMID']", b), ""),
    stringsAsFactors = FALSE)
  list(metadata = md, sections = sections, references = refs)
}

#' @rdname sc_read_grobid
#' @export
sc_compare_grobid <- function(doc, tei) {
  check_document(doc)
  g <- if (is.list(tei) && !is.null(tei$sections)) tei else sc_read_grobid(tei)
  ns <- function(z) norm_cmp(sub("^[0-9.]+\\s+", "", z))
  a <- ns(doc$sections$title)
  b <- ns(g$sections$title)
  d1 <- tolower(doc$references$doi[!is.na(doc$references$doi)])
  d2 <- tolower(g$references$doi[!is.na(g$references$doi)])
  data.frame(
    measure = c("title_match", "sections_sciparseR", "sections_grobid", "sections_shared",
                "references_sciparseR", "references_grobid", "dois_shared"),
    value = c(identical(norm_cmp(doc$metadata$title %|NA|% ""), norm_cmp(g$metadata$title %|NA|% "")),
              length(a), length(b), length(intersect(a, b)),
              nrow(doc$references), nrow(g$references), length(intersect(d1, d2))),
    stringsAsFactors = FALSE)
}

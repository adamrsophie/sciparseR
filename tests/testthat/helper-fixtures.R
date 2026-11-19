fixture <- function(name) system.file("extdata", name, package = "sciparseR", mustWork = TRUE)
truth <- function(name) sub("\\.(pdf|xml)$", ".truth.json", fixture(name))
metric <- function(ev, m) ev$value[ev$metric == m]

jats_text <- function(text, methods = "Patients were randomly assigned to receive Drug A or placebo.", extra = "") {
  f <- tempfile(fileext = ".xml")
  writeLines(paste0("<article><body><sec><title>Methods</title><p>", methods, "</p></sec>",
                    "<sec><title>Results</title>", paste0("<p>", text, "</p>", collapse = ""), extra,
                    "</sec></body></article>"), f)
  sc_parse(f)
}
result_records <- function(text) {
  r <- sc_extract(jats_text(text), "numerical_results")$records
  r[r$field == "result", , drop = FALSE]
}

layout_fixture <- function(name) test_path("fixtures", name)
jats_table <- function(header, rows, caption = "Outcomes") {
  paste0('<table-wrap id="t1"><label>Table 2</label><caption><p>', caption, '</p></caption><table><thead><tr>',
         paste0("<th>", header, "</th>", collapse = ""), "</tr></thead><tbody>",
         paste0(vapply(rows, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>"), ""), collapse = ""),
         "</tbody></table></table-wrap>")
}

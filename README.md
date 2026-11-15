# sciparseR <img src="man/figures/logo.png" align="right" height="139" alt="sciparseR logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/adamrsophie/sciparseR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/adamrsophie/sciparseR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/adamrsophie/sciparseR/actions/workflows/pkgdown.yaml/badge.svg)](https://adamrsophie.github.io/sciparseR/)
<!-- badges: end -->

sciparseR turns scientific PDF files and JATS XML into a structured,
versioned document model in which every element keeps its source: page,
coordinates or XPath, the method that found it and its status. On top of
the document model, a schema-driven extraction layer pulls out study
design, PICO elements and numerical results, binds each number to its
arm, outcome, time point, analysis set and effect measure, and stores the
exact supporting text for every value. Deterministic validators then
check the numbers and flag contradictions between text and tables.

The package is native first: the core needs only pdftools and xml2, runs
offline and is deterministic. OCR (tesseract), language models (any
[ellmer](https://ellmer.tidyverse.org) provider, or a ChatGPT plan
through [ellmercodex](https://github.com/simxnherrera/ellmercodex)) and
ONNX models are optional, used only when you ask for them, and never
make a value look more certain than its evidence.

## Installation

```r
# install.packages("pak")
pak::pak("adamrsophie/sciparseR")
```

## Parse a document

```r
library(sciparseR)

doc <- sc_parse(system.file("extdata", "trial_tables.pdf", package = "sciparseR"))
doc
#> <sc_document> doc-acfa0be8d4838c2d
#>   source:   trial_tables.pdf (pdf, sha256 acfa0be8d483...)
#>   pages:    1
#>   blocks:   17   sections: 2   tables: 2   figures: 0
#>   ...

sc_sections(doc)[, c("level", "title", "label")]
sc_table_to_dataframe(doc, "Table 2")
sc_table_cells(doc, "Table 1")[, c("row", "col", "col_span", "is_header", "text", "markers")]
```

JATS XML is a first class source:

```r
j <- sc_parse(system.file("extdata", "trial_article.xml", package = "sciparseR"))
sc_equations(j)[, c("label", "latex", "latex_status")]
sc_citations(j)
```

## Extract, validate, review

```r
x <- sc_extract(doc, "clinical_trial")
x <- sc_validate(x)

sc_observations(x, wide = TRUE)[, c("outcome", "arm", "comparator", "timepoint",
                                    "effect_measure", "estimate", "ci_lower", "ci_upper")]
sc_evidence(x)[1:3, c("field", "value", "page", "text", "char_start", "char_end")]
sc_review_queue(x)
```

Every record has a status (`reported`, `missing`, `not_reported`,
`ambiguous`, `unsupported`) and a data origin (`reported`, `derived`,
`model_inferred`, `human_corrected`). A value after a plus-minus sign
without a stated SD or SE stays `ambiguous`; an empty table cell is
`not_reported`; a dash is `missing`. Corrections (`sc_correct()`) and
derivations (`sc_derive()`) add records and never overwrite the source.

## Language models

```r
eng <- sc_llm_engine("anthropic", model = "claude-sonnet-5-5")   # ANTHROPIC_API_KEY
eng <- sc_llm_engine("openai", model = "gpt-6")                   # OPENAI_API_KEY
eng <- sc_llm_engine("openrouter", model = "anthropic/claude-opus-5-5")
eng <- sc_llm_engine("chatgpt", model = "gpt-6-luna")             # after ellmercodex::codex_login()
sc_llm_providers()                                                # everything ellmer supports

y <- sc_extract(doc, "pico", engine = "llm", llm = eng)
```

Only text passages are sent. Each answer must quote the passage it came
from; values whose quote cannot be found in the cited block are kept with
status `unsupported`.

## Export

```r
sc_export_json(doc, "trial.json")        # complete model, JSON Schema in inst/schema
sc_export_markdown(doc, "trial.md")
sc_export_jats(doc, "trial.xml")
sc_export_parquet(x, "extraction/")
sc_export_evidence(x, "evidence.csv")
```

## What it does not do

sciparseR does not reconstruct LaTeX from PDF text, digitize figures or
Kaplan-Meier curves, or run learned layout and table models in this
version. When a value cannot be recovered with certainty it says so in
the status column instead of guessing. See the
[articles](https://adamrsophie.github.io/sciparseR/articles/) for the
document model, tables, extraction, validation, language models, OCR
and batch processing.

## License

GPL-3. See [ECOSYSTEM notes](https://adamrsophie.github.io/sciparseR/articles/ecosystem.html)
for the licenses of the optional tools sciparseR works with.

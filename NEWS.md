# sciparseR 0.1.0

First release.

## Document model

* `sc_parse()` turns born-digital PDF files (through pdftools) and JATS XML
  (through xml2) into a versioned `sc_document` with tidy tables of pages,
  spans, lines, blocks, sections, tables, cells, figures, equations,
  captions, footnotes, references, citations, supplementary material and
  warnings. Every element has a stable identifier, its page, coordinates
  (PDF points, top-left origin of the displayed page) or XPath, and the
  method and status of its extraction. Documents record the source SHA-256,
  package, backend and configuration versions.
* PDF layout: line segmentation, superscript detection, recursive XY-cut
  reading order with gutter detection, repeated header, footer and page
  number detection, conservative hyphenation repair with a document
  vocabulary, paragraph continuation across pages, detection of 90, 180
  and 270 degree page rotation and image-only page detection.
  `sc_to_pdf_space()` converts boxes back to PDF user space.
* PDF tables: caption anchored and uncaptioned detection, column intervals,
  rule based spanning headers, header row and column spans, header rows, wrapped labels, footnote
  markers, missing value markers, table footnotes and conservative
  continuation joining (`joined` versus `possible`).
* Figures from captions and rendered ink, equations with labels, Vancouver
  style reference parsing with DOI, PMID and PMCID, and numeric,
  superscript and author year citation linking.
* JATS: metadata, sections, paragraphs, tables with row and column spans,
  figures, display formulas with TeX and MathML, references, cross
  references, table and document footnotes and supplementary material;
  named entities are kept as characters. `sc_fetch_pmc()`
  downloads open access articles and only with `allow_network = TRUE`.
* Optional OCR of image-only pages with tesseract, with per page status and
  word confidence. OCR words go through the same line, block, section,
  table and figure steps as a text layer; elements built from them say so
  in their method or status.

## Extraction

* Schemas (`sc_schema()`, `sc_field()`) with built-in clinical trial,
  PICO, study design, trial details and numerical results schemas.
* Deterministic rules engine for design, design features, population,
  eligibility, arms, doses, allocation ratio, randomized, analyzed and per
  arm sample sizes, follow-up, assessment times, analysis populations and
  outcomes.
* Numerical results from text and tables as atomic records grouped into
  observations, bound to outcome, arm, comparator, time point, analysis
  set, population, effect measure and adjustment, with a `binding` column naming the
  source of each binding. Unlabeled dispersions are `ambiguous`; empty and
  dashed cells are `not_reported` and `missing`. Abbreviated measures
  that the document never defines are `ambiguous`.
* Evidence for every value: checksum, page, section, block or cell, exact
  text, character offsets, bounding box and XPath.
* Language model engine through ellmer: every ellmer provider
  (Anthropic, OpenAI, OpenRouter, Google Gemini and Vertex, Azure OpenAI,
  AWS Bedrock, Ollama, Mistral, Groq, DeepSeek and others) and ChatGPT
  plans through ellmercodex, with quote verification, `model_inferred`
  data origin, prompt hashes and token and cost accounting. Tables are
  sent as cell passages; numbers, categories and context are checked
  against the quote and passage.
* `sc_validate()` with count, confidence interval, ratio, p-value, p and
  CI agreement, binding, dispersion, sample size, time point and text
  versus table conflict rules; values failing an error rule become
  `inconsistent` and are not used for derivation; justified exceptions; `sc_review_queue()`,
  `sc_correct()` and `sc_derive()`.

## Output and tooling

* Exports to JSON (with JSON Schema and a reader), Markdown, CSV,
  Parquet, JATS, evidence tables and per table CSV files.
* `sc_parse_batch()` with a content addressed cache and a `keep = FALSE`
  mode for large corpora (`sc_batch_document()` reads documents back),
  `sc_evaluate()` against ground truth (block recall, character error
  rate, reading order, sections, table detection, cells, spans, GriTS-Con
  and TEDS), `sc_evaluate_extraction()` for values and their bindings,
  `sc_benchmark()` for time and memory, figure crops and evidence highlighting, GROBID TEI
  comparison and a checksum verified ONNX model registry (no parser step
  uses ONNX models yet).
* `sc_derive()` computes the SD from an SE only with the number analyzed
  that the document reports with the result, or one you pass; the
  randomized arm size is used only with `allow_randomized_n = TRUE` and
  is noted in the record.

## Accuracy safeguards

* Each value keeps its interval type (confidence, credible, prediction),
  p-value operator, test role (effect, baseline comparison, interaction,
  trend, heterogeneity) and a `source_quality` flag for values read from
  OCR, low confidence OCR or a table whose structure is uncertain. Dose
  arms of one drug stay apart, and the comparison direction follows the
  text or a column header; without either, an active arm against one
  control is read as active versus control and labelled as a convention.
* Negated design terms ("not randomized") are skipped; derivations record
  their inputs and assumptions, ignore inputs that failed validation and
  never use credible or prediction intervals; values that disagree only
  by rounding are not conflicts.
* Language model answers are accepted only when the number plays a role
  in the quote that fits the field (the 95 of "95% CI" is a level), the
  value fits the field type, a table value sits in the named row and
  column, and the measure, interval level, unit, population and
  adjustment agree with the passage. When an answer cannot be read,
  absent fields are `not_assessed`.
* `sc_evaluate_extraction()` scores accepted output (`reported`, not
  invalidated) and reports `candidate_` metrics for all values; every
  supplied context field is scored and an empty context never matches.
  `sc_evaluate()` matches blocks one to one, uses area IoU for full boxes
  (`_vertical` metrics for truth with only a vertical extent), and
  computes TEDS and GriTS-Con as the PubTabNet and Table Transformer
  reference implementations do.
* PDF rotation is read from the page tree without rendering, pages whose
  only text is a page number are queued for OCR, side by side tables and
  figures keep their own captions, and JATS keeps structured abstracts,
  floats groups and appendices. The batch cache key includes the native
  library and OCR data versions.

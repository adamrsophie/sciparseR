## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local macOS, R release
* GitHub Actions: macOS (release), Windows (release), Ubuntu (release,
  with the PDF manual), Ubuntu (devel)

## Notes for the reviewers

* Tests and examples need no network access and no credentials. Optional
  features (tesseract OCR, language models through ellmer, ONNX models,
  Parquet export) are in Suggests and are skipped when not installed.
* `sc_fetch_pmc()` downloads from PubMed Central only when called with
  `allow_network = TRUE`; its example is not run.
* Examples and vignettes write only to `tempdir()`.

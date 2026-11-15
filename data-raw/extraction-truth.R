# Extraction ground truth for trial_tables.pdf: the values the fixture
# generator writes into the text and tables, with the context a careful
# reader would bind to each. Fields left out are not scored.

trial_extraction_truth <- function() {
  r <- function(field, statistic, value, source, ...) {
    c(list(field = field, statistic = statistic, value = value, source = source, page = 1L), list(...))
  }
  rows <- list(
    r("randomized_n", "count", 238, "text"),
    r("analyzed_n", "count", 230, "text"),
    r("arm_n", "count", 120, "text", arm = "Drug A"),
    r("arm_n", "count", 118, "text", arm = "placebo"),
    # text results
    r("result", "events", 24, "text", outcome = "primary outcome", arm = "Drug A"),
    r("result", "denominator", 120, "text", outcome = "primary outcome", arm = "Drug A"),
    r("result", "percent", 20.0, "text", outcome = "primary outcome", arm = "Drug A"),
    r("result", "events", 36, "text", outcome = "primary outcome", arm = "placebo"),
    r("result", "denominator", 118, "text", outcome = "primary outcome", arm = "placebo"),
    r("result", "percent", 30.5, "text", outcome = "primary outcome", arm = "placebo"),
    r("result", "estimate", 0.62, "text", outcome = "primary outcome", arm = "Drug A", comparator = "placebo",
      effect_measure = "hazard_ratio"),
    r("result", "ci_lower", 0.41, "text", outcome = "primary outcome", arm = "Drug A", comparator = "placebo"),
    r("result", "ci_upper", 0.94, "text", outcome = "primary outcome", arm = "Drug A", comparator = "placebo"),
    r("result", "p_value", 0.02, "text", outcome = "primary outcome", arm = "Drug A", comparator = "placebo"),
    r("result", "estimate", 0.60, "text", analysis_set = "per_protocol", effect_measure = "hazard_ratio"),
    r("result", "ci_lower", 0.39, "text", analysis_set = "per_protocol"),
    r("result", "ci_upper", 0.93, "text", analysis_set = "per_protocol"),
    r("result", "mean", 8.1, "text", outcome = "systolic blood pressure", arm = "Drug A"),
    r("result", "dispersion", 13.2, "text", outcome = "systolic blood pressure", arm = "Drug A"),
    # Table 1
    r("baseline", "mean", 61.2, "table", outcome = "age", arm = "Drug A"),
    r("baseline", "sd", 9.8, "table", outcome = "age", arm = "Drug A"),
    r("baseline", "mean", 60.7, "table", outcome = "age", arm = "placebo"),
    r("baseline", "sd", 10.1, "table", outcome = "age", arm = "placebo"),
    r("baseline", "p_value", 0.70, "table", outcome = "age"),
    r("baseline", "count", 54, "table", outcome = "female sex", arm = "Drug A"),
    r("baseline", "percent", 45.0, "table", outcome = "female sex", arm = "Drug A"),
    r("baseline", "count", 50, "table", outcome = "female sex", arm = "placebo"),
    r("baseline", "percent", 42.4, "table", outcome = "female sex", arm = "placebo"),
    r("baseline", "p_value", 0.69, "table", outcome = "female sex"),
    r("baseline", "mean", 27.1, "table", outcome = "bmi", arm = "Drug A"),
    r("baseline", "sd", 4.2, "table", outcome = "bmi", arm = "Drug A"),
    r("baseline", "count", 30, "table", outcome = "diabetes", arm = "Drug A"),
    r("baseline", "percent", 25.0, "table", outcome = "diabetes", arm = "Drug A"),
    r("baseline", "count", 28, "table", outcome = "diabetes", arm = "placebo"),
    r("baseline", "percent", 23.7, "table", outcome = "diabetes", arm = "placebo"),
    r("baseline", "p_value", 0.82, "table", outcome = "diabetes")
  )
  # Table 2: 12 months, intention-to-treat population
  t2 <- function(statistic, value, outcome, ...) {
    r("result", statistic, value, "table", outcome = outcome, timepoint = "12 months",
      analysis_set = "intention_to_treat", ...)
  }
  for (o in list(list("primary outcome", 24, 120, 20.0, 36, 118, 30.5, 0.62, 0.41, 0.94, 0.02),
                 list("all-cause death", 5, 120, 4.2, 7, 118, 5.9, 0.71, 0.23, 2.24, 0.56))) {
    rows <- c(rows, list(
      t2("events", o[[2]], o[[1]], arm = "Drug A"), t2("denominator", o[[3]], o[[1]], arm = "Drug A"),
      t2("percent", o[[4]], o[[1]], arm = "Drug A"), t2("events", o[[5]], o[[1]], arm = "placebo"),
      t2("denominator", o[[6]], o[[1]], arm = "placebo"), t2("percent", o[[7]], o[[1]], arm = "placebo"),
      t2("estimate", o[[8]], o[[1]], arm = "Drug A", comparator = "placebo", effect_measure = "hazard_ratio"),
      t2("ci_lower", o[[9]], o[[1]], arm = "Drug A", comparator = "placebo"),
      t2("ci_upper", o[[10]], o[[1]], arm = "Drug A", comparator = "placebo"),
      t2("p_value", o[[11]], o[[1]])))
  }
  c(rows, list(
    t2("mean", -8.1, "change in sbp", arm = "Drug A"), t2("se", 1.2, "change in sbp", arm = "Drug A"),
    t2("mean", -3.4, "change in sbp", arm = "placebo"), t2("se", 1.1, "change in sbp", arm = "placebo"),
    t2("p_value", 0.004, "change in sbp")))
}

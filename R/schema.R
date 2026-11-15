#' Extraction schemas
#'
#' A schema is a named list of fields to extract. Built-in schemas cover
#' study design, PICO, trial details and numerical results of clinical
#' studies; user schemas are built from [sc_field()] definitions and work
#' with the rules engine (through a field `pattern`) and the language model
#' engine (through the field description and vocabulary).
#'
#' Built-in schemas:
#' * `"study_design"`: design class and design features.
#' * `"pico"`: population, eligibility, intervention, comparator, outcomes,
#'   design.
#' * `"trial_details"`: arms, doses, allocation ratio, randomized, analyzed
#'   and per-arm sample sizes, follow-up, assessment times, analysis
#'   population, primary and secondary outcomes.
#' * `"numerical_results"`: arm-level and comparative statistics from text
#'   and tables (means, SDs, SEs, medians, quartiles, counts, denominators,
#'   percentages, ratio and difference measures, confidence intervals,
#'   p-values) and baseline characteristics.
#' * `"clinical_trial"`: all of the above.
#'
#' @param name Name of a built-in schema, or the name of a new schema when
#'   `fields` is given.
#' @param fields For a user schema, a list of [sc_field()] objects.
#' @param version Schema version string.
#' @return An object of class `sc_schema`.
#' @examples
#' sc_schema("pico")
#' my <- sc_schema("registration", fields = list(
#'   sc_field("registry_id", "Trial registration number",
#'            pattern = "\\b(NCT[0-9]{8})\\b")
#' ))
#' my
#' @export
sc_schema <- function(name = c("clinical_trial", "pico", "study_design",
                               "trial_details", "numerical_results"),
                      fields = NULL, version = "1.0.0") {
  if (!is.null(fields)) {
    check_string(name[1], "name")
    if (!is.list(fields) || !length(fields) || !all(vapply(fields, inherits, logical(1), "sc_field"))) {
      abort("`fields` must be a non-empty list of sc_field() objects.")
    }
    nms <- vapply(fields, function(f) f$name, character(1))
    if (anyDuplicated(nms)) abort("Field names must be unique.")
    names(fields) <- nms
    return(structure(list(name = name[1], version = version, builtin = FALSE,
                          fields = fields), class = "sc_schema"))
  }
  name <- match.arg(name)
  structure(list(name = name, version = version, builtin = TRUE,
                 fields = builtin_fields(name)), class = "sc_schema")
}

#' Define an extraction field
#'
#' @param name Field name (letters, digits and underscores).
#' @param description Plain language description, used in language model
#'   prompts and documentation.
#' @param type Value type: `"character"`, `"numeric"`, `"integer"`,
#'   `"categorical"` or `"logical"`.
#' @param vocabulary For categorical fields, a named character vector of
#'   regular expressions: names are the controlled terms, values the
#'   patterns that map text to them (or an unnamed vector of terms).
#' @param required If `TRUE`, a missing value is recorded explicitly with
#'   status `"not_reported"`.
#' @param multiple Whether several values are allowed.
#' @param pattern Optional regular expression (Perl) with one capture group
#'   used by the rules engine for user fields.
#' @param sections Optional canonical section labels to search (for example
#'   `c("abstract", "methods")`); `NULL` searches all body text.
#' @param unit Optional unit of the value.
#' @return An object of class `sc_field`.
#' @examples
#' sc_field("sample_size", "Number of participants", type = "integer",
#'          pattern = "([0-9]+) participants were enrolled")
#' @export
sc_field <- function(name, description = "", type = c("character", "numeric", "integer", "categorical", "logical"),
                     vocabulary = NULL, required = FALSE, multiple = FALSE,
                     pattern = NULL, sections = NULL, unit = NULL) {
  check_string(name, "name")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) abort("Field names use letters, digits and underscores.")
  type <- match.arg(type)
  if (type == "categorical" && is.null(vocabulary)) abort("Categorical fields need a `vocabulary`.")
  if (!is.null(pattern)) {
    ok <- tryCatch({
      regexpr(pattern, "", perl = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok) abort("`pattern` is not a valid regular expression.")
  }
  if (!is.null(vocabulary) && is.null(names(vocabulary))) {
    vocabulary <- stats::setNames(paste0("\\b", vocabulary, "\\b"), vocabulary)
  }
  structure(list(name = name, description = description, type = type,
                 vocabulary = vocabulary, required = required, multiple = multiple,
                 pattern = pattern, sections = sections, unit = unit),
            class = "sc_field")
}

#' @export
print.sc_schema <- function(x, ...) {
  cat("<sc_schema> ", x$name, " ", x$version, if (x$builtin) " (built-in)", "\n", sep = "")
  for (f in x$fields) {
    cat("  ", formatC(f$name, width = -24), " ", f$type,
        if (f$required) " required", if (f$multiple) " multiple", "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.sc_field <- function(x, ...) {
  cat("<sc_field> ", x$name, " (", x$type, ")", if (nzchar(x$description)) paste0(": ", x$description), "\n", sep = "")
  invisible(x)
}

design_vocabulary <- c(
  meta_analysis = "\\bmeta-?analys[ie]s\\b|\\bmeta-?analytic",
  systematic_review = "\\bsystematic (literature )?review\\b",
  randomized_controlled_trial = "\\brandomi[sz]ed\\b[^.]{0,60}\\btrial\\b|\\brandomly (assigned|allocated)\\b|\\bRCT\\b|\\brandomi[sz]ed controlled\\b",
  case_control = "\\bcase[- ]control\\b",
  cohort = "\\bcohort (study|studies|design)\\b|\\b(prospective|retrospective) cohort\\b",
  cross_sectional = "\\bcross[- ]sectional\\b"
)

feature_vocabulary <- c(
  double_blind = "\\bdouble[- ](blind|masked)",
  single_blind = "\\bsingle[- ](blind|masked)",
  open_label = "\\bopen[- ]label\\b",
  placebo_controlled = "\\bplacebo[- ]controlled\\b",
  multicentre = "\\bmulti-?cent(re|er)\\b",
  crossover = "\\bcross-?over (trial|design)\\b",
  cluster_randomized = "\\bcluster[- ]randomi[sz]ed\\b",
  non_inferiority = "\\bnon-?inferiority\\b",
  parallel_group = "\\bparallel[- ]group\\b"
)

analysis_set_vocabulary <- c(
  modified_intention_to_treat = "\\bmodified intention[- ]to[- ]treat\\b|\\bmITT\\b",
  intention_to_treat = "\\bintention[- ]to[- ]treat\\b|\\bintent[- ]to[- ]treat\\b|\\bITT\\b",
  per_protocol = "\\bper[- ]protocol\\b",
  safety = "\\bsafety (population|set|analysis)\\b",
  as_treated = "\\bas[- ]treated\\b"
)

builtin_fields <- function(name) {
  f <- list(
    study_design = sc_field("study_design", "Overall study design", "categorical", design_vocabulary, required = TRUE),
    design_features = sc_field("design_features", "Design features such as blinding and control type", "categorical", feature_vocabulary, multiple = TRUE),
    population = sc_field("population", "Population studied (who was eligible)", required = TRUE),
    eligibility_inclusion = sc_field("eligibility_inclusion", "Inclusion criteria", multiple = TRUE),
    eligibility_exclusion = sc_field("eligibility_exclusion", "Exclusion criteria", multiple = TRUE),
    intervention = sc_field("intervention", "Experimental intervention", required = TRUE),
    comparator = sc_field("comparator", "Comparator or control", required = TRUE),
    arms = sc_field("arms", "Trial arms", multiple = TRUE),
    dose = sc_field("dose", "Intervention dose and schedule", multiple = TRUE),
    allocation_ratio = sc_field("allocation_ratio", "Allocation ratio"),
    randomized_n = sc_field("randomized_n", "Number of participants randomized", "integer", required = TRUE),
    analyzed_n = sc_field("analyzed_n", "Number of participants analyzed", "integer"),
    arm_n = sc_field("arm_n", "Number of participants per arm", "integer", multiple = TRUE),
    follow_up = sc_field("follow_up", "Follow-up duration", "numeric"),
    assessment_times = sc_field("assessment_times", "Outcome assessment times", multiple = TRUE),
    analysis_population = sc_field("analysis_population", "Analysis population", "categorical", analysis_set_vocabulary, multiple = TRUE),
    primary_outcome = sc_field("primary_outcome", "Primary outcome", required = TRUE, multiple = TRUE),
    secondary_outcomes = sc_field("secondary_outcomes", "Secondary outcomes", multiple = TRUE),
    result = sc_field("result", "Arm level or comparative numerical result", "numeric", multiple = TRUE),
    baseline = sc_field("baseline", "Baseline characteristic", "numeric", multiple = TRUE)
  )
  sets <- list(
    study_design = c("study_design", "design_features"),
    pico = c("population", "eligibility_inclusion", "eligibility_exclusion", "intervention",
             "comparator", "primary_outcome", "secondary_outcomes", "study_design"),
    trial_details = c("arms", "dose", "allocation_ratio", "randomized_n", "analyzed_n", "arm_n",
                      "follow_up", "assessment_times", "analysis_population",
                      "primary_outcome", "secondary_outcomes", "intervention", "comparator"),
    numerical_results = c("result", "baseline"),
    clinical_trial = names(f)
  )
  f[unique(sets[[name]])]
}

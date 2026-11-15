# Generates the synthetic fixtures in inst/extdata together with ground
# truth JSON files. Run from the package root:
#   Rscript data-raw/make-fixtures.R
# Requires grDevices with cairo, jsonlite, png and qpdf. The PDFs are
# committed, so tests never regenerate them (cairo output depends on the
# platform fonts).

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

W <- 612
H <- 792

# ---- tiny layout engine -------------------------------------------------

truth <- NULL
cur_page <- 0L

start_doc <- function(file) {
  grDevices::cairo_pdf(file, width = W / 72, height = H / 72,
                       pointsize = 10, onefile = TRUE, family = "Helvetica")
  truth <<- list(blocks = list(), sections = list(), tables = list(),
                 figures = list(), equations = list(), references = list())
  cur_page <<- 0L
}

new_page <- function() {
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  cur_page <<- cur_page + 1L
}

put <- function(x, y, s, size = 10, font = 1, adj = 0) {
  graphics::text(x, H - y, s, adj = c(adj, 1), cex = size / 10, font = font)
  invisible(text_width(s, size, font))
}

text_width <- function(s, size = 10, font = 1) {
  graphics::strwidth(s, units = "user", cex = size / 10, font = font)
}

add_block <- function(type, text, section = NA_character_, page = cur_page) {
  truth$blocks[[length(truth$blocks) + 1L]] <<- list(
    page = page, type = type, text = text, section = section)
}

# Wrap words into lines no wider than `width`. A token of the form
# "left-|right" is always split at the bar, the hyphen ending the line, so
# that hyphenation repair can be tested.
wrap <- function(text, width, size = 10, font = 1) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character()
  cur <- ""
  for (w in words) {
    plain <- gsub("|", "", w, fixed = TRUE)
    cand <- if (nzchar(cur)) paste(cur, plain) else plain
    if (grepl("|", w, fixed = TRUE)) {
      parts <- strsplit(w, "|", fixed = TRUE)[[1]]
      first <- if (nzchar(cur)) paste(cur, parts[1]) else parts[1]
      lines <- c(lines, first)
      cur <- parts[2]
    } else if (text_width(cand, size, font) <= width) {
      cur <- cand
    } else {
      lines <- c(lines, cur)
      cur <- plain
    }
  }
  c(lines, cur)
}

# Draw a paragraph; returns the y after the paragraph. Records the clean
# text (hyphen bars removed) as a truth block.
para <- function(x, y, width, text, size = 10, leading = 1.25, font = 1,
                 type = "paragraph", section = NA_character_, record = TRUE,
                 max_y = Inf) {
  lines <- wrap(text, width, size, font)
  step <- size * leading
  drawn <- 0L
  for (ln in lines) {
    if (y + step > max_y) break
    put(x, y, ln, size, font)
    y <- y + step
    drawn <- drawn + 1L
  }
  if (record) add_block(type, gsub("-|", "", text, fixed = TRUE), section)
  attr(y, "rest") <- if (drawn < length(lines)) lines[-seq_len(drawn)] else character()
  y
}

heading <- function(x, y, s, size = 12, level = 1L, font = 2) {
  put(x, y, s, size, font)
  add_block("heading", s, s)
  truth$sections[[length(truth$sections) + 1L]] <<- list(title = s, level = level,
                                                       page = cur_page)
  y + size * 1.8
}

hrule <- function(x0, x1, y, lwd = 0.8) {
  graphics::segments(x0, H - y, x1, H - y, lwd = lwd)
}

header_footer <- function(header, page_no) {
  put(72, 36, header, 8)
  put(W / 2, H - 40, as.character(page_no), 8, adj = 0.5)
}

# Draw a table from a character matrix. `xs` are left x positions of the
# columns, `y` the top. Returns the y after the table. `spans` is a list of
# list(row, col, ncol, text) header cells spanning several columns.
draw_table <- function(cells, xs, y, size = 9, header_rows = 1L,
                       rules = TRUE, spans = list(), x_end = 540,
                       label = NULL, page = cur_page, continued_of = NULL) {
  step <- size * 1.6
  top <- y
  if (rules) hrule(xs[1], x_end, y - 3, 0.9)
  for (r in seq_len(nrow(cells))) {
    sp <- Filter(function(s) s$row == r, spans)
    for (s in sp) {
      sx1 <- if (s$col + s$ncol <= length(xs)) xs[s$col + s$ncol] - 12 else x_end
      put((xs[s$col] + sx1) / 2, y, s$text, size, 1, adj = 0.5)
      if (rules) hrule(xs[s$col], sx1, y + size + 2, 0.5)
    }
    for (cc in seq_len(ncol(cells))) {
      txt <- cells[r, cc]
      if (!nzchar(txt) || any(vapply(sp, function(s) s$col == cc, logical(1)))) next
      if (grepl("\\^", txt)) {
        parts <- strsplit(txt, "^", fixed = TRUE)[[1]]
        wd <- put(xs[cc], y, parts[1], size, if (r <= header_rows) 2 else 1)
        put(xs[cc] + wd + 0.5, y - 2.5, parts[2], size * 0.65)
      } else {
        put(xs[cc], y, txt, size, if (r <= header_rows && !length(sp)) 2 else 1)
      }
    }
    y <- y + step
    if (rules && r == header_rows) hrule(xs[1], x_end, y - 4, 0.6)
  }
  if (rules) hrule(xs[1], x_end, y - 4, 0.9)
  clean <- gsub("^", "", cells, fixed = TRUE)
  # a header label with only empty header cells above it spans those rows
  # and is anchored at the top row, as in JATS
  if (header_rows > 1L) {
    for (cc in seq_len(ncol(clean))) {
      hr <- clean[seq_len(header_rows), cc]
      if (any(vapply(spans, function(s) cc >= s$col && cc < s$col + s$ncol, logical(1)))) next
      k <- which(nzchar(hr))
      if (length(k) == 1L && k > 1L && all(!nzchar(hr[seq_len(k - 1L)]))) {
        clean[1L, cc] <- hr[k]
        clean[k, cc] <- ""
        spans[[length(spans) + 1L]] <- list(row = 1L, col = cc, ncol = 1L, nrow = k, text = hr[k])
      }
    }
  }
  truth$tables[[length(truth$tables) + 1L]] <<- list(
    label = label, page = page, header_rows = header_rows,
    cells = unname(lapply(seq_len(nrow(clean)), function(i) unname(clean[i, ]))),
    spans = spans, continued_of = continued_of, top = top, bottom = y)
  y + 6
}

finish <- function(file) {
  grDevices::dev.off()
  json <- sub("\\.pdf$", ".truth.json", file)
  jsonlite::write_json(truth, json, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
}

# ---- 1. single column, two pages ---------------------------------------

f <- file.path(out_dir, "single_column.pdf")
start_doc(f)
new_page()
header_footer("Journal of Synthetic Medicine 2026; 12: 101-102", 101)
y <- 70
put(72, y, "Effect of Drug A on Blood Pressure: a Synthetic Example", 16, 2)
add_block("title", "Effect of Drug A on Blood Pressure: a Synthetic Example")
y <- y + 26
put(72, y, "Jane Doe, Omar Khan, Li Wei", 10)
add_block("authors", "Jane Doe, Omar Khan, Li Wei")
y <- y + 24
y <- heading(72, y, "Abstract", 11)
y <- para(72, y, 468, paste(
  "Background: Hypertension is common. Methods: We describe a synthetic",
  "example used to test document parsing. Results: Parsing recovered all",
  "sections. Conclusions: Synthetic documents support reproducible tests."),
  section = "Abstract") + 8
y <- heading(72, y, "1. Introduction", 12)
y <- para(72, y, 468, paste(
  "High blood pressure remains a leading cause of cardiovascular disease",
  "worldwide [1]. Several trials have tested new agents [2,3]. Most",
  "previous studies were not random-|ized and used small samples, so their",
  "estimates were imprecise. This synthetic report follows the usual",
  "structure of a clinical paper so that every element can be checked."),
  section = "1. Introduction") + 8
y <- heading(72, y, "2. Methods", 12)
y <- heading(72, y, "2.1 Participants", 10.5, level = 2L)
y <- para(72, y, 468, paste(
  "Adults aged 18 years or older with untreated hypertension were eligible.",
  "Participants were recruited from twelve clinics between 2020 and 2022.",
  "Each participant gave written informed consent before any study",
  "procedure, and the protocol was approved by an ethics committee."),
  section = "2.1 Participants") + 8
y <- heading(72, y, "2.2 Outcomes", 10.5, level = 2L)
long <- paste(
  "The primary outcome was the change in systolic blood pressure from",
  "baseline to week 12, measured in the seated position after five minutes",
  "of rest with a validated automated device. Secondary outcomes included",
  "diastolic blood pressure, heart rate and adverse events. All",
  "measurements were taken by staff unaware of the treatment assignment,",
  "and the device was calibrated every month according to the instructions",
  "of the manufacturer. The statistical analysis plan was finalized before",
  "the database was locked and is available from the authors on request.")
y2 <- para(72, y, 468, long, section = "2.2 Outcomes", max_y = y + 52)
rest <- attr(y2, "rest")
put(72, 712, "* Corresponding author: jane.doe@example.org", 7.5)
truth$footnotes <- list(list(page = 1L, text = "* Corresponding author: jane.doe@example.org"))
new_page()
header_footer("Journal of Synthetic Medicine 2026; 12: 101-102", 102)
y <- 70
for (ln in rest) {
  put(72, y, ln, 10)
  y <- y + 12.5
}
y <- y + 8
y <- heading(72, y, "3. Results", 12)
y <- para(72, y, 468, paste(
  "Of 240 screened adults, 200 were enrolled. The mean age was 58 years.",
  "Blood pressure fell in both groups during follow-up."),
  section = "3. Results") + 8
y <- heading(72, y, "4. Discussion", 12)
y <- para(72, y, 468, paste(
  "This synthetic example shows the structure that the parser must",
  "recover, including a paragraph that continues on the next page."),
  section = "4. Discussion") + 8
y <- heading(72, y, "References", 12)
refs <- c(
  "1. Smith J, Jones K. Global burden of hypertension. J Synth Med. 2020;12:100-110. doi:10.1000/jsm.2020.001",
  "2. Garcia M, Chen L, Patel R. A randomized trial of drug B. Synth Trials. 2021;3(2):45-52. PMID: 31234567",
  "3. Novak P. Long term follow-up of drug C. Open Synth J. 2022;7:e12. PMCID: PMC7654321")
for (r in refs) {
  y <- para(72, y, 468, r, size = 9, type = "reference", section = "References") + 4
}
truth$references <- as.list(refs)
finish(f)

# ---- 2. two column -----------------------------------------------------

two_col <- function(f, broken = FALSE) {
  start_doc(f)
  new_page()
  if (!broken) header_footer("Synthetic Methods Letters 4 (2026)", 7)
  left <- c(
    "Introduction",
    paste("Column detection is a classic problem in document analysis. A",
          "reader expects the left column to be read before the right one,",
          "even when lines in both columns share the same baseline."),
    "Methods",
    paste("We generated pages with two columns of equal width separated by",
          "a gutter of twenty points. Headings use a bold face so that a",
          "parser can detect them from font information alone."))
  right <- c(
    "Results",
    paste("The parser recovered the order of every block on the page. The",
          "right column begins only after the left column has ended, which",
          "matches the order intended by the author."),
    "Discussion",
    paste("Simple geometric rules are sufficient for clean two column",
          "layouts. Complex layouts with floats require further work."))
  draw_col <- function(x, items) {
    y <- 150
    for (i in seq_along(items)) {
      if (i %% 2 == 1) {
        put(x, y, items[i], 11, 2)
        y <- y + 18
      } else {
        y <- para(x, y, 224, items[i], record = FALSE) + 10
      }
    }
  }
  title <- "Reading Order in Two Column Layouts"
  abstract <- paste("Abstract: We test whether reading order is recovered for",
                    "a page with a full width title, a full width abstract and",
                    "two columns of body text.")
  if (broken) {
    # content stream order: right column, left column, then abstract, title
    draw_col(316, right)
    draw_col(72, left)
    para(72, 100, 468, abstract, record = FALSE)
    put(72, 70, title, 15, 2)
  } else {
    put(72, 70, title, 15, 2)
    para(72, 100, 468, abstract, record = FALSE)
    draw_col(72, left)
    draw_col(316, right)
  }
  add_block("title", title)
  add_block("paragraph", abstract)
  for (items in list(left, right)) {
    for (i in seq_along(items)) {
      if (i %% 2 == 1) {
        add_block("heading", items[i], items[i])
        truth$sections[[length(truth$sections) + 1L]] <<- list(title = items[i], level = 1L, page = 1L)
      } else {
        add_block("paragraph", items[i], items[i - 1])
      }
    }
  }
  if (!broken) {
    new_page()
    header_footer("Synthetic Methods Letters 4 (2026)", 8)
    y <- para(72, 80, 224, paste("A second page continues the left column with",
                                 "text that a reader expects next."),
              section = "Discussion")
    y <- para(316, 80, 224, paste("The right column of the second page",
                                  "closes the letter."), section = "Discussion")
  }
  finish(f)
}
two_col(file.path(out_dir, "two_column.pdf"))
two_col(file.path(out_dir, "broken_order.pdf"), broken = TRUE)

# ---- 3. trial with tables ----------------------------------------------

trial <- function(f, contradictory = FALSE) {
  start_doc(f)
  new_page()
  header_footer("Synthetic Trials Journal 2026", 1)
  y <- 66
  title <- if (contradictory) "Drug A Versus Placebo: a Report with Inconsistent Numbers" else
    "Drug A Versus Placebo in Hypertension: a Randomized Controlled Trial"
  put(72, y, title, 14, 2)
  add_block("title", title)
  y <- y + 28
  y <- heading(72, y, "Methods", 12)
  y <- para(72, y, 468, paste(
    "This was a multicentre, double-blind, randomized, placebo-controlled",
    "trial. Adults aged 18 years or older with hypertension were eligible.",
    "Exclusion criteria included pregnancy and severe renal disease. A total",
    "of 238 patients were randomly assigned in a 1:1 ratio to receive Drug A",
    "10 mg once daily (n = 120) or placebo (n = 118). Patients were followed",
    "for 12 months. The primary outcome was major cardiovascular events at 12",
    "months. Secondary outcomes included all-cause death and change in",
    "systolic blood pressure. Analyses followed the intention-to-treat",
    "principle."), section = "Methods") + 8
  y <- heading(72, y, "Results", 12)
  res <- if (contradictory) paste(
    "The primary outcome occurred in 24 of 120 patients (20.0%) in the Drug",
    "A group and 36 of 118 patients (30.5%) in the placebo group (hazard",
    "ratio 0.62, 95% CI 0.41 to 0.94; P = 0.02). Serious adverse events",
    "occurred in 130 of 120 patients in the Drug A group. The odds ratio for",
    "withdrawal was 1.8 (95% CI 2.1 to 3.0; P = 1.30).") else paste(
    "Of the 238 randomized patients, 230 were included in the analysis. The",
    "primary outcome occurred in 24 of 120 patients (20.0%) in the Drug A",
    "group and 36 of 118 patients (30.5%) in the placebo group (hazard ratio",
    "0.62, 95% CI 0.41 to 0.94; P = 0.02). In the per-protocol population,",
    "the adjusted hazard ratio was 0.60 (95% CI 0.39 to 0.93). Systolic blood",
    "pressure decreased by 8.1 ± 13.2 mm Hg with Drug A.")
  y <- para(72, y, 468, res, section = "Results") + 10

  put(72, y, "Table 1. Baseline characteristics of the participants", 9.5, 2)
  add_block("caption", "Table 1. Baseline characteristics of the participants", "Results")
  y <- y + 18
  t1 <- rbind(
    c("Characteristic", "Drug A (n = 120)", "Placebo (n = 118)", "P value"),
    c("Age, years, mean (SD)", "61.2 (9.8)", "60.7 (10.1)", "0.70"),
    c("Female sex, n (%)", "54 (45.0)", "50 (42.4)", "0.69"),
    c("BMI, kg/m2, mean (SD)", "27.1 (4.2)", "–", ""),
    c("Diabetes, n (%)^a", "30 (25.0)", "28 (23.7)", "0.82"))
  t1 <- rbind(c("", "Treatment group", "", ""), t1)
  y <- draw_table(t1, c(72, 250, 360, 470), y, header_rows = 2L,
                  spans = list(list(row = 1L, col = 2L, ncol = 2L, text = "Treatment group")),
                  label = "Table 1")
  put(72, y, "a", 6)
  put(77, y + 1, "Defined as HbA1c of 6.5% or higher.", 8)
  put(72, y + 11, "Abbreviations: SD, standard deviation; BMI, body mass index.", 8)
  truth$table_footnotes <- list("a Defined as HbA1c of 6.5% or higher.",
                                "Abbreviations: SD, standard deviation; BMI, body mass index.")
  y <- y + 34

  put(72, y, "Table 2. Outcomes at 12 months (intention-to-treat population)", 9.5, 2)
  add_block("caption", "Table 2. Outcomes at 12 months (intention-to-treat population)", "Results")
  y <- y + 18
  hr <- if (contradictory) "0.68 (0.45–1.02)" else "0.62 (0.41–0.94)"
  t2 <- rbind(
    c("Outcome", "Drug A", "Placebo", "HR (95% CI)", "P"),
    c("Primary outcome, n/N (%)", "24/120 (20.0)", "36/118 (30.5)", hr, "0.02"),
    c("All-cause death, n/N (%)", "5/120 (4.2)", "7/118 (5.9)", "0.71 (0.23–2.24)", "0.56"),
    c("Change in SBP, mm Hg, mean (SE)", "-8.1 (1.2)", "-3.4 (1.1)", "", "0.004"))
  y <- draw_table(t2, c(72, 240, 320, 400, 500), y, header_rows = 1L,
                  rules = FALSE, label = "Table 2")
  finish(f)
}
trial(file.path(out_dir, "trial_tables.pdf"))
trial(file.path(out_dir, "contradictory.pdf"), contradictory = TRUE)

# ---- 4. scanned page ---------------------------------------------------

f <- file.path(out_dir, "scanned.pdf")
img <- tempfile(fileext = ".png")
grDevices::png(img, width = 850, height = 300, res = 100)
graphics::par(mar = c(0, 0, 0, 0))
graphics::plot.new()
graphics::text(0.02, 0.8, "This page is an image of text.", adj = 0, cex = 2)
graphics::text(0.02, 0.5, "It has no text layer at all.", adj = 0, cex = 2)
grDevices::dev.off()
bitmap <- png::readPNG(img)
start_doc(f)
new_page()
y <- heading(72, 72, "Scanned Supplement", 12)
para(72, y, 468, "The next page was scanned and contains no extractable text.")
new_page()
graphics::rasterImage(bitmap, 72, H - 72 - 180, 72 + 510, H - 72)
truth$image_only_pages <- 2L
finish(f)

# ---- 5. rotated page ---------------------------------------------------

f0 <- tempfile(fileext = ".pdf")
start_doc(f0)
new_page()
y <- heading(72, 72, "Rotated Page", 12)
para(72, y, 468, paste("This page is stored upright and displayed rotated by",
                       "ninety degrees through the page rotation key."))
finish(f0)
f <- file.path(out_dir, "rotated.pdf")
qpdf::pdf_rotate_pages(f0, pages = 1, angle = 90, output = f)
file.copy(sub("\\.pdf$", ".truth.json", f0), sub("\\.pdf$", ".truth.json", f), overwrite = TRUE)

# ---- 6. unicode --------------------------------------------------------

f <- file.path(out_dir, "unicode.pdf")
start_doc(f)
new_page()
y <- heading(72, 72, "Unicode Characters", 12)
u <- c("Authors: José Müller, Åsa Østergaard and François Noël.",
       "Doses of 5 µg/kg were given at 37 °C; α = 0.05 and β ≤ 0.20.",
       "Mean change was −3.4 ± 1.2 mm Hg (95% CI −5.1 to −1.7).",
       "Greek: α β γ δ μ σ χ²; relations: ≤ ≥ ≠ ≈.")
for (s in u) y <- para(72, y, 468, s) + 4
finish(f)

# ---- 7. figure, equation, continued table ------------------------------

f <- file.path(out_dir, "figure_equation.pdf")
start_doc(f)
new_page()
header_footer("Synthetic Figures 2026", 1)
y <- heading(72, 70, "Statistical Model", 12)
y <- para(72, y, 468, paste("The outcome was modelled with a linear regression",
                            "of the following form, fitted by least squares."),
          section = "Statistical Model") + 10
eq <- "y = β0 + β1 x + ε"
put(W / 2, y, eq, 11, adj = 0.5)
put(520, y, "(1)", 10)
truth$equations <- list(list(page = 1L, text = eq, label = "(1)"))
y <- y + 30
# vector figure
fx0 <- 150; fx1 <- 460; fy0 <- y; fy1 <- y + 200
graphics::rect(fx0, H - fy1, fx1, H - fy0, border = "black", lwd = 0.8)
xx <- seq(0, 1, length.out = 30)
graphics::lines(fx0 + 10 + xx * 290, H - fy1 + 10 + 180 * exp(-2 * xx), lwd = 1.2)
graphics::lines(fx0 + 10 + xx * 290, H - fy1 + 10 + 180 * exp(-1 * xx), lwd = 1.2, lty = 2)
y <- fy1 + 12
cap <- "Figure 1. Survival curves by treatment group (synthetic data)."
put(72, y, cap, 9.5)
add_block("caption", cap, "Statistical Model")
truth$figures <- list(list(page = 1L, label = "Figure 1", box = c(fx0, fy0, fx1, fy1)))
y <- y + 30
put(72, y, "Table 3. Adverse events by system organ class", 9.5, 2)
add_block("caption", "Table 3. Adverse events by system organ class", "Statistical Model")
y <- y + 18
ae <- rbind(c("Event", "Drug A", "Placebo"),
            c("Headache", "12", "9"), c("Dizziness", "8", "5"),
            c("Nausea", "6", "6"), c("Fatigue", "5", "4"),
            c("Cough", "4", "3"), c("Rash", "3", "1"))
y <- draw_table(ae, c(72, 300, 420), y, header_rows = 1L, label = "Table 3")
new_page()
header_footer("Synthetic Figures 2026", 2)
put(72, 70, "Table 3 (continued). Adverse events by system organ class", 9.5, 2)
add_block("caption", "Table 3 (continued). Adverse events by system organ class", "Statistical Model")
ae2 <- rbind(c("Event", "Drug A", "Placebo"),
             c("Insomnia", "2", "2"), c("Edema", "2", "0"), c("Syncope", "1", "0"))
y <- draw_table(ae2, c(72, 300, 420), 88, header_rows = 1L, label = "Table 3",
                continued_of = "Table 3")
finish(f)

unlink(c(img, f0))
cat("fixtures written to", out_dir, "\n")

#!/usr/bin/env Rscript

# Render 06_main_question_example.Rmd to a PDF with executed code/plots.
#
# Usage:
#   Rscript scripts/render_main_question_pdf.R            # quick mode
#   Rscript scripts/render_main_question_pdf.R --deep     # larger run

args <- commandArgs(trailingOnly = TRUE)
deep <- "--deep" %in% args
quick_mode <- !deep

required_root <- c("DESCRIPTION", "vignettes", "scripts")
missing_root <- required_root[!file.exists(required_root)]
if (length(missing_root) > 0L) {
  stop(
    "Run this script from package root. Missing path(s): ",
    paste(missing_root, collapse = ", "),
    call. = FALSE
  )
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("This script requires the `rmarkdown` package.", call. = FALSE)
}

# Try common RStudio pandoc location on macOS if pandoc is not already found.
if (!rmarkdown::pandoc_available()) {
  pandoc_candidates <- c(
    "/Applications/RStudio.app/Contents/MacOS/pandoc",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64"
  )
  for (p in pandoc_candidates) {
    if (dir.exists(p)) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      if (rmarkdown::pandoc_available()) {
        break
      }
    }
  }
}

if (!rmarkdown::pandoc_available()) {
  manual_pandoc <- Sys.which("pandoc")
  if (nzchar(manual_pandoc)) {
    Sys.setenv(RSTUDIO_PANDOC = dirname(manual_pandoc))
  }
}

if (!rmarkdown::pandoc_available()) {
  stop(
    "Pandoc not found. Run from RStudio (which bundles pandoc), or set `RSTUDIO_PANDOC`.\n",
    "Example:\n",
    "  Sys.setenv(RSTUDIO_PANDOC = \"/Applications/RStudio.app/Contents/MacOS/pandoc\")",
    call. = FALSE
  )
}

input <- file.path("vignettes", "06_main_question_example.Rmd")
if (!file.exists(input)) {
  stop("Cannot find input file: ", input, call. = FALSE)
}

# Try to make package functions available during render.
if (requireNamespace("pkgload", quietly = TRUE)) {
  try(pkgload::load_all(".", quiet = TRUE), silent = TRUE)
}

output_file <- if (quick_mode) {
  "06_main_question_example_quick.pdf"
} else {
  "06_main_question_example_deep.pdf"
}

cat("Rendering ", input, " -> ", output_file, "\n", sep = "")
cat("quick_mode: ", quick_mode, "\n", sep = "")

result <- tryCatch(
  {
    rmarkdown::render(
      input = input,
      output_format = "pdf_document",
      output_file = output_file,
      output_dir = normalizePath("vignettes", winslash = "/", mustWork = TRUE),
      params = list(run_code = TRUE, quick_mode = quick_mode),
      envir = new.env(parent = globalenv()),
      quiet = FALSE
    )
  },
  error = function(e) {
    msg <- conditionMessage(e)
    cat("\nRender failed:\n", msg, "\n", sep = "")
    cat(
      "\nCommon fixes:\n",
      "1) Pandoc unavailable: run from RStudio or set `RSTUDIO_PANDOC`.\n",
      "2) PDF/LaTeX unavailable: install TinyTeX once.\n",
      sep = ""
    )
    cat(
      "\nTinyTeX install commands:\n",
      "  install.packages(\"tinytex\")\n",
      "  tinytex::install_tinytex()\n",
      sep = ""
    )
    stop(e)
  }
)

cat("\nDone. Output written to:\n", normalizePath(result, winslash = "/", mustWork = TRUE), "\n", sep = "")

#!/usr/bin/env Rscript

# Vignette Generator
# ------------------
# Canonical vignette sources live in `scripts/vignette_templates/` and are
# copied into `vignettes/` so the package has one clear, ordered documentation
# sequence:
#   01_project_motivation.Rmd
#   02_thompson_sampling_theory.Rmd
#   03_package_methodology.Rmd
#   04_backgammon_basics.Rmd
#   05_thompson_workflow.Rmd
#   06_main_question_example.Rmd
#   07_easy_function_calls.Rmd
#
# Usage:
#   Rscript scripts/generate_vignettes.R
#   Rscript scripts/generate_vignettes.R --check
#   Rscript scripts/generate_vignettes.R --render
#   Rscript scripts/generate_vignettes.R --check --render

args <- commandArgs(trailingOnly = TRUE)
do_check <- "--check" %in% args
do_render <- "--render" %in% args

root_required <- c("DESCRIPTION", "vignettes", "scripts")
missing_root <- root_required[!file.exists(root_required)]
if (length(missing_root) > 0L) {
  stop(
    "Run this script from package root. Missing path(s): ",
    paste(missing_root, collapse = ", "),
    call. = FALSE
  )
}

template_dir <- file.path("scripts", "vignette_templates")
if (!dir.exists(template_dir)) {
  stop("Missing template directory: `scripts/vignette_templates`.", call. = FALSE)
}

targets <- c(
  project_motivation = "01_project_motivation.Rmd",
  thompson_sampling_theory = "02_thompson_sampling_theory.Rmd",
  package_methodology = "03_package_methodology.Rmd",
  backgammon_basics = "04_backgammon_basics.Rmd",
  thompson_workflow = "05_thompson_workflow.Rmd",
  main_question_example = "06_main_question_example.Rmd",
  easy_function_calls = "07_easy_function_calls.Rmd"
)

copy_if_changed <- function(src, dst) {
  src_txt <- paste(readLines(src, warn = FALSE), collapse = "\n")
  dst_txt <- if (file.exists(dst)) paste(readLines(dst, warn = FALSE), collapse = "\n") else NULL

  if (!identical(src_txt, dst_txt)) {
    writeLines(src_txt, dst, useBytes = TRUE)
    message("wrote: ", dst)
  } else {
    message("unchanged: ", dst)
  }
}

for (file_nm in unname(targets)) {
  src <- file.path(template_dir, file_nm)
  dst <- file.path("vignettes", file_nm)

  if (!file.exists(src)) {
    stop("Missing template file: ", src, call. = FALSE)
  }
  copy_if_changed(src, dst)
}

if (isTRUE(do_check)) {
  missing_out <- targets[!file.exists(file.path("vignettes", unname(targets)))]
  if (length(missing_out) > 0L) {
    stop(
      "Missing generated vignette file(s): ",
      paste(unname(missing_out), collapse = ", "),
      call. = FALSE
    )
  }
  message("check passed: all canonical vignette files are present.")
}

if (isTRUE(do_render)) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("`--render` requires the `rmarkdown` package.", call. = FALSE)
  }
  if (!rmarkdown::pandoc_available()) {
    stop("`--render` requested but pandoc is unavailable.", call. = FALSE)
  }

  for (file_nm in unname(targets)) {
    path <- file.path("vignettes", file_nm)
    message("rendering: ", path)
    rmarkdown::render(
      input = path,
      output_format = "rmarkdown::html_vignette",
      quiet = TRUE,
      envir = new.env(parent = globalenv())
    )
  }
}

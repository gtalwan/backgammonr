# Build cached opening-roll truths for the package's three reward systems.
#
# File:
#   analysis/03_build_reward_truth_caches.R
# Functions:
#   bg_opening_truth_build_all, bg_truth_project, bg_truth_save,
#   bg_opening_truth_load_all, bg_opening_truth_index
#
# Purpose:
# - build one high-budget scored-outcome opening battery for the 21 opening rolls;
# - keep that master battery in its own cache folder;
# - project that battery into reward-specific cache folders for:
#     * scalar_payoff
#     * win_loss
#     * categorical_outcome
# - write cache indexes under analysis/output/ so later studies can start from
#   cached truths instead of rebuilding them.
#
# Design note:
# The expensive part is the rollout simulation. Each truth object already stores
# the full per-move scored outcome counts, so after the master battery exists we
# can materialize reward-specific truth caches without simulating the 21 openings
# again.

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", mustWork = TRUE)
} else if (file.exists(file.path("..", "DESCRIPTION"))) {
  normalizePath("..", mustWork = TRUE)
} else {
  stop("Run this script from the package root or from `analysis/`.", call. = FALSE)
}

master_cache_dir <- file.path(repo_root, "cache", "opening_truths_master")
scalar_cache_dir <- file.path(repo_root, "cache", "opening_truths_scalar_payoff")
win_loss_cache_dir <- file.path(repo_root, "cache", "opening_truths_win_loss")
categorical_cache_dir <- file.path(repo_root, "cache", "opening_truths_categorical")
output_root <- file.path(repo_root, "analysis", "output", "truth_cache_builds")

for (path in c(
  master_cache_dir,
  scalar_cache_dir,
  win_loss_cache_dir,
  categorical_cache_dir,
  output_root
)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

truth_budget <- 1000000L
truth_block_size <- 512L
truth_seed <- 1L
overwrite_existing <- FALSE

detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (!is.numeric(detected_cores) || length(detected_cores) != 1L || is.na(detected_cores)) {
  detected_cores <- 1L
}
n_cores <- max(1L, min(12L, as.integer(detected_cores) - 1L))
truth_parallel <- identical(.Platform$OS.type, "unix") && n_cores > 1L

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(repo_root, quiet = TRUE)
} else {
  library(backgammonr)
}

announce <- function(title, file = NULL, functions = NULL) {
  line <- paste(rep("=", nchar(title)), collapse = "")
  cat("\n", line, "\n", title, "\n", line, "\n", sep = "")
  if (!is.null(file)) {
    cat("File:", file, "\n")
  }
  if (!is.null(functions)) {
    cat("Functions:", paste(functions, collapse = ", "), "\n")
  }
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

opening_truth_cache_path <- function(truth, cache_dir) {
  ref_summary <- truth$reference$summary[1L, , drop = FALSE]
  backgammonr:::bg_opening_truth_resolve_path(
    problem = truth$problem,
    cache_dir = cache_dir,
    reference_mode = ref_summary$reference_mode[[1L]],
    dice_mode = if ("dice_mode" %in% names(ref_summary)) ref_summary$dice_mode[[1L]] else "iid",
    crn = if ("crn" %in% names(ref_summary)) isTRUE(ref_summary$crn[[1L]]) else FALSE
  )
}

materialize_reward_cache <- function(
    source_truths,
    reward_model,
    posterior_model,
    unresolved_value,
    target_cache_dir,
    overwrite = FALSE) {
  reward_model <- match.arg(reward_model, c("scalar_payoff", "win_loss", "categorical_outcome"))

  for (truth_i in source_truths$truths) {
    projected_truth <- bg_truth_project(
      x = truth_i,
      reward_model = reward_model,
      posterior_model = posterior_model,
      unresolved_value = unresolved_value
    )
    save_path <- opening_truth_cache_path(projected_truth, target_cache_dir)
    if (!file.exists(save_path) || isTRUE(overwrite)) {
      bg_truth_save(projected_truth, save_path, overwrite = overwrite)
    }
  }

  bg_opening_truth_load_all(
    cache_dir = target_cache_dir,
    reward_model = reward_model,
    posterior_model = posterior_model,
    unresolved_value = unresolved_value
  )
}

announce(
  "Build the master scored-outcome opening battery",
  file = "R/bg_truth.R",
  functions = c("bg_opening_truth_build_all")
)
cat(
  paste(
    "This stage runs the expensive rollout simulation once for each of the 21",
    "opening rolls under the full scored-outcome representation. The resulting",
    "cached truths are the master battery used to materialize the other reward",
    "systems."
  ),
  "\n"
)

master_truths <- bg_opening_truth_build_all(
  budget = truth_budget,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5,
  reference_mode = "equal",
  cache = TRUE,
  cache_dir = master_cache_dir,
  n_cores = n_cores,
  parallel = truth_parallel,
  truth_block_size = truth_block_size,
  overwrite = overwrite_existing,
  seed = truth_seed,
  verbose = TRUE
)

announce(
  "Project the master battery into reward-specific cache folders",
  file = "R/bg_truth.R",
  functions = c("bg_truth_project", "bg_truth_save", "bg_opening_truth_load_all")
)
cat(
  paste(
    "Each projected cache reuses the stored scored outcome counts from the",
    "master battery. No new rollouts are simulated in this projection stage."
  ),
  "\n"
)

scalar_truths <- materialize_reward_cache(
  source_truths = master_truths,
  reward_model = "scalar_payoff",
  posterior_model = "beta_pseudo",
  unresolved_value = 0.5,
  target_cache_dir = scalar_cache_dir,
  overwrite = overwrite_existing
)

win_loss_truths <- materialize_reward_cache(
  source_truths = master_truths,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0,
  target_cache_dir = win_loss_cache_dir,
  overwrite = overwrite_existing
)

categorical_truths <- materialize_reward_cache(
  source_truths = master_truths,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5,
  target_cache_dir = categorical_cache_dir,
  overwrite = overwrite_existing
)

master_index <- bg_opening_truth_index(
  cache_dir = master_cache_dir,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5
)

scalar_index <- bg_opening_truth_index(
  cache_dir = scalar_cache_dir,
  reward_model = "scalar_payoff",
  posterior_model = "beta_pseudo",
  unresolved_value = 0.5
)

win_loss_index <- bg_opening_truth_index(
  cache_dir = win_loss_cache_dir,
  reward_model = "win_loss",
  posterior_model = "beta_bernoulli",
  unresolved_value = 0
)

categorical_index <- bg_opening_truth_index(
  cache_dir = categorical_cache_dir,
  reward_model = "categorical_outcome",
  posterior_model = "dirichlet_multinomial",
  unresolved_value = 0.5
)

write_csv(master_index, file.path(output_root, "master_truth_index.csv"))
write_csv(scalar_index, file.path(output_root, "scalar_payoff_truth_index.csv"))
write_csv(win_loss_index, file.path(output_root, "win_loss_truth_index.csv"))
write_csv(categorical_index, file.path(output_root, "categorical_truth_index.csv"))

write_csv(bg_truth_certify(master_truths), file.path(output_root, "master_truth_certification.csv"))
write_csv(bg_truth_certify(scalar_truths), file.path(output_root, "scalar_payoff_truth_certification.csv"))
write_csv(bg_truth_certify(win_loss_truths), file.path(output_root, "win_loss_truth_certification.csv"))
write_csv(bg_truth_certify(categorical_truths), file.path(output_root, "categorical_truth_certification.csv"))

announce(
  "Cache build summary",
  file = "analysis/03_build_reward_truth_caches.R",
  functions = c("bg_opening_truth_index")
)
summary_table <- data.frame(
  cache = c("master", "scalar_payoff", "win_loss", "categorical_outcome"),
  cache_dir = normalizePath(
    c(master_cache_dir, scalar_cache_dir, win_loss_cache_dir, categorical_cache_dir),
    mustWork = FALSE
  ),
  n_cached = c(
    sum(master_index$status == "cached"),
    sum(scalar_index$status == "cached"),
    sum(win_loss_index$status == "cached"),
    sum(categorical_index$status == "cached")
  ),
  stringsAsFactors = FALSE
)
print(summary_table)

cat(
  "\nSaved cache indexes under:\n",
  normalizePath(output_root, mustWork = FALSE),
  "\n",
  sep = ""
)

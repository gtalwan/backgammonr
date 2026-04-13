# Evaluation metrics, diagnostics, stopping summaries, and backend checks.
#
# This file is the compact metrics/diagnostics home for TS-family runs and
# repeated studies.

# -----------------------------------------------------------------------------
# Source: bg_diagnostics.R
# -----------------------------------------------------------------------------
# Diagnostics and evaluation summaries for the research layer.
#
# This file is the main readout layer for the package's statistical workflows.
# It gathers the metrics and diagnostic helpers that answer:
# - does a method recommend the right move?
# - how much regret or ranking error remains?
# - how does it spend budget across the action set?
# - when does it look stable enough to stop?
# - does the posterior model look mismatched to the empirical rollout data?
#
# Evaluation helpers accept several study object types but normalize them down
# to a simple list of runs before computing metrics.
bg_eval_runs <- function(x) {
  if (inherits(x, "bg_ts_run")) {
    return(list(x))
  }
  if (inherits(x, "bg_ts_profile") || inherits(x, "bg_method_compare")) {
    return(unname(x$runs))
  }
  if (inherits(x, "bg_opening_study")) {
    return(unname(x$comparison$runs))
  }
  if (inherits(x, "bg_state_battery")) {
    if (is.null(x$comparison)) {
      stop("`bg_state_battery` does not contain an attached comparison study.", call. = FALSE)
    }
    return(bg_eval_runs(x$comparison))
  }

  stop(
    "`x` must be a `bg_ts_run`, `bg_ts_profile`, `bg_method_compare`, ",
    "`bg_opening_study`, or `bg_state_battery` with an attached comparison.",
    call. = FALSE
  )
}

# Each run can use its embedded proxy reference or an externally supplied truth
# object. Centralize that lookup so all evaluation functions agree.
bg_eval_reference_for_run <- function(run, truth = NULL) {
  if (is.null(truth)) {
    if (is.null(run$reference)) {
      return(NULL)
    }
    return(bg_normalize_truth_reference(run$reference, "run$reference"))
  }

  if (inherits(truth, "bg_truth_battery")) {
    hits <- truth$truths[names(truth$truths) %in% run$problem$problem_id]
    if (length(hits) == 1L) {
      return(bg_normalize_truth_reference(hits[[1L]], "truth"))
    }
    stop(
      "No truth object in `truth$truths` matched `run$problem$problem_id`.",
      call. = FALSE
    )
  }

  if (is.list(truth) &&
      !inherits(truth, "bg_reference") &&
      !inherits(truth, "bg_truth_state")) {
    if (length(truth) == 1L) {
      return(bg_normalize_truth_reference(truth[[1L]], "truth[[1L]]"))
    }
    if (!is.null(names(truth)) && run$problem$problem_id %in% names(truth)) {
      return(bg_normalize_truth_reference(truth[[run$problem$problem_id]], "truth"))
    }
    stop(
      "When `truth` is a list, it must be length 1 or named by `problem_id`.",
      call. = FALSE
    )
  }

  bg_normalize_truth_reference(truth, "truth")
}

bg_eval_checkpoint_grid <- function(run, checkpoints = NULL) {
  available <- sort(unique(run$checkpoint_table$checkpoint))
  if (is.null(checkpoints)) {
    return(available)
  }
  checkpoints <- sort(unique(bg_coerce_integerish(checkpoints, "checkpoints", length(checkpoints))))
  checkpoints[checkpoints %in% available]
}

bg_eval_reference_lookup <- function(reference) {
  snapshot <- bg_reference_snapshot_public(reference)
  tab <- reference$action_table[order(reference$action_table$candidate_index), , drop = FALSE]
  best_row <- tab[which.max(tab$reference_mean), , drop = FALSE]
  best_lower <- if ("reference_mc_lower_95" %in% names(best_row)) best_row$reference_mc_lower_95[[1L]] else NA_real_

  list(
    table = tab,
    best_index = snapshot$best_index,
    best_value = snapshot$best_value,
    best_lower = best_lower,
    value_lookup = snapshot$value_lookup,
    rank_lookup = snapshot$rank_lookup,
    label_lookup = snapshot$label_lookup
  )
}

bg_eval_action_table_at_checkpoint <- function(run, checkpoint, reference = NULL) {
  checkpoint <- bg_coerce_integerish(checkpoint, "checkpoint", 1L)

  if (!is.null(run$action_table) && checkpoint == run$budget) {
    tab <- as.data.frame(run$action_table, stringsAsFactors = FALSE)
  } else {
    tab <- run$checkpoint_actions[run$checkpoint_actions$checkpoint == checkpoint, , drop = FALSE]
  }

  if (nrow(tab) == 0L) {
    return(tab)
  }

  if (!"move_label" %in% names(tab) && "move" %in% names(tab)) {
    tab$move_label <- vapply(tab$move, bg_move_label, character(1L))
  }
  if (!"model_relative_prob_best" %in% names(tab) && "prob_best" %in% names(tab)) {
    tab$model_relative_prob_best <- tab$prob_best
  }
  if (!"model_relative_expected_regret" %in% names(tab) &&
      "posterior_expected_regret" %in% names(tab)) {
    tab$model_relative_expected_regret <- tab$posterior_expected_regret
  }

  checkpoint_row <- run$checkpoint_table[run$checkpoint_table$checkpoint == checkpoint, , drop = FALSE]
  recommended_index <- if (nrow(checkpoint_row) == 0L) NA_integer_ else checkpoint_row$recommended_index[[1L]]

  # Re-rank from the estimated values stored at the checkpoint rather than from
  # the original candidate order.
  ord <- order(-tab$estimate, tab$candidate_index)
  tab <- tab[ord, , drop = FALSE]
  tab$rank <- seq_len(nrow(tab))
  tab$recommended <- tab$candidate_index == recommended_index

  if (!is.null(reference)) {
    ref <- bg_eval_reference_lookup(reference)
    tab$proxy_reference_mean <- unname(ref$value_lookup[as.character(tab$candidate_index)])
    tab$proxy_reference_rank <- unname(ref$rank_lookup[as.character(tab$candidate_index)])
    tab$simple_regret <- ref$best_value - tab$proxy_reference_mean
  }

  rownames(tab) <- NULL
  tab
}

bg_eval_recommended_row <- function(tab) {
  if (nrow(tab) == 0L) {
    return(tab)
  }
  hit <- tab[tab$recommended, , drop = FALSE]
  if (nrow(hit) == 0L) {
    hit <- tab[1L, , drop = FALSE]
  }
  hit[1L, , drop = FALSE]
}

bg_eval_meta_row <- function(run, checkpoint) {
  row <- run$checkpoint_table[run$checkpoint_table$checkpoint == checkpoint, , drop = FALSE]
  if (nrow(row) == 0L) {
    row <- run$checkpoint_table[1L, , drop = FALSE]
  }

  data.frame(
    problem_id = run$problem$problem_id,
    allocation_policy = row$allocation_policy[[1L]],
    checkpoint = checkpoint,
    seed = if (is.null(run$settings$seed)) NA_integer_ else run$settings$seed,
    ts_mode = if (is.null(run$ts_mode)) NA_character_ else run$ts_mode,
    stringsAsFactors = FALSE
  )
}

bg_eval_selection_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  probs <- as.numeric(table(x)) / length(x)
  if (length(probs) <= 1L) {
    return(0)
  }
  -sum(probs * log(probs)) / log(length(probs))
}

bg_eval_safe_range_stat <- function(x, fn = c("min", "max", "mean", "sd")) {
  fn <- match.arg(fn)
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  switch(
    fn,
    min = min(x),
    max = max(x),
    mean = mean(x),
    sd = if (length(x) <= 1L) NA_real_ else stats::sd(x)
  )
}

bg_eval_pairwise_metrics <- function(ids, est_values, truth_values) {
  if (length(ids) <= 1L) {
    return(list(
      pairwise_ordering_accuracy = NA_real_,
      pairwise_disagreement_count = NA_real_
    ))
  }

  pairs <- utils::combn(ids, 2L)
  truth_sign <- sign(truth_values[pairs[1L, ]] - truth_values[pairs[2L, ]])
  est_sign <- sign(est_values[pairs[1L, ]] - est_values[pairs[2L, ]])
  valid <- truth_sign != 0 & est_sign != 0

  list(
    pairwise_ordering_accuracy = if (any(valid)) mean(truth_sign[valid] == est_sign[valid]) else NA_real_,
    pairwise_disagreement_count = if (any(valid)) sum(truth_sign[valid] != est_sign[valid]) else NA_real_
  )
}

bg_eval_rank_metrics <- function(tab, reference, top_k = 3L) {
  ref <- bg_eval_reference_lookup(reference)
  if (nrow(tab) == 0L || nrow(ref$table) == 0L) {
    return(list(
      spearman = NA_real_,
      kendall = NA_real_,
      top_k_overlap = NA_real_,
      top_k_overlap_n = NA_real_,
      pairwise_ordering_accuracy = NA_real_,
      pairwise_disagreement_count = NA_real_,
      weighted_rank_loss = NA_real_,
      restricted_top_m = NA_integer_,
      restricted_pairwise_ordering_accuracy = NA_real_,
      restricted_pairwise_disagreement_count = NA_real_
    ))
  }

  est_lookup <- stats::setNames(tab$rank, tab$candidate_index)
  truth_lookup <- stats::setNames(ref$table$rank, ref$table$candidate_index)
  common_ids <- intersect(names(truth_lookup), names(est_lookup))
  if (length(common_ids) <= 1L) {
    return(list(
      spearman = NA_real_,
      kendall = NA_real_,
      top_k_overlap = 1,
      top_k_overlap_n = 1,
      pairwise_ordering_accuracy = NA_real_,
      pairwise_disagreement_count = NA_real_,
      weighted_rank_loss = NA_real_,
      restricted_top_m = 1L,
      restricted_pairwise_ordering_accuracy = NA_real_,
      restricted_pairwise_disagreement_count = NA_real_
    ))
  }

  est_ranks <- as.numeric(est_lookup[common_ids])
  truth_ranks <- as.numeric(truth_lookup[common_ids])

  est_values <- stats::setNames(tab$estimate, tab$candidate_index)[common_ids]
  truth_values <- stats::setNames(ref$table$reference_mean, ref$table$candidate_index)[common_ids]

  top_k <- min(bg_coerce_integerish(top_k, "top_k", 1L), length(common_ids))
  est_top <- names(sort(est_lookup[common_ids], decreasing = FALSE))[seq_len(top_k)]
  truth_top <- names(sort(truth_lookup[common_ids], decreasing = FALSE))[seq_len(top_k)]
  overlap_n <- length(intersect(est_top, truth_top))

  weights <- 1 / truth_ranks
  weighted_rank_loss <- sum(weights * abs(est_ranks - truth_ranks)) / sum(weights)

  pairwise_metrics <- bg_eval_pairwise_metrics(
    ids = common_ids,
    est_values = est_values,
    truth_values = truth_values
  )

  restricted_ids <- common_ids[truth_ranks <= top_k]
  restricted_metrics <- bg_eval_pairwise_metrics(
    ids = restricted_ids,
    est_values = est_values,
    truth_values = truth_values
  )

  list(
    spearman = suppressWarnings(stats::cor(est_ranks, truth_ranks, method = "spearman")),
    kendall = suppressWarnings(stats::cor(est_ranks, truth_ranks, method = "kendall")),
    top_k_overlap = overlap_n / top_k,
    top_k_overlap_n = overlap_n,
    pairwise_ordering_accuracy = pairwise_metrics$pairwise_ordering_accuracy,
    pairwise_disagreement_count = pairwise_metrics$pairwise_disagreement_count,
    weighted_rank_loss = weighted_rank_loss,
    restricted_top_m = top_k,
    restricted_pairwise_ordering_accuracy = restricted_metrics$pairwise_ordering_accuracy,
    restricted_pairwise_disagreement_count = restricted_metrics$pairwise_disagreement_count
  )
}

bg_eval_rows <- function(x, truth = NULL, checkpoints = NULL, row_builder) {
  runs <- bg_eval_runs(x)
  rows <- vector("list", length(runs) * 8L)
  row_id <- 1L

  for (run in runs) {
    reference <- bg_eval_reference_for_run(run, truth = truth)
    run_checkpoints <- bg_eval_checkpoint_grid(run, checkpoints = checkpoints)
    # Every public evaluation front door is just a different row builder over
    # the same run x checkpoint grid.
    for (checkpoint in run_checkpoints) {
      rows[[row_id]] <- row_builder(run, checkpoint, reference)
      row_id <- row_id + 1L
    }
  }

  out <- do.call(rbind, rows[seq_len(row_id - 1L)])
  rownames(out) <- NULL
  out
}

bg_eval_group_id <- function(df, keys) {
  if (nrow(df) == 0L) {
    return(character(0L))
  }
  do.call(
    interaction,
    c(df[, keys, drop = FALSE], list(drop = TRUE, lex.order = TRUE))
  )
}

# Runtime and throughput live in the run checkpoint tables, not in the public
# top1 evaluation panel. Keep one small extractor here so path-level summaries
# can join them without changing the older front doors.
bg_eval_runtime_panel <- function(x, checkpoints = NULL) {
  runs <- bg_eval_runs(x)
  rows <- vector("list", length(runs))

  for (i in seq_along(runs)) {
    run <- runs[[i]]
    run_checkpoints <- bg_eval_checkpoint_grid(run, checkpoints = checkpoints)
    tab <- run$checkpoint_table[run$checkpoint_table$checkpoint %in% run_checkpoints, , drop = FALSE]
    if (nrow(tab) == 0L) {
      next
    }
    rows[[i]] <- data.frame(
      problem_id = run$problem$problem_id,
      allocation_policy = tab$allocation_policy,
      checkpoint = tab$checkpoint,
      seed = if (is.null(run$settings$seed)) NA_integer_ else run$settings$seed,
      ts_mode = if (is.null(run$ts_mode)) NA_character_ else run$ts_mode,
      runtime_seconds = tab$runtime_seconds,
      rollout_throughput = tab$rollout_throughput,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) {
    out <- data.frame(
      problem_id = character(),
      allocation_policy = character(),
      checkpoint = integer(),
      seed = integer(),
      ts_mode = character(),
      runtime_seconds = numeric(),
      rollout_throughput = numeric(),
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

#' Evaluate top-decision performance
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector. By default all available
#'   checkpoints are used.
#' @param epsilon Numeric tolerance for epsilon-optimal selection.
#'
#' @return A data frame with one row per run and checkpoint.
#' @export
bg_eval_top1 <- function(x, truth = NULL, checkpoints = NULL, epsilon = 0.01) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) || epsilon < 0) {
    stop("`epsilon` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  bg_eval_rows(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    row_builder = function(run, checkpoint, reference) {
      tab <- bg_eval_action_table_at_checkpoint(run, checkpoint, reference = reference)
      meta <- bg_eval_meta_row(run, checkpoint)
      chosen <- bg_eval_recommended_row(tab)

      if (nrow(tab) == 0L || is.null(reference)) {
        return(cbind(
          meta,
          data.frame(
            recommended_index = if (nrow(chosen) == 0L) NA_integer_ else chosen$candidate_index[[1L]],
            recommended_move_label = if (nrow(chosen) == 0L) NA_character_ else chosen$move_label[[1L]],
            truth_best_index = NA_integer_,
            truth_best_move_label = NA_character_,
            top1_match = NA,
            simple_regret = NA_real_,
            epsilon_optimal = NA,
            selected_reference_rank = NA_integer_,
            recommended_prob_best = if (nrow(chosen) == 0L) NA_real_ else chosen$model_relative_prob_best[[1L]],
            posterior_top_k_mass = NA_real_,
            stringsAsFactors = FALSE
          )
        ))
      }

      ref <- bg_eval_reference_lookup(reference)
      chosen_index <- chosen$candidate_index[[1L]]
      chosen_prob_best <- if ("model_relative_prob_best" %in% names(chosen)) {
        chosen$model_relative_prob_best[[1L]]
      } else {
        NA_real_
      }
      prob_best_mass <- if ("model_relative_prob_best" %in% names(tab)) {
        sum(sort(tab$model_relative_prob_best, decreasing = TRUE)[seq_len(min(2L, nrow(tab)))], na.rm = TRUE)
      } else {
        NA_real_
      }
      chosen_value <- ref$value_lookup[[as.character(chosen_index)]]
      simple_regret <- ref$best_value - chosen_value

      cbind(
        meta,
        data.frame(
          recommended_index = chosen_index,
          recommended_move_label = chosen$move_label[[1L]],
          truth_best_index = ref$best_index,
          truth_best_move_label = ref$label_lookup[[as.character(ref$best_index)]],
          top1_match = chosen_index == ref$best_index,
          simple_regret = simple_regret,
          epsilon_optimal = simple_regret <= epsilon,
          selected_reference_rank = ref$rank_lookup[[as.character(chosen_index)]],
          recommended_prob_best = chosen_prob_best,
          posterior_top_k_mass = prob_best_mass,
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Evaluate simple regret over runs and checkpoints
#'
#' @inheritParams bg_eval_top1
#'
#' @return A data frame with simple-regret-focused columns.
#' @keywords internal
#' @noRd
bg_eval_simple_regret <- function(x, truth = NULL, checkpoints = NULL, epsilon = 0.01) {
  out <- bg_eval_top1(x, truth = truth, checkpoints = checkpoints, epsilon = epsilon)
  out[, c(
    "problem_id",
    "allocation_policy",
    "checkpoint",
    "seed",
    "recommended_move_label",
    "simple_regret",
    "epsilon_optimal"
  ), drop = FALSE]
}

#' Evaluate ranking recovery
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_k Integer-like top-k value used for overlap summaries.
#'
#' @return A data frame with one row per run and checkpoint.
#' @export
bg_eval_rank <- function(x, truth = NULL, checkpoints = NULL, top_k = 3L) {
  bg_eval_rows(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    row_builder = function(run, checkpoint, reference) {
      tab <- bg_eval_action_table_at_checkpoint(run, checkpoint, reference = reference)
      meta <- bg_eval_meta_row(run, checkpoint)
      metrics <- if (is.null(reference)) {
        list(
          spearman = NA_real_,
          kendall = NA_real_,
          top_k_overlap = NA_real_,
          top_k_overlap_n = NA_real_,
          pairwise_ordering_accuracy = NA_real_,
          pairwise_disagreement_count = NA_real_,
          weighted_rank_loss = NA_real_
        )
      } else {
        bg_eval_rank_metrics(tab, reference = reference, top_k = top_k)
      }

      cbind(
        meta,
        data.frame(
          n_actions = nrow(tab),
          top_k = bg_coerce_integerish(top_k, "top_k", 1L),
          spearman = metrics$spearman,
          kendall = metrics$kendall,
          top_k_overlap = metrics$top_k_overlap,
          top_k_overlap_n = metrics$top_k_overlap_n,
          pairwise_ordering_accuracy = metrics$pairwise_ordering_accuracy,
          pairwise_disagreement_count = metrics$pairwise_disagreement_count,
          weighted_rank_loss = metrics$weighted_rank_loss,
          restricted_top_m = metrics$restricted_top_m,
          restricted_pairwise_ordering_accuracy = metrics$restricted_pairwise_ordering_accuracy,
          restricted_pairwise_disagreement_count = metrics$restricted_pairwise_disagreement_count,
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Evaluate top-k recovery
#'
#' @inheritParams bg_eval_rank
#'
#' @return A data frame with top-k-focused ranking summaries.
#' @export
bg_eval_topk <- function(x, truth = NULL, checkpoints = NULL, k = 3L) {
  k <- bg_coerce_integerish(k, "k", 1L)
  rank_out <- bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = k)
  top1_out <- bg_eval_top1(x, truth = truth, checkpoints = checkpoints)
  out <- merge(
    rank_out[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "ts_mode",
      "top_k",
      "top_k_overlap",
      "top_k_overlap_n",
      "restricted_top_m",
      "restricted_pairwise_ordering_accuracy",
      "restricted_pairwise_disagreement_count"
    ), drop = FALSE],
    top1_out[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "ts_mode",
      "recommended_index",
      "recommended_move_label",
      "selected_reference_rank",
      "posterior_top_k_mass"
    ), drop = FALSE],
    by = c("problem_id", "allocation_policy", "checkpoint", "seed", "ts_mode"),
    all = TRUE,
    sort = FALSE
  )
  out$truth_top2_hit <- !is.na(out$selected_reference_rank) & out$selected_reference_rank <= 2L
  out$truth_top_k_hit <- !is.na(out$selected_reference_rank) & out$selected_reference_rank <= out$top_k
  out
}

#' Evaluate restricted ranking recovery among proxy-truth top moves
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_m Integer-like number of proxy-truth top actions retained when
#'   computing restricted ranking summaries.
#'
#' @return A data frame with one row per run and checkpoint.
#' @export
bg_eval_restricted_rank <- function(x, truth = NULL, checkpoints = NULL, top_m = 3L) {
  top_m <- bg_coerce_integerish(top_m, "top_m", 1L)
  out <- bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = top_m)
  out[, c(
    "problem_id",
    "allocation_policy",
    "checkpoint",
    "seed",
    "ts_mode",
    "restricted_top_m",
    "restricted_pairwise_ordering_accuracy",
    "restricted_pairwise_disagreement_count"
  ), drop = FALSE]
}

#' Evaluate pairwise ordering recovery
#'
#' @inheritParams bg_eval_rank
#'
#' @return A data frame with pairwise-ordering summaries.
#' @keywords internal
#' @noRd
bg_eval_pairwise <- function(x, truth = NULL, checkpoints = NULL) {
  out <- bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = 2L)
  out[, c(
    "problem_id",
    "allocation_policy",
    "checkpoint",
    "seed",
    "pairwise_ordering_accuracy",
    "pairwise_disagreement_count"
  ), drop = FALSE]
}

#' Evaluate allocation quality
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_k Integer-like number of truth-top actions used for budget-share
#'   summaries.
#' @param dominated_gap Numeric gap threshold used when proxy intervals are
#'   unavailable for dominated-action screening.
#'
#' @return A data frame with one row per run and checkpoint.
#' @export
bg_eval_allocation <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    top_k = 2L,
    dominated_gap = 0.05) {
  if (!is.numeric(dominated_gap) || length(dominated_gap) != 1L || is.na(dominated_gap) || dominated_gap < 0) {
    stop("`dominated_gap` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  bg_eval_rows(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    row_builder = function(run, checkpoint, reference) {
      tab <- bg_eval_action_table_at_checkpoint(run, checkpoint, reference = reference)
      meta <- bg_eval_meta_row(run, checkpoint)
      counts <- if (nrow(tab) == 0L) numeric(0L) else tab$allocation_count
      concentration <- bg_allocation_concentration(counts)

      share_top_k_truth <- NA_real_
      share_top2_truth <- NA_real_
      share_best_truth <- NA_real_
      share_mc_screened_suboptimal <- NA_real_
      mc_screened_suboptimal_count <- NA_integer_
      gap_weighted_wasted_allocation <- NA_real_

      if (nrow(tab) > 0L && !is.null(reference)) {
        ref <- bg_eval_reference_lookup(reference)
        ref_tab <- ref$table[order(ref$table$rank), , drop = FALSE]
        top2_ids <- ref_tab$candidate_index[seq_len(min(2L, nrow(ref_tab)))]
        top_k_ids <- ref_tab$candidate_index[seq_len(min(top_k, nrow(ref_tab)))]
        tab_counts <- stats::setNames(tab$allocation_count, tab$candidate_index)
        total_alloc <- sum(tab$allocation_count)

        share_top2_truth <- sum(tab_counts[as.character(top2_ids)], na.rm = TRUE) / total_alloc
        share_top_k_truth <- sum(tab_counts[as.character(top_k_ids)], na.rm = TRUE) / total_alloc
        share_best_truth <- tab_counts[[as.character(ref$best_index)]] / total_alloc
        gap_lookup <- ref$best_value - ref_tab$reference_mean
        names(gap_lookup) <- ref_tab$candidate_index
        gap_weighted_wasted_allocation <- sum(
          (tab_counts / total_alloc) * unname(gap_lookup[names(tab_counts)]),
          na.rm = TRUE
        )

        screened_suboptimal <- if (all(c("reference_mc_upper_95", "reference_mc_lower_95") %in% names(ref_tab)) &&
            is.finite(ref$best_lower)) {
          # When proxy-reference intervals exist, use the MC screening rule
          # rather than a plain gap threshold.
          ref_tab$reference_mc_upper_95 < ref$best_lower
        } else {
          (ref$best_value - ref_tab$reference_mean) > dominated_gap
        }
        screened_ids <- ref_tab$candidate_index[screened_suboptimal]
        mc_screened_suboptimal_count <- length(screened_ids)
        share_mc_screened_suboptimal <- if (length(screened_ids) == 0L) {
          0
        } else {
          sum(tab_counts[as.character(screened_ids)], na.rm = TRUE) / total_alloc
        }
      }

      cbind(
        meta,
        data.frame(
          total_allocation = sum(counts),
          n_allocated_actions = sum(counts > 0, na.rm = TRUE),
          allocation_entropy = concentration$entropy,
          allocation_hhi = concentration$hhi,
          allocation_hhi_normalized = concentration$hhi_normalized,
          allocation_max_share = concentration$max_share,
          allocation_effective_arm_fraction = concentration$effective_arm_fraction,
          share_top2_truth = share_top2_truth,
          share_top_k_truth = share_top_k_truth,
          share_best_truth = share_best_truth,
          share_mc_screened_suboptimal = share_mc_screened_suboptimal,
          mc_screened_suboptimal_count = mc_screened_suboptimal_count,
          gap_weighted_wasted_allocation = gap_weighted_wasted_allocation,
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Evaluate budget efficiency over a checkpoint path
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param epsilon Numeric tolerance for epsilon-optimal selection.
#'
#' @return A data frame with one row per run path.
#' @export
bg_eval_efficiency <- function(x, truth = NULL, checkpoints = NULL, epsilon = 0.01) {
  keys <- c("problem_id", "allocation_policy", "seed", "ts_mode")
  top1 <- bg_eval_top1(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    epsilon = epsilon
  )
  runtime_panel <- bg_eval_runtime_panel(x, checkpoints = checkpoints)
  path_panel <- merge(
    top1,
    runtime_panel,
    by = c("problem_id", "allocation_policy", "checkpoint", "seed", "ts_mode"),
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(path_panel) == 0L) {
    return(data.frame(
      problem_id = character(),
      allocation_policy = character(),
      seed = integer(),
      ts_mode = character(),
      n_checkpoints = integer(),
      max_checkpoint = integer(),
      max_runtime_seconds = numeric(),
      first_budget_top1_match = numeric(),
      first_budget_epsilon_optimal = numeric(),
      first_runtime_top1_match = numeric(),
      first_runtime_epsilon_optimal = numeric(),
      auc_top1_match = numeric(),
      auc_simple_regret = numeric(),
      mean_brier_top1 = numeric(),
      final_recommended_move_label = character(),
      truth_best_move_label = character(),
      final_top1_match = logical(),
      final_simple_regret = numeric(),
      final_selected_reference_rank = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(
    split(path_panel, bg_eval_group_id(path_panel, keys), drop = TRUE),
    function(df) {
      df <- df[order(df$checkpoint), , drop = FALSE]
      last <- df[nrow(df), , drop = FALSE]
      path_metrics <- bg_cpp_eval_path_metrics(
        checkpoint = df$checkpoint,
        runtime_seconds = df$runtime_seconds,
        top1_match = df$top1_match,
        epsilon_optimal = df$epsilon_optimal,
        simple_regret = df$simple_regret,
        recommended_prob_best = df$recommended_prob_best
      )

      data.frame(
        problem_id = last$problem_id,
        allocation_policy = last$allocation_policy,
        seed = last$seed,
        ts_mode = last$ts_mode,
        n_checkpoints = path_metrics$n_checkpoints,
        max_checkpoint = path_metrics$max_checkpoint,
        max_runtime_seconds = path_metrics$max_runtime_seconds,
        first_budget_top1_match = path_metrics$first_budget_top1_match,
        first_budget_epsilon_optimal = path_metrics$first_budget_epsilon_optimal,
        first_runtime_top1_match = path_metrics$first_runtime_top1_match,
        first_runtime_epsilon_optimal = path_metrics$first_runtime_epsilon_optimal,
        auc_top1_match = path_metrics$auc_top1_match,
        auc_simple_regret = path_metrics$auc_simple_regret,
        mean_brier_top1 = path_metrics$mean_brier_top1,
        final_recommended_move_label = last$recommended_move_label,
        truth_best_move_label = last$truth_best_move_label,
        final_top1_match = last$top1_match,
        final_simple_regret = last$simple_regret,
        final_selected_reference_rank = last$selected_reference_rank,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Evaluate probability-best calibration
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param bins Integer-like number of equal-width probability bins.
#'
#' @return A list with `raw` checkpoint rows and a binned `summary`.
#' @export
bg_eval_calibration <- function(x, truth = NULL, checkpoints = NULL, bins = 5L) {
  bins <- bg_coerce_integerish(bins, "bins", 1L)
  if (bins < 1L) {
    stop("`bins` must be at least 1.", call. = FALSE)
  }

  raw <- bg_eval_top1(
    x = x,
    truth = truth,
    checkpoints = checkpoints
  )
  raw$top1_outcome <- ifelse(is.na(raw$top1_match), NA_real_, as.numeric(raw$top1_match))
  raw$brier_top1 <- ifelse(
    is.na(raw$recommended_prob_best) | is.na(raw$top1_outcome),
    NA_real_,
    (raw$recommended_prob_best - raw$top1_outcome)^2
  )
  raw$calibration_bin <- ifelse(
    is.na(raw$recommended_prob_best),
    NA_integer_,
    pmin(floor(raw$recommended_prob_best * bins) + 1L, bins)
  )
  raw$calibration_bin_lower <- ifelse(
    is.na(raw$calibration_bin),
    NA_real_,
    (raw$calibration_bin - 1L) / bins
  )
  raw$calibration_bin_upper <- ifelse(
    is.na(raw$calibration_bin),
    NA_real_,
    raw$calibration_bin / bins
  )

  valid_raw <- raw[is.finite(raw$recommended_prob_best) & !is.na(raw$top1_outcome), , drop = FALSE]
  summary_keys <- c("problem_id", "allocation_policy", "checkpoint", "ts_mode")

  summary_rows <- lapply(
    split(valid_raw, bg_eval_group_id(valid_raw, summary_keys), drop = TRUE),
    function(df) {
      bins_df <- bg_cpp_calibration_summary(
        predicted_prob = df$recommended_prob_best,
        observed_top1 = df$top1_outcome,
        bins = bins
      )
      meta <- df[1L, summary_keys, drop = FALSE]
      cbind(meta[rep(1L, nrow(bins_df)), , drop = FALSE], bins_df)
    }
  )

  summary <- if (length(summary_rows) == 0L) {
    data.frame(
      problem_id = character(),
      allocation_policy = character(),
      checkpoint = integer(),
      ts_mode = character(),
      calibration_bin = integer(),
      bin_lower = numeric(),
      bin_upper = numeric(),
      n = integer(),
      mean_predicted_prob_best = numeric(),
      observed_top1_rate = numeric(),
      mean_brier_top1 = numeric(),
      calibration_gap = numeric(),
      ece_component = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, summary_rows)
  }

  rownames(raw) <- NULL
  rownames(summary) <- NULL
  list(raw = raw, summary = summary)
}

#' Evaluate gap-aware correctness
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param gap_tol Numeric top-gap threshold used to flag near-tie states.
#' @param top_k Integer-like value used for posterior top-k mass summaries.
#'
#' @return A data frame with one row per run and checkpoint.
#' @keywords internal
#' @noRd
bg_eval_gap_aware <- function(x, truth = NULL, checkpoints = NULL, gap_tol = 0.01, top_k = 2L) {
  if (!is.numeric(gap_tol) || length(gap_tol) != 1L || is.na(gap_tol) || gap_tol < 0) {
    stop("`gap_tol` must be a nonnegative numeric scalar.", call. = FALSE)
  }

  bg_eval_rows(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    row_builder = function(run, checkpoint, reference) {
      tab <- bg_eval_action_table_at_checkpoint(run, checkpoint, reference = reference)
      meta <- bg_eval_meta_row(run, checkpoint)
      chosen <- bg_eval_recommended_row(tab)

      if (nrow(tab) == 0L || is.null(reference)) {
        return(cbind(
          meta,
          data.frame(
            top_two_gap_estimate = NA_real_,
            near_tie = NA,
            mc_not_separated_from_best_set_size = NA_integer_,
            chosen_mc_not_separated_from_best = NA,
            chosen_gap_to_best = NA_real_,
            posterior_top1_prob = NA_real_,
            posterior_top_k_mass = NA_real_,
            stringsAsFactors = FALSE
          )
        ))
      }

      ref <- bg_eval_reference_lookup(reference)
      ref_tab <- ref$table[order(ref$table$rank), , drop = FALSE]
      indist_ids <- ref_tab$candidate_index[ref_tab$reference_mc_upper_95 >= ref$best_lower]
      chosen_index <- chosen$candidate_index[[1L]]
      top_gap <- if (nrow(ref_tab) >= 2L) {
        ref_tab$reference_mean[[1L]] - ref_tab$reference_mean[[2L]]
      } else {
        NA_real_
      }
      posterior_top_k_mass <- if ("model_relative_prob_best" %in% names(tab)) {
        sum(sort(tab$model_relative_prob_best, decreasing = TRUE)[seq_len(min(top_k, nrow(tab)))], na.rm = TRUE)
      } else {
        NA_real_
      }

      cbind(
        meta,
        data.frame(
          top_two_gap_estimate = top_gap,
          near_tie = isTRUE(is.finite(top_gap) && top_gap <= gap_tol),
          mc_not_separated_from_best_set_size = length(indist_ids),
          chosen_mc_not_separated_from_best = chosen_index %in% indist_ids,
          chosen_gap_to_best = ref$best_value - ref$value_lookup[[as.character(chosen_index)]],
          posterior_top1_prob = if ("model_relative_prob_best" %in% names(chosen)) chosen$model_relative_prob_best[[1L]] else NA_real_,
          posterior_top_k_mass = posterior_top_k_mass,
          stringsAsFactors = FALSE
        )
      )
    }
  )
}

#' Build a reference-aware evaluation panel
#'
#' `bg_eval_reference_aware()` joins the package's main evaluation layers into
#' one compact data frame so later workflows can summarize decision quality,
#' ranking recovery, uncertainty, and allocation in one object.
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of the embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_k Integer-like top-k value used by ranking and allocation
#'   summaries.
#' @param epsilon Numeric tolerance for epsilon-optimal selection.
#' @param gap_tol Numeric gap threshold used to flag near-tie states.
#'
#' @return A data frame with one row per run and checkpoint.
#' @export
bg_eval_reference_aware <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    top_k = 3L,
    epsilon = 0.01,
    gap_tol = 0.01) {
  keys <- c("problem_id", "allocation_policy", "checkpoint", "seed", "ts_mode")

  top1 <- bg_eval_top1(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    epsilon = epsilon
  )
  rank <- bg_eval_rank(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    top_k = top_k
  )
  topk_eval <- bg_eval_topk(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    k = top_k
  )[, c(
    "problem_id",
    "allocation_policy",
    "checkpoint",
    "seed",
    "ts_mode",
    "truth_top2_hit",
    "truth_top_k_hit"
  ), drop = FALSE]
  alloc <- bg_eval_allocation(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    top_k = top_k
  )
  gap <- bg_eval_gap_aware(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    gap_tol = gap_tol,
    top_k = top_k
  )

  out <- Reduce(
    function(left, right) merge(left, right, by = keys, all = TRUE, sort = FALSE),
    list(
      top1,
      rank,
      topk_eval,
      alloc,
      gap
    )
  )

  ordered_cols <- c(
    "problem_id",
    "allocation_policy",
    "checkpoint",
    "seed",
    "ts_mode",
    "recommended_move_label",
    "truth_best_move_label",
    "top1_match",
    "simple_regret",
    "epsilon_optimal",
    "selected_reference_rank",
    "recommended_prob_best",
    "posterior_top_k_mass",
    "truth_top2_hit",
    "truth_top_k_hit",
    "spearman",
    "kendall",
    "top_k",
    "top_k_overlap",
    "pairwise_ordering_accuracy",
    "restricted_pairwise_ordering_accuracy",
    "weighted_rank_loss",
    "total_allocation",
    "n_allocated_actions",
    "allocation_entropy",
    "allocation_hhi",
    "allocation_hhi_normalized",
    "allocation_max_share",
    "allocation_effective_arm_fraction",
    "share_top2_truth",
    "share_top_k_truth",
    "share_best_truth",
    "share_mc_screened_suboptimal",
    "gap_weighted_wasted_allocation",
    "top_two_gap_estimate",
    "near_tie",
    "mc_not_separated_from_best_set_size",
    "chosen_mc_not_separated_from_best",
    "chosen_gap_to_best"
  )

  out <- out[, intersect(ordered_cols, names(out)), drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Summarize TS behavior in one compact diagnostics bundle
#'
#' `bg_ts_diagnostics()` answers three questions for one run or comparison
#' object: whether the method spends budget on the most relevant moves, whether
#' it gets the recommendation right, and how quickly it stabilizes.
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of embedded proxy
#'   references.
#' @param checkpoints Optional checkpoint vector.
#' @param top_k Integer-like top-k value used by ranking and allocation
#'   summaries.
#' @param epsilon Numeric tolerance for epsilon-optimal selection.
#' @param gap_tol Numeric gap threshold used to flag near-tie states.
#'
#' @return A named list with allocation, accuracy, efficiency, failures, and a
#'   joined reference-aware panel.
#' @export
bg_ts_diagnostics <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    top_k = 3L,
    epsilon = 0.01,
    gap_tol = 0.01) {
  panel <- bg_eval_reference_aware(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    top_k = top_k,
    epsilon = epsilon,
    gap_tol = gap_tol
  )

  failures <- panel[
    (!is.na(panel$recommended_prob_best) & panel$recommended_prob_best >= 0.8 & !is.na(panel$top1_match) & !panel$top1_match) |
      (!is.na(panel$near_tie) & panel$near_tie) |
      (!is.na(panel$allocation_entropy) & panel$allocation_entropy >= 0.85) |
      (!is.na(panel$share_top_k_truth) & panel$share_top_k_truth <= 0.4),
    ,
    drop = FALSE
  ]

  list(
    allocation = bg_eval_allocation(x, truth = truth, checkpoints = checkpoints, top_k = top_k),
    accuracy = bg_eval_reference_aware(
      x = x,
      truth = truth,
      checkpoints = checkpoints,
      top_k = top_k,
      epsilon = epsilon,
      gap_tol = gap_tol
    )[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "ts_mode",
      "top1_match",
      "simple_regret",
      "selected_reference_rank",
      "spearman",
      "kendall",
      "truth_top2_hit",
      "truth_top_k_hit",
      "top_k_overlap",
      "pairwise_ordering_accuracy",
      "restricted_pairwise_ordering_accuracy"
    ), drop = FALSE],
    efficiency = bg_eval_efficiency(x, truth = truth, checkpoints = checkpoints),
    failures = failures,
    panel = panel
  )
}

#' Summarize seed stability
#'
#' @param x A `bg_ts_profile`, `bg_method_compare`, `bg_opening_study`, or
#'   `bg_state_battery` with attached comparison results.
#' @param truth Optional truth object used instead of embedded proxy references.
#' @param checkpoints Optional checkpoint vector.
#' @param metric Metric to summarize across seeds.
#'
#' @return A data frame grouped by problem, method, and checkpoint.
#' @keywords internal
#' @noRd
bg_eval_seed_stability <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    metric = c("top1_match", "simple_regret", "spearman")) {
  metric <- match.arg(metric)

  raw <- switch(
    metric,
    top1_match = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    simple_regret = bg_eval_top1(x, truth = truth, checkpoints = checkpoints),
    spearman = bg_eval_rank(x, truth = truth, checkpoints = checkpoints)
  )

  metric_col <- switch(
    metric,
    top1_match = "top1_match",
    simple_regret = "simple_regret",
    spearman = "spearman"
  )

  top1_raw <- bg_eval_top1(x, truth = truth, checkpoints = checkpoints)
  split_key <- interaction(raw$problem_id, raw$allocation_policy, raw$checkpoint, drop = TRUE)

  rows <- lapply(
    split(raw, split_key),
    function(df) {
      top_df <- top1_raw[
        top1_raw$problem_id == df$problem_id[[1L]] &
          top1_raw$allocation_policy == df$allocation_policy[[1L]] &
          top1_raw$checkpoint == df$checkpoint[[1L]],
        ,
        drop = FALSE
      ]

      data.frame(
        problem_id = df$problem_id[[1L]],
        allocation_policy = df$allocation_policy[[1L]],
        checkpoint = df$checkpoint[[1L]],
        metric = metric,
        n_seeds = length(unique(df$seed[!is.na(df$seed)])),
        mean_value = bg_eval_safe_range_stat(df[[metric_col]], fn = "mean"),
        sd_value = bg_eval_safe_range_stat(df[[metric_col]], fn = "sd"),
        min_value = bg_eval_safe_range_stat(df[[metric_col]], fn = "min"),
        max_value = bg_eval_safe_range_stat(df[[metric_col]], fn = "max"),
        n_distinct_recommendations = length(unique(top_df$recommended_move_label[!is.na(top_df$recommended_move_label)])),
        selection_entropy = bg_eval_selection_entropy(top_df$recommended_move_label),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# Experimental stopping diagnostics
# -----------------------------------------------------------------------------

bg_stopping_stability_path <- function(df) {
  df <- df[order(df$checkpoint), , drop = FALSE]
  stable_run <- integer(nrow(df))
  changed <- logical(nrow(df))

  if (nrow(df) < 1L) {
    df$recommendation_changed <- logical(0L)
    df$stable_checkpoint_run <- integer(0L)
    return(df)
  }

  stable_run[[1L]] <- 1L
  changed[[1L]] <- FALSE
  if (nrow(df) >= 2L) {
    for (i in 2:nrow(df)) {
      same_as_prev <- identical(df$recommended_move_label[[i]], df$recommended_move_label[[i - 1L]])
      changed[[i]] <- !same_as_prev
      stable_run[[i]] <- if (isTRUE(same_as_prev)) stable_run[[i - 1L]] + 1L else 1L
    }
  }

  df$recommendation_changed <- changed
  df$stable_checkpoint_run <- stable_run
  df
}

bg_stopping_checkpoint_summary <- function(raw) {
  if (nrow(raw) < 1L) {
    return(data.frame(
      problem_id = character(),
      allocation_policy = character(),
      checkpoint = integer(),
      ts_mode = character(),
      n_seeds = integer(),
      mean_recommended_prob_best = numeric(),
      mean_prob_good_selection_analogue = numeric(),
      mean_expected_opportunity_cost_analogue = numeric(),
      mean_candidate_set_size = numeric(),
      mean_candidate_set_size_prob_floor = numeric(),
      mean_top_two_posterior_gap = numeric(),
      stop_rate_prob_best = numeric(),
      stop_rate_good_selection = numeric(),
      stop_rate_eoc = numeric(),
      stop_rate_stability = numeric(),
      stop_rate_combined = numeric(),
      n_distinct_recommendations = integer(),
      recommendation_instability = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  split_key <- interaction(raw$problem_id, raw$allocation_policy, raw$checkpoint, raw$ts_mode, drop = TRUE, lex.order = TRUE)
  rows <- lapply(
    split(raw, split_key),
    function(df) {
      recommendation_modal_share <- bg_truth_modal_share(df$recommended_move_label)

      data.frame(
        problem_id = df$problem_id[[1L]],
        allocation_policy = df$allocation_policy[[1L]],
        checkpoint = df$checkpoint[[1L]],
        ts_mode = df$ts_mode[[1L]],
        n_seeds = length(unique(df$seed[!is.na(df$seed)])),
        mean_recommended_prob_best = mean(df$recommended_prob_best, na.rm = TRUE),
        mean_prob_good_selection_analogue = mean(df$prob_good_selection_analogue, na.rm = TRUE),
        mean_expected_opportunity_cost_analogue = mean(df$expected_opportunity_cost_analogue, na.rm = TRUE),
        mean_candidate_set_size = mean(df$candidate_set_size, na.rm = TRUE),
        mean_candidate_set_size_prob_floor = mean(df$candidate_set_size_prob_floor, na.rm = TRUE),
        mean_top_two_posterior_gap = mean(df$top_two_posterior_gap, na.rm = TRUE),
        stop_rate_prob_best = mean(df$suggest_stop_prob_best, na.rm = TRUE),
        stop_rate_good_selection = mean(df$suggest_stop_good_selection, na.rm = TRUE),
        stop_rate_eoc = mean(df$suggest_stop_eoc, na.rm = TRUE),
        stop_rate_stability = mean(df$suggest_stop_stability, na.rm = TRUE),
        stop_rate_combined = mean(df$suggest_stop_combined, na.rm = TRUE),
        n_distinct_recommendations = length(unique(df$recommended_move_label[!is.na(df$recommended_move_label)])),
        recommendation_instability = if (is.na(recommendation_modal_share)) NA_real_ else 1 - recommendation_modal_share,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

bg_stopping_threshold_summary <- function(raw, confidence_thresholds) {
  if (nrow(raw) < 1L) {
    return(data.frame(
      problem_id = character(),
      allocation_policy = character(),
      checkpoint = integer(),
      ts_mode = character(),
      confidence_threshold = numeric(),
      n_flagged = integer(),
      false_confidence_rate = numeric(),
      correct_selection_rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  if (!all(c("top1_match", "recommended_prob_best") %in% names(raw))) {
    return(data.frame(
      problem_id = character(),
      allocation_policy = character(),
      checkpoint = integer(),
      ts_mode = character(),
      confidence_threshold = numeric(),
      n_flagged = integer(),
      false_confidence_rate = numeric(),
      correct_selection_rate = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- vector("list", nrow(unique(raw[c("problem_id", "allocation_policy", "checkpoint", "ts_mode")]))) 
  row_id <- 1L
  split_key <- interaction(raw$problem_id, raw$allocation_policy, raw$checkpoint, raw$ts_mode, drop = TRUE, lex.order = TRUE)
  for (df in split(raw, split_key)) {
    for (threshold in confidence_thresholds) {
      flagged <- df[!is.na(df$recommended_prob_best) & df$recommended_prob_best >= threshold, , drop = FALSE]
      rows[[row_id]] <- data.frame(
        problem_id = df$problem_id[[1L]],
        allocation_policy = df$allocation_policy[[1L]],
        checkpoint = df$checkpoint[[1L]],
        ts_mode = df$ts_mode[[1L]],
        confidence_threshold = threshold,
        n_flagged = nrow(flagged),
        false_confidence_rate = if (nrow(flagged) < 1L) NA_real_ else mean(!flagged$top1_match, na.rm = TRUE),
        correct_selection_rate = if (nrow(flagged) < 1L) NA_real_ else mean(flagged$top1_match, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  out <- do.call(rbind, rows[seq_len(row_id - 1L)])
  rownames(out) <- NULL
  out
}

#' Diagnose experimental stopping rules
#'
#' `bg_stopping_diagnostics()` summarizes model-relative stop-rule analogues for
#' TS studies. The returned suggestions are experimental and should be treated
#' as diagnostics, not guarantees.
#'
#' @param x A supported TS/comparison object.
#' @param truth Optional truth object used instead of embedded proxy references.
#' @param checkpoints Optional checkpoint vector.
#' @param epsilon_good Numeric tolerance used to define the posterior
#'   near-optimal set in the draw-free `prob_good_selection_analogue`.
#' @param prob_best_threshold Model-relative probability-best threshold used by
#'   the stop suggestion.
#' @param prob_good_threshold Threshold for the near-optimal-set probability
#'   analogue.
#' @param eoc_threshold Threshold for the expected-opportunity-cost analogue.
#' @param stability_checkpoints Integer-like minimum run length of unchanged
#'   recommendations before the stability suggestion fires.
#' @param prob_floor Probability-best floor used to define the posterior
#'   candidate set size.
#' @param confidence_thresholds Numeric vector of thresholds used for
#'   false-confidence summaries.
#'
#' @return A list with `raw`, `checkpoint_summary`, and `threshold_summary`
#'   tables.
#' @export
bg_stopping_diagnostics <- function(
    x,
    truth = NULL,
    checkpoints = NULL,
    epsilon_good = 0.01,
    prob_best_threshold = 0.9,
    prob_good_threshold = 0.95,
    eoc_threshold = 0.01,
    stability_checkpoints = 2L,
    prob_floor = 0.05,
    confidence_thresholds = c(0.7, 0.8, 0.9, 0.95)) {
  if (!is.numeric(epsilon_good) || length(epsilon_good) != 1L || is.na(epsilon_good) || epsilon_good < 0) {
    stop("`epsilon_good` must be a nonnegative numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(prob_best_threshold) || length(prob_best_threshold) != 1L || is.na(prob_best_threshold) || prob_best_threshold <= 0 || prob_best_threshold > 1) {
    stop("`prob_best_threshold` must be a numeric scalar in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(prob_good_threshold) || length(prob_good_threshold) != 1L || is.na(prob_good_threshold) || prob_good_threshold <= 0 || prob_good_threshold > 1) {
    stop("`prob_good_threshold` must be a numeric scalar in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(eoc_threshold) || length(eoc_threshold) != 1L || is.na(eoc_threshold) || eoc_threshold < 0) {
    stop("`eoc_threshold` must be a nonnegative numeric scalar.", call. = FALSE)
  }
  stability_checkpoints <- bg_coerce_integerish(stability_checkpoints, "stability_checkpoints", 1L)
  if (stability_checkpoints < 1L) {
    stop("`stability_checkpoints` must be at least 1.", call. = FALSE)
  }
  if (!is.numeric(prob_floor) || length(prob_floor) != 1L || is.na(prob_floor) || prob_floor <= 0 || prob_floor > 1) {
    stop("`prob_floor` must be a numeric scalar in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(confidence_thresholds) || length(confidence_thresholds) < 1L || anyNA(confidence_thresholds) ||
      any(confidence_thresholds <= 0) || any(confidence_thresholds > 1)) {
    stop("`confidence_thresholds` must be a numeric vector with values in (0, 1].", call. = FALSE)
  }
  confidence_thresholds <- sort(unique(as.numeric(confidence_thresholds)))

  raw <- bg_eval_rows(
    x = x,
    truth = truth,
    checkpoints = checkpoints,
    row_builder = function(run, checkpoint, reference) {
      tab <- bg_eval_action_table_at_checkpoint(run, checkpoint, reference = reference)
      meta <- bg_eval_meta_row(run, checkpoint)
      chosen <- bg_eval_recommended_row(tab)

      if (nrow(tab) < 1L || nrow(chosen) < 1L) {
        return(cbind(
          meta,
          data.frame(
            recommended_move_label = NA_character_,
            recommended_prob_best = NA_real_,
            prob_good_selection_analogue = NA_real_,
            expected_opportunity_cost_analogue = NA_real_,
            candidate_set_size = NA_integer_,
            candidate_set_size_prob_floor = NA_integer_,
            top_two_posterior_gap = NA_real_,
            reference_screen_clear = NA,
            stringsAsFactors = FALSE
          )
        ))
      }

      prob_best <- if ("model_relative_prob_best" %in% names(tab)) tab$model_relative_prob_best else rep(NA_real_, nrow(tab))
      expected_regret <- if ("model_relative_expected_regret" %in% names(tab)) tab$model_relative_expected_regret else rep(NA_real_, nrow(tab))
      prob_best_sorted <- sort(prob_best, decreasing = TRUE, na.last = TRUE)
      prob_good <- sum(prob_best[expected_regret <= epsilon_good], na.rm = TRUE)
      candidate_set_size <- sum(expected_regret <= epsilon_good, na.rm = TRUE)
      candidate_set_size_prob_floor <- sum(prob_best >= prob_floor, na.rm = TRUE)

      reference_screen_clear <- if (is.null(reference)) {
        NA
      } else {
        cert <- bg_truth_certify(reference)
        isTRUE(cert$mc_gap_excludes_zero[[1L]] && cert$n_near_optimal[[1L]] <= 1L)
      }

      cbind(
        meta,
        data.frame(
          recommended_move_label = chosen$move_label[[1L]],
          recommended_prob_best = chosen$model_relative_prob_best[[1L]],
          prob_good_selection_analogue = prob_good,
          expected_opportunity_cost_analogue = chosen$model_relative_expected_regret[[1L]],
          candidate_set_size = candidate_set_size,
          candidate_set_size_prob_floor = candidate_set_size_prob_floor,
          top_two_posterior_gap = if (length(prob_best_sorted) >= 2L) prob_best_sorted[[1L]] - prob_best_sorted[[2L]] else NA_real_,
          reference_screen_clear = reference_screen_clear,
          stringsAsFactors = FALSE
        )
      )
    }
  )

  top1 <- bg_eval_top1(x, truth = truth, checkpoints = checkpoints)
  raw <- merge(
    raw,
    top1[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "ts_mode",
      "top1_match",
      "simple_regret",
      "selected_reference_rank"
    ), drop = FALSE],
    by = c("problem_id", "allocation_policy", "checkpoint", "seed", "ts_mode"),
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(raw) < 1L) {
    return(list(
      raw = raw,
      checkpoint_summary = bg_stopping_checkpoint_summary(raw),
      threshold_summary = bg_stopping_threshold_summary(raw, confidence_thresholds = confidence_thresholds),
      settings = list(
        epsilon_good = epsilon_good,
        prob_best_threshold = prob_best_threshold,
        prob_good_threshold = prob_good_threshold,
        eoc_threshold = eoc_threshold,
        stability_checkpoints = stability_checkpoints,
        prob_floor = prob_floor,
        confidence_thresholds = confidence_thresholds
      )
    ))
  }

  group_key <- interaction(raw$problem_id, raw$allocation_policy, raw$seed, raw$ts_mode, drop = TRUE, lex.order = TRUE)
  raw <- do.call(rbind, lapply(split(raw, group_key), bg_stopping_stability_path))
  raw$suggest_stop_prob_best <- raw$recommended_prob_best >= prob_best_threshold
  raw$suggest_stop_good_selection <- raw$prob_good_selection_analogue >= prob_good_threshold
  raw$suggest_stop_eoc <- raw$expected_opportunity_cost_analogue <= eoc_threshold
  raw$suggest_stop_stability <- raw$stable_checkpoint_run >= stability_checkpoints
  raw$suggest_stop_combined <- raw$suggest_stop_prob_best &
    raw$suggest_stop_good_selection &
    raw$suggest_stop_eoc &
    raw$suggest_stop_stability
  rownames(raw) <- NULL

  list(
    raw = raw,
    checkpoint_summary = bg_stopping_checkpoint_summary(raw),
    threshold_summary = bg_stopping_threshold_summary(raw, confidence_thresholds = confidence_thresholds),
    settings = list(
      epsilon_good = epsilon_good,
      prob_best_threshold = prob_best_threshold,
      prob_good_threshold = prob_good_threshold,
      eoc_threshold = eoc_threshold,
      stability_checkpoints = stability_checkpoints,
      prob_floor = prob_floor,
      confidence_thresholds = confidence_thresholds
    )
  )
}

# -----------------------------------------------------------------------------
# Posterior adequacy checks
# -----------------------------------------------------------------------------

bg_posterior_adequacy_inputs <- function(x, stats_table = NULL) {
  if (inherits(x, "bg_ts_run")) {
    return(list(problem = x$problem, stats_table = x$action_table))
  }
  if (inherits(x, "bg_reference")) {
    return(list(problem = x$problem, stats_table = x$action_table))
  }
  if (inherits(x, "bg_problem")) {
    if (is.null(stats_table)) {
      stop("When `x` is a `bg_problem`, `stats_table` must be supplied.", call. = FALSE)
    }
    return(list(problem = x, stats_table = stats_table))
  }

  stop("`x` must be a `bg_ts_run`, `bg_reference`, or `bg_problem`.", call. = FALSE)
}

bg_posterior_adequacy_category_l1 <- function(problem, tab) {
  if (!identical(problem$settings$reward_model_canonical, "categorical_outcome") ||
      !identical(problem$settings$posterior_model_canonical, "dirichlet_multinomial")) {
    return(rep(NA_real_, nrow(tab)))
  }

  cols <- bg_scored_outcome_columns()
  counts <- as.matrix(tab[, cols, drop = FALSE])
  totals <- rowSums(counts)
  empirical <- counts / ifelse(totals > 0L, totals, 1L)

  alpha_prior <- problem$settings$posterior_prior$alpha
  if (length(alpha_prior) == 3L) {
    counts <- cbind(
      loss = tab$losses,
      unresolved = tab$unresolved,
      win = tab$wins
    )
    totals <- rowSums(counts)
    empirical <- counts / ifelse(totals > 0L, totals, 1L)
    posterior <- sweep(counts + rep(alpha_prior, each = nrow(tab)), 1L, totals + sum(alpha_prior), "/")
    return(rowSums(abs(empirical - posterior)))
  }

  posterior <- sweep(counts + rep(alpha_prior[cols], each = nrow(tab)), 1L, totals + sum(alpha_prior[cols]), "/")
  rowSums(abs(empirical - posterior))
}

bg_posterior_safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 1L) {
    return(NA_real_)
  }
  max(x)
}

#' Check posterior adequacy against empirical rollout summaries
#'
#' `bg_posterior_adequacy()` is a lightweight, model-relative diagnostic. It
#' compares empirical rollout moments to the posterior summaries implied by the
#' chosen reward/posterior stack and flags gross mismatch heuristically.
#'
#' @param x A `bg_ts_run`, `bg_reference`, or `bg_problem`.
#' @param stats_table Optional stats table when `x` is a `bg_problem`.
#' @param draws Integer-like number of posterior draws used by the summary
#'   backend.
#' @param mean_gap_z_threshold Numeric z-style threshold used by the heuristic
#'   mismatch flag.
#' @param category_l1_threshold Numeric threshold used by the categorical
#'   frequency mismatch flag.
#' @param seed Optional seed passed to the posterior-summary backend.
#'
#' @return A list with `summary` and `action_table`.
#' @export
bg_posterior_adequacy <- function(
    x,
    stats_table = NULL,
    draws = 256L,
    mean_gap_z_threshold = 2,
    category_l1_threshold = 0.2,
    seed = NULL) {
  inputs <- bg_posterior_adequacy_inputs(x, stats_table = stats_table)
  problem <- inputs$problem
  tab <- as.data.frame(inputs$stats_table, stringsAsFactors = FALSE)
  draws <- bg_coerce_integerish(draws, "draws", 1L)
  if (!is.numeric(mean_gap_z_threshold) || length(mean_gap_z_threshold) != 1L || is.na(mean_gap_z_threshold) || mean_gap_z_threshold <= 0) {
    stop("`mean_gap_z_threshold` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(category_l1_threshold) || length(category_l1_threshold) != 1L || is.na(category_l1_threshold) || category_l1_threshold <= 0) {
    stop("`category_l1_threshold` must be a positive numeric scalar.", call. = FALSE)
  }

  if (nrow(tab) < 1L) {
    return(list(
      summary = data.frame(
        problem_id = problem$problem_id,
        reward_model = problem$settings$reward_model_canonical,
        posterior_model = problem$settings$posterior_model_canonical,
        n_actions = 0L,
        mean_abs_mean_gap = NA_real_,
        max_abs_mean_gap = NA_real_,
        mean_abs_mean_gap_z = NA_real_,
        mean_abs_variance_gap = NA_real_,
        mean_category_l1_gap = NA_real_,
        max_category_l1_gap = NA_real_,
        gross_mismatch_actions = 0L,
        stringsAsFactors = FALSE
      ),
      action_table = tab
    ))
  }

  # Legacy scalar TS paths do not always carry the full sufficient-stat set in
  # their public action tables. Reconstruct the minimal reward summaries when
  # the underlying reward model makes that safe.
  if (!all(c("reward_sum", "reward_sum_sq") %in% names(tab))) {
    if ("wins" %in% names(tab) && "losses" %in% names(tab)) {
      tab$single_loss <- if ("single_loss" %in% names(tab)) tab$single_loss else tab$losses
      tab$gammon_loss <- if ("gammon_loss" %in% names(tab)) tab$gammon_loss else 0L
      tab$backgammon_loss <- if ("backgammon_loss" %in% names(tab)) tab$backgammon_loss else 0L
      tab$unresolved <- if ("unresolved" %in% names(tab)) tab$unresolved else 0L
      tab$single_win <- if ("single_win" %in% names(tab)) tab$single_win else tab$wins
      tab$gammon_win <- if ("gammon_win" %in% names(tab)) tab$gammon_win else 0L
      tab$backgammon_win <- if ("backgammon_win" %in% names(tab)) tab$backgammon_win else 0L
      tab <- bg_recompute_rollout_rewards(problem, tab)
    }
  }

  posterior <- bg_posterior_summary_from_stats(
    problem = problem,
    stats_table = tab,
    draws = draws,
    seed = seed
  )
  empirical_mean <- ifelse(tab$allocation_count > 0L, tab$reward_sum / tab$allocation_count, NA_real_)
  empirical_variance <- ifelse(
    tab$allocation_count > 1L,
    pmax((tab$reward_sum_sq - (tab$reward_sum^2 / tab$allocation_count)) / (tab$allocation_count - 1L), 0),
    NA_real_
  )
  empirical_se <- sqrt(empirical_variance / pmax(tab$allocation_count, 1L))
  posterior_variance <- posterior$posterior_sd^2
  category_l1_gap <- bg_posterior_adequacy_category_l1(problem, tab)

  action_table <- data.frame(
    candidate_index = tab$candidate_index,
    move_label = if ("move_label" %in% names(tab)) tab$move_label else NA_character_,
    allocation_count = tab$allocation_count,
    empirical_mean = empirical_mean,
    posterior_mean = posterior$estimate,
    mean_gap = posterior$estimate - empirical_mean,
    abs_mean_gap = abs(posterior$estimate - empirical_mean),
    abs_mean_gap_z = abs((posterior$estimate - empirical_mean) / ifelse(is.finite(empirical_se) & empirical_se > 0, empirical_se, NA_real_)),
    empirical_variance = empirical_variance,
    posterior_variance = posterior_variance,
    abs_variance_gap = abs(posterior_variance - empirical_variance),
    category_l1_gap = category_l1_gap,
    stringsAsFactors = FALSE
  )
  action_table$gross_mismatch <- (!is.na(action_table$abs_mean_gap_z) & action_table$abs_mean_gap_z >= mean_gap_z_threshold) |
    (!is.na(action_table$category_l1_gap) & action_table$category_l1_gap >= category_l1_threshold)

  summary <- data.frame(
    problem_id = problem$problem_id,
    reward_model = problem$settings$reward_model_canonical,
    posterior_model = problem$settings$posterior_model_canonical,
    n_actions = nrow(action_table),
    mean_abs_mean_gap = mean(action_table$abs_mean_gap, na.rm = TRUE),
    max_abs_mean_gap = bg_posterior_safe_max(action_table$abs_mean_gap),
    mean_abs_mean_gap_z = mean(action_table$abs_mean_gap_z, na.rm = TRUE),
    mean_abs_variance_gap = mean(action_table$abs_variance_gap, na.rm = TRUE),
    mean_category_l1_gap = mean(action_table$category_l1_gap, na.rm = TRUE),
    max_category_l1_gap = bg_posterior_safe_max(action_table$category_l1_gap),
    gross_mismatch_actions = sum(action_table$gross_mismatch, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  list(summary = summary, action_table = action_table)
}

# -----------------------------------------------------------------------------
# Backend parity diagnostics
# -----------------------------------------------------------------------------

bg_backend_parity_checkpoint_diff <- function(fast_run, explicit_run, checkpoint) {
  fast_tab <- bg_eval_action_table_at_checkpoint(fast_run, checkpoint, reference = fast_run$reference)
  explicit_tab <- bg_eval_action_table_at_checkpoint(explicit_run, checkpoint, reference = explicit_run$reference)
  merged <- merge(
    fast_tab[, c("candidate_index", "allocation_count", "estimate", "model_relative_prob_best"), drop = FALSE],
    explicit_tab[, c("candidate_index", "allocation_count", "estimate", "model_relative_prob_best"), drop = FALSE],
    by = "candidate_index",
    suffixes = c("_fast", "_explicit"),
    all = TRUE,
    sort = FALSE
  )
  merged[is.na(merged)] <- 0

  fast_total <- sum(merged$allocation_count_fast)
  explicit_total <- sum(merged$allocation_count_explicit)
  fast_share <- if (fast_total > 0L) merged$allocation_count_fast / fast_total else rep(0, nrow(merged))
  explicit_share <- if (explicit_total > 0L) merged$allocation_count_explicit / explicit_total else rep(0, nrow(merged))

  data.frame(
    checkpoint = checkpoint,
    allocation_share_l1 = sum(abs(fast_share - explicit_share)),
    max_estimate_gap = max(abs(merged$estimate_fast - merged$estimate_explicit), na.rm = TRUE),
    max_prob_best_gap = max(abs(merged$model_relative_prob_best_fast - merged$model_relative_prob_best_explicit), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Compare fast and explicit TS backends on the same scalar stack
#'
#' `bg_backend_parity()` runs canonical TS or TTTS through both the legacy fast
#' scalar backend and the explicit posterior backend, then reports how closely
#' the two paths agree checkpoint by checkpoint.
#'
#' @param problem A `bg_problem` object on the `scalar_payoff + beta_pseudo`
#'   stack.
#' @param allocation_policy Either `"thompson"` or `"top_two_thompson"`.
#' @param budgets Integer-like checkpoint vector.
#' @param seeds Integer-like seed vector.
#' @param proxy_reference Optional proxy reference. When omitted, one is built
#'   automatically.
#' @param reference_budget Integer-like budget used when auto-building the proxy
#'   reference.
#' @param ttts_beta Probability of keeping the Thompson winner in TTTS.
#' @param progress Logical scalar; if `TRUE`, display progress.
#'
#' @return A list with `raw`, `summary`, and the shared `reference`.
#' @keywords internal
#' @noRd
bg_backend_parity <- function(
    problem,
    allocation_policy = c("thompson", "top_two_thompson"),
    budgets = c(32L, 64L, 128L, 256L),
    seeds = 1:10,
    proxy_reference = NULL,
    reference_budget = max(4096L, 8L * max(budgets)),
    ttts_beta = 0.5,
    progress = interactive()) {
  if (!inherits(problem, "bg_problem")) {
    stop("`problem` must inherit from class 'bg_problem'.", call. = FALSE)
  }
  if (!bg_problem_uses_fast_ts_path(problem)) {
    stop(
      "`bg_backend_parity()` is currently defined only for `scalar_payoff + beta_pseudo` problems.",
      call. = FALSE
    )
  }

  allocation_policy <- match.arg(allocation_policy)
  budgets <- bg_normalize_study_budgets(budgets)
  seeds <- sort(unique(bg_coerce_integerish(seeds, "seeds", length(seeds))))
  bg_assert_scalar_flag(progress, "progress")

  if (is.null(proxy_reference)) {
    proxy_reference <- bg_reference(
      problem = problem,
      budget = reference_budget,
      seed = bg_derive_seed(min(seeds), "backend-parity-reference")
    )
  }

  seed_rows <- bg_task_apply(
    tasks = as.list(seeds),
    n_cores = 1L,
    parallel = FALSE,
    progress = progress,
    progress_label = "backend parity runs",
    worker = function(seed_i) {
      fast_run <- bg_ts_decide(
        problem = problem,
        budget = max(budgets),
        allocation_policy = allocation_policy,
        proxy_reference = proxy_reference,
        checkpoints = budgets,
        ts_mode = "sequential",
        seed = seed_i,
        ttts_beta = ttts_beta
      )
      explicit_run <- bg_run_posterior_ts(
        problem = problem,
        allocation_policy = allocation_policy,
        budget = max(budgets),
        checkpoints = budgets,
        reference = proxy_reference,
        seed = seed_i,
        ttts_beta = ttts_beta
      )

      top1_fast <- bg_eval_top1(fast_run)
      top1_explicit <- bg_eval_top1(explicit_run)
      merged <- merge(
        top1_fast[, c("checkpoint", "recommended_index", "recommended_move_label", "top1_match", "simple_regret", "recommended_prob_best"), drop = FALSE],
        top1_explicit[, c("checkpoint", "recommended_index", "recommended_move_label", "top1_match", "simple_regret", "recommended_prob_best"), drop = FALSE],
        by = "checkpoint",
        suffixes = c("_fast", "_explicit"),
        sort = FALSE
      )
      diffs <- do.call(
        rbind,
        lapply(budgets, function(ck) bg_backend_parity_checkpoint_diff(fast_run, explicit_run, ck))
      )
      out <- merge(merged, diffs, by = "checkpoint", all.x = TRUE, sort = FALSE)
      out$seed <- seed_i
      out$allocation_policy <- allocation_policy
      out$recommended_match <- out$recommended_index_fast == out$recommended_index_explicit
      out$simple_regret_gap <- out$simple_regret_fast - out$simple_regret_explicit
      out$prob_best_gap <- out$recommended_prob_best_fast - out$recommended_prob_best_explicit
      out
    }
  )

  raw <- do.call(rbind, seed_rows)
  rownames(raw) <- NULL
  summary <- aggregate(
    raw[, c("recommended_match", "simple_regret_gap", "prob_best_gap", "allocation_share_l1", "max_estimate_gap", "max_prob_best_gap")],
    by = list(
      allocation_policy = raw$allocation_policy,
      checkpoint = raw$checkpoint
    ),
    FUN = mean,
    na.rm = TRUE
  )
  rownames(summary) <- NULL

  list(
    raw = raw,
    summary = summary,
    reference = proxy_reference,
    settings = list(
      allocation_policy = allocation_policy,
      budgets = budgets,
      seeds = seeds,
      ttts_beta = ttts_beta
    )
  )
}

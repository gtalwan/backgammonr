# Public evaluation summaries for TS runs, comparison studies, and proxy references.
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
      weighted_rank_loss = NA_real_
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
      weighted_rank_loss = NA_real_
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

  pairs <- utils::combn(common_ids, 2L)
  truth_sign <- sign(truth_values[pairs[1L, ]] - truth_values[pairs[2L, ]])
  est_sign <- sign(est_values[pairs[1L, ]] - est_values[pairs[2L, ]])
  valid <- truth_sign != 0 & est_sign != 0
  pairwise_accuracy <- if (any(valid)) {
    mean(truth_sign[valid] == est_sign[valid])
  } else {
    NA_real_
  }
  pairwise_disagreement_count <- if (any(valid)) {
    sum(truth_sign[valid] != est_sign[valid])
  } else {
    NA_real_
  }

  list(
    spearman = suppressWarnings(stats::cor(est_ranks, truth_ranks, method = "spearman")),
    kendall = suppressWarnings(stats::cor(est_ranks, truth_ranks, method = "kendall")),
    top_k_overlap = overlap_n / top_k,
    top_k_overlap_n = overlap_n,
    pairwise_ordering_accuracy = pairwise_accuracy,
    pairwise_disagreement_count = pairwise_disagreement_count,
    weighted_rank_loss = weighted_rank_loss
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
#' @keywords internal
#' @noRd
bg_eval_topk <- function(x, truth = NULL, checkpoints = NULL, k = 3L) {
  rank_out <- bg_eval_rank(x, truth = truth, checkpoints = checkpoints, top_k = k)
  top1_out <- bg_eval_top1(x, truth = truth, checkpoints = checkpoints)
  merge(
    rank_out[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "top_k",
      "top_k_overlap",
      "top_k_overlap_n"
    ), drop = FALSE],
    top1_out[, c(
      "problem_id",
      "allocation_policy",
      "checkpoint",
      "seed",
      "recommended_move_label",
      "selected_reference_rank",
      "posterior_top_k_mass"
    ), drop = FALSE],
    by = c("problem_id", "allocation_policy", "checkpoint", "seed"),
    all = TRUE,
    sort = FALSE
  )
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
      share_best_truth <- NA_real_
      share_mc_screened_suboptimal <- NA_real_
      mc_screened_suboptimal_count <- NA_integer_

      if (nrow(tab) > 0L && !is.null(reference)) {
        ref <- bg_eval_reference_lookup(reference)
        ref_tab <- ref$table[order(ref$table$rank), , drop = FALSE]
        top_k_ids <- ref_tab$candidate_index[seq_len(min(top_k, nrow(ref_tab)))]
        tab_counts <- stats::setNames(tab$allocation_count, tab$candidate_index)
        total_alloc <- sum(tab$allocation_count)

        share_top_k_truth <- sum(tab_counts[as.character(top_k_ids)], na.rm = TRUE) / total_alloc
        share_best_truth <- tab_counts[[as.character(ref$best_index)]] / total_alloc

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
          allocation_max_share = concentration$max_share,
          share_top_k_truth = share_top_k_truth,
          share_best_truth = share_best_truth,
          share_mc_screened_suboptimal = share_mc_screened_suboptimal,
          mc_screened_suboptimal_count = mc_screened_suboptimal_count,
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
    "spearman",
    "kendall",
    "top_k",
    "top_k_overlap",
    "pairwise_ordering_accuracy",
    "weighted_rank_loss",
    "total_allocation",
    "n_allocated_actions",
    "allocation_entropy",
    "allocation_hhi",
    "allocation_max_share",
    "share_top_k_truth",
    "share_best_truth",
    "share_mc_screened_suboptimal",
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

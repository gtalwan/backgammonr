# Pure-R fallbacks for native convenience summaries.
#
# These helpers exist for development sessions where:
# - the installed/loaded DLL is older than the current source tree; or
# - the machine cannot rebuild the package immediately.
#
# They intentionally cover only the small summary helpers that are easy to
# reproduce in R. Core engine functionality still depends on the compiled
# backgammon DLL.

if (!exists("bg_cpp_reference_summary", inherits = FALSE)) {
  bg_cpp_reference_summary <- function(
      allocation_count,
      unresolved,
      reward_sum,
      reward_sum_sq,
      prior_alpha,
      prior_beta) {
    n_actions <- length(allocation_count)
    if (!all(lengths(list(unresolved, reward_sum, reward_sum_sq)) == n_actions)) {
      stop("Reference-summary inputs must all have the same length.", call. = FALSE)
    }

    n <- as.numeric(allocation_count)
    reference_mean <- ifelse(n > 0, reward_sum / n, NA_real_)
    sample_variance <- ifelse(
      n > 1,
      pmax((reward_sum_sq - ((reward_sum * reward_sum) / n)) / (n - 1), 0),
      NA_real_
    )
    variance_for_se <- ifelse(is.finite(sample_variance), sample_variance, 0)
    reference_se <- ifelse(n > 0, sqrt(pmax(variance_for_se, 0) / n), NA_real_)

    data.frame(
      reference_mean = reference_mean,
      sample_variance = sample_variance,
      reference_se = reference_se,
      reference_mc_lower_95 = pmin(pmax(reference_mean - 1.96 * reference_se, 0), 1),
      reference_mc_upper_95 = pmin(pmax(reference_mean + 1.96 * reference_se, 0), 1),
      reference_interval_type = rep("mc_normal_approx", n_actions),
      reference_alpha = ifelse(n > 0, as.numeric(prior_alpha) + reward_sum, NA_real_),
      reference_beta = ifelse(n > 0, as.numeric(prior_beta) + (n - reward_sum), NA_real_),
      unresolved_fraction = ifelse(n > 0, as.numeric(unresolved) / n, NA_real_),
      stringsAsFactors = FALSE
    )
  }
}

if (!exists("bg_cpp_eval_path_metrics", inherits = FALSE)) {
  bg_cpp_eval_path_metrics <- function(
      checkpoint,
      runtime_seconds,
      top1_match,
      epsilon_optimal,
      simple_regret,
      recommended_prob_best) {
    n <- length(checkpoint)
    if (!all(lengths(list(runtime_seconds, top1_match, epsilon_optimal, simple_regret, recommended_prob_best)) == n)) {
      stop("Path-metric inputs must all have the same length.", call. = FALSE)
    }
    if (n < 1L) {
      stop("Path metrics require at least one checkpoint.", call. = FALSE)
    }

    ord <- order(checkpoint)
    first_budget_top1_match <- NA_real_
    first_budget_epsilon_optimal <- NA_real_
    first_runtime_top1_match <- NA_real_
    first_runtime_epsilon_optimal <- NA_real_
    max_runtime_seconds <- NA_real_

    top1_numeric <- ifelse(is.na(top1_match), NA_real_, as.numeric(top1_match))
    brier_values <- rep(NA_real_, n)

    for (idx in ord) {
      if (!is.na(top1_match[[idx]]) && isTRUE(top1_match[[idx]]) && is.na(first_budget_top1_match)) {
        first_budget_top1_match <- checkpoint[[idx]]
        first_runtime_top1_match <- runtime_seconds[[idx]]
      }
      if (!is.na(epsilon_optimal[[idx]]) && isTRUE(epsilon_optimal[[idx]]) && is.na(first_budget_epsilon_optimal)) {
        first_budget_epsilon_optimal <- checkpoint[[idx]]
        first_runtime_epsilon_optimal <- runtime_seconds[[idx]]
      }
      if (is.finite(runtime_seconds[[idx]])) {
        max_runtime_seconds <- runtime_seconds[[idx]]
      }
      if (!is.na(top1_match[[idx]]) && is.finite(recommended_prob_best[[idx]])) {
        outcome <- if (isTRUE(top1_match[[idx]])) 1 else 0
        diff <- pmin(pmax(recommended_prob_best[[idx]], 0), 1) - outcome
        brier_values[[idx]] <- diff * diff
      }
    }

    trapezoid_area <- function(x, y) {
      valid <- is.finite(x) & is.finite(y)
      x <- x[valid]
      y <- y[valid]
      if (length(x) < 2L) {
        return(NA_real_)
      }
      ord_xy <- order(x)
      x <- x[ord_xy]
      y <- y[ord_xy]
      sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
    }

    list(
      first_budget_top1_match = first_budget_top1_match,
      first_budget_epsilon_optimal = first_budget_epsilon_optimal,
      first_runtime_top1_match = first_runtime_top1_match,
      first_runtime_epsilon_optimal = first_runtime_epsilon_optimal,
      auc_top1_match = trapezoid_area(checkpoint, top1_numeric),
      auc_simple_regret = trapezoid_area(checkpoint, simple_regret),
      mean_brier_top1 = if (all(is.na(brier_values))) NA_real_ else mean(brier_values, na.rm = TRUE),
      n_checkpoints = n,
      max_checkpoint = max(checkpoint),
      max_runtime_seconds = max_runtime_seconds
    )
  }
}

if (!exists("bg_cpp_calibration_summary", inherits = FALSE)) {
  bg_cpp_calibration_summary <- function(predicted_prob, observed_top1, bins) {
    if (length(predicted_prob) != length(observed_top1)) {
      stop("Calibration inputs must have the same length.", call. = FALSE)
    }
    bins <- bg_coerce_integerish(bins, "bins", 1L)
    if (bins < 1L) {
      stop("`bins` must be at least 1.", call. = FALSE)
    }

    out <- data.frame(
      calibration_bin = seq_len(bins),
      bin_lower = (seq_len(bins) - 1L) / bins,
      bin_upper = seq_len(bins) / bins,
      n = 0L,
      mean_predicted_prob_best = NA_real_,
      observed_top1_rate = NA_real_,
      mean_brier_top1 = NA_real_,
      calibration_gap = NA_real_,
      ece_component = NA_real_,
      stringsAsFactors = FALSE
    )

    valid <- is.finite(predicted_prob) & is.finite(observed_top1)
    if (!any(valid)) {
      return(out)
    }

    p <- pmin(pmax(predicted_prob[valid], 0), 1)
    y <- ifelse(observed_top1[valid] > 0, 1, 0)
    bin_id <- pmin(floor(p * bins) + 1L, bins)

    for (i in seq_len(bins)) {
      idx <- which(bin_id == i)
      if (length(idx) < 1L) {
        next
      }
      out$n[[i]] <- length(idx)
      out$mean_predicted_prob_best[[i]] <- mean(p[idx])
      out$observed_top1_rate[[i]] <- mean(y[idx])
      out$mean_brier_top1[[i]] <- mean((p[idx] - y[idx])^2)
      out$calibration_gap[[i]] <- out$observed_top1_rate[[i]] - out$mean_predicted_prob_best[[i]]
      out$ece_component[[i]] <- abs(out$calibration_gap[[i]]) * (length(idx) / length(p))
    }

    out
  }
}

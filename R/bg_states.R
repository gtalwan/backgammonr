# State classification, hardness features, and later-state pilot workflows.
#
# The opening battery is the main laboratory, but this file handles the
# descriptive state features that support hardness analysis and the small
# non-opening pilot battery used to test whether opening findings generalize.
bg_player_point_counts <- function(board, player) {
  if (player == 1L) {
    return(pmax(board$points, 0L))
  }
  pmax(-board$points, 0L)
}

bg_player_occupied_points <- function(board, player) {
  which(bg_player_point_counts(board, player) > 0L)
}

bg_player_home_points <- function(player) {
  if (player == 1L) {
    return(1:6)
  }
  19:24
}

bg_player_opponent_home_points <- function(player) {
  if (player == 1L) {
    return(19:24)
  }
  1:6
}

bg_board_is_opening_position <- function(board) {
  identical(unclass(board), unclass(bg_initial_board()))
}

bg_player_all_in_home <- function(board, player) {
  if (board$bar[[if (player == 1L) 1L else 2L]] > 0L) {
    return(FALSE)
  }
  occupied <- bg_player_occupied_points(board, player)
  if (length(occupied) == 0L) {
    return(TRUE)
  }
  all(occupied %in% bg_player_home_points(player))
}

bg_players_in_contact <- function(board) {
  if (sum(board$bar) > 0L) {
    return(TRUE)
  }

  p1_points <- bg_player_occupied_points(board, 1L)
  p2_points <- bg_player_occupied_points(board, -1L)
  if (length(p1_points) == 0L || length(p2_points) == 0L) {
    return(FALSE)
  }

  max(p1_points) > min(p2_points)
}

bg_player_prime_length <- function(board, player) {
  counts <- bg_player_point_counts(board, player)
  made <- counts >= 2L
  if (!any(made)) {
    return(0L)
  }

  runs <- rle(made)
  max(runs$lengths[runs$values])
}

bg_player_anchor_count <- function(board, player) {
  counts <- bg_player_point_counts(board, player)
  sum(counts[bg_player_opponent_home_points(player)] >= 2L)
}

bg_prefixed_feature_row <- function(df, prefix) {
  names(df) <- paste0(prefix, names(df))
  df
}

bg_state_class_label <- function(
    board,
    contact,
    player_all_home,
    opponent_all_home,
    player_prime,
    opponent_prime,
    player_anchors,
    opponent_anchors,
    current_features,
    opponent_features) {
  if (bg_board_is_opening_position(board)) {
    return("opening")
  }

  if (player_all_home && opponent_all_home && (sum(board$off) > 0L)) {
    return("bear_off")
  }

  if (!contact && (sum(board$off) > 0L || player_all_home || opponent_all_home)) {
    return("bear_in")
  }

  if (!contact) {
    return("race")
  }

  if (sum(board$bar) > 0L &&
      max(current_features$own_home_made_points, opponent_features$own_home_made_points) >= 3L) {
    return("blitz")
  }

  if (max(player_prime, opponent_prime) >= 5L) {
    return("priming_game")
  }

  if (max(player_anchors, opponent_anchors) >= 2L) {
    return("backgame")
  }

  if (max(player_anchors, opponent_anchors) >= 1L) {
    return("holding_game")
  }

  "mutual_contact"
}

#' Heuristically classify a board state
#'
#' `bg_state_classify()` provides a pragmatic state taxonomy for experiment
#' design. The labels are heuristic and designed for statistical stratification,
#' not for exact strategic adjudication.
#'
#' @param x A `bg_board` or `bg_problem` object.
#' @param player Optional player viewpoint. Defaults to the player to move.
#'
#' @return A one-row data frame of state features and a heuristic `state_class`.
#' @export
bg_state_classify <- function(x, player = NULL) {
  problem <- if (inherits(x, "bg_problem")) x else NULL
  board <- if (inherits(x, "bg_problem")) x$board else x

  if (!is_bg_board(board)) {
    stop("`x` must inherit from class 'bg_board' or 'bg_problem'.", call. = FALSE)
  }

  if (is.null(player)) {
    player <- board$turn
  }
  if (!player %in% c(1L, -1L)) {
    stop("`player` must be 1L or -1L when supplied.", call. = FALSE)
  }

  current_features <- bg_board_features(board, player = player)$board_features
  opponent_features <- bg_board_features(board, player = -player)$board_features
  player_all_home <- bg_player_all_in_home(board, player)
  opponent_all_home <- bg_player_all_in_home(board, -player)
  contact <- bg_players_in_contact(board)
  player_prime <- bg_player_prime_length(board, player)
  opponent_prime <- bg_player_prime_length(board, -player)
  player_anchors <- bg_player_anchor_count(board, player)
  opponent_anchors <- bg_player_anchor_count(board, -player)
  state_class <- bg_state_class_label(
    board = board,
    contact = contact,
    player_all_home = player_all_home,
    opponent_all_home = opponent_all_home,
    player_prime = player_prime,
    opponent_prime = opponent_prime,
    player_anchors = player_anchors,
    opponent_anchors = opponent_anchors,
    current_features = current_features,
    opponent_features = opponent_features
  )

  cbind(
    data.frame(
      problem_id = if (is.null(problem)) NA_character_ else problem$problem_id,
      state_class = state_class,
      exact_opening = bg_board_is_opening_position(board),
      contact = contact,
      player_all_in_home = player_all_home,
      opponent_all_in_home = opponent_all_home,
      player_prime_length = player_prime,
      opponent_prime_length = opponent_prime,
      player_anchor_count = player_anchors,
      opponent_anchor_count = opponent_anchors,
      n_legal_moves = if (is.null(problem)) NA_integer_ else nrow(problem$candidate_table),
      stringsAsFactors = FALSE
    ),
    bg_prefixed_feature_row(current_features, "current_"),
    bg_prefixed_feature_row(opponent_features, "opponent_")
  )
}

#' Summarize state difficulty
#'
#' @param x A `bg_problem`, `bg_truth_state`, `bg_reference`, or `bg_board`
#'   object.
#' @param truth Optional truth object used when `x` is a `bg_problem` or
#'   `bg_board`.
#' @param near_optimal_tol Numeric tolerance used to count near-optimal actions.
#'
#' @return A one-row data frame with structural and truth-aware difficulty
#'   features.
#' @export
bg_state_difficulty <- function(x, truth = NULL, near_optimal_tol = 0.01) {
  reference <- NULL
  problem <- NULL

  if (inherits(x, "bg_truth_state")) {
    problem <- x$problem
    reference <- x$reference
  } else if (inherits(x, "bg_reference")) {
    problem <- x$problem
    reference <- x
  } else if (inherits(x, "bg_problem")) {
    problem <- x
    if (!is.null(truth)) {
      reference <- bg_normalize_truth_reference(truth, "truth")
    }
  } else if (is_bg_board(x)) {
    problem <- NULL
    if (!is.null(truth)) {
      reference <- bg_normalize_truth_reference(truth, "truth")
    }
  } else {
    stop(
      "`x` must be a `bg_problem`, `bg_truth_state`, `bg_reference`, or `bg_board`.",
      call. = FALSE
    )
  }

  class_df <- bg_state_classify(if (is.null(problem)) x else problem)

  if (is.null(reference)) {
    class_df$top_two_gap_estimate <- NA_real_
    class_df$n_near_optimal <- NA_integer_
    class_df$mc_not_separated_from_best_set_size <- NA_integer_
    class_df$mean_reference_se <- NA_real_
    class_df$max_reference_se <- NA_real_
    class_df$variance_ratio <- NA_real_
    class_df$difficulty_score <- NA_real_
    return(class_df)
  }

  truth_diag <- bg_truth_reference_metrics(reference, near_optimal_tol = near_optimal_tol)
  move_table <- truth_diag$move_table
  positive_var <- move_table$sample_variance[is.finite(move_table$sample_variance) & move_table$sample_variance > 0]
  variance_ratio <- if (length(positive_var) >= 2L) {
    max(positive_var) / min(positive_var)
  } else {
    NA_real_
  }

  top_gap <- truth_diag$summary$top_two_gap_estimate[[1L]]
  difficulty_score <- if (is.finite(top_gap) && top_gap > 0) {
    log1p(class_df$n_legal_moves[[1L]]) *
      (1 + truth_diag$summary$mean_reference_se[[1L]]) / top_gap
  } else {
    NA_real_
  }

  class_df$top_two_gap_estimate <- top_gap
  class_df$n_near_optimal <- truth_diag$summary$n_near_optimal[[1L]]
  class_df$mc_not_separated_from_best_set_size <- truth_diag$summary$mc_not_separated_from_best_set_size[[1L]]
  class_df$mean_reference_se <- truth_diag$summary$mean_reference_se[[1L]]
  class_df$max_reference_se <- truth_diag$summary$max_reference_se[[1L]]
  class_df$variance_ratio <- variance_ratio
  class_df$difficulty_score <- difficulty_score
  class_df
}

bg_state_battery_turn_indices <- function(game, positions_per_game) {
  positions_per_game <- bg_coerce_integerish(positions_per_game, "positions_per_game", 1L)
  n_turns <- length(game$turns)
  if (n_turns == 0L) {
    return(integer(0L))
  }
  unique(pmax(1L, pmin(n_turns, round(seq(1, n_turns, length.out = min(positions_per_game, n_turns))))))
}

#' Build a battery of game-derived states
#'
#' `bg_state_battery()` samples states from generated games, attaches a coarse
#' taxonomy, and can optionally build proxy truths and method comparisons on the
#' resulting decision problems.
#'
#' @param n_games Integer-like number of games to sample.
#' @param positions_per_game Integer-like number of local decision problems to
#'   extract from each sampled game.
#' @param board Initial board used for game generation.
#' @param sample_selection Game-generation policy.
#' @param simulation_policy Rollout continuation policy used for the extracted
#'   decision problems.
#' @param max_turns Integer-like game length cap.
#' @param seeds Integer-like seed vector for game generation.
#' @param reference_budget Optional proxy-truth budget. When supplied, a truth
#'   battery is built for the sampled states.
#' @param methods Optional method vector. When supplied together with `budgets`,
#'   a comparison study is attached.
#' @param budgets Optional budget vector for the attached comparison study.
#' @param comparison_seeds Integer-like seed vector for the attached comparison
#'   study.
#' @param n_cores Integer-like worker count used inside proxy-truth generation.
#' @param parallel Logical scalar; if `FALSE`, force one worker.
#' @param cache Logical scalar; if `TRUE`, reuse cached truth objects.
#' @param cache_dir Optional cache directory.
#' @param save_path Optional `.rds` path for the returned battery object.
#' @param overwrite Logical scalar controlling cached truth replacement.
#' @param verbose Logical scalar; if `TRUE`, display a progress bar.
#'
#' @return A `bg_state_battery` object.
#' @keywords internal
#' @noRd
bg_state_battery <- function(
    n_games = 8L,
    positions_per_game = 4L,
    board = bg_initial_board(),
    sample_selection = c("random", "aggressive", "defensive"),
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    max_turns = 80L,
    seeds = NULL,
    reference_budget = NULL,
    methods = NULL,
    budgets = NULL,
    comparison_seeds = 1:10,
    n_cores = bg_default_workers_truth(),
    parallel = TRUE,
    cache = FALSE,
    cache_dir = NULL,
    save_path = NULL,
    overwrite = FALSE,
    verbose = interactive()) {
  n_games <- bg_coerce_integerish(n_games, "n_games", 1L)
  positions_per_game <- bg_coerce_integerish(positions_per_game, "positions_per_game", 1L)
  sample_selection <- match.arg(sample_selection)
  simulation_policy <- bg_match_simulation_policy_public(simulation_policy)
  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  bg_assert_scalar_flag(verbose, "verbose")

  if (is.null(seeds)) {
    seeds <- seq_len(n_games)
  }
  seeds <- bg_coerce_integerish(seeds, "seeds", length(seeds))
  n_games <- length(seeds)

  pb <- if (isTRUE(verbose)) {
    utils::txtProgressBar(min = 0, max = n_games, style = 3)
  } else {
    NULL
  }
  on.exit(if (!is.null(pb)) close(pb), add = TRUE)

  sampled_games <- vector("list", n_games)
  problems <- list()
  state_rows <- list()
  problem_id <- 1L

  for (i in seq_along(seeds)) {
    game <- bg_play_game(
      board = board,
      selection = sample_selection,
      max_turns = max_turns,
      seed = seeds[[i]]
    )
    sampled_games[[i]] <- game

    turn_idx <- bg_state_battery_turn_indices(game, positions_per_game = positions_per_game)
    for (idx in turn_idx) {
      turn <- game$turns[[idx]]
      problem <- bg_problem(
        state = turn$board_before,
        roll = turn$roll,
        simulation_policy = simulation_policy,
        problem_id = sprintf("game_%03d_turn_%03d", i, idx)
      )
      class_df <- bg_state_classify(problem)
      class_df$game_id <- i
      class_df$turn_index <- idx
      class_df$roll <- bg_truth_roll_label(turn$roll)

      problems[[problem_id]] <- problem
      state_rows[[problem_id]] <- class_df
      problem_id <- problem_id + 1L
    }

    if (!is.null(pb)) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  names(problems) <- vapply(problems, function(x) x$problem_id, character(1L))
  state_table <- do.call(rbind, state_rows)
  rownames(state_table) <- NULL

  truths <- NULL
  if (!is.null(reference_budget)) {
    truths <- bg_truth_battery(
      problems = problems,
      budget = reference_budget,
      n_cores = n_cores,
      parallel = parallel,
      cache = cache,
      cache_dir = cache_dir,
      overwrite = overwrite,
      verbose = FALSE,
      seed = min(seeds)
    )
    difficulty_rows <- do.call(
      rbind,
      lapply(
        truths$truths,
        function(truth_obj) bg_state_difficulty(truth_obj)
      )
    )
    state_table <- merge(
      state_table,
      difficulty_rows[, c(
        "problem_id",
        "top_two_gap_estimate",
        "n_near_optimal",
        "mc_not_separated_from_best_set_size",
        "mean_reference_se",
        "max_reference_se",
        "variance_ratio",
        "difficulty_score"
      ), drop = FALSE],
      by = "problem_id",
      all.x = TRUE,
      sort = FALSE
    )
  }

  comparison <- NULL
  if (!is.null(methods) && !is.null(budgets)) {
    proxy_refs <- if (is.null(truths)) NULL else lapply(truths$truths, `[[`, "reference")
    comparison <- bg_compare_methods(
      problems = problems,
      methods = methods,
      budgets = budgets,
      seeds = comparison_seeds,
      proxy_references = proxy_refs,
      n_cores = n_cores,
      parallel = parallel,
      progress = verbose
    )
  }

  out <- structure(
    list(
      problems = problems,
      state_table = state_table,
      sampled_games = sampled_games,
      truths = truths,
      comparison = comparison,
      settings = list(
        n_games = n_games,
        positions_per_game = positions_per_game,
        sample_selection = sample_selection,
        simulation_policy = simulation_policy,
        max_turns = max_turns,
        seeds = seeds,
        reference_budget = reference_budget,
        methods = methods,
        budgets = budgets,
        comparison_seeds = comparison_seeds,
        n_cores = n_cores,
        parallel = isTRUE(parallel)
      )
    ),
    class = "bg_state_battery"
  )
  if (!is.null(save_path)) {
    bg_study_save(out, save_path, overwrite = overwrite)
  }
  out
}

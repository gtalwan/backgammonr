# Legacy benchmarking workflows built around the scalar rollout engine.
bg_normalize_benchmark_methods <- function(methods, arg_name = "methods") {
  if (!is.character(methods) || length(methods) < 1L || anyNA(methods)) {
    stop(sprintf("`%s` must be a non-missing character vector.", arg_name), call. = FALSE)
  }

  methods <- vapply(methods, bg_match_simulation_archetype, character(1L), USE.NAMES = FALSE)

  if (anyDuplicated(methods)) {
    stop(sprintf("`%s` must not contain duplicates.", arg_name), call. = FALSE)
  }

  methods
}

bg_new_matchup_benchmark <- function(x) {
  x$initial_board <- bg_new_board(x$initial_board)
  x$games <- as.data.frame(x$games, stringsAsFactors = FALSE)
  x$summary <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  x$settings <- list(
    player1_selection = as.character(x$settings$player1_selection[[1L]]),
    player2_selection = as.character(x$settings$player2_selection[[1L]]),
    n_games = as.integer(x$settings$n_games[[1L]]),
    max_turns = as.integer(x$settings$max_turns[[1L]]),
    used_scripted_rolls = isTRUE(x$settings$used_scripted_rolls[[1L]]),
    rollout_budget = as.integer(x$settings$rollout_budget[[1L]]),
    rollout_policy = as.character(x$settings$rollout_policy[[1L]]),
    max_rollout_turns = as.integer(x$settings$max_rollout_turns[[1L]]),
    runtime_seconds = as.numeric(x$settings$runtime_seconds[[1L]])
  )
  structure(x, class = "bg_matchup_benchmark")
}

#' Test whether an object is a matchup-benchmark result
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_matchup_benchmark"`.
is_bg_matchup_benchmark <- function(x) {
  inherits(x, "bg_matchup_benchmark")
}

#' Benchmark a matchup between two archetypes
#'
#' Runs many games between two specified archetypes and records both per-game
#' outcomes and aggregate runtime-aware summary statistics.
#'
#' The per-game `games` component is easy to convert to a tibble or other R data
#' frame abstraction, while the one-row `summary` component provides headline
#' benchmarking metrics such as win rate, average turns, and elapsed runtime.
#'
#' Supported archetypes are:
#'
#' - `"random"`
#' - `"aggressive"`
#' - `"defensive"`
#' - `"rollout"`
#' - `"ocba_rollout"`
#' - `"thompson_rollout"`
#' - `"ttts_rollout"`
#'
#' @param player1 Archetype for player 1.
#' @param player2 Archetype for player 2.
#' @param n_games Integer-like scalar giving the number of games to simulate.
#' @param board A `bg_board` object giving the initial position for each game.
#' @param max_turns Integer-like scalar giving the maximum number of turns per
#'   game.
#' @param roll_sequence Optional list of rolls or a single `bg_roll` object.
#'   When supplied, the same scripted roll sequence is used for every
#'   replication.
#' @param seed Optional integer-like scalar for reproducible stochastic
#'   simulations.
#' @param rollout_budget Integer-like scalar giving the fixed total rollout budget for
#'   rollout-based players.
#' @param rollout_policy Baseline policy used inside rollout playouts.
#' @param max_rollout_turns Integer-like scalar giving the maximum number of
#'   turns for each rollout playout.
#'
#' @return An object of class `bg_matchup_benchmark` with components:
#'   - `games`: one row per simulated game;
#'   - `summary`: one-row aggregate benchmark summary;
#'   - `settings`: benchmark settings;
#'   - `initial_board`: the starting board.
#'
#'
#' @examples
#' board <- bg_initial_board()
#' bm <- benchmark_matchup(
#'   player1 = "random",
#'   player2 = "aggressive",
#'   n_games = 3L,
#'   board = board,
#'   max_turns = 20L,
#'   seed = 123
#' )
#' bm$summary
#'
#' # Base-R plotting example.
#' summary_df <- bm$summary
#' barplot(
#'   height = summary_df$player1_win_rate,
#'   names.arg = paste(summary_df$player1_selection, "vs", summary_df$player2_selection),
#'   ylab = "Player 1 win rate",
#'   main = "Matchup benchmark"
#' )
benchmark_matchup <- function(
    player1 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    player2 = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    n_games = 100L,
    board = bg_initial_board(),
    max_turns = 1000L,
    roll_sequence = NULL,
    seed = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L) {
  if (!is_bg_board(board)) {
    stop("`board` must inherit from class 'bg_board'.", call. = FALSE)
  }

  player1 <- bg_match_simulation_archetype(player1)
  player2 <- bg_match_simulation_archetype(player2)
  bg_validate_board(board)

  n_games <- bg_coerce_integerish(n_games, "n_games", 1L)
  if (n_games < 1L) {
    stop("`n_games` must be at least 1.", call. = FALSE)
  }

  max_turns <- bg_coerce_integerish(max_turns, "max_turns", 1L)
  if (max_turns < 0L) {
    stop("`max_turns` must be nonnegative.", call. = FALSE)
  }

  seed_args <- bg_normalize_seed_args(seed)
  uses_rollout <- bg_is_rollout_family_selection(player1) || bg_is_rollout_family_selection(player2)
  rollout_args <- if (uses_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    list(rollout_budget = 16L, rollout_policy = "random", max_rollout_turns = 1000L)
  }

  out <- if (is.null(roll_sequence)) {
    bg_cpp_benchmark_matchup_random(
      unclass(board),
      n_games,
      max_turns,
      player1,
      player2,
      rollout_args$rollout_budget,
      rollout_args$rollout_policy,
      rollout_args$max_rollout_turns,
      seed_args$seed,
      seed_args$use_seed
    )
  } else {
    rolls <- bg_normalize_roll_sequence(roll_sequence)
    bg_cpp_benchmark_matchup_scripted(
      unclass(board),
      rolls,
      n_games,
      max_turns,
      player1,
      player2,
      rollout_args$rollout_budget,
      rollout_args$rollout_policy,
      rollout_args$max_rollout_turns,
      seed_args$seed,
      seed_args$use_seed
    )
  }

  bg_new_matchup_benchmark(out)
}

#' Summarize a matchup benchmark
#'
#' @param object A `bg_matchup_benchmark` object.
#' @param ... Unused.
#'
#' @return A one-row summary data frame.
summary.bg_matchup_benchmark <- function(object, ...) {
  object$summary
}

bg_new_benchmark_case <- function(x) {
  x$board <- bg_new_board(x$board)
  x$roll <- bg_new_roll(x$roll)
  structure(x, class = "bg_benchmark_case")
}

bg_as_benchmark_case <- function(x, index = NULL) {
  if (is_bg_benchmark_case(x)) {
    return(x)
  }

  if (!is.list(x)) {
    stop("Each benchmark case must be a `bg_benchmark_case` object or a list-like case.", call. = FALSE)
  }

  if (is.null(x$board) || is.null(x$roll)) {
    stop("Each benchmark case must contain `board` and `roll` fields.", call. = FALSE)
  }

  case_id <- x$case_id
  if (is.null(case_id) || (is.character(case_id) && length(case_id) == 1L && !is.na(case_id) && !nzchar(case_id))) {
    case_id <- if (is.null(index)) "case_1" else paste0("case_", index)
  }

  if (!is.character(case_id) || length(case_id) != 1L || is.na(case_id) || !nzchar(case_id)) {
    stop("`case_id` must be a non-missing character scalar when supplied.", call. = FALSE)
  }

  bg_new_benchmark_case(list(
    case_id = case_id,
    board = unclass(bg_as_benchmark_board(x$board)),
    roll = unclass(bg_as_roll(x$roll))
  ))
}

bg_as_benchmark_board <- function(board) {
  if (!is_bg_board(board)) {
    stop("Each benchmark case `board` must be a `bg_board` object.", call. = FALSE)
  }
  bg_validate_board(board)
  board
}

bg_unclass_benchmark_case <- function(x) {
  case <- bg_as_benchmark_case(x)
  list(
    case_id = case$case_id,
    board = unclass(case$board),
    roll = unclass(case$roll)
  )
}

bg_normalize_benchmark_cases <- function(cases) {
  if (is_bg_benchmark_case(cases)) {
    return(list(bg_unclass_benchmark_case(bg_as_benchmark_case(unclass(cases), index = 1L))))
  }

  if (is.list(cases) && all(c("board", "roll") %in% names(cases))) {
    return(list(bg_unclass_benchmark_case(bg_as_benchmark_case(cases, index = 1L))))
  }

  if (!is.list(cases) || length(cases) < 1L) {
    stop("`cases` must be a non-empty list of benchmark cases.", call. = FALSE)
  }

  Map(function(case, idx) bg_unclass_benchmark_case(bg_as_benchmark_case(case, index = idx)), cases, seq_along(cases))
}

#' Test whether an object is a benchmark case
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_benchmark_case"`.
is_bg_benchmark_case <- function(x) {
  inherits(x, "bg_benchmark_case")
}

#' Construct a move-evaluation benchmark case
#'
#' Creates a reusable benchmark case consisting of a board state and a dice roll.
#' Cases can be passed to [benchmark_move_evaluators()] to compare different
#' move-selection archetypes on the same decision problem.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object or roll-like list.
#' @param case_id Optional character scalar identifying the case.
#'
#' @return An object of class `bg_benchmark_case`.
#'
#' @examples
#' points <- integer(24)
#' points[8] <- 1L
#' case <- bg_benchmark_case(
#'   board = bg_board(points = points, off = c(14L, 15L), turn = 1L),
#'   roll = bg_roll(3, 2),
#'   case_id = "simple_case"
#' )
#' case
bg_benchmark_case <- function(board, roll, case_id = NULL) {
  board <- bg_as_benchmark_board(board)
  roll <- bg_as_roll(roll)

  if (is.null(case_id)) {
    case_id <- ""
  }

  if (!is.character(case_id) || length(case_id) != 1L || is.na(case_id)) {
    stop("`case_id` must be a non-missing character scalar when supplied.", call. = FALSE)
  }

  bg_new_benchmark_case(list(
    case_id = case_id,
    board = unclass(board),
    roll = unclass(roll)
  ))
}

bg_new_move_evaluation_benchmark <- function(x) {
  x$results <- as.data.frame(x$results, stringsAsFactors = FALSE)
  x$summary <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  reference_method <- as.character(x$settings$reference_method[[1L]])
  if (is.na(reference_method) || identical(reference_method, "")) {
    reference_method <- NULL
  }

  x$settings <- list(
    methods = as.character(x$settings$methods),
    reference_method = reference_method,
    rollout_budget = as.integer(x$settings$rollout_budget[[1L]]),
    rollout_policy = as.character(x$settings$rollout_policy[[1L]]),
    max_rollout_turns = as.integer(x$settings$max_rollout_turns[[1L]]),
    reference_rollout_budget = as.integer(x$settings$reference_rollout_budget[[1L]]),
    reference_rollout_policy = as.character(x$settings$reference_rollout_policy[[1L]]),
    reference_max_rollout_turns = as.integer(x$settings$reference_max_rollout_turns[[1L]])
  )
  structure(x, class = "bg_move_evaluation_benchmark")
}

#' Test whether an object is a move-evaluation benchmark result
#'
#' @param x An object.
#'
#' @return `TRUE` if `x` inherits from class `"bg_move_evaluation_benchmark"`.
is_bg_move_evaluation_benchmark <- function(x) {
  inherits(x, "bg_move_evaluation_benchmark")
}

#' Benchmark move-evaluation archetypes on fixed decision cases
#'
#' Benchmarks one or more archetypes on a fixed collection of board-plus-roll
#' decision cases. For each case and method, the benchmark records:
#'
#' - the number of legal moves,
#' - the chosen move index,
#' - decision runtime,
#' - and, when a `reference_method` is supplied, whether the chosen move matches
#'   the reference move.
#'
#' Legal moves are generated once per case and reused across methods, so the
#' runtime measurements focus on move evaluation and selection rather than move
#' generation.
#'
#' `best_move_match_rate` is computed only over cases with more than one legal
#' move, because single-move cases are not informative for move discrimination.
#'
#' @param cases A non-empty list of benchmark cases created by
#'   [bg_benchmark_case()], or a single case.
#' @param methods Character vector of archetypes to benchmark.
#' @param reference_method Optional reference archetype used to compute
#'   `best_move_match_rate`. When `NULL`, no reference comparison is performed.
#' @param rollout_budget Rollout budget used by rollout-based methods in
#'   `methods`.
#' @param rollout_policy Baseline rollout policy used by rollout-based methods in
#'   `methods`.
#' @param max_rollout_turns Maximum number of turns per rollout playout for
#'   rollout-based methods in `methods`.
#' @param reference_rollout_budget Optional rollout budget for the
#'   `reference_method` when it is rollout-based.
#' @param reference_rollout_policy Optional rollout policy for the
#'   `reference_method` when it is rollout-based.
#' @param reference_max_rollout_turns Optional rollout horizon for the
#'   `reference_method` when it is rollout-based.
#' @param seed Optional integer-like scalar for reproducible stochastic methods.
#'
#' @return An object of class `bg_move_evaluation_benchmark` with components:
#'   - `results`: one row per case-method pair;
#'   - `summary`: one row per method;
#'   - `settings`: benchmark settings.
#'
#'
#' @examples
#' case1 <- bg_benchmark_case(
#'   board = bg_board(points = c(rep(0L, 7), 1L, rep(0L, 16)), off = c(14L, 15L), turn = 1L),
#'   roll = bg_roll(3, 2),
#'   case_id = "case_1"
#' )
#'
#' bm <- benchmark_move_evaluators(
#'   cases = list(case1),
#'   methods = c("aggressive", "rollout"),
#'   reference_method = "rollout",
#'   rollout_budget = 4L,
#'   seed = 123
#' )
#'
#' bm$summary
#'
#' # Base-R plotting example.
#' barplot(
#'   height = bm$summary$mean_runtime_seconds,
#'   names.arg = bm$summary$method,
#'   ylab = "Mean runtime (seconds)",
#'   main = "Move-evaluation benchmark"
#' )
benchmark_move_evaluators <- function(
    cases,
    methods = c("random", "aggressive", "defensive", "rollout", "equal_rollout", "greedy_rollout", "ucb_rollout", "ocba_rollout", "thompson_rollout", "ttts_rollout"),
    reference_method = NULL,
    rollout_budget = 16L,
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    reference_rollout_budget = rollout_budget,
    reference_rollout_policy = rollout_policy,
    reference_max_rollout_turns = max_rollout_turns,
    seed = NULL) {
  cases <- bg_normalize_benchmark_cases(cases)
  methods <- bg_normalize_benchmark_methods(methods, arg_name = "methods")

  if (is.null(reference_method)) {
    reference_method_string <- ""
  } else {
    if (!is.character(reference_method) || length(reference_method) != 1L || is.na(reference_method)) {
      stop("`reference_method` must be a single non-missing archetype name when supplied.", call. = FALSE)
    }
    reference_method_string <- bg_normalize_benchmark_methods(reference_method, arg_name = "reference_method")
    reference_method_string <- reference_method_string[[1L]]
  }

  methods_use_rollout <- any(vapply(methods, bg_is_rollout_family_selection, logical(1L)))
  reference_uses_rollout <- !identical(reference_method_string, "") && bg_is_rollout_family_selection(reference_method_string)

  rollout_args <- if (methods_use_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = rollout_budget,
      rollout_policy = rollout_policy,
      max_rollout_turns = max_rollout_turns
    )
  } else {
    list(rollout_budget = 16L, rollout_policy = "random", max_rollout_turns = 1000L)
  }
  reference_rollout_args <- if (reference_uses_rollout) {
    bg_normalize_rollout_args(
      rollout_budget = reference_rollout_budget,
      rollout_policy = reference_rollout_policy,
      max_rollout_turns = reference_max_rollout_turns
    )
  } else {
    rollout_args
  }
  seed_args <- bg_normalize_seed_args(seed)

  out <- bg_cpp_benchmark_move_evaluators(
    cases,
    methods,
    reference_method_string,
    rollout_args$rollout_budget,
    rollout_args$rollout_policy,
    rollout_args$max_rollout_turns,
    reference_rollout_args$rollout_budget,
    reference_rollout_args$rollout_policy,
    reference_rollout_args$max_rollout_turns,
    seed_args$seed,
    seed_args$use_seed
  )

  bg_new_move_evaluation_benchmark(out)
}

#' Summarize a move-evaluation benchmark
#'
#' @param object A `bg_move_evaluation_benchmark` object.
#' @param ... Unused.
#'
#' @return A summary data frame with one row per method.
summary.bg_move_evaluation_benchmark <- function(object, ...) {
  object$summary
}

# Core decision-problem construction and board/action feature helpers.
bg_ts_private <- new.env(parent = emptyenv())
bg_ts_private$problem_cache <- new.env(parent = emptyenv())

bg_match_simulation_policy_public <- function(policy) {
  if (length(policy) > 1L) {
    policy <- policy[[1L]]
  }
  match.arg(
    policy,
    choices = c("random", "heuristic", "aggressive", "defensive", "ts_local")
  )
}

bg_resolve_simulation_policy <- function(
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive")) {
  simulation_policy <- bg_match_simulation_policy_public(simulation_policy)
  heuristic_policy <- match.arg(heuristic_policy)

  if (simulation_policy == "heuristic") {
    return(list(
      simulation_policy = simulation_policy,
      simulation_policy_engine = heuristic_policy,
      note = sprintf(
        "Simulation policy 'heuristic' currently resolves to the '%s' heuristic rollout policy.",
        heuristic_policy
      ),
      experimental = FALSE
    ))
  }

  if (simulation_policy == "ts_local") {
    return(list(
      simulation_policy = simulation_policy,
      simulation_policy_engine = "thompson_rollout",
      note = paste(
        "Simulation policy 'ts_local' is experimental and changes the rollout-model estimand.",
        "It uses local Thompson-rollout move choice inside continuation playouts."
      ),
      experimental = TRUE
    ))
  }

  list(
    simulation_policy = simulation_policy,
    simulation_policy_engine = simulation_policy,
    note = NULL,
    experimental = FALSE
  )
}

# Cache keys are based on the full rollout-model estimand, not just board + roll.
# Changing continuation policy or model stack should produce a distinct problem.
bg_problem_key <- function(
    board,
    roll,
    simulation_policy_engine,
    max_rollout_turns,
    unresolved_value,
    reward_model,
    posterior_model,
    model_signature = NULL) {
  paste(
    paste(unclass(board)$points, collapse = ","),
    paste(unclass(board)$bar, collapse = ","),
    paste(unclass(board)$off, collapse = ","),
    unclass(board)$turn,
    paste(bg_as_roll(roll)$dice, collapse = "-"),
    simulation_policy_engine,
    max_rollout_turns,
    format(as.numeric(unresolved_value), scientific = FALSE, trim = TRUE),
    reward_model,
    posterior_model,
    if (is.null(model_signature)) "" else as.character(model_signature),
    sep = "::"
  )
}

bg_move_action_features <- function(move) {
  move <- bg_as_move_sequence(move)
  steps <- move$steps

  data.frame(
    n_steps = move$n_steps,
    n_hits = sum(vapply(steps, function(step) isTRUE(step$hit), logical(1L))),
    n_bar_entries = sum(vapply(steps, function(step) step$from == 0L, logical(1L))),
    n_bear_off = sum(vapply(steps, function(step) step$to == 25L, logical(1L))),
    total_step_distance = sum(vapply(steps, function(step) abs(step$to - step$from), numeric(1L))),
    stringsAsFactors = FALSE
  )
}

bg_problem_candidate_table <- function(board, legal_moves, simulation_policy_engine, max_rollout_turns, unresolved_value) {
  if (length(legal_moves) == 0L) {
    out <- data.frame(
      candidate_index = integer(0L),
      representative_index = integer(0L),
      n_equivalent_sequences = integer(0L),
      stringsAsFactors = FALSE
    )
    out$move <- I(vector("list", 0L))
    out$move_label <- character(0L)
    out$n_steps <- integer(0L)
    out$n_hits <- integer(0L)
    out$n_bar_entries <- integer(0L)
    out$n_bear_off <- integer(0L)
    out$total_step_distance <- numeric(0L)
    return(out)
  }

  legal_moves_unclass <- lapply(legal_moves, bg_unclass_move_sequence)
  collapsed <- bg_cpp_rollout_blocks(
    unclass(board),
    legal_moves_unclass,
    integer(0L),
    integer(0L),
    integer(0L),
    simulation_policy_engine,
    max_rollout_turns,
    unresolved_value,
    "iid",
    FALSE,
    1L,
    0L,
    FALSE
  )$candidate_map

  collapsed <- as.data.frame(collapsed, stringsAsFactors = FALSE)
  collapsed$move <- I(lapply(collapsed$candidate_index, function(i) legal_moves[[i]]))
  collapsed$move_label <- vapply(collapsed$move, bg_move_label, character(1L))
  collapsed$n_steps <- vapply(collapsed$move, function(move) move$n_steps, integer(1L))
  action_features <- do.call(
    rbind,
    lapply(collapsed$move, bg_move_action_features)
  )
  rownames(action_features) <- NULL
  cbind(
    collapsed,
    action_features[, setdiff(names(action_features), "n_steps"), drop = FALSE],
    stringsAsFactors = FALSE
  )
}

#' Build a one-state, one-roll Thompson-sampling decision problem
#'
#' `bg_problem()` creates the canonical decision object used by the new
#' Thompson-first public API. It freezes:
#'
#' - one board state;
#' - one realized dice roll;
#' - one legal action set;
#' - one rollout continuation policy;
#' - one truncation and payoff mapping;
#' - one explicit reward model and posterior model.
#'
#' This makes the package's scientific object explicit: a fixed-budget
#' best-action-identification problem under Monte Carlo noise, not a claim about
#' exact backgammon truth.
#'
#' @param state A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param simulation_policy Continuation policy used after the root action
#'   inside rollouts. `"random"` is the benchmark default. `"heuristic"`
#'   resolves to a fixed heuristic continuation policy. `"ts_local"` is
#'   experimental because it changes the rollout-model estimand.
#' @param heuristic_policy Heuristic continuation policy used when
#'   `simulation_policy = "heuristic"`.
#' @param max_rollout_turns Integer-like rollout truncation horizon.
#' @param unresolved_value Numeric payoff assigned to unresolved rollouts.
#' @param prior_alpha Positive pseudo-count used by Thompson-family summaries.
#' @param prior_beta Positive pseudo-count used by Thompson-family summaries.
#' @param reward_model Reward definition used by the rollout engine.
#' @param posterior_model Posterior family used by Thompson-style summaries.
#' @param posterior_prior Optional named list overriding the default prior for
#'   the chosen `reward_model` / `posterior_model` pair.
#' @param legal_moves Optional precomputed legal move set.
#' @param cache Logical scalar; if `TRUE`, reuse a cached problem object when
#'   the board, roll, and rollout-model settings match.
#' @param problem_id Optional string identifier used in repeated studies.
#'
#' @return A `bg_problem` object.
#' @export
bg_problem <- function(
    state,
    roll,
    simulation_policy = c("random", "heuristic", "aggressive", "defensive", "ts_local"),
    heuristic_policy = c("aggressive", "defensive"),
    max_rollout_turns = 220L,
    unresolved_value = 0.5,
    prior_alpha = 1,
    prior_beta = 1,
    reward_model = c("scalar_payoff"),
    posterior_model = c("beta_pseudo"),
    posterior_prior = NULL,
    legal_moves = NULL,
    cache = TRUE,
    problem_id = NULL) {
  if (!is_bg_board(state)) {
    stop("`state` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_validate_board(state)
  roll <- bg_as_roll(roll)
  bg_assert_scalar_flag(cache, "cache")

  resolved_policy <- bg_resolve_simulation_policy(
    simulation_policy = simulation_policy,
    heuristic_policy = heuristic_policy
  )
  max_rollout_turns <- bg_coerce_integerish(max_rollout_turns, "max_rollout_turns", 1L)

  if (!is.numeric(unresolved_value) || length(unresolved_value) != 1L || is.na(unresolved_value)) {
    stop("`unresolved_value` must be a numeric scalar.", call. = FALSE)
  }
  if (unresolved_value < 0 || unresolved_value > 1) {
    stop("`unresolved_value` must lie in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(prior_alpha) || length(prior_alpha) != 1L || is.na(prior_alpha) || prior_alpha <= 0) {
    stop("`prior_alpha` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(prior_beta) || length(prior_beta) != 1L || is.na(prior_beta) || prior_beta <= 0) {
    stop("`prior_beta` must be a positive numeric scalar.", call. = FALSE)
  }
  model_spec <- bg_resolve_model_spec(
    reward_model = reward_model,
    posterior_model = posterior_model,
    unresolved_value = unresolved_value,
    prior_alpha = prior_alpha,
    prior_beta = prior_beta,
    posterior_prior = posterior_prior
  )

  key <- bg_problem_key(
    board = state,
    roll = roll,
    simulation_policy_engine = resolved_policy$simulation_policy_engine,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value,
    reward_model = model_spec$reward_model_canonical,
    posterior_model = model_spec$posterior_model_canonical,
    model_signature = model_spec$model_signature
  )
  if (isTRUE(cache) && exists(key, envir = bg_ts_private$problem_cache, inherits = FALSE)) {
    cached <- get(key, envir = bg_ts_private$problem_cache, inherits = FALSE)
    if (!is.null(problem_id)) {
      cached$problem_id <- problem_id
    }
    return(cached)
  }

  legal_moves <- if (is.null(legal_moves)) {
    bg_legal_moves(state, roll)
  } else {
    lapply(bg_normalize_move_sequence_list(legal_moves), bg_new_move_sequence)
  }
  # Downstream TS code reasons over unique root-action candidates, not raw legal
  # move sequences that may collapse to the same successor state.
  candidate_table <- bg_problem_candidate_table(
    board = state,
    legal_moves = legal_moves,
    simulation_policy_engine = resolved_policy$simulation_policy_engine,
    max_rollout_turns = max_rollout_turns,
    unresolved_value = unresolved_value
  )

  out <- structure(
    list(
      board = state,
      roll = roll,
      legal_moves = legal_moves,
      candidate_table = candidate_table,
      problem_id = if (is.null(problem_id)) paste0("problem_", substr(key, 1L, 12L)) else as.character(problem_id),
      settings = list(
        simulation_policy = resolved_policy$simulation_policy,
        simulation_policy_engine = resolved_policy$simulation_policy_engine,
        simulation_policy_note = resolved_policy$note,
        simulation_policy_experimental = resolved_policy$experimental,
        max_rollout_turns = max_rollout_turns,
        unresolved_value = as.numeric(unresolved_value),
        prior_alpha = as.numeric(prior_alpha),
        prior_beta = as.numeric(prior_beta),
        reward_model = model_spec$reward_model,
        posterior_model = model_spec$posterior_model,
        reward_model_canonical = model_spec$reward_model_canonical,
        posterior_model_canonical = model_spec$posterior_model_canonical,
        posterior_family = model_spec$posterior_family,
        posterior_prior = model_spec$prior,
        model_signature = model_spec$model_signature,
        model_exact = isTRUE(model_spec$exact),
        model_note = model_spec$note,
        legacy_alias = isTRUE(model_spec$legacy_alias)
      )
    ),
    class = "bg_problem"
  )

  if (isTRUE(cache)) {
    assign(key, out, envir = bg_ts_private$problem_cache)
  }
  out
}

#' Compute interpretable board and action features
#'
#' `bg_board_features()` exposes a compact, interpretable feature view of a
#' board or `bg_problem`. These features are intended for structured
#' experiments, difficulty analysis, and move-by-move studies, not as claims of
#' exact backgammon truth.
#'
#' @param x A `bg_board` or `bg_problem` object.
#' @param player Optional player viewpoint. Defaults to the player to move.
#'
#' @return A list with `board_features` and, when available, `action_features`.
#' @export
bg_board_features <- function(x, player = NULL) {
  if (inherits(x, "bg_problem")) {
    board <- x$board
    if (is.null(player)) {
      player <- board$turn
    }
    board_features <- as.data.frame(bg_cpp_heuristic_board_features(unclass(board), player), stringsAsFactors = FALSE)
    action_features <- x$candidate_table[, c(
      "candidate_index",
      "move_label",
      "n_steps",
      "n_hits",
      "n_bar_entries",
      "n_bear_off",
      "total_step_distance",
      "n_equivalent_sequences"
    ), drop = FALSE]
    return(list(
      board_features = board_features,
      action_features = action_features
    ))
  }

  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board' or 'bg_problem'.", call. = FALSE)
  }

  if (is.null(player)) {
    player <- x$turn
  }

  list(
    board_features = as.data.frame(bg_cpp_heuristic_board_features(unclass(x), player), stringsAsFactors = FALSE),
    action_features = NULL
  )
}

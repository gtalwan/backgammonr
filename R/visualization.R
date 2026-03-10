bg_match_board_perspective <- function(perspective) {
  match.arg(perspective, choices = c("player_1", "player_2"))
}

bg_point_token_ascii <- function(value) {
  if (value > 0L) {
    return(paste0("X", value))
  }
  if (value < 0L) {
    return(paste0("O", abs(value)))
  }
  "."
}

bg_ascii_cell <- function(token, highlight = FALSE, width = 6L) {
  token <- as.character(token)
  if (isTRUE(highlight)) {
    token <- paste0("[", token, "]")
  }
  sprintf(paste0("%", width, "s"), token)
}

bg_board_rows_for_perspective <- function(perspective) {
  perspective <- bg_match_board_perspective(perspective)
  if (perspective == "player_1") {
    return(list(top = 24:13, bottom = 12:1))
  }
  list(top = 1:12, bottom = 13:24)
}

bg_move_highlight_points <- function(move) {
  move <- bg_as_move_sequence(move)
  pts <- integer(0L)
  for (step in move$steps) {
    if (!is.null(step$from) && step$from >= 1L && step$from <= 24L) {
      pts <- c(pts, as.integer(step$from))
    }
    if (!is.null(step$to) && step$to >= 1L && step$to <= 24L) {
      pts <- c(pts, as.integer(step$to))
    }
  }
  unique(pts)
}

bg_plot_highlight_points <- function(highlight_points = NULL, highlight_move = NULL) {
  points <- integer(0L)
  if (!is.null(highlight_points)) {
    points <- c(points, as.integer(highlight_points))
  }
  if (!is.null(highlight_move)) {
    points <- c(points, bg_move_highlight_points(highlight_move))
  }
  points <- unique(points)
  points[!is.na(points) & points >= 1L & points <= 24L]
}

#' Format a backgammon board as an ASCII diagram
#'
#' Produces a readable board diagram with top and bottom point lanes, bar/off
#' counts, and optional point highlighting.
#'
#' @param x A `bg_board` object.
#' @param ... Unused.
#' @param show_indices Logical scalar. If `TRUE`, show point numbers.
#' @param perspective Board perspective. `"player_1"` shows points `24 -> 13`
#'   on top and `12 -> 1` on bottom. `"player_2"` flips orientation.
#' @param highlight_points Optional integer vector of points to highlight.
#' @param highlight_move Optional `bg_move_sequence` to highlight source and
#'   destination points.
#'
#' @return A single character string.
#' @export
format.bg_board <- function(
    x,
    ...,
    show_indices = TRUE,
    perspective = c("player_1", "player_2"),
    highlight_points = NULL,
    highlight_move = NULL) {
  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board'.", call. = FALSE)
  }
  bg_assert_scalar_flag(show_indices, "show_indices")
  bg_validate_board(x)
  perspective <- bg_match_board_perspective(perspective)

  rows <- bg_board_rows_for_perspective(perspective)
  highlighted <- bg_plot_highlight_points(highlight_points, highlight_move)

  top_tokens <- vapply(rows$top, function(i) bg_point_token_ascii(x$points[[i]]), character(1L))
  bottom_tokens <- vapply(rows$bottom, function(i) bg_point_token_ascii(x$points[[i]]), character(1L))

  top_cells <- mapply(
    function(tok, idx) bg_ascii_cell(tok, idx %in% highlighted),
    top_tokens,
    rows$top,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  bottom_cells <- mapply(
    function(tok, idx) bg_ascii_cell(tok, idx %in% highlighted),
    bottom_tokens,
    rows$bottom,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  top_label <- paste0("Top (", rows$top[[1L]], " -> ", rows$top[[length(rows$top)]], ")")
  bottom_label <- paste0("Bottom (", rows$bottom[[1L]], " -> ", rows$bottom[[length(rows$bottom)]], ")")

  lines <- c(
    "<bg_board_ascii>",
    paste0("turn: ", if (x$turn == 1L) "player_1" else "player_2"),
    paste0("perspective: ", perspective),
    paste0("bar:  p1=", x$bar[[1L]], " p2=", x$bar[[2L]]),
    paste0("off:  p1=", x$off[[1L]], " p2=", x$off[[2L]]),
    ""
  )

  lines <- c(lines, top_label)
  if (isTRUE(show_indices)) {
    lines <- c(lines, paste(vapply(rows$top, function(idx) sprintf("%6d", idx), character(1L)), collapse = ""))
  }
  lines <- c(lines, paste(top_cells, collapse = ""))
  lines <- c(lines, bottom_label)
  if (isTRUE(show_indices)) {
    lines <- c(lines, paste(vapply(rows$bottom, function(idx) sprintf("%6d", idx), character(1L)), collapse = ""))
  }
  lines <- c(lines, paste(bottom_cells, collapse = ""))

  paste(lines, collapse = "\n")
}

bg_board_plot_positions <- function(perspective = c("player_1", "player_2")) {
  perspective <- bg_match_board_perspective(perspective)
  rows <- bg_board_rows_for_perspective(perspective)
  slot_x <- c(seq(0.7, 5.7, by = 1), seq(7.3, 12.3, by = 1))

  out <- data.frame(
    point = integer(24L),
    lane = character(24L),
    x = numeric(24L),
    stringsAsFactors = FALSE
  )

  out$point <- c(rows$top, rows$bottom)
  out$lane <- c(rep("top", length(rows$top)), rep("bottom", length(rows$bottom)))
  out$x <- rep(slot_x, 2L)
  out
}

bg_draw_board_base <- function(main = NULL) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 13.6), ylim = c(0, 2), xaxs = "i", yaxs = "i")
  graphics::rect(0, 0, 13.6, 2, col = "#d6b58a", border = "#5b3a1a", lwd = 2)
  graphics::rect(6.1, 0, 6.9, 2, col = "#8f6b3f", border = "#5b3a1a", lwd = 2)

  slot_x <- c(seq(0.7, 5.7, by = 1), seq(7.3, 12.3, by = 1))
  for (i in seq_along(slot_x)) {
    x <- slot_x[[i]]
    alt_col <- if (i %% 2L == 0L) "#f0dfc2" else "#a66b3f"
    graphics::polygon(c(x - 0.45, x + 0.45, x), c(2, 2, 1.1), col = alt_col, border = "#5b3a1a")
    graphics::polygon(c(x - 0.45, x + 0.45, x), c(0, 0, 0.9), col = alt_col, border = "#5b3a1a")
  }

  if (!is.null(main) && nzchar(main)) {
    graphics::title(main = main, line = 0.5)
  }
}

bg_draw_checkers <- function(board, positions, highlight_points = integer(0L)) {
  p1_col <- "#1f4e79"
  p2_col <- "#bf3d1f"
  border_col <- "#222222"
  max_visible <- 6L

  for (i in seq_len(nrow(positions))) {
    point <- positions$point[[i]]
    value <- board$points[[point]]
    if (value == 0L) {
      next
    }

    n <- abs(value)
    lane <- positions$lane[[i]]
    x <- positions$x[[i]]
    col <- if (value > 0L) p1_col else p2_col
    visible <- min(n, max_visible)

    for (k in seq_len(visible)) {
      y <- if (lane == "top") 1.9 - (k - 1L) * 0.13 else 0.1 + (k - 1L) * 0.13
      lwd <- if (point %in% highlight_points) 2.5 else 1
      graphics::symbols(
        x = x,
        y = y,
        circles = 0.11,
        inches = FALSE,
        add = TRUE,
        fg = border_col,
        bg = col,
        lwd = lwd
      )
    }

    if (n > max_visible) {
      y_txt <- if (lane == "top") 1.05 else 0.95
      graphics::text(x, y_txt, labels = as.character(n), cex = 0.7, col = "#111111")
    }
  }
}

bg_draw_board_labels <- function(board, positions, perspective) {
  top <- positions[positions$lane == "top", , drop = FALSE]
  bottom <- positions[positions$lane == "bottom", , drop = FALSE]

  graphics::text(top$x, rep(2.03, nrow(top)), labels = top$point, cex = 0.65, xpd = NA)
  graphics::text(bottom$x, rep(-0.03, nrow(bottom)), labels = bottom$point, cex = 0.65, xpd = NA)
  graphics::text(6.5, 1.95, labels = paste0("bar p1=", board$bar[[1L]]), cex = 0.7)
  graphics::text(6.5, 0.05, labels = paste0("bar p2=", board$bar[[2L]]), cex = 0.7)
  graphics::text(13.15, 1.95, labels = paste0("off p1=", board$off[[1L]]), cex = 0.7, xpd = NA, adj = c(0, 0.5))
  graphics::text(13.15, 0.05, labels = paste0("off p2=", board$off[[2L]]), cex = 0.7, xpd = NA, adj = c(0, 0.5))
  graphics::text(0.1, 1.0, labels = paste0("turn: ", if (board$turn == 1L) "player_1" else "player_2"), adj = c(0, 0.5), cex = 0.72)
  graphics::text(13.5, 1.0, labels = perspective, adj = c(1, 0.5), cex = 0.7)
}

#' Plot a backgammon board
#'
#' Draws a static board with checker stacks, bar counts, and borne-off counts.
#'
#' @param x A `bg_board` object.
#' @param y Unused.
#' @param perspective Board perspective (`"player_1"` or `"player_2"`).
#' @param highlight_points Optional integer vector of points to highlight.
#' @param highlight_move Optional `bg_move_sequence` whose source/destination
#'   points should be highlighted.
#' @param main Optional plot title.
#' @param add Logical scalar. If `TRUE`, draws checkers and overlays on an
#'   existing board canvas.
#' @param ... Unused.
#'
#' @return The input board, invisibly.
#' @export
plot.bg_board <- function(
    x,
    y = NULL,
    perspective = c("player_1", "player_2"),
    highlight_points = NULL,
    highlight_move = NULL,
    main = NULL,
    add = FALSE,
    ...) {
  if (!is_bg_board(x)) {
    stop("`x` must inherit from class 'bg_board'.", call. = FALSE)
  }
  perspective <- bg_match_board_perspective(perspective)
  bg_validate_board(x)

  positions <- bg_board_plot_positions(perspective)
  highlighted <- bg_plot_highlight_points(highlight_points, highlight_move)

  if (!isTRUE(add)) {
    bg_draw_board_base(main = main)
  }
  bg_draw_checkers(x, positions, highlight_points = highlighted)
  bg_draw_board_labels(x, positions, perspective = perspective)

  invisible(x)
}

#' Plot a board (user wrapper)
#'
#' Convenience wrapper around [plot.bg_board()].
#'
#' @param board A `bg_board` object.
#' @inheritParams plot.bg_board
#'
#' @return The input board, invisibly.
#' @export
bg_plot_board <- function(board, ...) {
  plot.bg_board(board, ...)
}

#' Compare two boards side-by-side
#'
#' Draws `before` and `after` board states in a two-panel layout.
#'
#' @param before A `bg_board` object.
#' @param after A `bg_board` object.
#' @param perspective Board perspective.
#' @param main_before Title for the left panel.
#' @param main_after Title for the right panel.
#' @param ... Passed to [plot.bg_board()].
#'
#' @return A named list containing the plotted boards.
#' @export
bg_compare_boards <- function(
    before,
    after,
    perspective = c("player_1", "player_2"),
    main_before = "Before",
    main_after = "After",
    ...) {
  perspective <- bg_match_board_perspective(perspective)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)

  graphics::par(mfrow = c(1, 2), mar = c(2.5, 1, 3, 1))
  plot.bg_board(before, perspective = perspective, main = main_before, ...)
  plot.bg_board(after, perspective = perspective, main = main_after, ...)

  invisible(list(before = before, after = after))
}

#' Plot a move by showing before/after boards
#'
#' Applies one move to a board and displays a side-by-side board comparison.
#'
#' @param board A `bg_board` object.
#' @param move A `bg_move_sequence` object.
#' @param perspective Board perspective.
#' @param ... Passed to [bg_compare_boards()].
#'
#' @return A list with `before`, `after`, and `move`.
#' @export
bg_plot_move <- function(board, move, perspective = c("player_1", "player_2"), ...) {
  move <- bg_as_move_sequence(move)
  after <- bg_apply_move_sequence(board, move)
  highlighted <- bg_move_highlight_points(move)
  bg_compare_boards(
    before = board,
    after = after,
    perspective = perspective,
    main_before = paste0("Before: ", bg_move_label(move)),
    main_after = "After move",
    highlight_points = highlighted,
    ...
  )
  invisible(list(before = board, after = after, move = move))
}

bg_shorten_label <- function(x, max_chars = 28L) {
  x <- as.character(x)
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1L, max_chars - 3L), "..."), x)
}

#' Plot legal moves as board small multiples
#'
#' Shows the current board and a panel of after-move boards for top candidate
#' moves.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param top_n Maximum number of candidate moves to display.
#' @param method Optional allocation method used to rank moves.
#' @param total_budget Optional rollout budget used when `method` is supplied.
#' @param perspective Board perspective.
#' @param ... Additional arguments passed to [bg_rank_moves()] when
#'   `method` is supplied.
#'
#' @return A list with plotted ranking data.
#' @export
bg_plot_legal_moves <- function(
    board,
    roll,
    top_n = 6L,
    method = NULL,
    total_budget = NULL,
    perspective = c("player_1", "player_2"),
    ...) {
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (top_n < 1L) {
    stop("`top_n` must be at least 1.", call. = FALSE)
  }
  perspective <- bg_match_board_perspective(perspective)
  legal_moves <- bg_legal_moves(board, roll)

  if (length(legal_moves) == 0L) {
    plot.bg_board(board, perspective = perspective, main = "No legal moves")
    return(invisible(list(board = board, legal_moves = legal_moves, ranking = data.frame())))
  }

  ranking <- if (is.null(method)) {
    data.frame(
      candidate_index = seq_along(legal_moves),
      move_label = vapply(legal_moves, bg_move_label, character(1L)),
      move = I(legal_moves),
      rank = seq_along(legal_moves),
      stringsAsFactors = FALSE
    )
  } else {
    if (is.null(total_budget)) {
      total_budget <- 32L
    }
    bg_rank_moves(
      board = board,
      roll = roll,
      method = method,
      total_budget = total_budget,
      ...
    )
  }

  n_show <- min(as.integer(top_n), nrow(ranking))
  if (n_show < 1L) {
    plot.bg_board(board, perspective = perspective, main = "No candidate rows")
    return(invisible(list(board = board, legal_moves = legal_moves, ranking = ranking)))
  }

  n_panels <- n_show + 1L
  ncol <- min(3L, n_panels)
  nrow <- ceiling(n_panels / ncol)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mfrow = c(nrow, ncol), mar = c(2.3, 1, 2.7, 1))

  plot.bg_board(board, perspective = perspective, main = "Current board")
  for (i in seq_len(n_show)) {
    move <- ranking$move[[i]]
    after <- bg_apply_move_sequence(board, move)
    score <- if ("estimate" %in% names(ranking)) paste0(" p=", formatC(ranking$estimate[[i]], digits = 3, format = "f")) else ""
    title <- paste0("#", ranking$rank[[i]], " ", bg_shorten_label(ranking$move_label[[i]]), score)
    plot.bg_board(after, perspective = perspective, highlight_move = move, main = title)
  }

  invisible(list(board = board, legal_moves = legal_moves, ranking = ranking, shown = ranking[seq_len(n_show), , drop = FALSE]))
}

bg_extract_ranking_table <- function(x) {
  if (inherits(x, "bg_move_recommendation")) {
    return(x$ranking)
  }
  if (inherits(x, "bg_action_evaluation")) {
    return(x$results)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  stop("`x` must be a `bg_action_evaluation`, `bg_move_recommendation`, or data frame.", call. = FALSE)
}

#' Plot move rankings with uncertainty intervals
#'
#' @param x A `bg_action_evaluation`, `bg_move_recommendation`, or ranking data
#'   frame.
#' @param main Optional title.
#' @param ... Unused.
#'
#' @return The plotting data frame, invisibly.
#' @export
bg_plot_move_ranking <- function(x, main = "Move ranking with uncertainty", ...) {
  tab <- bg_extract_ranking_table(x)
  if (nrow(tab) == 0L) {
    graphics::plot.new()
    graphics::title("No legal moves")
    return(invisible(tab))
  }

  if (!"rank" %in% names(tab)) {
    tab <- tab[order(-tab$estimate, tab$candidate_index), , drop = FALSE]
    tab$rank <- seq_len(nrow(tab))
  } else {
    tab <- tab[order(tab$rank), , drop = FALSE]
  }

  if (!"move_label" %in% names(tab)) {
    tab$move_label <- if ("candidate_index" %in% names(tab)) {
      paste0("candidate_", tab$candidate_index)
    } else {
      paste0("move_", seq_len(nrow(tab)))
    }
  }

  labels <- paste0("#", tab$rank, ": ", bg_shorten_label(tab$move_label))
  means <- tab$estimate
  lower <- if ("lower_95" %in% names(tab)) tab$lower_95 else means
  upper <- if ("upper_95" %in% names(tab)) tab$upper_95 else means
  recommended <- if ("recommended" %in% names(tab)) as.logical(tab$recommended) else rep(FALSE, nrow(tab))
  cols <- ifelse(recommended, "#2e7d32", "#4a6fa5")

  bar_x <- graphics::barplot(
    height = means,
    col = cols,
    border = "#1a1a1a",
    ylim = c(0, max(1, upper, na.rm = TRUE)),
    names.arg = rep("", nrow(tab)),
    ylab = "Estimated win probability",
    main = main
  )

  graphics::arrows(bar_x, lower, bar_x, upper, angle = 90, code = 3, length = 0.04, lwd = 1.2)
  graphics::axis(1, at = bar_x, labels = labels, las = 2, cex.axis = 0.72)

  invisible(tab)
}

#' Plot adaptive allocation traces
#'
#' @param x A `bg_action_evaluation` object created with `trace = TRUE`.
#' @param top_n Number of top candidates to show by final estimate.
#' @param metric Trace metric to plot.
#' @param ... Unused.
#'
#' @return Trace rows used in the plot, invisibly.
#' @export
bg_plot_allocation_trace <- function(
    x,
    top_n = 4L,
    metric = c("allocation_count", "estimate", "selection_score"),
    ...) {
  if (!inherits(x, "bg_action_evaluation")) {
    stop("`x` must inherit from class 'bg_action_evaluation'.", call. = FALSE)
  }
  metric <- match.arg(metric)
  top_n <- bg_coerce_integerish(top_n, "top_n", 1L)
  if (top_n < 1L) {
    stop("`top_n` must be at least 1.", call. = FALSE)
  }
  if (is.null(x$trace) || nrow(x$trace) == 0L) {
    stop("`x` does not contain a non-empty trace. Re-run evaluation with `trace = TRUE`.", call. = FALSE)
  }

  tr <- x$trace
  final_checkpoint <- max(tr$checkpoint)
  final <- tr[tr$checkpoint == final_checkpoint, , drop = FALSE]
  final <- final[order(-final$estimate), , drop = FALSE]
  keep <- head(final$candidate_index, as.integer(top_n))
  plot_df <- tr[tr$candidate_index %in% keep, , drop = FALSE]
  plot_df <- plot_df[order(plot_df$candidate_index, plot_df$checkpoint), , drop = FALSE]

  y <- plot_df[[metric]]
  if (!is.numeric(y)) {
    stop(sprintf("Trace metric `%s` is not numeric.", metric), call. = FALSE)
  }

  xlim <- range(plot_df$checkpoint, na.rm = TRUE)
  ylim <- range(y, na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.01, 0.01)
  }

  graphics::plot(
    NA,
    xlim = xlim,
    ylim = ylim,
    xlab = "Checkpoint",
    ylab = metric,
    main = paste("Allocation trace:", x$method)
  )

  ids <- sort(unique(plot_df$candidate_index))
  cols <- grDevices::hcl.colors(length(ids), "Dark 3")
  for (i in seq_along(ids)) {
    rows <- plot_df[plot_df$candidate_index == ids[[i]], , drop = FALSE]
    graphics::lines(rows$checkpoint, rows[[metric]], type = "l", lwd = 2, col = cols[[i]])
  }
  graphics::legend(
    "topleft",
    legend = paste0("candidate ", ids),
    col = cols,
    lwd = 2,
    cex = 0.8,
    bty = "n"
  )

  invisible(plot_df)
}

#' Plot recommendation stability across budgets
#'
#' Evaluates multiple methods over an increasing budget grid and plots the
#' selected candidate index as budget increases.
#'
#' @param board A `bg_board` object.
#' @param roll A `bg_roll` object.
#' @param budgets Integer-like vector of budgets.
#' @param methods Allocation methods to compare.
#' @param rollout_policy Baseline rollout policy.
#' @param max_rollout_turns Maximum rollout turns.
#' @param dice_mode Dice mode.
#' @param crn Logical scalar for CRN use.
#' @param seed Optional integer-like scalar.
#' @param ... Unused.
#'
#' @return A data frame of budget-by-method recommendations, invisibly.
#' @export
bg_plot_budget_stability <- function(
    board,
    roll,
    budgets = c(8L, 16L, 32L, 64L),
    methods = c("thompson", "ttts", "equal", "ucb", "ocba"),
    rollout_policy = c("random", "aggressive", "defensive"),
    max_rollout_turns = 1000L,
    dice_mode = c("iid", "stratified_first_roll", "stratified_first_two_rolls"),
    crn = FALSE,
    seed = NULL,
    ...) {
  budgets <- bg_coerce_integerish(budgets, "budgets", length(budgets))
  if (any(budgets < 1L)) {
    stop("All elements of `budgets` must be at least 1.", call. = FALSE)
  }
  budgets <- sort(unique(as.integer(budgets)))

  methods <- unique(vapply(methods, bg_match_allocation_method, character(1L), USE.NAMES = FALSE))
  methods <- vapply(methods, bg_canonicalize_allocation_method, character(1L), USE.NAMES = FALSE)
  rollout_policy <- bg_match_rollout_policy(rollout_policy)
  dice_mode <- match.arg(dice_mode)
  bg_assert_scalar_flag(crn, "crn")

  rows <- list()
  row_id <- 1L
  for (method in methods) {
    for (budget in budgets) {
      eval <- bg_evaluate_actions_method(
        board = board,
        roll = roll,
        method = method,
        total_budget = budget,
        rollout_policy = rollout_policy,
        max_rollout_turns = max_rollout_turns,
        dice_mode = dice_mode,
        crn = crn,
        seed = bg_derive_seed(seed, "budget_stability", method, budget, dice_mode, crn)
      )
      rows[[row_id]] <- data.frame(
        method = method,
        total_budget = budget,
        recommended_index = eval$recommended_index,
        recommended_label = if (nrow(eval$results) > 0L) {
          eval$results$move_label[eval$results$recommended][1L]
        } else {
          "<pass>"
        },
        recommended_estimate = if (nrow(eval$results) > 0L) {
          eval$results$estimate[eval$results$recommended][1L]
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
      row_id <- row_id + 1L
    }
  }

  out <- do.call(rbind, rows)
  methods_ord <- unique(out$method)
  cols <- grDevices::hcl.colors(length(methods_ord), "Set 2")

  ylim <- range(out$recommended_index, na.rm = TRUE)
  if (diff(ylim) == 0) {
    ylim <- ylim + c(-0.2, 0.2)
  }
  graphics::plot(
    NA,
    xlim = range(out$total_budget),
    ylim = ylim,
    xlab = "Total rollout budget",
    ylab = "Recommended candidate index",
    main = "Budget stability"
  )
  for (i in seq_along(methods_ord)) {
    rows_i <- out[out$method == methods_ord[[i]], , drop = FALSE]
    rows_i <- rows_i[order(rows_i$total_budget), , drop = FALSE]
    graphics::lines(rows_i$total_budget, rows_i$recommended_index, type = "b", pch = 16, lwd = 2, col = cols[[i]])
  }
  graphics::legend("topleft", legend = methods_ord, col = cols, lwd = 2, pch = 16, bty = "n")

  invisible(out)
}

#' Plot benchmark summary metrics
#'
#' @param x A `bg_allocation_benchmark` object.
#' @param metric Benchmark metric to display.
#' @param ... Unused.
#'
#' @return Summary rows used in plotting, invisibly.
#' @export
bg_plot_benchmark_summary <- function(
    x,
    metric = c(
      "probability_correct_selection",
      "mean_simple_regret",
      "mean_mse",
      "mean_runtime_seconds"
    ),
    ...) {
  if (!inherits(x, "bg_allocation_benchmark")) {
    stop("`x` must inherit from class 'bg_allocation_benchmark'.", call. = FALSE)
  }
  metric <- match.arg(metric)
  summary_df <- x$summary
  if (nrow(summary_df) == 0L) {
    graphics::plot.new()
    graphics::title("Empty benchmark summary")
    return(invisible(summary_df))
  }
  if (!metric %in% names(summary_df)) {
    stop(sprintf("Metric `%s` not found in benchmark summary.", metric), call. = FALSE)
  }

  if ("total_budget" %in% names(summary_df)) {
    lines_df <- summary_df
    lines_df$series <- lines_df$method
    if ("dice_mode" %in% names(lines_df)) {
      lines_df$series <- paste0(lines_df$series, " | ", lines_df$dice_mode)
    }
    if ("crn" %in% names(lines_df)) {
      lines_df$series <- paste0(lines_df$series, " | crn=", as.character(lines_df$crn))
    }

    series <- unique(lines_df$series)
    cols <- grDevices::hcl.colors(length(series), "Dark 2")
    ylim <- range(lines_df[[metric]], na.rm = TRUE)
    if (diff(ylim) == 0) {
      ylim <- ylim + c(-0.01, 0.01)
    }
    graphics::plot(
      NA,
      xlim = range(lines_df$total_budget, na.rm = TRUE),
      ylim = ylim,
      xlab = "Budget",
      ylab = metric,
      main = paste("Benchmark summary:", metric)
    )
    for (i in seq_along(series)) {
      rows <- lines_df[lines_df$series == series[[i]], , drop = FALSE]
      rows <- rows[order(rows$total_budget), , drop = FALSE]
      graphics::lines(rows$total_budget, rows[[metric]], type = "b", pch = 16, col = cols[[i]], lwd = 2)
    }
    graphics::legend("topright", legend = series, col = cols, lwd = 2, pch = 16, cex = 0.72, bty = "n")
    return(invisible(lines_df))
  }

  bar_x <- graphics::barplot(
    height = summary_df[[metric]],
    names.arg = summary_df$method,
    col = "#5b8fd1",
    border = "#1a1a1a",
    las = 2,
    ylab = metric,
    main = paste("Benchmark summary:", metric)
  )
  invisible(cbind(summary_df, bar_x = bar_x))
}

pkgload::load_all(".", quiet = TRUE)

n_cores <- max(1L, parallel::detectCores(logical = FALSE) - 1L)

truth_1_1 <- bg_opening_master_truth_build_one(
  roll = "1-1",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_1_2 <- bg_opening_master_truth_build_one(
  roll = "1-2",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_1_3 <- bg_opening_master_truth_build_one(
  roll = "1-3",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_1_4 <- bg_opening_master_truth_build_one(
  roll = "1-4",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_1_5 <- bg_opening_master_truth_build_one(
  roll = "1-5",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_1_6 <- bg_opening_master_truth_build_one(
  roll = "1-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_2_2 <- bg_opening_master_truth_build_one(
  roll = "2-2",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_2_3 <- bg_opening_master_truth_build_one(
  roll = "2-3",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_2_4 <- bg_opening_master_truth_build_one(
  roll = "2-4",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_2_5 <- bg_opening_master_truth_build_one(
  roll = "2-5",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_2_6 <- bg_opening_master_truth_build_one(
  roll = "2-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_3_3 <- bg_opening_master_truth_build_one(
  roll = "3-3",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_3_4 <- bg_opening_master_truth_build_one(
  roll = "3-4",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_3_5 <- bg_opening_master_truth_build_one(
  roll = "3-5",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_3_6 <- bg_opening_master_truth_build_one(
  roll = "3-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_4_4 <- bg_opening_master_truth_build_one(
  roll = "4-4",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_4_5 <- bg_opening_master_truth_build_one(
  roll = "4-5",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_4_6 <- bg_opening_master_truth_build_one(
  roll = "4-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_5_5 <- bg_opening_master_truth_build_one(
  roll = "5-5",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_5_6 <- bg_opening_master_truth_build_one(
  roll = "5-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)

truth_6_6 <- bg_opening_master_truth_build_one(
  roll = "6-6",
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/opening_truths_master",
  overwrite = FALSE,
  seed = 1L
)




game <- bg_play_game(
  board = bg_initial_board(),
  max_turns = 12L,
  selection = "random",
  seed = 123L
)

mid_board <- game$final_board
mid_roll <- bg_roll_dice(seed = 999L)

cat("\nMidgame board\n")
bg_print_board(mid_board)

cat("\nRoll\n")
print(mid_roll)

mid_truth <- bg_master_truth_state(
  state = mid_board,
  roll = mid_roll,
  budget = 2000000L,
  n_cores = n_cores,
  parallel = TRUE,
  truth_block_size = 512L,
  cache = TRUE,
  cache_dir = "cache/state_truths_master",
  overwrite = FALSE,
  seed = 1L
)






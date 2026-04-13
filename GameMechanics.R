pkgload::load_all(".", quiet = TRUE)

board <- bg_initial_board()
roll <- bg_roll(1, 6)
moves <- bg_legal_moves(board, roll)

cat("\nInitial board\n")
bg_print_board(board)

cat("\nRoll\n")
print(roll)

cat("\nFirst legal move\n")
print(moves[[1]])

after <- bg_apply_move_sequence(board, moves[[1]])
cat("\nBoard after first legal move\n")
bg_print_board(after)

turn <- bg_play_turn(board, roll = roll)
cat("\nTurn summary\n")
print(turn)

game <- bg_play_game(board, max_turns = 20L, seed = 1L)
cat("\nGame summary\n")
print(game)

####



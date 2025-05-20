test_that("score_game correctly identifies the winner and ranks players", {
  # Simulate a game
  result <- play_game(n_players = 4)

  # Score the game
  scores <- score_game(result)

  # Basic structure checks
  expect_type(scores, "integer")
  expect_named(scores)
  expect_equal(length(scores), 4)

  # At least one player must have 0 cards (the winner)
  expect_true(any(scores == 0))

  # The winner should be named the same as result$winner
  winner_name <- names(scores)[scores == 0]
  expect_true(result$winner %in% winner_name)

  # Ensure scores are sorted
  expect_equal(sort(scores), scores)
})

test_that("summarise_hand() works for valid player", {
  result <- play_game(3)
  hands <- result$hands

  summary <- summarise_hand(hands, "Player_1")

  expect_type(summary, "integer")
  expect_true(length(summary) >= 1)
  expect_named(summary)
})

test_that("summarise_hand() works for valid player", {
  cards <- new_card_vctr(c("red_3", "blue_skip", "green_2", "yellow_reverse"))
  hands <- list(Player_1 = cards)

  summary <- summarise_hand(hands, "Player_1")

  expect_type(summary, "integer")
  expect_true(length(summary) >= 1)
  expect_named(summary)
})

test_that("deal_hands() returns correct structure for 4 players", {
  deck <- create_uno_deck()
  result <- deal_hands(deck, n_players = 4)

  expect_named(result, c("hands", "deck", "discard"))
  expect_equal(length(result$hands), 4)
  expect_equal(nrow(result$discard), 1)
  expect_equal(nrow(result$deck), 108 - (4 * 7) - 1)

  expect_true(all(sapply(result$hands, nrow) == 7))
  expect_true(all(sapply(result$hands, is.data.frame)))
  expect_true(is.data.frame(result$deck))
  expect_true(is.data.frame(result$discard))
})

test_that("deal_hands() works for multiple player counts", {
  deck <- create_uno_deck()

  result_2 <- deal_hands(deck, 2)
  expect_equal(length(result_2$hands), 2)
  expect_true(all(sapply(result_2$hands, nrow) == 7))

  result_6 <- deal_hands(deck, 6)
  expect_equal(length(result_6$hands), 6)
  expect_true(all(sapply(result_6$hands, nrow) == 7))

  result_10 <- deal_hands(deck, 10)
  expect_equal(length(result_10$hands), 10)
  expect_true(all(sapply(result_10$hands, nrow) == 7))
})

test_that("deal_hands() returns hands with correct columns", {
  deck <- create_uno_deck()
  result <- deal_hands(deck, 4)

  for (hand in result$hands) {
    expect_named(hand, c("color", "value", "type"))
    expect_type(hand$color, "character")
    expect_type(hand$value, "character")
    expect_type(hand$type, "character")
  }
})

test_that("deal_hands() errors when too many players", {
  deck <- create_uno_deck()
  expect_error(deal_hands(deck, n_players = 20), "Not enough cards")
})

test_that("deal_hands() errors with invalid deck input", {
  expect_error(deal_hands("not_a_deck", 4), "deck must be a data frame")
  expect_error(deal_hands(NULL, 4), "deck must be a data frame")
})

test_that("deal_hands() errors with invalid n_players input", {
  deck <- create_uno_deck()

  expect_error(deal_hands(deck, 1))
  expect_error(deal_hands(deck, 4.5))
  expect_error(deal_hands(deck, "four"))
  expect_error(deal_hands(deck, c(2, 3)))
})

test_that("deal_hands() deals correct structure for default case", {
  deck <- create_uno_deck()
  result <- deal_hands(deck, n_players = 4)

  expect_named(result, c("hands", "deck", "discard"))
  expect_equal(length(result$hands), 4)
  expect_true(all(sapply(result$hands, nrow) == 7))
  expect_equal(nrow(result$deck), 108 - (4 * 7) - 1)
  expect_equal(nrow(result$discard), 1)
  expect_true(all(sapply(result$hands, is.data.frame)))
  expect_true(is.data.frame(result$deck))
  expect_true(is.data.frame(result$discard))
})

test_that("deal_hands() works for various valid player counts", {
  deck <- create_uno_deck()
  for (n in 2:10) {
    result <- deal_hands(deck, n_players = n)
    expect_equal(length(result$hands), n)
    expect_true(all(sapply(result$hands, nrow) == 7))
    expect_equal(nrow(result$discard), 1)
    expect_equal(nrow(result$deck), 108 - (n * 7) - 1)
  }
})

test_that("deal_hands() fails with too many players", {
  deck <- create_uno_deck()
  expect_error(
    deal_hands(deck, n_players = 20),
    regexp = "Too many players"
  )
})

test_that("deal_hands() fails with invalid deck types", {
  expect_error(deal_hands("not_a_deck", 4), regexp = "`deck` must be a data frame")
  expect_error(deal_hands(NULL, 4), regexp = "`deck` must be a data frame")
})

test_that("deal_hands() fails with invalid n_players values", {
  deck <- create_uno_deck()
  msg <- "`n_players` must be a single positive whole number \\(≥ 2\\).*"

  expect_error(deal_hands(deck, n_players = 1), regexp = msg)
  expect_error(deal_hands(deck, n_players = 4.5), regexp = msg)
  expect_error(deal_hands(deck, n_players = "three"), regexp = msg)
  expect_error(deal_hands(deck, n_players = factor(4)), regexp = msg)
  expect_error(deal_hands(deck, n_players = c(2, 3)), regexp = msg)
})

test_that("deal_hands() returns valid column structure in each hand", {
  deck <- create_uno_deck()
  result <- deal_hands(deck, 4)
  for (hand in result$hands) {
    expect_named(hand, c("color", "value", "type"))
    expect_type(hand$color, "character")
    expect_type(hand$value, "character")
    expect_type(hand$type, "character")
  }
})

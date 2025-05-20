test_that("setup_game() returns correct structure", {
  state <- setup_game(n_players = 4)
  expect_named(state, c("hands", "deck", "discard", "direction", "turn"))
  expect_type(state$hands, "list")
  expect_equal(length(state$hands), 4)
  expect_true(all(sapply(state$hands, nrow) == 7))
  expect_equal(nrow(state$discard), 1)
  expect_true(is.data.frame(state$deck))
  expect_true(is.integer(state$direction) || is.numeric(state$direction))
  expect_true(state$direction %in% c(1, -1))
  expect_true(state$turn %in% 1:4)
})

test_that("setup_game() works for various valid player counts", {
  for (n in 2:10) {
    state <- setup_game(n)
    expect_equal(length(state$hands), n)
    expect_true(all(sapply(state$hands, nrow) == 7))
    expect_equal(nrow(state$discard), 1)
  }
})

test_that("play_turns_loop() completes and returns a winner", {
  state <- setup_game(4)
  result <- play_turns_loop(
    hands = state$hands,
    deck = state$deck,
    discard = state$discard,
    direction = state$direction,
    turn = state$turn
  )
  expect_named(result, c("winner", "hands", "discard"))
  expect_true(result$winner %in% names(result$hands))
  expect_equal(nrow(result$hands[[result$winner]]), 0)
})

test_that("play_turns_loop() handles exhausted deck gracefully", {
  state <- setup_game(4)
  # Remove the draw pile to simulate exhaustion
  result <- play_turns_loop(
    hands = state$hands,
    deck = state$deck[0, ],
    discard = state$discard,
    direction = state$direction,
    turn = state$turn
  )
  expect_null(result$winner)
  expect_match(result$reason, "Deck exhausted")
})

test_that("play_game() returns valid structure", {
  result <- play_game(n_players = 4)
  expect_named(result, c("winner", "hands", "discard"))
  expect_true(result$winner %in% names(result$hands))
  expect_equal(nrow(result$hands[[result$winner]]), 0)
  expect_true(is.data.frame(result$discard))
})

test_that("play_game() fails on invalid player counts", {
  expect_error(play_game(n_players = 1), "must be a single whole number")
  expect_error(play_game(n_players = 4.5), "must be a single whole number")
  expect_error(play_game(n_players = "four"), "must be a single whole number")
  expect_error(play_game(n_players = c(2, 3)), "must be a single whole number")
})

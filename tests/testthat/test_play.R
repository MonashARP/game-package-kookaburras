test_that("play_game() returns correct structure", {
  result <- play_game(4)

  expect_type(result, "list")
  expect_named(result, c("winner", "hands", "discard"))
  expect_true(is.character(result$winner) || is.null(result$winner))
  expect_true(is.list(result$hands))
  expect_s3_class(result$discard, "card_vctr")
})

test_that("play_game() assigns correct number of hands", {
  result <- play_game(4)
  expect_equal(length(result$hands), 4)
  expect_named(result$hands, paste0("Player_", 1:4))
})

test_that("play_game() discard pile is valid card_vctr", {
  result <- play_game(4)
  discard <- result$discard

  expect_s3_class(discard, "card_vctr")
  expect_gt(length(discard), 1)

  # Optional formatting check
  expect_true(all(grepl("^[a-z]+_.+$", vctrs::vec_data(discard))))
})

test_that("play_game() works for different player counts", {
  for (n in 2:6) {
    result <- play_game(n)
    expect_equal(length(result$hands), n)
  }
})

test_that("winner is one of the players or NULL", {
  result <- play_game(4)
  valid_names <- paste0("Player_", 1:4)
  expect_true(is.null(result$winner) || result$winner %in% valid_names)
})

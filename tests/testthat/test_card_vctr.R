test_that("card_vctr class works correctly", {
  cards <- new_card_vctr(c("red_5", "blue_skip", "wild_draw4"))

  # Class and internal structure
  expect_s3_class(cards, "card_vctr")
  expect_type(vctrs::vec_data(cards), "character")

  # Format output
  expect_equal(
    format(cards),
    c(
      "red      | 5          | number",
      "blue     | skip       | action",
      "wild     | draw4      | wild"
    )
  )

  # Suit and value methods
  expect_equal(card_suit(cards), c("red", "blue", "wild"))
  expect_equal(card_value(cards), c("5", "skip", "draw4"))
})

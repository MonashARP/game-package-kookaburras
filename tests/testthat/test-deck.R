test_that("UNO deck has 108 cards", {
  deck <- create_uno_deck()
  expect_equal(nrow(deck), 108)
})

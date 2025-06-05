test_that("create_uno_deck() returns a tibble with 108 cards", {
  deck <- create_uno_deck()
  expect_s3_class(deck, "tbl_df")
  expect_equal(nrow(deck), 108)
})

test_that("deck has correct columns and no missing values", {
  deck <- create_uno_deck()
  expect_named(deck, c("color", "value", "type"))
  expect_false(any(is.na(deck)))
})

test_that("number of each type of card is correct", {
  deck <- create_uno_deck()
  type_counts <- table(deck$type)

  expect_true("number" %in% names(type_counts))
  expect_true("action" %in% names(type_counts))
  expect_true("wild" %in% names(type_counts))

  expect_equal(type_counts[["number"]], 76)
  expect_equal(type_counts[["action"]], 24)
  expect_equal(type_counts[["wild"]], 8)
})

test_that("each action card appears twice per color", {
  deck <- create_uno_deck()
  expect_equal(sum(deck$color == "red" & deck$value == "skip"), 2)
  expect_equal(sum(deck$color == "red" & deck$value == "reverse"), 2)
  expect_equal(sum(deck$color == "red" & deck$value == "+2"), 2)

  expect_equal(sum(deck$color == "blue" & deck$value == "skip"), 2)
  expect_equal(sum(deck$color == "blue" & deck$value == "reverse"), 2)
  expect_equal(sum(deck$color == "blue" & deck$value == "+2"), 2)

  expect_equal(sum(deck$color == "green" & deck$value == "skip"), 2)
  expect_equal(sum(deck$color == "green" & deck$value == "reverse"), 2)
  expect_equal(sum(deck$color == "green" & deck$value == "+2"), 2)

  expect_equal(sum(deck$color == "yellow" & deck$value == "skip"), 2)
  expect_equal(sum(deck$color == "yellow" & deck$value == "reverse"), 2)
  expect_equal(sum(deck$color == "yellow" & deck$value == "+2"), 2)
})

test_that("wild cards appear exactly four times each", {
  deck <- create_uno_deck()
  expect_equal(sum(deck$value == "wild"), 4)
  expect_equal(sum(deck$value == "wild_draw4"), 4)
  expect_true(all(deck$color[deck$type == "wild"] == "wild"))
})

test_that("each color has correct number cards", {
  deck <- create_uno_deck()
  for (col in c("red", "green", "blue", "yellow")) {
    expect_equal(sum(deck$color == col & deck$type == "number" & deck$value == "0"), 1)
    for (num in as.character(1:9)) {
      expect_equal(sum(deck$color == col & deck$type == "number" & deck$value == num), 2)
    }
  }
})

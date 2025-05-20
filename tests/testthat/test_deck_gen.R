test_that("create_uno_deck() generates a complete valid deck", {
  deck <- create_uno_deck()

  # Check total number of cards
  expect_equal(nrow(deck), 108)

  # Check card type counts
  type_counts <- table(deck$type)
  expected <- c(number = 76, action = 24, wild = 8)
  actual <- as.vector(type_counts)
  names(actual) <- names(type_counts)
  expect_equal(actual[names(expected)], expected)

  # Check action card counts
  action_counts <- table(deck$value[deck$type == "action"])
  expect_equal(as.integer(action_counts["skip"]), 8)
  expect_equal(as.integer(action_counts["reverse"]), 8)
  expect_equal(as.integer(action_counts["+2"]), 8)

  # Check wild card counts
  wild_counts <- table(deck$value[deck$type == "wild"])
  expect_equal(as.integer(wild_counts["wild"]), 4)
  expect_equal(as.integer(wild_counts["wild_draw4"]), 4)

  # Check number card count for 0 and others
  zero_counts <- sum(deck$value == "0" & deck$type == "number")
  other_num_counts <- sum(deck$value %in% as.character(1:9) & deck$type == "number")
  expect_equal(zero_counts, 4)        # One per color
  expect_equal(other_num_counts, 72)  # Two per color x 9 = 18 x 4 = 72

  # Validate column names
  expect_named(deck, c("color", "value", "type"))

  # Ensure no missing values
  expect_false(any(is.na(deck)))

  # Verify number and action cards per color
  colors <- c("red", "green", "blue", "yellow")
  numbers <- as.character(0:9)
  actions <- c("skip", "reverse", "+2")

  for (col in colors) {
    # One zero per color
    expect_equal(
      sum(deck$color == col & deck$type == "number" & deck$value == "0"),
      1,
      info = paste("Expected 1 zero for", col)
    )

    # Two of each number from 1 to 9
    for (n in numbers[2:10]) {
      expect_equal(
        sum(deck$color == col & deck$type == "number" & deck$value == n),
        2,
        info = paste("Expected 2 of number", n, "for", col)
      )
    }

    # Two of each action card
    for (act in actions) {
      expect_equal(
        sum(deck$color == col & deck$type == "action" & deck$value == act),
        2,
        info = paste("Expected 2 of action", act, "for", col)
      )
    }
  }

  # Check data types of all columns
  expect_type(deck$color, "character")
  expect_type(deck$value, "character")
  expect_type(deck$type, "character")

  # Check that wild-colored cards are only of type "wild"
  expect_false(any(deck$color == "wild" & deck$type != "wild"))

  # Check that all wild cards are also colored "wild"
  expect_false(any(deck$type == "wild" & deck$color != "wild"))

  # Ensure no invalid values or types
  valid_colors <- c("red", "green", "blue", "yellow", "wild")
  valid_types <- c("number", "action", "wild")
  valid_values <- c(as.character(0:9), "skip", "reverse", "+2", "wild", "wild_draw4")

  expect_true(all(deck$color %in% valid_colors))
  expect_true(all(deck$type %in% valid_types))
  expect_true(all(deck$value %in% valid_values))

  # Check that all card combinations are valid
  allowed_normal <- expand.grid(
    color = colors,
    type = c("number", "action"),
    value = c(as.character(0:9), actions)
  )
  allowed_wild <- expand.grid(
    color = "wild",
    type = "wild",
    value = c("wild", "wild_draw4")
  )
  allowed_deck <- rbind(allowed_normal, allowed_wild)
  deck_check <- deck[, c("color", "type", "value")]
  deck_check[] <- lapply(deck_check, as.character)
  allowed_deck[] <- lapply(allowed_deck, as.character)

  row_match <- apply(deck_check, 1, function(row) {
    any(apply(allowed_deck, 1, function(allowed) all(row == allowed)))
  })
  expect_true(all(row_match), info = "Deck contains invalid card combinations")
})

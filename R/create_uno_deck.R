#' Creating a UNO deck
#'
#' Builds a standard 108-card UNO deck with numbers, actions, and wild cards.
#'
#' @return A tibble with columns: color, value, type.
#' @export
create_uno_deck <- function() {
  colors <- c("red", "green", "blue", "yellow")
  numbers <- as.character(0:9)
  actions <- c("skip", "reverse", "+2")
  wilds <- c("wild", "wild_draw4")

  # Creating number cards
  number_list <- list()
  for (col in colors) {

    # Each colour has one '0' card and two of '1-9' cards
    zero <- data.frame(color = col, value = "0", type = "number")
    nums <- data.frame(
      color = rep(col, each = 2 * length(numbers[-1])),
      value = rep(numbers[-1], each = 2),
      type = "number"
    )

    # List of data frames for each color
    number_list[[length(number_list) + 1]] <- rbind(zero, nums)
  }

  # Creating action cards
  action_list <- list()
  for (col in colors) {

    # Each color has two of each action card
    acts <- data.frame(
      color = rep(col, each = 2 * length(actions)),
      value = rep(actions, each = 2),
      type = "action"
    )

    # List of data frames for each color
    action_list[[length(action_list) + 1]] <- acts
  }

  # Creating wild cards, no colour
  wild_cards <- data.frame(
    color = "wild",
    value = rep(wilds, each = 4),
    type = "wild"
  )

  # Creating full deck by joining above data frames
  full_deck <- rbind(
    rbind(number_list[[1]], number_list[[2]], number_list[[3]], number_list[[4]]),
    rbind(action_list[[1]], action_list[[2]], action_list[[3]], action_list[[4]]),
    wild_cards
  )

  # Converting to tibble
  tibble::as_tibble(full_deck)
}



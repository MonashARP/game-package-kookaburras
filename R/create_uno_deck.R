#' Create a Standard UNO Deck
#'
#' Builds the complete 108-card UNO deck used in a standard game.
#' Each of the four colors — red, green, blue, and yellow — includes:
#' \itemize{
#'   \item One \strong{0} card
#'   \item Two copies each of cards numbered \strong{1} through \strong{9}
#'   \item Two copies each of the action cards: \strong{skip}, \strong{reverse}, and \strong{+2}
#' }
#' Additionally, the deck includes eight uncolored wild cards:
#' \itemize{
#'   \item Four \strong{wild} cards
#'   \item Four \strong{wild_draw4} cards
#' }
#'
#' This function is typically used as the first step in game setup, and
#' returns a tidy tibble containing all card data.
#'
#' @return A tibble with 108 rows and 3 columns:
#' \describe{
#'   \item{color}{Card color — one of \code{"red"}, \code{"green"}, \code{"blue"}, \code{"yellow"}, or \code{"wild"}}
#'   \item{value}{Card face value — numbers \code{"0"} to \code{"9"}, or action names}
#'   \item{type}{Card type — either \code{"number"}, \code{"action"}, or \code{"wild"}}
#' }
#'
#' @examples
#' # Generate and inspect the deck
#' deck <- create_uno_deck()
#' dim(deck)         # Should return 108 rows, 3 columns
#' table(deck$type)  # Count of card types
#' head(deck)        # Preview the top of the deck
#'
#' # Filter for wild cards
#' deck[deck$type == "wild", ]
#'
#' # Count how many of each card value exist
#' table(deck$value)
#'
#' # Filter just the blue action cards
#' subset(deck, color == "blue" & type == "action")
#'
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



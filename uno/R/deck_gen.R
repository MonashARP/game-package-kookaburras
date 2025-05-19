# Load required tidyverse libraries
library(tidyverse)

# Defines the components of the deck
colors <- c("red", "green", "blue", "yellow")
numbers <- 0:9
actions <- c("skip", "reverse", "+2")
wilds <- c("wild", "wild_draw4")

#' Create a Complete UNO Deck
#'
#' Constructs a full UNO card deck consisting of 108 cards, following official game rules.
#' The function generates all standard components: number cards (0–9), action cards (Skip, Reverse, +2),
#' and wild cards (Wild, Wild Draw 4). This is the foundational step in setting up a UNO game simulation.
#'
#' For each of the four main colors (red, green, blue, yellow), the deck includes:
#' \itemize{
#'   \item One copy of the number card 0
#'   \item Two copies of each number card from 1 to 9
#'   \item Two copies of each action card: \code{"skip"}, \code{"reverse"}, and \code{"+2"}
#' }
#' In addition to the color-specific cards, the deck contains:
#' \itemize{
#'   \item Four \code{"wild"} cards
#'   \item Four \code{"wild_draw4"} cards
#' }
#'
#' The resulting deck is sorted by color, card type, and value for readability and is ready for dealing via
#' \code{\link{deal_hands}} or gameplay via \code{\link{play_game}}.
#'
#' @details
#' The UNO deck is generated using a data frame expansion approach powered by \code{expand_grid()} and
#' \code{dplyr} tools. Number cards are replicated using a conditional multiplier to meet UNO rules. \cr
#' This function is designed to be reproducible and efficient for simulations and testing. \cr
#' It returns a tibble suitable for direct use in the \code{\link{deal_hands}} function or shuffling workflows.
#'
#' @return A tibble (data frame) with exactly 108 rows and 3 columns:
#' \itemize{
#'   \item \strong{color} – Character. Card color: one of \code{"red"}, \code{"green"}, \code{"blue"}, \code{"yellow"}, or \code{"wild"}
#'   \item \strong{value} – Character. Either a number (as string), action (e.g. \code{"reverse"}), or wild type
#'   \item \strong{type} – Character. Card type: one of \code{"number"}, \code{"action"}, or \code{"wild"}
#' }
#'
#' @examples
#' # Generate the full UNO deck
#' deck <- create_uno_deck()
#'
#' # Check number of cards
#' nrow(deck)  # Should return 108
#'
#' # Summary by type
#' table(deck$type)
#'
#' # Check unique card colors
#' unique(deck$color)
#'
#' # Preview the first few rows
#' head(deck)
#'
#' # Use with deal_hands()
#' dealt <- deal_hands(deck, n_players = 4)
#' lapply(dealt$hands, head)
#'
#' @export
create_uno_deck <- function() {
  # Generating Number Cards
  number_cards <- expand_grid(
    color = colors,
    value = as.character(numbers)
  ) %>%
    mutate(type = "number") %>%
    group_by(value) %>%
    mutate(replication = if_else(value == "0", 1, 2)) %>%
    ungroup() %>%
    slice(rep(1:n(), replication)) %>%
    select(-replication)

  # Generated the Action Cards
  action_cards <- expand_grid(
    color = colors,
    value = actions
  ) %>%
    mutate(type = "action") %>%
    slice(rep(1:n(), 2))

  #  Generating the Wild Cards
  wild_cards <- expand_grid(
    color = "wild",
    value = wilds
  ) %>%
    mutate(type = "wild") %>%
    slice(rep(1:n(), 4))

  # Combining All Cards into a Deck for the Game
  deck <- bind_rows(number_cards, action_cards, wild_cards) %>%
    arrange(color, type, value)

  return(deck)
}

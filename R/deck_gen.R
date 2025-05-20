#' Create a Complete UNO Deck
#'
#' Constructs a full UNO card deck consisting of 108 cards, following official game rules.
#' This includes number cards (0–9), action cards (Skip, Reverse, +2), and wild cards (Wild, Wild Draw 4).
#' The resulting deck is returned as a tidy tibble and is sorted by color, type, and value.
#'
#' Each of the four colors (\code{red}, \code{green}, \code{blue}, \code{yellow}) includes:
#' \itemize{
#'   \item One \code{0} card
#'   \item Two of each number card from \code{1–9}
#'   \item Two of each action card: \code{"skip"}, \code{"reverse"}, \code{"+2"}
#' }
#'
#' Additionally:
#' \itemize{
#'   \item Four \code{"wild"} cards
#'   \item Four \code{"wild_draw4"} cards
#' }
#'
#' @details
#' - This function uses \code{expand_grid()} and \code{uncount()} to generate and replicate UNO cards. \cr
#' - Number and action cards are repeated according to official UNO rules. \cr
#' - The function returns a tibble with 108 cards structured for use in simulations. \cr
#'
#' This deck can be passed directly to downstream functions such as \code{\link{deal_hands}} or \code{\link{play_game}}.
#'
#' @return
#' A tibble with exactly 108 rows and 3 columns:
#' \itemize{
#'   \item \strong{color} – Card color: one of \code{"red"}, \code{"green"}, \code{"blue"}, \code{"yellow"}, or \code{"wild"}
#'   \item \strong{value} – Card value: a string representing number, action, or wild type (e.g., \code{"5"}, \code{"+2"}, \code{"wild_draw4"})
#'   \item \strong{type} – Card type: one of \code{"number"}, \code{"action"}, or \code{"wild"}
#' }
#'
#' @examples
#' # Generate the UNO deck
#' deck <- create_uno_deck()
#'
#' # Validate the total card count
#' nrow(deck)  # Should return 108
#'
#' # Check card distribution
#' table(deck$type)
#'
#' # Preview color types
#' unique(deck$color)
#'
#' # Use the deck to deal to players
#' hands <- deal_hands(deck, n_players = 4)
#' sapply(hands$hands, nrow)
#'
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid uncount
#' @importFrom dplyr mutate bind_rows arrange if_else
#' @importFrom magrittr %>%
#' @export
create_uno_deck <- function() {

  # Defines the components of the deck
  colors <- c("red", "green", "blue", "yellow")
  numbers <- 0:9
  actions <- c("skip", "reverse", "+2")
  wilds <- c("wild", "wild_draw4")

  # Generating Number Cards
  number_cards <- expand_grid(
    color = colors,
    value = as.character(numbers)
  )  %>%
    mutate(type = "number", times = if_else(value == "0", 1L, 2L)) %>%
    uncount(times)

  # Generated the Action Cards
  action_cards <- expand_grid(
    color = colors,
    value = actions
  ) %>%
    mutate(type = "action") %>%
    uncount(2)

  wild_cards <- tibble(color = "wild", value = wilds, type = "wild") %>%
    uncount(4)

  deck <- bind_rows(number_cards, action_cards, wild_cards) %>%
    arrange(color, type, value)

  return(deck)


}






#' Deal Cards to UNO Players
#'
#' Shuffles the full UNO deck and deals 7 cards to each player. After dealing,
#' the next card from the deck is used to start the discard pile, and the
#' remaining cards form the draw pile. Player hands are returned as a named
#' list, and the function ensures there are enough cards to support the number
#' of players.
#'
#' This function is typically called during game setup, after generating the deck
#' using \code{\link{create_uno_deck}}.
#'
#' @param deck A tibble of UNO cards, typically created using \code{create_uno_deck()}.
#' @param n_players Number of players (must be 2 or more). Defaults to 4.
#'
#' @return A list with three components:
#' \describe{
#'   \item{hands}{A named list of tibbles, each with 7 cards for one player}
#'   \item{deck}{A tibble of remaining cards (the draw pile)}
#'   \item{discard}{A one-row tibble representing the initial discard card}
#' }
#'
#' @examples
#' set.seed(123)
#' deck <- create_uno_deck()
#' result <- deal_hands(deck, n_players = 4)
#'
#' # View Player 1's hand
#' result$hands$Player_1
#'
#' # Check the discard card
#' result$discard
#'
#' # Number of cards left in the draw pile
#' nrow(result$deck)
#'
#' # Validate hand sizes
#' sapply(result$hands, nrow)
#'
#' @export
deal_hands <- function(deck, n_players = 4) {

  # Checking input is of valid type and at least 2 players to play.
  if (!is.data.frame(deck)) stop("deck must be a data frame.")
  if (!is.numeric(n_players) || n_players < 2) stop("n_players must numeric and be at least 2.")

  # Checking the deck has enough cards to deal based on number of players
  cards_per_player <- 7
  total_needed <- n_players * cards_per_player

  # Stopping if more number of players than and not enough cards to deal
  if (nrow(deck) < total_needed + 1) {
    stop("Not enough cards to deal hands and a discard.")
  }

  # Shuffling the deck
  shuffled <- deck[sample(nrow(deck)), , drop = FALSE]

  # Splitting the shuffled deck into player cards set, starting card and remaining deck
  # to use as draw pile during rounds
  dealt_cards <- shuffled[1:total_needed, , drop = FALSE]
  discard <- shuffled[total_needed + 1, , drop = FALSE]
  remaining_deck <- shuffled[(total_needed + 2):nrow(shuffled), , drop = FALSE]

  # Splitting the dealt cards into hands for each player
  hands <- split(dealt_cards, rep(1:n_players, each = cards_per_player))
  names(hands) <- paste0("Player_", seq_len(n_players))

  # Returning the list with hands, remaining deck and discard card
  list(
    hands = hands,
    deck = remaining_deck,
    discard = discard
  )
}



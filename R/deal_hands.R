#' Deal 7 cards to each player after shuffling the deck
#'
#'
#'
#' @param deck A tibble with UNO cards created using create_deck()
#' @param n_players Number of players
#'
#' @return A list with:
#' hands - list of individual player card set,
#' deck - remaining cards forming the draw pile and
#' discard - the starting card for the game to begin
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



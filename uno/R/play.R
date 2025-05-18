#' Set up UNO game with shuffled deck and dealt hands
#'
#' @param n_players Number of players (default = 4).
#' @return A list containing hands, deck, discard pile, turn, and direction.
#' @export
setup_game <- function(n_players = 4) {
  state <- deal_hands(create_uno_deck(), n_players)
  list(
    hands = state$hands,
    deck = state$deck,
    discard = state$discard,
    direction = 1,
    turn = 1
  )
}

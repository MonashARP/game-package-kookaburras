#' Summarise the hand of a specific player
#'
#' @description
#' Given a named list of player hands, this function summarizes how many cards
#' of each color a specific player has.
#'
#' @param hands A named list of `card_vctr` hands.
#' @param player A character string, e.g., "Player_1".
#'
#' @return A named integer vector with counts of each color.
#'
#' @importFrom stats setNames
#' @export
summarise_hand <- function(hands, player) {
  cards <- hands[[player]]
  tbl <- table(card_suit(cards))
  setNames(as.integer(tbl), names(tbl))
}

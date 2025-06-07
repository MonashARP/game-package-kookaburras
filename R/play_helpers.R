#' Summarise the hand of a specific player
#'
#' @param hands A named list of card_vctr hands
#' @param player A character string, e.g., "Player_1"
#' @return A named integer vector with counts of each color
#' @export
summarise_hand <- function(hands, player) {
  cards <- hands[[player]]
  tbl <- table(card_suit(cards))
  setNames(as.integer(tbl), names(tbl))
}

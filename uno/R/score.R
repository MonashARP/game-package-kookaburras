#' Score the UNO game
#'
#' Calculates the number of cards left for each player at the end of the game,
#' and ranks them by who won first (0 cards) and who had fewer cards.
#'
#' @param game_state Output from `play_game()`.
#' @return A named vector of remaining card counts per player, sorted from least to most.
#' @export
score_game <- function(game_state) {
  scores <- sapply(game_state$hands, nrow)
  scores <- sort(scores)
  return(scores)
}

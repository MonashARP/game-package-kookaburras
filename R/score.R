#' Compute Final UNO Scores
#'
#' Calculates and ranks the final scores for each player at the end of a UNO game.
#' Scores are determined by counting how many cards remain in each player's hand.
#' The player who played all their cards first (i.e., has 0 cards) is the winner.
#'
#' This function is designed to be called on the output from \code{\link{play_game}()}.
#' It returns a named integer vector of remaining card counts per player, sorted
#' from the fewest cards to the most.
#'
#' @details
#' - The result highlights the winner (score of 0), followed by players with remaining cards. \cr
#' - The function assumes that \code{game_state$hands} is a named list of tibbles representing
#'   each player's hand after the game loop has ended. \cr
#' - The sorted output makes it easy to visualize rankings and detect close finishes in competitive analysis or simulations.
#'
#' This function is useful for summary statistics, ranking output, or leaderboard generation
#' in extended gameplay sessions or Monte Carlo simulations.
#'
#' @param game_state A list containing the final game state as returned by
#'   \code{\link{play_game}()}. Must contain a named list element called \code{hands},
#'   where each element represents a player’s hand (a tibble of remaining cards).
#'
#' @return A named integer vector where:
#' \describe{
#'   \item{Names}{Player names (e.g., \code{"Player_1"}, \code{"Player_2"}, ...)}
#'   \item{Values}{Number of cards left in each player's hand. The winner will always have a value of \code{0}.}
#' }
#'
#' @examples
#' # Simulate a game with 4 players
#' result <- play_game(n_players = 4)
#'
#' # Score the game
#' score_game(result)
#'
#' # Identify winner programmatically
#' which(score_game(result) == 0)
#'
#' # Visualize ranking
#' barplot(score_game(result), main = "UNO Player Scores", col = "skyblue")
#'
#' @export
score_game <- function(game_state) {
  scores <- sapply(game_state$hands, nrow)
  scores <- sort(scores)
  return(scores)
}

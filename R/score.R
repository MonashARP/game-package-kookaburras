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
#'
#' Summarizes final hands and the winner after `play_game()`.
#'
#' @param game_state A list returned by `play_game()`
#'
#' @return A tibble with players, cards remaining, and result status (printed).
#'         Invisibly returns the summary tibble for programmatic use.
#' @export
score_game <- function(game_state) {
  hands <- game_state$hands
  winner <- game_state$winner
  reason <- game_state$reason %||% "Game completed"

  player_names <- names(hands)
  card_counts <- vapply(hands, nrow, integer(1))

  score_table <- tibble::tibble(
    Player = player_names,
    Cards_Remaining = card_counts,
    Result = ifelse(player_names == winner, "🏆 Winner", "")
  ) |>
    dplyr::arrange(Cards_Remaining)

  final_hands <- dplyr::bind_rows(
    lapply(player_names, function(p) dplyr::mutate(hands[[p]], Player = p)),
    .id = NULL
  ) |>
    dplyr::select(Player, color, value, type)

  cat("\n🎯 Final Score Summary:\n")
  print(score_table, n = nrow(score_table))

  if (!is.null(winner)) {
    cat(glue::glue("\n🎉 {winner} had 0 cards left and wins the game!\n"))
  } else {
    cat(glue::glue("\n⛔ Game ended without a winner: {reason}\n"))
  }

  cat("\n🃏 Final Hands Table:\n")
  print(final_hands, n = nrow(final_hands))

  invisible(score_table)
}

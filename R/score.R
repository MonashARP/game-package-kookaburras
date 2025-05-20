#' Compute and Display Final UNO Scores
#'
#' Calculates, ranks, and prints the final scores of all players at the end of a UNO game.
#' Scores are determined by counting how many cards remain in each player's hand. The winner
#' is identified as the player with zero cards.
#'
#' This function is typically called after simulating a full game using \code{\link{play_game}()}.
#' In addition to returning a summary tibble, it prints a formatted score table and the final hands
#' of each player for transparency and post-game analysis.
#'
#' @param game_state A named list returned by \code{\link{play_game}()} containing:
#'   \itemize{
#'     \item \code{hands} – A named list of tibbles, each representing a player's final hand.
#'     \item \code{winner} – Character string of the winning player (e.g., \code{"Player_2"}), or \code{NULL} if no winner.
#'     \item \code{reason} – Optional message explaining why the game ended (e.g., deck exhausted).
#'   }
#'
#' @details
#' - Each player's hand is evaluated by counting remaining cards. \cr
#' - The output tibble ranks players from lowest to highest card count. \cr
#' - The result is printed in a user-friendly format showing the winner and all final hands. \cr
#' - This is ideal for logging, leaderboard generation, and simulation diagnostics. \cr
#'
#' \strong{Note:} If no winner exists (e.g., the deck ran out), a warning message is shown.
#'
#' @return A tibble with 3 columns:
#' \describe{
#'   \item{\strong{Player}}{Character. Player name (e.g., \code{"Player_1"}, \code{"Player_2"}, ...)}
#'   \item{\strong{Cards_Remaining}}{Integer. Number of cards remaining in each player’s hand.}
#'   \item{\strong{Result}}{Character. \code{"🏆 Winner"} for the winning player, blank for others.}
#' }
#'
#' The summary tibble is returned invisibly (useful for programmatic scoring), but results are also printed.
#'
#' @examples
#' # Simulate and score a complete game
#' result <- play_game(n_players = 4)
#' score_game(result)
#'
#' # Get winner programmatically
#' winner <- score_game(result) |> dplyr::filter(Result == "🏆 Winner") |> dplyr::pull(Player)
#'
#' # Visualize the score table
#' barplot(score_game(result)$Cards_Remaining,
#'         names.arg = score_game(result)$Player,
#'         col = "skyblue", main = "Final Scores")
#'
#' @export
score_game <- function(game_state) {
  hands <- game_state$hands
  winner <- game_state$winner
  reason <- game_state$reason %||% "Game completed"

  player_names <- names(hands)
  card_counts <- vapply(hands, nrow, integer(1))

  score_table <- tibble(
    Player = player_names,
    Cards_Remaining = card_counts,
    Result = ifelse(player_names == winner, "🏆 Winner", "")
  ) |>
    arrange(Cards_Remaining)

  final_hands <- bind_rows(
    lapply(player_names, function(p) dplyr::mutate(hands[[p]], Player = p)),
    .id = NULL
  ) |>
    dplyr::select(Player, color, value, type)


  cat("\n🎯 Final Score Summary:\n")
  print.data.frame(score_table, row.names = FALSE)

  if (!is.null(winner)) {
    cat(glue::glue("\n🎉 {winner} had 0 cards left and wins the game!\n"))
  } else {
    cat(glue::glue("\n⛔ Game ended without a winner: {reason}\n"))
  }

  cat("\n🃏 Final Hands Table:\n")
  print.data.frame(final_hands, row.names = FALSE)

  invisible(score_table)
}

#' Initialize a UNO Game Session
#'
#' Sets up a playable UNO game state by generating a complete deck, shuffling it,
#' dealing 7 cards to each player, drawing the first discard card, and initializing
#' gameplay direction and player turn order.
#'
#' This is typically the first step before calling \code{\link{play_turns_loop}}
#' or \code{\link{play_game}} to simulate gameplay.
#'
#' @param n_players Integer. Number of players to initialize. Default is 4. \cr
#'   This version supports any value ≥ 2 (limited by card count). \cr
#'   \strong{Note:} Future versions aim to support extended player formats with
#'   rule adaptations and multi-deck configurations.
#'
#' @details
#' - The function shuffles a standard 108-card deck via \code{\link{create_uno_deck}} \cr
#' - Cards are dealt using \code{\link{deal_hands}}, one per player for 7 rounds \cr
#' - One card is drawn to start the discard pile \cr
#' - Turn direction is set to clockwise (\code{1}); turn index starts from Player 1
#'
#' @return A named list containing:
#' \itemize{
#'   \item \strong{hands} – Named list of tibbles, one per player, each with 7 cards
#'   \item \strong{deck} – Remaining deck (after hands and discard) used as the draw pile
#'   \item \strong{discard} – One-row tibble showing the top discard card
#'   \item \strong{direction} – Integer. Initial play direction. \code{1} = clockwise, \code{-1} = counter-clockwise
#'   \item \strong{turn} – Integer. Starting player's index (1-based)
#' }
#'
#' @examples
#' # Create initial game state for 4 players
#' game_state <- setup_game(n_players = 4)
#'
#' # View the structure of output
#' names(game_state)
#'
#' # Access Player 1’s hand
#' game_state$hands$Player_1
#'
#' # View initial discard card
#' game_state$discard
#'
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

#' Simulate UNO Turn-by-Turn Gameplay
#'
#' Executes the main gameplay loop of UNO by iterating through player turns until a winner is found.
#' Handles all playable card logic, card draws, action cards (Skip, Reverse, +2), and wild card behavior,
#' including forced draws and color changes.
#'
#' This function is best used internally by \code{\link{play_game}} or during custom simulations
#' where you want to experiment with specific starting states.
#'
#' @param hands A named list of tibbles representing each player’s hand.
#'   Each tibble must have columns \code{color}, \code{value}, and \code{type}.
#'
#' @param deck A tibble representing the remaining draw pile.
#'   Cards are removed from here when players must draw.
#'
#' @param discard A tibble representing the discard pile.
#'   The last row is treated as the top card in play.
#'
#' @param direction An integer indicating turn order.
#'   \code{1} for clockwise (default), \code{-1} for counter-clockwise.
#'
#' @param turn Integer. The current player’s turn index (1-based).
#'   This is updated automatically within the loop.
#'
#' @details
#' - Handles action cards: \code{"skip"}, \code{"reverse"}, \code{"+2"}, \code{"wild"}, \code{"wild_draw4"} \cr
#' - If no playable card, player draws 1 from deck \cr
#' - Turn logic accounts for action card effects (e.g., next player drawing, reversing direction) \cr
#' - Wilds automatically assign a random color
#'
#' @return A named list with the following components:
#' \itemize{
#'   \item \strong{winner} – A character string. Name of the winning player (e.g., \code{"Player_3"}).
#'   \item \strong{hands} – A named list of tibbles showing the final hands of all players.
#'   Only the winner has zero cards.
#'   \item \strong{discard} – A tibble representing the final discard pile, including the last played card.
#' }
#'
#' @examples
#' # Full setup and simulation step-by-step
#' state <- setup_game()
#' outcome <- play_turns_loop(
#'   hands = state$hands,
#'   deck = state$deck,
#'   discard = state$discard,
#'   direction = state$direction,
#'   turn = state$turn
#' )
#' outcome$winner
#'
#' @export
play_turns_loop <- function(hands, deck, discard, direction, turn) {
  n_players <- length(hands)

  repeat {
    current_player <- paste0("Player_", turn)
    hand <- hands[[current_player]]
    top_card <- tail(discard, 1)

    playable <- which(
      hand$color == top_card$color |
        hand$value == top_card$value |
        hand$color == "wild"
    )

    if (length(playable) == 0) {
      if (nrow(deck) == 0) stop("Deck exhausted!")
      drawn <- deck[1, , drop = FALSE]
      deck <- deck[-1, ]
      hand <- rbind(hand, drawn)
    } else {
      play_idx <- sample(playable, 1)
      played_card <- hand[play_idx, ]
      discard <- rbind(discard, played_card)
      hand <- hand[-play_idx, ]

      # Action card handling
      if (played_card$value == "skip") {
        turn <- (turn + direction - 1) %% n_players + 1
      } else if (played_card$value == "reverse") {
        direction <- -direction
      } else if (played_card$value == "+2") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        if (nrow(deck) >= 2) {
          hands[[next_player]] <- rbind(hands[[next_player]], deck[1:2, ])
          deck <- deck[-(1:2), ]
        }
        turn <- (turn + direction - 1) %% n_players + 1
      } else if (played_card$value == "wild_draw4") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        if (nrow(deck) >= 4) {
          hands[[next_player]] <- rbind(hands[[next_player]], deck[1:4, ])
          deck <- deck[-(1:4), ]
        }
        turn <- (turn + direction - 1) %% n_players + 1
      }

      # Wild card color selection
      if (played_card$value %in% c("wild", "wild_draw4")) {
        discard[nrow(discard), "color"] <- sample(c("red", "blue", "green", "yellow"), 1)
      }
    }

    hands[[current_player]] <- hand

    if (nrow(hand) == 0) {
      message(current_player, " wins!")
      return(list(winner = current_player, hands = hands, discard = discard))
    }

    turn <- (turn + direction - 1) %% n_players + 1
  }
}

#' Simulate a Complete UNO Game
#'
#' Runs a full UNO game simulation from setup to finish, returning the final
#' game outcome. Internally, this function calls \code{\link{setup_game}} to
#' initialize the deck, deal cards, and configure play direction and turn order.
#' It then executes the gameplay loop using \code{\link{play_turns_loop}} until
#' one player wins by discarding all their cards.
#'
#' This function is the most user-friendly entry point and is ideal for batch simulations,
#' gameplay testing, educational demos, or scoring.
#'
#' @param n_players Integer. Number of players to simulate (default is 4). \cr
#'   This version supports any number of players ≥2, based on deck capacity. \cr
#'   \strong{Note:} In future versions of the package, we plan to expand
#'   support for larger groups with rule adaptations, deck validation,
#'   and team-based formats.
#'
#' @details
#' This function performs two main stages:
#' \itemize{
#'   \item \strong{Game initialization} – \code{\link{setup_game}} shuffles the deck, deals hands, and sets turn order.
#'   \item \strong{Gameplay loop} – \code{\link{play_turns_loop}} simulates turns until one player wins.
#' }
#'
#' The result can be passed to \code{\link{score_game}} for post-game analysis and ranking.
#'
#' @return A named list with the final game results:
#' \itemize{
#'   \item \strong{winner} – Character. Name of the winning player (e.g., \code{"Player_1"}).
#'   \item \strong{hands} – Named list of tibbles representing each player’s final hand.
#'   Only the winner will have an empty hand.
#'   \item \strong{discard} – Tibble showing the full discard pile used throughout the game.
#' }
#'
#' @examples
#' # Run a complete game
#' result <- play_game(n_players = 4)
#'
#' # Extract winner
#' result$winner
#'
#' # Check final card counts per player
#' sapply(result$hands, nrow)
#'
#' # View last card played
#' tail(result$discard)
#'
#' # Score and rank players
#' score_game(result)
#'
#' @export
play_game <- function(n_players = 4) {
  setup <- setup_game(n_players)
  play_turns_loop(
    hands = setup$hands,
    deck = setup$deck,
    discard = setup$discard,
    direction = setup$direction,
    turn = setup$turn
  )
}

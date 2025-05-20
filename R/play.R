#' Initialize a UNO Game Session
#'
#' Sets up a playable UNO game state by generating a complete deck, shuffling it,
#' dealing 7 cards to each player, drawing the first discard card, and initializing
#' gameplay direction and player turn order.
#'
#' This function is typically the first step before calling \code{\link{play_turns_loop}}
#' or \code{\link{play_game}} to simulate a full game.
#'
#' @param n_players Integer. Number of players to initialize. Must be ≥ 2. Default is 4. \cr
#'   This version supports dynamic randomization of direction and starting player. \cr
#'   \strong{Note:} Future versions may restore support for deterministic turn order and
#'   clockwise play by default, with options to override.
#'
#' @details
#' - Shuffles the deck from \code{\link{create_uno_deck}} \cr
#' - Deals 7 cards to each player \cr
#' - One card is drawn as the first discard \cr
#' - Direction is randomly chosen (1 = clockwise, -1 = counter-clockwise) \cr
#' - Starting player is randomly selected from the available players
#'
#' @return A named list:
#' \itemize{
#'   \item \strong{hands} – Named list of player hands (each a tibble with 7 cards)
#'   \item \strong{deck} – Remaining deck (after dealing and discard)
#'   \item \strong{discard} – One-row tibble for top discard card
#'   \item \strong{direction} – Integer: 1 or -1
#'   \item \strong{turn} – Integer: randomly chosen starting player's index
#' }
#'
#' @examples
#' state <- setup_game(n_players = 4)
#' names(state)
#' state$hands$Player_1
#' state$discard
#'
#' @export
setup_game <- function(n_players) {
  deck <- create_uno_deck()
  deck <- deck[sample(nrow(deck)), , drop = FALSE]

  # Deal hands
  hands <- split(deck[1:(n_players * 7), , drop = FALSE],
                 rep(1:n_players, each = 7))
  names(hands) <- paste0("Player_", 1:n_players)

  # Remaining deck and discard
  deck_index <- n_players * 7 + 1
  discard <- deck[deck_index, , drop = FALSE]
  deck_index <- deck_index + 1
  deck <- deck[deck_index:nrow(deck), , drop = FALSE]

  list(
    hands = hands,
    deck = deck,
    discard = discard,
    direction = sample(c(1, -1), 1),
    turn = sample(1:n_players, 1)
  )
}

#' Simulate UNO Turn-by-Turn Gameplay
#'
#' Runs the main loop of a UNO game where each player takes a turn until one wins
#' or the draw pile is exhausted. Handles matching, drawing, and playing cards
#' with full support for action and wild card effects.
#'
#' @param hands A named list of tibbles (one per player), with each tibble containing
#'   columns \code{color}, \code{value}, and \code{type}.
#' @param deck A tibble representing the draw pile.
#' @param discard A tibble representing the discard pile.
#' @param direction Integer: 1 = clockwise, -1 = counter-clockwise.
#' @param turn Integer (1-based): index of the current player.
#'
#' @details
#' - Handles special cards: \code{"skip"}, \code{"reverse"}, \code{"+2"}, \code{"wild"}, \code{"wild_draw4"} \cr
#' - If no playable card, player draws one from the draw pile \cr
#' - Wilds automatically assign a random color after play \cr
#' - If the deck is exhausted, the game ends with no winner (to be improved)
#'
#' \strong{Note:} In a future version, deck reshuffling and wild color choice prompts
#' will be added to make the simulation more robust.
#'
#' @return A named list:
#' \itemize{
#'   \item \strong{winner} – Character string (e.g., \code{"Player_2"}), or \code{NULL} if deck runs out
#'   \item \strong{hands} – Final state of each player's hand (winner has 0 cards)
#'   \item \strong{discard} – Final discard pile including the last played card
#' }
#'
#' @examples
#' state <- setup_game(4)
#' play_turns_loop(
#'   hands = state$hands,
#'   deck = state$deck,
#'   discard = state$discard,
#'   direction = state$direction,
#'   turn = state$turn
#' )
#'
#' @export
play_turns_loop <- function(hands, deck, discard, direction, turn) {
  n_players <- length(hands)
  deck_index <- 1
  deck_size <- nrow(deck)

  repeat {
    current_player <- paste0("Player_", turn)
    hand <- hands[[current_player]]
    top_card <- discard[nrow(discard), ]

    playable <- which(
      hand$color == top_card$color |
        hand$value == top_card$value |
        hand$color == "wild"
    )

    if (length(playable) == 0) {
      if (deck_index > deck_size) {
        return(list(
          winner = NULL,
          hands = hands,
          discard = discard,
          reason = "Deck exhausted"
        ))
      }
      drawn <- deck[deck_index, , drop = FALSE]
      deck_index <- deck_index + 1
      hand <- rbind(hand, drawn)
    } else {
      play_idx <- sample(playable, 1)
      played_card <- hand[play_idx, , drop = FALSE]
      discard <- rbind(discard, played_card)
      hand <- hand[-play_idx, , drop = FALSE]

      if (played_card$value == "skip") {
        turn <- (turn + direction - 1) %% n_players + 1
      } else if (played_card$value == "reverse") {
        direction <- -direction
      } else if (played_card$value == "+2") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        n_draw <- min(2, deck_size - deck_index + 1)
        if (n_draw > 0) {
          drawn <- deck[deck_index:(deck_index + n_draw - 1), , drop = FALSE]
          hands[[next_player]] <- rbind(hands[[next_player]], drawn)
          deck_index <- deck_index + n_draw
        }
        turn <- (turn + direction - 1) %% n_players + 1
      } else if (played_card$value == "wild_draw4") {
        next_player <- paste0("Player_", (turn + direction - 1) %% n_players + 1)
        n_draw <- min(4, deck_size - deck_index + 1)
        if (n_draw > 0) {
          drawn <- deck[deck_index:(deck_index + n_draw - 1), , drop = FALSE]
          hands[[next_player]] <- rbind(hands[[next_player]], drawn)
          deck_index <- deck_index + n_draw
        }
        turn <- (turn + direction - 1) %% n_players + 1
      }

      if (played_card$value %in% c("wild", "wild_draw4")) {
        discard[nrow(discard), "color"] <- sample(c("red", "green", "blue", "yellow"), 1)
      }
    }

    hands[[current_player]] <- hand

    if (nrow(hand) == 0) {
      return(list(
        winner = current_player,
        hands = hands,
        discard = discard
      ))
    }

    turn <- (turn + direction - 1) %% n_players + 1
  }
}

#' Simulate a Complete UNO Game
#'
#' Combines setup and gameplay to simulate a full UNO match from start to finish.
#' This is the easiest entry point for users to run a game in one line.
#'
#' @param n_players Integer. Number of players to simulate. Default is 4.
#'
#' @details
#' This function performs two stages:
#' \itemize{
#'   \item \strong{Game setup} using \code{\link{setup_game}}
#'   \item \strong{Turn simulation} using \code{\link{play_turns_loop}}
#' }
#'
#' @return A named list with:
#' \itemize{
#'   \item \strong{winner} – Name of the winning player
#'   \item \strong{hands} – Final hand of each player
#'   \item \strong{discard} – Final discard pile
#' }
#'
#' @examples
#' result <- play_game(n_players = 4)
#' result$winner
#' sapply(result$hands, nrow)
#' tail(result$discard)
#'
#' @export
play_game <- function(n_players = 4) {
  if (!is.numeric(n_players) || length(n_players) != 1 || n_players %% 1 != 0 || n_players < 2) {
    stop("`n_players` must be a single whole number (≥ 2).")
  }

  setup <- setup_game(n_players)

  result <- play_turns_loop(
    hands = setup$hands,
    deck = setup$deck,
    discard = setup$discard,
    direction = setup$direction,
    turn = setup$turn
  )

  score_game(result)

  # Return full result (hands, winner, discard)
  invisible(result)
}

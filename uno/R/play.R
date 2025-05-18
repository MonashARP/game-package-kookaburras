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

#' Simulate the turn-by-turn logic of a UNO game
#'
#' @param hands Player hands
#' @param deck Draw pile
#' @param discard Discard pile
#' @param direction 1 (clockwise) or -1 (counter-clockwise)
#' @param turn Current player number
#' @return Game result list with winner, final hands, and discard pile
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

#' Game logic
#'
#' The gmae logic is divided into teo main functions
#'
#' `play_game()`-  Initializes the game by creating a deck, dealing hands to players, and starting the game loop.
#'  `play_turns_loop()` - Handles the main game loop, processing each player's turn until a winner is found.
#'  This included drawing cards, playing cards, and handling special actions like skips and reverses. Logic for
#'   wild cards and drawing cards is also included.
#'
#'

# Defining play_game and setting default no of players as 4.
play_game <- function(n_players = 4) {

  # Calling the create_uno_deck and deal_hands functions to create a deck and deal hands to players.
  deck <- create_uno_deck()
  dealt <- deal_hands(deck, n_players)

  # function call returns assigned to variables
  hands <- dealt$hands
  discard <- dealt$discard
  draw_pile <- dealt$deck

  # Setting up direction and turn. Assume clockwise direction and starting with Player 1
  direction <- 1
  turn <- 1

  # Calling play_turns_loop to start the game
  result <- play_turns_loop(
    hands = hands,
    deck = draw_pile,
    discard = discard,
    direction = direction,
    turn = turn
  )

  return(result)
}


# Defining the play_turns_loop function to handle game logic
play_turns_loop <- function(hands, deck, discard, direction, turn) {

  # Calculating number of players based on the length of hands list
  n_players <- length(hands)
  deck_index <- 1                  # Setting deck_index to 1 to loop through the remaining deck
  deck_size <- nrow(deck)         # Cards remaining in deck after dealing hands to players


  winner <- NULL
  game_running <- TRUE

  #Using while loop to run the game until a player wins or deck is exhausted
  while (game_running) {

    # Getting the current player
    player_name <- paste0("Player_", turn)      # Player's current cards in hand
    hand <- hands[[player_name]]
    top_card <- discard[nrow(discard), ]       # Top card on the discard plie


    # Checking cards against the top card
    playable_flags <- logical(nrow(hand))
    for (i in seq_len(nrow(hand))) {            # Looping through each card in players hand
      card <- hand[i, ]
      playable_flags[i] <- card$color == top_card$color || # Compares card to top card in discard pile
        card$value == top_card$value ||                    # Checks if it matches by colour, number or is wild card
        card$color == "wild"
    }

    playable <- c()
    for (j in seq_along(playable_flags)) {
      if (playable_flags[j]) {
        playable <- c(playable, j)
      }
    }

    if (length(playable) == 0) {
      if (deck_index > deck_size) {
        return(list(
          winner = NULL,
          hands = hands,
          discard = discard,
          reason = "Deck exhausted"
        ))
      }
      hand[nrow(hand) + 1, ] <- deck[deck_index, ]
      deck_index <- deck_index + 1
    } else {
      play_index <- sample(playable, 1)
      played_card <- hand[play_index, , drop = FALSE]

      if (nrow(hand) > 1) {
        hand <- hand[-play_index, , drop = FALSE]
      } else {
        hand <- hand[0, , drop = FALSE]
      }

      discard[nrow(discard) + 1, ] <- played_card

      if (played_card$value == "skip") {
        turn <- (turn + direction - 1) %% n_players + 1
      } else if (played_card$value == "reverse") {
        direction <- -direction
      } else if (played_card$value == "+2") {
        next_turn <- (turn + direction - 1) %% n_players + 1
        next_name <- paste0("Player_", next_turn)
        draw_n <- min(2, deck_size - deck_index + 1)
        if (draw_n > 0) {
          draw <- deck[deck_index:(deck_index + draw_n - 1), , drop = FALSE]
          hands[[next_name]] <- rbind(hands[[next_name]], draw)
          deck_index <- deck_index + draw_n
        }
        turn <- next_turn
      } else if (played_card$value == "wild_draw4") {
        next_turn <- (turn + direction - 1) %% n_players + 1
        next_name <- paste0("Player_", next_turn)
        draw_n <- min(4, deck_size - deck_index + 1)
        if (draw_n > 0) {
          draw <- deck[deck_index:(deck_index + draw_n - 1), , drop = FALSE]
          hands[[next_name]] <- rbind(hands[[next_name]], draw)
          deck_index <- deck_index + draw_n
        }
        turn <- next_turn
      }

      if (played_card$value %in% c("wild", "wild_draw4")) {
        discard[nrow(discard), "color"] <- sample(c("red", "green", "blue", "yellow"), 1)
      }
    }

    hands[[player_name]] <- hand

    if (nrow(hand) == 0) {
      winner <- player_name
      game_running <- FALSE
    } else {
      turn <- (turn + direction - 1) %% n_players + 1
    }
  }

  list(
    winner = winner,
    hands = hands,
    discard = discard
  )
}

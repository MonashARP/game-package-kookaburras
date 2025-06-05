#' Start a New UNO Game
#'
#' This function sets up and plays a full game of UNO using standard rules.
#' It creates the deck, deals cards to each player, and runs the game loop
#' using `play_turns_loop()`. The game continues until one player wins or
#' the deck runs out of cards.
#'
#' @param n_players Number of players in the game (must be ≥ 2). Default is 4.
#'
#' @return A list containing:
#' \describe{
#'   \item{winner}{Name of the winning player (e.g., "Player_3"), or NULL if no player wins before the deck is exhausted}
#'   \item{hands}{Final hand of each player at the end of the game, as a named list of tibbles}
#'   \item{discard}{Final discard pile showing the full play history as a tibble with columns \code{color}, \code{value}, and \code{type}.
#'   \strong{Note:} Wild cards (including \code{wild} and \code{wild_draw4}) will appear with the chosen color (e.g., \code{color = "green"} and \code{value = "wild"}), indicating the color selected by the player when the card was played.}
#' }
#'
#' @examples
#' # Run a game with 4 players
#' result <- play_game(4)
#'
#' # See the winner
#' result$winner
#'
#' # Preview the discard pile
#' head(result$discard)
#'
#' # Check final hand of Player 2
#' result$hands$Player_2
#'
#' @export
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

#' Game Loop Logic for UNO
#'
#' Handles the main logic of the UNO game by processing each player's turn.
#' This includes checking playable cards, handling drawing from the deck,
#' applying special actions (skip, reverse, +2, wild), and checking for a winner.
#'
#' This function is not typically called directly by the user. Instead, use
#' \code{\link{play_game}} to run a full game.
#'
#' @param hands A named list of tibbles representing each player's hand.
#' @param deck A tibble of remaining cards to draw from.
#' @param discard A tibble representing the current discard pile.
#' @param direction An integer (1 or -1) to indicate the direction of play.
#' @param turn The starting player number (typically 1).
#'
#' @return A list containing:
#' \describe{
#'   \item{winner}{Name of the winning player (e.g. "Player_3") or NULL if the game ends in a draw}
#'   \item{hands}{A named list of each player's final hand}
#'   \item{discard}{A tibble showing the final discard pile, including the full play history.
#'   \strong{Note:} For wild and wild_draw4 cards, the \code{color} column reflects the color the player chose when playing the card (e.g. \code{"green wild"} means the player played a wild card and chose green).}
#' }
#'
#' @keywords internal
play_turns_loop <- function(hands, deck, discard, direction, turn) {
  ...
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

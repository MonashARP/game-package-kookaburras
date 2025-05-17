deal_hands <- function(deck, n_players = 4) {
  # Deck Shuffling
  deck <- deck[sample(nrow(deck)), ]

  #   Intialize individual hands of players
  hands <- vector("list", n_players)
  for (i in 1:n_players) {
    hands[[i]] <- deck[0, ]
  }
  names(hands) <- paste0("Player_", 1:n_players)

  #Distribute cards per rounds
  for (round in 1:7) {
    for (i in 1:n_players) {
      hands[[i]] <- rbind(hands[[i]], deck[1, ])
      deck <- deck[-1, ]
    }
  }

  # Remainig cards
  discard <- deck[1, , drop = FALSE]

  #Revome 1st card after show
  deck <- deck[-1, ]

  return(list(
    hands = hands,
    deck = deck,
    discard = discard
  ))
}

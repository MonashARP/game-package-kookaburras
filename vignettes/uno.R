## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(uno)
library(dplyr)


## ----eval=FALSE---------------------------------------------------------------
# # Install devtools if not already installed
# install.packages("devtools")
# 
# # Install uno from GitHub
# devtools::install_github("MonashARP/game-package-kookaburras")

## -----------------------------------------------------------------------------
deck <- create_uno_deck()
head(deck)
nrow(deck)  # Should return 108

## -----------------------------------------------------------------------------
deal <- deal_hands(deck, n_players = 4)

# Explore hands
names(deal$hands)
deal$hands$Player_1

# View discard pile
deal$discard

## -----------------------------------------------------------------------------
result <- play_game()

# View winner
result$winner

# Discard pile preview
tail(result$discard)


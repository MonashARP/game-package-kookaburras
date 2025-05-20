# uno

`uno` is an R package that simulates a full multiplayer UNO game, including CPU-generated turns, realistic card rules, and winner detection. Built for reproducibility, testing, and fun, it's ideal for both classroom simulations and package development learning.

Developed by Team Kookaburras as part of the Monash ETC Advanced R course.

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install the uno package
devtools::install_github("MonashARP/game-package-kookaburras", subdir = "uno")
```

## Features

-   Shuffles and builds the full 108-card UNO deck

-   Deals hands fairly across players

-   Fully handles action cards: Skip, Reverse, +2, Wild, and Wild Draw 4

-   Simulates turn-by-turn gameplay

-   Detects the winner and provides end-game scoring

-   Extensively documented and tested

## Quick Start

``` r
library(uno)

# Create and deal
deck <- create_uno_deck()
deal <- deal_hands(deck, n_players = 4)

# Simulate a full game
result <- play_game(n_players = 4)

# View the winner
result$winner

# Score the game
score_game(result)
```

## Main Functions

| Function | Description |
|--------------------|----------------------------------------------------|
| `create_uno_deck()` | Generate and shuffle the 108-card UNO deck |
| `deal_hands()` | Deal 7 cards to each player from the deck |
| `setup_game()` | Initialize game state including turn, discard, direction |
| `play_turns_loop()` | Simulate turn-by-turn gameplay logic |
| `play_game()` | Simulate a full game end-to-end from dealing to winner |
| `score_game()` | Rank players by the number of cards remaining |

## Example: Simulate a Game

``` r
library(uno)

# Create the full UNO deck
deck <- create_uno_deck()

# Deal to 4 players
deal <- deal_hands(deck, n_players = 4)

# Simulate a full UNO game
result <- play_game(n_players = 4)

# View winner
result$winner

# Score all players
score_game(result)

# Visualise card count
barplot(score_game(result),
        main = "Cards Remaining per Player",
        col = "darkorange")
```

## Testing

This package includes a full test suite using testthat. To run all tests, use:

``` r
devtools::test()
```

**Tests include**:
 - Return structure validity

 - Deck size after shuffling (108 cards)

 - Hands contain 7 cards each

 - Action card effects (+2, Skip, Reverse, Wild Draw 4)

 - Scoring logic (winner has 0, others ranked by remaining cards)

 - Deck exhaustion edge case handling

 - Compatibility for multiple player sizes (2 to 4)
 



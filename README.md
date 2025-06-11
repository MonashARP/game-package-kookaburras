
# `uno`: Multiplayer UNO Game in R

The `uno` R package simulates a full multiplayer UNO game using realistic card rules and automated gameplay.

Designed as part of the **Monash ETC Advanced R course**, this package allows players to experience turn-by-turn logic, action and wild card effects, and winner detection, all within a reproducible and testable R environment.

> Developed by Team Kookaburras.

---

## Installation

Install the development version directly from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install uno package from GitHub
devtools::install_github("MonashARP/game-package-kookaburras")
```

---

## Features

- Generates a full UNO deck (108 cards)
- Deals cards fairly to all players
- Handles all action and wild cards
- Simulates turn-by-turn game logic
- Detects winner or deck exhaustion
- Includes internal game loop control
- Documented and unit-tested

---

## Quick Start

```r
library(uno)

# Create and shuffle the full deck
deck <- create_uno_deck()

# Deal hands to players
deal <- deal_hands(deck, n_players = 4)

# Simulate a complete game
result <- play_game(n_players = 4)

# View the winner
result$winner

# Inspect final hands
result$hands$Player_2

# Check discard pile history
tail(result$discard)
```

---

## Main Functions

| Function            | Description                                    |
|---------------------|------------------------------------------------|
| `create_uno_deck()` | Generate the 108-card UNO deck                 |
| `deal_hands()`      | Deal 7 cards per player and start discard pile |
| `play_turns_loop()` | Internal engine that handles each player’s turn |
| `play_game()`       | Runs a full game from start to winner or draw  |

> *Note: `play_turns_loop()` is called internally by `play_game()` and not used directly.*

---

## Testing

The package includes a suite of unit tests using the `testthat` framework. To run the tests:

```r
devtools::test()
```

Tests cover:

- Deck size and card type correctness  
- Proper card dealing and hand sizes  
- Internal consistency of discard pile  
- Wild card behavior and color assignment  
- Game loop and winner detection logic

---

## Learn More

Explore the full vignette and documentation on the [UNO Package Website](https://monasharp.github.io/game-package-kookaburras/).

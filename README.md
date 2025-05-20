# uno 🎴

**`uno`** is an R package that simulates a full multiplayer UNO game with CPU-generated turns, realistic action cards, and player scoring.

Developed by Team Kookaburras for the Monash ETC course project.

---

## 📦 Installation

Install the development version of the package from GitHub:

```r
# Install devtools if needed
install.packages("devtools")

# Install uno package from GitHub
devtools::install_github("MonashARP/game-package-kookaburras", subdir = "uno")
```

### Example Usage
```
library(uno)

# Play a full game with 4 players
result <- play_game(n_players = 4)

# View the winner
cat("Winner:", result$winner)

# Show final hands
score_game(result)
```

### Future Improvements
UNO call penalty

Deck recycling when empty

Wild Draw 4 challenge rules

Optional player interactivity




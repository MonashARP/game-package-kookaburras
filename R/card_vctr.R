#' @importFrom vctrs new_vctr vec_data vec_cast vec_ptype2
#' @importFrom crayon red blue green yellow silver

NULL

#' Construct a custom UNO card vector
#'
#' This function creates a new UNO card vector using a custom S3 vctrs class called `card_vctr`.
#' It stores card values as strings like `"red_3"`, `"blue_skip"`, `"wild"`, or `"wild_draw4"`,
#' and enables type-safe operations on UNO cards throughout the package.
#'
#' Use this constructor when you want to define or manipulate UNO cards directly,
#' especially for testing, simulations, or building player hands manually.
#'
#' @param x A character vector of UNO-style card strings, typically in the format `"color_value"`
#' (e.g., `"yellow_7"`, `"green_reverse"`, `"wild_draw4"`). No validation is done at this stage.
#'
#' @return An object of class `card_vctr`, which behaves like a typed character vector
#' with UNO-specific behavior when passed to helper functions such as `card_suit()` and `card_value()`.
#'
#' @examples
#' cards <- new_card_vctr(c("red_3", "blue_skip", "wild_draw4"))
#'
#' # Extract suits from these cards
#' card_suit(cards)
#'
#' # Use in a hand summary
#' hands <- list(Player_1 = cards)
#' summarise_hand(hands, "Player_1")
#'
#' @export
new_card_vctr <- function(x = character()) {
  vctrs::new_vctr(x, class = "card_vctr")
}

#' @export
format.card_vctr <- function(x, ...) {
  suit <- card_suit(x)
  val <- card_value(x)
  type <- ifelse(val %in% as.character(0:9), "number",
                 ifelse(val %in% c("skip", "reverse", "+2"), "action",
                        ifelse(val %in% c("wild", "draw4", "wild_draw4"), "wild", "unknown")))
  sprintf("%-8s | %-10s | %s", suit, val, type)
}

#' @export
#' @export
print.card_vctr <- function(x, ...) {
  suit <- card_suit(x)
  val <- card_value(x)

  type <- ifelse(val %in% as.character(0:9), "number",
                 ifelse(val %in% c("skip", "reverse", "+2"), "action",
                        ifelse(val %in% c("wild", "draw4", "wild_draw4"), "wild", "unknown")))

  color_display <- purrr::map_chr(suit, function(col) {
    switch(col,
           red    = crayon::red(col),
           blue   = crayon::blue(col),
           green  = crayon::green(col),
           yellow = crayon::yellow(col),
           wild   = crayon::silver("wild"),
           col)
  })

  cat("<card_vctr> (length = ", length(x), ")\n", sep = "")
  cat("color     | value      | type\n")
  cat("----------|------------|----------\n")
  for (i in seq_along(x)) {
    cat(sprintf("%-10s| %-10s| %s\n",
                color_display[i],
                val[i],
                type[i]))
  }
  invisible(x)
}


#' @export
vec_ptype2.card_vctr.card_vctr <- function(x, y, ...) new_card_vctr()

#' @export
vec_cast.card_vctr.card_vctr <- function(x, to, ...) new_card_vctr(vctrs::vec_data(x))

#' Extract the color suit from UNO cards
#'
#' This generic function extracts the suit (or color) of UNO cards
#' such as `"red"`, `"blue"`, `"green"`, `"yellow"`, or `"wild"`
#' from a `card_vctr` object.
#'
#' It is useful for analyzing player hands, counting card colors,
#' or implementing color-based rules during gameplay.
#'
#' @param x A `card_vctr` — a custom vector representing UNO cards,
#'   typically created using `new_card_vctr()` or returned from gameplay functions.
#'
#' @return A character vector of suits corresponding to each card.
#'   If `x` contains `"green_3"`, `"red_skip"`, and `"wild_draw4"`,
#'   the returned vector will be `"green"`, `"red"`, and `"wild"`.
#'
#' @examples
#' cards <- new_card_vctr(c("green_3", "red_skip", "wild_draw4"))
#' card_suit(cards)
#'
#' @export
card_suit <- function(x) {
  UseMethod("card_suit")
}

#' @export
card_suit.card_vctr <- function(x) {
  sub("_.*", "", vctrs::vec_data(x))
}

#' Generic to get card value (e.g., "3", "skip", "draw4")
#'
#' @param x A card_vctr
#' @return Character vector of values
#' @export
card_value <- function(x) {
  UseMethod("card_value")
}

#' @export
card_value.card_vctr <- function(x) {
  sub(".*_", "", vctrs::vec_data(x))
}

#' Convert a tibble to a card_vctr
#'
#' @param tbl A tibble with columns `color` and `value`
#' @return A `card_vctr` vector
#' @export
convert_to_card_vctr <- function(tbl) {
  stopifnot(all(c("color", "value") %in% names(tbl)))
  new_card_vctr(paste(tbl$color, tbl$value, sep = "_"))
}

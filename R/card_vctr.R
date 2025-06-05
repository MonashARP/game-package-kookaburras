#' @importFrom vctrs new_vctr vec_data vec_cast vec_ptype2
NULL
#' Create a card vector (custom vctrs class)
#'
#' @param x Character vector like "red_3", "blue_skip", etc.
#'
#' @return A custom `card_vctr` object
#' @export
#'
#' @examples
#' new_card_vctr(c("red_3", "blue_skip", "wild_draw4"))
new_card_vctr <- function(x = character()) {
  vctrs::new_vctr(x, class = "card_vctr")
}

#' @export
format.card_vctr <- function(x, ...) {
  paste0("<", vctrs::vec_data(x), ">")
}

#' @export
vec_ptype2.card_vctr.card_vctr <- function(x, y, ...) new_card_vctr()

#' @export
vec_cast.card_vctr.card_vctr <- function(x, to, ...) new_card_vctr(vctrs::vec_data(x))

#' Generic to get card suit (e.g., "red", "blue", "wild")
#'
#' @param x A card_vctr
#' @return Character vector of suits
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

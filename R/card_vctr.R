#' @importFrom vctrs new_vctr vec_data vec_cast vec_ptype2
#' @importFrom crayon red blue green yellow silver

NULL

#' Create a card vector (custom vctrs class)
#'
#' @description Creates a new UNO card vector using a custom vctrs class.
#' @param x Character vector like "red_3", "blue_skip", etc.
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

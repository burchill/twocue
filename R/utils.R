#' `pluck` uniquely, more easily
#'
#' Basically just `pluck()` for data frames, but it turns bare columns into strings and then gets the unique values.
#' @param df a data frame
#' @param col a column name
#' @export
unipluck <- function(df, col) {
  q <- enexpr(col)
  if (!is.character(q) & !is.numeric(q))
    q <- rlang::as_name(q)
  df %>% pluck(q) %>% unique()
}

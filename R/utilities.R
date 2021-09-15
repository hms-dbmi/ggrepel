.pt <- 72.27 / 25.4

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}



#' Return a unit version of the argument.
#'
#' @param x Number or unit object.
#' @return unit(x, "lines") if number or the unchanged argument if it's already
#'  a unit object.
#' @noRd
to_unit <- function(x) {
  # don't change arg if already unit
  if (grid::is.unit(x)) {
    return(x)
  }

  # NA used to exclude points from repulsion calculations
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  grid::unit(x, "lines")
}

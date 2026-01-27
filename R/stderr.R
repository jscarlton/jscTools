#' Calculate the standard error of the mean
#'
#' This function takes a numeric vector and calculates the standard error of the
#' mean, handling NA values appropriately.
#'
#' @param x A numeric vector
#'
#' @return The standard error of the mean
#' @export
#' @importFrom stats var na.omit
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5, NA, 6)
#' stderr(x)
stderr <- function(x) {
  sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))
}
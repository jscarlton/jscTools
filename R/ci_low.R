#' Calculate the lower bound of a 95% confidence interval
#'
#' Calculates the lower bound of a 95% confidence interval given a mean and
#' standard error. Uses the normal approximation (z = 1.96).
#'
#' @param mean_var The mean value
#' @param se_var The standard error (typically calculated with \code{\link{stderr}})
#'
#' @return The lower bound of the 95% CI
#' @export
#' @seealso \code{\link{ci_high}}, \code{\link{stderr}}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' m <- mean(x)
#' se <- stderr(x)
#' ci_low(m, se)
#' ci_high(m, se)
ci_low <- function(mean_var, se_var) {
  mean_var - (1.96 * se_var)
}
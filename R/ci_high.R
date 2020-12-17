#'Calculate the upper bound of the 95% confidence interval given a standard error.
#' @param mean_var is mean of the data used to calculate the CI
#' @param se_var is the se (typically calculated with jscTools::stderr) used to calculate the CI
#' @export
#' @examples
#' ci_high(mean_var, se_var)

ci_high <- function(mean_var, se_var){
  mean_var + (1.96 * se_var)
}
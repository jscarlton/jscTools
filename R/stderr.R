#' A function for finding the standard error
#' 
#' This function takes a series of numbers (vector, etc.) and finds the standard error.
#' @param x is a list of numbers
#' @export
#' @examples
#' stderr()
stderr <- function(x){sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))}
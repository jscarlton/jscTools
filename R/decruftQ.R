#' A function for finding the standard error
#' 
#' This function takes a series of numbers (vector, etc.) and finds the standard error. See http://www.jscarlton.net/post/2017-04-04QualtricsImport/ for more information.
#' @param input_csv is the downloaded Qualtrics csv with 3 lines of cruft at the top
#' @export
#' @examples
#' decruftQ()
decruftQ <- function(input_csv, skip = 3){
require(tidyverse)
colnamesQ <- unlist(read_csv(input_csv,n_max = 1, col_names = FALSE))
output_data <- read_csv(input_csv, skip = skip, col_names = colnamesQ)
return(output_data)
}
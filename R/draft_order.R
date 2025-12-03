#' Take a list of names and turn them into an order for picking in a podcast draft, with the option of the host going last
#' @param host is the name of the host, defaults to Stuart
#' @param names the names of the participants
#' @export
#' @examples
#' names <- ("Carolyn", "Megan", "Brian")
#' draft_order(names)

draft_order <- function(names, host = "Stuart"){
  name_list <- sample(names)
  append(name_list, host)  
}
#' A function for copying text to the clipboard on macOS. Note that this is very simple...it doesn't allow for different separators, etc. That could be added fairly easily, but I haven't found the need yet. 
#' 
#' @param x is the data to be written to the clipboard. I have not extensively tested what can and can't be written at this point. Since the function calls write.table, the help files for it are likely a good start.
#' 
cbWrite <- function(x){
  clip <- pipe("pbcopy", "w")
  write.table(x, file = clip)
  close(clip)
}

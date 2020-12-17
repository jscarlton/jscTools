#' A function for creating a wordcloud from a text transcript
#' 
#' @param input_path The path of the text file to turn into a wordcloud
#' @param save_path The path where you want the saved image to go. Should end in ".png".
#' @param cloud_width The width of your saved wordcloud image, in pixels. Defaults to 2000.
#' @param cloud_height The height of your saved wordcloud image, in pixels. Defaults to 2000. 
#' @param cloud_res  The resolution of your saved wordcloud image, in dpi Defaults to 300
#' @param cloud_max_words The maximum number of words you want to plot in your wordcloud. Defaults to 100.
#' @param color_pal The color palette that you want to use for your wordcloud. Choose one color if you want a monochrome wordcloud or a series of colors if you want it to vary the color by the relative frequency of the word. See the wordcloud documentation for details. Defaults to c(brewer.pal(5,"Blues")[3],brewer.pal(5,"Blues")[4],brewer.pal(5,"Blues")[5])
#' @keywords text analysis, wordcloud
#' @export

transcript_cloud <- function(input_path, save_path, cloud_width=2000, cloud_height=2000, cloud_res = 300, cloud_max_words = 100, color_pal = c(brewer.pal(5,"Blues")[3],brewer.pal(5,"Blues")[4],brewer.pal(5,"Blues")[5])){

  library(tidyverse)
  library(tidytext)
  library(readtext)
  library(wordcloud)
    
  transcript <- readtext(input_path)
  tidyTranscript <- transcript %>%
    unnest_tokens(word, text)
  data("stop_words")
  
  tidyTranscript <- tidyTranscript %>%
    anti_join(stop_words)
  
  # Make wordcloud
  png(save_path, width = cloud_width, height = cloud_height, res = cloud_res)
  tidyTranscript %>%
    count(word, sort = TRUE) %>%
    with(wordcloud(word, n, max.words = cloud_max_words, random.order = FALSE, colors = color_pal))
  dev.off()
}
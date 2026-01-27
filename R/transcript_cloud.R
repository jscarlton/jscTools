#' Create a word cloud from a text transcript
#'
#' Generates a word cloud visualization from a text file (e.g., interview
#' transcript, survey responses). Removes common stop words and saves the
#' result as a PNG image.
#'
#' @param input_path The path of the text file to turn into a wordcloud.
#'   Can use \code{here::here()} for project-relative paths.
#' @param save_path The path where you want the saved image to go. Should end in ".png".
#' @param cloud_width The width of your saved wordcloud image, in pixels. Defaults to 2000.
#' @param cloud_height The height of your saved wordcloud image, in pixels. Defaults to 2000.
#' @param cloud_res The resolution of your saved wordcloud image, in dpi. Defaults to 300.
#' @param cloud_max_words The maximum number of words to plot. Defaults to 100.
#' @param color_pal Color palette for the wordcloud. Can be a single color for
#'   monochrome or multiple colors to vary by word frequency. Defaults to a
#'   blue gradient.
#'
#' @return Invisibly returns the tidy transcript data
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' transcript_cloud(
#'   here::here("data", "interview.txt"),
#'   here::here("output", "figures", "interview_cloud.png")
#' )
#'
#' # Custom colors
#' transcript_cloud(
#'   "interview.txt",
#'   "cloud.png",
#'   color_pal = c("#1a9850", "#91cf60", "#d9ef8b")
#' )
#' }

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
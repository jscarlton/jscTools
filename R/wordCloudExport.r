#' A function for exporting a word cloud, using the wordcloud package. Requires the following libraries: wordcloud, tm, SnowballC
#' 
#' This function takes a series of numbers (vector, etc.) and finds the standard error.
#' @param cloudCorpus the corpus to be turned into a word cloud. I created it expecting a column of responses to an open-ended survey question.
#' @param exportName the name of the file you want to export. Defaults to wordcloud.png
#' @param cloudStopWords a list of stopwords for the word cloud. Defaults to stopwords("english"). Can add more stopwords with newStopWords <- c("foo", stopwords("english")).
#' @export
#' @examples
#' wordCloudExport()
#' 
wordCloudExport <- function(cloudCorpus, exportName = "wordcloud.png", cloudStopWords = stopwords("english"), cloudRandomOrder = FALSE, cloudRotPer = 0, cloudMaxWords = Inf, fileWidth = 2000, fileHeight = 2000, fileRes = 300){
  def.corpus <- VCorpus(DataframeSource(data.frame(cloudCorpus)))
  def.corpus <- tm_map(def.corpus, removePunctuation)
  def.corpus <- tm_map(def.corpus, content_transformer(tolower))
  def.corpus <- tm_map(def.corpus, function(x) removeWords(x, cloudStopWords))
  
  tdm <- TermDocumentMatrix(def.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  png(exportName, width=fileWidth, height=fileHeight, res = fileRes)
  wordcloud(d$word,d$freq, max.words= cloudMaxWords, colors="black", random.order = cloudRandomOrder, rot.per = cloudRotPer)
  dev.off()
  
}
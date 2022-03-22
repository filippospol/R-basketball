# Source of the following script is: 
# https://mikeyharper.uk/creating-twitter-wordclouds/
# Just a few minor modifications made

# load libraries: --------------------------------------------------------
library(rtweet)
library(tm)
library(stringr)
library(qdapRegex) 
library(wordcloud2)

# create function: -------------------------------------------------------
tweet_wordcloud <- function(w,size) {
  
  # This function returns the 50 most frequent words used on Tweets
  # that include a specific word, during the last week
  
  # get raw data:
  raw_tweets <- search_tweets2(w, n = size, 
                               include_rts = FALSE, lang = "en")
  # retryonratelimit = TRUE
  
  # select only the text of each tweet:
  tweets <- str_c(raw_tweets$text, collapse = "") 
  
  # clean data:
  tweets <- 
    tweets %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp"))
  
  # data in summary table form:
  tweetsCorpus <- 
    Corpus(VectorSource(tweets)) %>%
    TermDocumentMatrix(control = list(
      tolower = TRUE,
      removeNumbers = TRUE,
      stopwords = TRUE,
      removePunctuation = TRUE)) %>%
    as.matrix()
  
  tweetsCorpus <- sort(rowSums(tweetsCorpus), decreasing=TRUE)
  tweetsCorpus <- data.frame(Word = names(tweetsCorpus),
                             freq=tweetsCorpus, row.names = NULL)
  tweetsCorpus <- tweetsCorpus %>% filter(Word != "nba")
  
  p <- wordcloud2(data = tweetsCorpus, minRotation = 0, maxRotation = 0,
                  backgroundColor = "#ffffff", # ellipticity = 0.6,
                  size = 2, fontFamily="Calibri", color = "random-dark")
  
  # print results; 1 is the table, 2 is the plot:
  return(list(head(tweetsCorpus,50),p))
  beepr::beep()
}


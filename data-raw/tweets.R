if (!file.exists("data-raw/tweets.csv")) {
  download.file("https://d1p17r2m4rzlbo.cloudfront.net/wp-content/uploads/2016/03/Airline-Sentiment-2-w-AA.csv",
                destfile = "./data-raw/tweets.csv",
                quiet = FALSE)
}

library(textclean)

raw <- read.csv("./data-raw/tweets.csv", stringsAsFactors = FALSE)
tweets <- raw[,c('airline_sentiment','text')]
tweets$text <- replace_non_ascii(tweets$text)
usethis::use_data(tweets, overwrite = TRUE)

library(tm) #used for text mining
library(SnowballC)

tweets$text <- as.character(tweets$text) #changing 'text' into character
airline_sentiment <- as.character(tweets$airline_sentiment)

### TEXT MINING
corpus <- Corpus(VectorSource(tweets$text))
corpus_clean <- tm_map(corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, "@")
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stemDocument)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

dtm <- DocumentTermMatrix(corpus_clean)
names <- colnames(dtm)

dict <- findFreqTerms(dtm, lowfreq = 5)

tweetsDTM <- DocumentTermMatrix(corpus_clean,list(dictionary=dict))
tweetsDTM <- as.matrix(tweetsDTM)

filter_names <- colnames(tweetsDTM)[colSums(tweetsDTM)==0]
keep_names <- setdiff(colnames(tweetsDTM), filter_names)

tweetsDTM <- tweetsDTM[,keep_names]
tweetsDTM <- cbind(airline_sentiment,as.data.frame(tweetsDTM))

usethis::use_data(tweetsDTM, overwrite = TRUE)

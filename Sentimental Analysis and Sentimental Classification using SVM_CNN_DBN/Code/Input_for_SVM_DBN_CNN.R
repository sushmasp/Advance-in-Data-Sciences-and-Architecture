library(readr)
library(tm)
library(wordcloud)
library(magrittr)
setwd("C:/Users/Nimi/Downloads/r studio/final")
review <- read_csv("review_py.csv")

head(review$Stars)

#Setting up source and corpus
corpus <- Corpus(VectorSource(review$ReviewContent))
corpus
inspect(corpus[1:3])
#tokenization

corpus.clean <- corpus %>%
  #tm_map( function(x) iconv(enc2utf8(x), sub = "byte")) %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument,language='english')

dtm <- DocumentTermMatrix(corpus.clean)
dtm

dtm2 <- as.matrix(dtm)

#Finding the most frequent terms
Frequency <- colSums(dtm2)
Frequency <- sort(Frequency,decreasing=TRUE)
Frequency
#tm_map(corpus.clean, stemDocument)

words<-names(Frequency)

wordcloud(words[1:100],Frequency[1:100])

dtm3 <- DocumentTermMatrix(corpus.clean,
                           control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                          stopwords = TRUE))

dtm3
s<-data.frame(as.matrix(dtm3))
s<-cbind(s,review$Stars)
dim(s)
write.csv(s,'corpus.csv',fileEncoding = "UTF-8")




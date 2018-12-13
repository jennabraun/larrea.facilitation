#wordcloud for thesis
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud")

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text <- readLines("Braun_MSc_Thesis_2018.txt")
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("table", "also", "per", "used")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

library(wordcloud2)
library(dplyr)
d <- filter(d, "freq" > 5)
wordcloud2(data = d, shape = "circle", size = 0.5)

figPath = system.file("F:/School/larrea.facilitation/bee.png",package = "wordcloud2")

wordcloud2(d, figPath = "F:/School/larrea.facilitation/bee.png", size = 0.5)

library(devtools)
devtools::install_github("lchiffon/wordcloud2")

letterCloud(d, "R")

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

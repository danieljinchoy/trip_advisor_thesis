install.packages("slam")
library(slam)

install.packages("tm", repos="http://R-Forge.R-project.org")
library(tm)
library(wordcloud)
library(tmap)
library(RColorBrewer)

favela <- read.csv("favela.csv", header = TRUE)
corpus <- Corpus(VectorSource(favela$review))

corpus[[1]][1]

# text cleaning

# convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# remove numbers
corpus <- tm_map(corpus, removeNumbers)
# remove english common stopwords
corpus <- tm_map(corpus, removeWords(stopwords("english")))
# remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# elimniate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)
# remove additional stopwords
# corpus <- tm_map(corpus, removeWords)

# create TDM
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

wordcloud((d$word, d$freq, random.order = FALSE, rot.per = 0.3, scale = c(4, .5), max.words = 101, colors = brewer.pal(8, "Dark2")))

  
# Script for Webscraping Text Data from Tripadvisor and Sentiment Analysis #
# written by Daniel Choy


# install packages
install.packages("slam")
install.packages("ggplot2")
install.packages("stringr")
install.packages("tidytext")
install.packages("tidyr")
install.packages("dplyr")
install.packages("rvest")
install.packages("xml2")
install.packages("reshpae2")
# call library
library(rvest)
library(xml2)
library(slam)
library(tidyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
# web scarping from Tripadvisor

url <- "https://www.tripadvisor.com/Attraction_Review-g303506-d554128-Reviews-or10190-Corcovado_Christ_the_Redeemer-Rio_de_Janeiro_State_of_Rio_de_Janeiro.html"

morepglist <-seq(10, 11000, 10)


pickhotel <- url
# get list of urllinks corresponding to different pages

# url link for first search page
urllinkmain=pickhotel
# counter for additional pages
morepg=as.numeric(morepglist)

urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]

urllink=rep(NA,length(morepg)+1)

urllink[1]=urllinkmain
for(i in 1:length(morepg)){
  urllink[i+1]=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
}
head(urllink)
REview_Data <- data.frame()
for (i in 1:10100) {
  reviews <- urllink[i] %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  usercount <- urllink[i] %>%
    read_html() %>%
    html_nodes("#REVIEWS .memberBadgingNoText")
  
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  value_rating <- reviews %>%
    html_node("li .recommend-answer") %>%
    html_attr("li") %>%
    gsub("ui_bubble_rating bubble_40", "", .) %>%
    gsub("0", "", .) %>%
    as.integer()
  
  
  rating <- reviews %>%
    html_node(".rating span") %>%
    html_attr("class") %>%
    gsub("ui_bubble_rating bubble_", "", .) %>%
    gsub("0", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%d %b %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  webpage <- read_html(urllink[i])
  
  count <- webpage %>%
    html_node(".see_all_count") %>%
    html_text()
  
  things <- webpage %>%
    html_node("span .header_popularity") %>%
    html_text()
  
  Noofcontribution <- usercount %>%
    html_node(".badgetext") %>%
    html_text()
  
  Noofusefulvotes <- usercount %>%
    html_node(".badgetext:last-child") %>%
    html_text()
  Restaurant_Name<-webpage%>%
    html_node(".heading_title")%>%
    html_text()
  REview_Data <- rbind(REview_Data, data.frame(Restaurant_Name,id, quote,value_rating ,rating,date, review,count,things,Noofcontribution,Noofusefulvotes, stringsAsFactors = FALSE))
}

REview_Data

# write data into csv format
write.csv(REview_Data,'corcovado.csv')





# Call data
favela <- read.csv("/Users/danielchoy/Desktop/Tripadvisor/comps3/favela.csv")
favela2 <- REview_Data[-c(70,299, ]

favela2 <- REview_Data[1:241, ]
favela3 <- REview_Data[242:10994, ]

# Chnage text data to to tidy format
tidy_favela <- favela3 %>%
  unnest_tokens(word, review)

# Remove stopwords
tidy_favela <- tidy_favela %>%
  anti_join(stop_words, by=c("word"="word"))


# Visualization of simple count
tidy_favela %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + coord_flip()

sentiments

# NRC Joy words
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

tidy_favela %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


favelasentiment <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# afinn

afinn <- tidy_favela %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

afinn

# bing and NRC
bing_and_nrc <- bind_rows(
  tidy_favela %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_favela %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bing_and_nrc

# Counts
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative", "joy", "fear", "trust", "anger")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)


bing_word_counts <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts




# visualization using ggplot2 - 1
bind_rows(afinn,
          bing_and_nrc) %>%
  ggplot(aes(tidy_favela$word, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# visualization using ggplot2 - 2

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

tmp <- bing_word_counts %>%
  filter(n > 70) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n))

ggplot(data = tmp, mapping = aes(x = word, y = n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# visualization using wordcloud 

tidy_favela %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# visuaolization using wordcloud - 2

tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# Looking at UNits Beyond Just Words - sentence

PandP_sentences <- data_frame(text = REview_Data[191:834, ]$review) %>%
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]  



bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- PandP_sentences %>%
  summarize(sentence = n())

wordcounts # number of sentences


# number of negative sentences
tidy_favela %>%
  semi_join(bingnegative) %>%
  summarize(bingnegative = n()) %>%
  top_n(1)


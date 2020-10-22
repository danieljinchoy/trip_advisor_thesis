install.packages("slam")
library(slam)
install.packages("ggplot")
library(ggplot2)
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)
favela <- read.csv("/Users/danielchoy/Desktop/Tripadvisor/comps/favela2.csv")


favela <- REview_Data

tidy_favela <- favela %>%
  unnest_tokens(new_review, review)

tidy_favela <- tidy_favela %>%
  anti_join(stop_words, by=c("new_review"="word"))

tidy_favela %>%
  count(new_review, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(new_review, n)) %>%
  ggplot(aes(word, n)) + geom_col() + coord_flip()


# Sentiment Analysis

install.packages("tidyr")
library(tidyr)

favelasentiment <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  count(new_review) %>%
  spread(sentimen, n, fill = 0) %>%
  mutate(sentiment = postive - negative)




# New set

favela <- read.csv("/Users/danielchoy/Desktop/Tripadvisor/comps/favela2.csv")


favela <- REview_Data

tidy_favela <- favela %>%
  unnest_tokens(word, review)

tidy_favela <- tidy_favela %>%
  anti_join(stop_words, by=c("word"="word"))

tidy_favela %>%
  count(word, sort = TRUE) %>%
  filter(n > 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col() + coord_flip()


install.packages("tidyr")
library(tidyr)
library(tidytext)
sentiments


tidy_favela %>%
  group_by(word) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, new_review)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_favela %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

# New Set

favelasentiment <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = postive - negative)

afinn <- tidy_favela %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")


bing_and_nrc <- bind_rows(
  tidy_favela %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_favela %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 

bind_rows(afinn) %>%
  ggplot(aes(word, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")



# new set - 2

favelasentiment <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(index = word %/% 10) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = postive - negative)

afinn <- tidy_favela %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

afinn

bing_and_nrc <- bind_rows(
  tidy_favela %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tidy_favela %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) 

bind_rows(afinn) %>%
  ggplot(aes(word, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")







bing_word_counts <- tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup



bing_word_counts

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

library(reshape2)
library(wordcloud)
library(RColorBrewer)

tidy_favela %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


mutate(mydata, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% c(stop_words$word, "jura")) %>%
  group_by(id) %>%
  summarise(text = paste(word, collapse = " "))

text_df <- data_frame(line = 1:392, text = txt) %>%
  txt_df %>%
  anti_join(stop_words)
install.packages("slam")
library(slam)

library(dplyr)
install.packages("tidytext")
library(tidytext)
favela <- read.csv("/Users/danielchoy/Desktop/Tripadvisor/comps/favela2.csv")

favela <- REview_Data

tidy_favela <- favela %>%
  unnest_tokens(new_review, review) %/%
  anti_join(stop_words)

tidy_favela <- tidy_favela %>%
  anti_join(stopwords, by=c("new_review"="word"))

tidy_favela <- tidy_favela %>%
  filter(!new_review %in% stop_words$word)

stop_words
tidy_favela



mutate(mydata, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% c(stop_words$word, "jura")) %>%
  group_by(id) %>%
  summarise(text = paste(word, collapse = " "))

text_df <- data_frame(line = 1:392, text = txt) %>%
  txt_df %>%
  anti_join(stop_words)
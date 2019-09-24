# sentiment analysis
library(tidytext)
library(tidyr)
library(wordcloud)
library(ggplot2)

theme_set(theme_bw())

library(stringr)
library(forcats)
library(wordcloud)

# import data
mvll_tidy <- vargas_llosa() %>%
  unnest_tokens(word, text)

mvll_tidy <- readRDS('mvll_tidy.rds') %>%
  unnest_tokens(word, text)

mvll_sentiment <- mvll_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(title, index = line %/% 80  , sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)


ggplot(mvll_sentiment, aes(index, sentiment, fill=title)) + 
  geom_col() + 
  facet_wrap(~title, scales = "free_x")


s <- mvll_tidy %>% 
  filter(title == "Notebooks of Don Rigoberto" ) %>%
  filter(!str_detect(word, "\u2019")) %>% # remove didn't, they're, etc.
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% 
  with(wordcloud(word, n, max.words = 40))


word_counts <- mvll_tidy %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup
}


# Esto no iria
word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show_legend = FALSE)  +
  facet_wrap(~sentiment, scales = "free_y") + 
  coord_flip()

# Word frequency
library(tidytext)
library(dplyr)
library(ggplot2)
source('./vargas_llosa.R')


tidy_mvll <- vargas_llosa() %>%
  unnest_tokens(word, text)

book_words <- tidy_mvll %>%
  count(title, word, sort = TRUE) %>%
  ungroup %>%
  bind_tf_idf(word, title, n)

plt <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))


grupo_1 <- libros$title[1:4]

plt %>%
  filter(title %in% libros$title[10:13]) %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~title, ncol = 2, scales="free") + 
  coord_flip()
# Word frequency
library(tidytext)
library(dplyr)
library(ggplot2)


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
  



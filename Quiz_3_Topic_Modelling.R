library(stringr)
library(tidyr)
library(tidytext)
library(tm)

#Randomly selected 2 books
#The Gods of Mars - A science fiction book
#Tarzan of the Apes - A movie/Adventure book
titles <- c("The Gods of Mars","Tarzan of the Apes")


books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

books

#SPlitting books by chapters
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

#Creating chapter wise Document Term Matrix
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

library(topicmodels)

#Clustering data into 2 groups
chapters_lda <- LDA(chapters_dtm, k = 2, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)

# Visualizing 2 clustered topics
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#Clearly we can see 2 different clusters which are alligned with the books we selected.
# 1. Something related to Science Helium, Eyes, Time
# 2. SOmething related Forest- Tarzan, Jungle

# This may not be accurate as some words may be overlapping, so it might cause confusion.


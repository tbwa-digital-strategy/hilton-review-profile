rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

setwd("/Users/harro.cyranka/Desktop/projects/hilton_tripadvisor_review_collection/bigram_model_fit")
library(tidyverse);library(tidytext);library(glmnet);library(broom);library(wordcloud2);library(ggwordcloud)

x <- read_csv("hilton_review_data.csv")

##Tri-grams
p <- x %>% select(review_body) %>%
  unnest_tokens(word, review_body, token = "ngrams", n = 3)
p2 <- p %>% separate("word", c("word1", "word2", "word3"), sep = " ")


##n = 4
k <- x %>% select(review_body) %>%
  unnest_tokens(word, review_body, token = "ngrams", n = 4)

k2 <- k %>% separate("word", c("word1", "word2", "word3", "word4"), sep = " ")

##n = 5
z <- x %>% select(review_body) %>%
  unnest_tokens(word, review_body, token = "ngrams", n = 5)

z2 <- z %>% separate("word", c("word1", "word2", "word3", "word4", "word5"), sep = " ")

##n = 6
l <- x %>% select(review_body) %>%
  unnest_tokens(word, review_body, token = "ngrams", n = 6)

l2 <- l %>% separate("word", c("word1", "word2", "word3", "word4", "word5", "word6"), sep = " ")

##Get three strings first


# Positive reviews --------------------------------------------------------
##Only negative
first <- z2 %>% filter(word1 == "only" & word2 == "negative") %>% 
  pull(word5) %>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

second <- z2 %>% filter(word4 == "be"&word5 == "improved") %>%
  pull(word2) %>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

third <- z2 %>% filter(word2 == "really"&word3 == "like") %>%
  pull(word5)%>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

fourth <- p2 %>% filter(word1 == "best"&word2 == "in") %>%
  pull(word3)%>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word")) 

fifth <-  z2 %>% filter(word4 == "have"&word5 == "liked") %>% 
  pull(word1) %>%table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word")) 

positive_reviews_words <- list(first, second, third, fourth, fifth)
names(positive_reviews_words) <- c("only_negative", "be_improved", "really_like",
                                   "best_in","have_liked")

writexl::write_xlsx(positive_reviews_words, "terms_associated_with_positive_review_expr.xlsx")

###
neg_1 <- z2 %>% filter(word1 == "don't" & word2 == "be") %>%
  pull(word3) %>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

neg_2 <- p2 %>% filter(word1 == "always" & word2 == "on") %>% 
  pull(word3) %>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

neg_3 <- p2 %>% filter(word2 == "are" & word3 == "horrible") %>%
  pull(word1) %>% table() %>% broom::tidy() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words, by = c("." = "word"))

neg_4 <- z2 %>% filter(word1 == "wasn't" & word2 == "a")

neg_5 <- l2 %>% filter(word1 == "hyatt" & word2 == "and")

negative_review_words <- list(neg_1, neg_2, neg_3, neg_4, neg_5)
names(negative_review_words) <- c("dont_be", "always_on", "are_horrible",
                                   "wasnt_a","hyatt_and")


writexl::write_xlsx(negative_review_words, "terms_associated_with_positive_negative_expr.xlsx")


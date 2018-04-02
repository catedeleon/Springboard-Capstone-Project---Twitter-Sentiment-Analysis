library(dplyr)
library(stringr)
library(tidytext)
library(twitteR)
library(streamR)
library(tidyverse)
library(tidyr)
library(tm)
library(ggmap)
library(ggplot2)
library(wordcloud)
library(lubridate)
library(data.table)
library(RColorBrewer)
library(reshape2)
library(syuzhet)
library(text2vec)

# Tweet extraction code is in a separate file (Tweet extraction.R)
# Load data frame

tweetsDF <- read.csv("tweetsDF.csv")

# Clean tweets text

tweetsDF$text <- tolower(tweetsDF$text)
tweetsDF$text <- iconv(tweetsDF$text, 'UTF-8', 'ASCII')
tweetsDF$text <-gsub("&amp", "", tweetsDF$text)
tweetsDF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweetsDF$text )
tweetsDF$text = gsub("@\\w+", "", tweetsDF$text)
tweetsDF$text = gsub("[[:punct:]]", "", tweetsDF$text)
tweetsDF$text = gsub("[[:digit:]]", "", tweetsDF$text)
tweetsDF$text = gsub("http\\w+", "", tweetsDF$text)
tweetsDF$text = gsub("[ \t]{2,}", "", tweetsDF$text)
tweetsDF$text = gsub("^\\s+|\\s+$", "", tweetsDF$text)

#get rid of unnecessary spaces
tweetsDF$text <- str_replace_all(tweetsDF$text," "," ")
# Get rid of URLs
# tweetsDF$text <- str_replace_all(tweetsDF$text, "http://t.co/[a-z,A-Z,0-9]*{8}", "")
tweetsDF$text <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweetsDF$text)
# Take out retweet header, there is only one
tweetsDF$text <- str_replace(tweetsDF$text,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
tweetsDF$text <- str_replace_all(tweetsDF$text,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
tweetsDF$text <- str_replace_all(tweetsDF$text,"@[a-z,A-Z]*","") 
# Remove words that start with rt
tweetsDF$text <- gsub("rt\\w+", "", tweetsDF$text)
# Remove extra gibberish
tweetsDF$text <- gsub("eduau\\w+", "", tweetsDF$text)

# Remove stop words

stop_words <- get_stopwords(language = "en")

custom_stop_words <- bind_rows(data_frame(word = c("cosrx","products", "bought", "kalau",
                                                   "au", "lg", "ii", "iii", "iv", "im", "buy",
                                                   "guna", "yg", "inu", "cosu", "tak", "rtshare",
                                                   "ium", "ni", "ml", "mcm", "rttak", "bersihwrong",
                                                   "eduaubdedubu", "yang", "nak", "rtcosrx",
                                                   "rm", "tiam", "rtreview", "rthave", "kat", "rtfor",
                                                   "baru", "amp", "rtsuggestion", "kalau", "cosu", "dah",
                                                   "rtfor", "eduaubdedubueduaubcedubfubb", "inu", "ada",
                                                   "pakai", "lain", "from", "chou", "rthave", "rtproducts",
                                                   "in", "rtshare", "liquidml", "rpml", "k", "au", "rt", "i",
                                                   "rthaveever", "the", "kiteduaubdedubueduaubcedubfubbwatsons",
                                                   "learntdelusionalcleanserlg", "rtfortry", "kitinu", "bhafrom",
                                                   "usedaha", "u", "itightkalaugentle", "cleansercosu", "denganmild",
                                                   "dlucosrx", "really", "patchpatchespublishedaffom", "tu", "now", "product",
                                                   "ill", "rp", "cleu", "madechoosegoingdepoliticized", "hi", "just",
                                                   "sta", "using", "like", "new", "ever", "go", "need", "week", "try",
                                                   "used", "trial", "use"), 
                                          lexicon = c("custom")), 
                               stop_words)

tweetsDF$text <- removeWords(tweetsDF$text, custom_stop_words$word)
tweetsDF$text <- removeWords(tweetsDF$text, stop_words$word)

# Create text and index subset
# Tokenize words

tidy_tweets <- select(tweetsDF, X, text) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 60) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Get Bing Sentiment

Bingsentiment <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(index=X,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(Bingsentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) 

# Compare the three sentiment lexicons

afinn <- tidy_tweets %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = X) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(tidy_tweets %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          tidy_tweets %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = X, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE,width=2) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Get words with "negative" bing sentiment

bing_neg <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")
tidy_tweets %>% 
  inner_join(bing_neg) %>% 
  count(word, sort = TRUE)

# Get top positive and negative words

bing_word_counts <- tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Create wordclouds

tidy_tweets %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

tidy_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 200)

# Topic modelling
# Run Latent Dirichlet allocation

tokens = tweetsDF$text %>% 
  tolower %>% 
  word_tokenizer
it = itoken(tokens, progressbar = FALSE)
v = create_vocabulary(it) %>% 
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

set.seed(123)
lda_model = LDA$new(n_topics = 5, doc_topic_prior = 0.3, topic_word_prior = 0.0001)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.0001, n_check_convergence = 25, 
                          progressbar = FALSE)
barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

lda_model$get_top_words(n = 10, topic_number = c(1L, 2L, 3L, 4L, 5L), lambda = 0.2)

lda_model$plot()

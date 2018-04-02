library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)
library(tm)

tweetsDF <- read.csv("~/Downloads/tweetsDF.csv", comment.char="#")

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
                                                   "ill", "rp", "cleu", "madechoosegoingdepoliticized", "hi", "used", "try", "ever"), 
                                          lexicon = c("custom")), 
                               stop_words)

tweetsDF$text <- removeWords(tweetsDF$text, custom_stop_words$word)
tweetsDF$text <- removeWords(tweetsDF$text, stop_words$word)

# Convert to character

tweetsDF$text <- as.character(tweetsDF$text)

write.csv(tweetsDF,"~/Downloads/clean_tweets.csv")

# Create text and index subset
# Tokenize words
tidy_tweets <- select(tweetsDF, X, text) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

# Get top words

tidy_tweets %>%
  count(word, sort = TRUE) %>%
  filter(n > 60) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

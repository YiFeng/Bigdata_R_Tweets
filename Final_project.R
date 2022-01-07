### Final project for BigData Analytics Course on 2021 Fall


# 1. Run connect.R and pull_tweets.R to get raw tweets DataFrame
# 2. Remove duplicate tweets
# 3. Pre-process tweets


rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.

library(tidyverse)

#set work directory
setwd("/Volumes/GoogleDrive/My Drive/Yi_UCI_courses/2021 Fall/BigData/Week 4_5")
#read in your twitter data from the debate
file_list <- list.files(pattern="*.Rds")
tweets <- do.call("rbind", lapply(file_list, function(x) readRDS(paste(x))))
rm(file_list)

#remove duplicate tweets and non english accounts (best done AFTER cleaning)
tweets <- tweets %>%
  arrange(text) %>%
  distinct(text, .keep_all=T) %>%

#pre-process tweets (like we did in Week 3, but better)
library(qdapRegex)

tweets$text <- tolower(tweets$text)
tweets$text <- rm_url(tweets$text, 
                             pattern=pastex("@rm_twitter_url", "@rm_url"), 
                             extract=FALSE)
tweets$text <- gsub('@\\w+', '', tweets$text) #remove mentions
tweets$text <- gsub("[^\x01-\x7F]", "", tweets$text) #remove emojis
tweets$text <- gsub('[[:cntrl:]]', '', tweets$text) #remove controls
tweets$text <- gsub('[[:punct:]]', '', tweets$text) #remove punctuation
tweets$text <- gsub('[0-9]+', '', tweets$text) #remove numbers
tweets$text <- gsub(' +',' ', tweets$text) #remove extra whitespaces
tweets$text <- gsub("^[[:space:]]*","", tweets$text) #remove leading space
tweets$text <- gsub("[[:space:]]*$","", tweets$text) #remove trailing space

tweets <- tweets %>%
  distinct(text, .keep_all=T) %>%
  filter(lang == "en") %>%
  filter(text != "")

#remove tweets from super users? 
#this makes sense to do if you want to focus on *likely* regular folks
tweets2 <- tweets %>%
  filter((followers_count > 100 & followers_count < 5000) & (statuses_count < 10000))

########################
# Basic bigram analysis
########################
library(tidytext)

debate_bigrams <- tweets2 %>%
  select(text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  arrange(desc(n)) %>%
  slice_max(n, n=20, with_ties = F)

debate_bigrams

# We see that many of the top bigrams include trump, biden, debate -- these words describe the event (especially given that they were the words used to search for tweets!). They don't give us new information about content though. So let's cull those (and some other) words from the text.

# REMOVE SOME WORDS
words_to_remove <- scan(file="dictionaries/words_to_remove.txt", 
                        what="character", 
                        encoding = "UTF-8", 
                        skip=0)
words_to_remove

# if using LIWC, uncomment and run this; not good for basic NLP though
# tweets2$text <- paste0("- ", tweets2$text) #to help with counting words

# if we commit to removing trump and biden, we will lose information about who people thought won...but if we don't care about that, we can safely remove them to get at other content
tweets2$text <- sapply(tweets2$text, function(x) gsub(paste(words_to_remove, collapse = '|'), ' ', x))

#remove the extra spaces that may result from this removal process
tweets2$text <- gsub('[[:punct:]]', '', tweets2$text) #remove punctuation
tweets2$text <- gsub(' +',' ', tweets2$text) ## Remove extra whitespaces
tweets2$text <- gsub("^[[:space:]]*","", tweets2$text)
tweets2$text <- gsub("[[:space:]]*$","", tweets2$text)

#remove empty rows; sometimes cleaning creates empty rows
tweets2 <- tweets2[!(is.na(tweets2$text) | tweets2$text==""), ]

#Let's re-run our bigram analysis to see what has changed
debate_bigrams <- tweets2 %>%
  arrange(text) %>%
  select(text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != "",
         word2 != "") %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  arrange(desc(n)) %>%
  slice_max(n, n=20, with_ties = F)

debate_bigrams
# Now we see more about what aspects of the debate people tweeted about! From here, we could decide to focus our analytic attention on specific topics (climate-related content, or racism in america, etc)

# Let's visualize our findings
ggplot(debate_bigrams, aes(x = reorder(bigram, n),  y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Bigram text\n", 
       y = "\nFrequency of bigrams", 
       caption = "n = 65,225 tweets") +
  theme_light(base_size=12)
#ggsave("debate bigrams.png")


###############################
# Group-level bigram analysis #
###############################

# We know that about half of our tweets come from power users who likely represent public figures, businesses, agencies, etc. We could run a separate bigram analysis on the tweets2 dataframe, but instead, let's put analyses of both data frames under the same hood and create bigrams for each group (super users and regular folk).

tweets3 <- tweets %>%
  mutate(user_type = 
           case_when(followers_count < 5000 & statuses_count < 10000 ~ 1, 
                     TRUE ~ 0))
table(tweets3$user_type)

#label each group   
tweets3$user_type[tweets3$user_type == 0] <- "Super Users"  
tweets3$user_type[tweets3$user_type == 1] <- "Regular Users" 
table(tweets3$user_type)

# REMOVE SOME WORDS
words_to_remove <- scan(file="dictionaries/words_to_remove.txt", 
                        what="character", 
                        encoding = "UTF-8", 
                        skip=0)
words_to_remove

# if using LIWC, uncomment and run this; not good for basic NLP though
#tweets3$text <- paste0("- ", tweets2$text) #to help with counting words

# if we commit to removing trump and biden, we will lose information about who people thought won...but if we don't care about that, we can safely remove them to get at other content
tweets3$text <- sapply(tweets3$text, function(x) gsub(paste(words_to_remove, collapse = '|'), ' ', x))

#remove the extra spaces that may result from this removal process
tweets3$text <- gsub('[[:punct:]]', '', tweets3$text) #remove punctuation
tweets3$text <- gsub(' +',' ', tweets3$text) ## Remove extra whitespaces
tweets3$text <- gsub("^[[:space:]]*","", tweets3$text)
tweets3$text <- gsub("[[:space:]]*$","", tweets3$text)


group_debate_bigrams <- tweets3 %>%
  select(user_type, text) %>% #adding in user_type
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(word1 != "",
         word2 != "") %>%
  count(user_type, word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")
  
group_debate_bigrams

# create a data frame with the numbers we want to visualize
top_debate_bigrams <- group_debate_bigrams %>%
  arrange(user_type, desc(n)) %>%
  group_by(user_type) %>%
    slice_max(n, n=20, with_ties = F) %>%
  ungroup() %>%
  #to help with visualization
  mutate(user_type = as.factor(user_type),
         bigram = reorder_within(bigram, n, user_type))
#more info: https://www.rdocumentation.org/packages/tidytext/versions/0.2.6/topics/reorder_within
 
# Let's visualize our findings
ggplot(top_debate_bigrams, aes(bigram, n, fill = user_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~user_type, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "Bigram text\n", 
       y = "\nFrequency of bigrams", 
       caption = "n = 65,324 tweets") +
  theme_light(base_size=12) +
  theme(axis.text.x = element_text(angle=45, hjust=1))
 

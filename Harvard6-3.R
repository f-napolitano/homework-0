# Modulo 6 Harvard - Wrangling
# Seccion 4 - Dates, Times, and Text Mining

# ----- 4.1.1. Dates and Times -----------------
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) + 
  geom_line()

# lubridate --> the tidyverse date  package
library(lubridate)

# select ramdom dates from polls and extract month, day, year from date strings
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
data.frame(date = dates,
           month = month(dates),
           day = day(dates),
           year = year(dates))

# for times
now() # current time in your time zone
now ("GMT")
now() %>% hour()
now() %>% minute()

# ----- 4.1.2. Text Mining -----------------
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(jsonlite)
library(dslabs)
set.seed(1)

url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 
# don't know why before doesn't work, loading trump_tweets from dslabs

head(trump_tweets)
trump_tweets %>% select(text) %>% head
trump_tweets %>% count(source) %>% arrange(desc(n))

# using "extract" to remove the "Twitter for" the part of the source and filter out retweets
trump_tweets %>% extract(source, "source", "Twitter for (.*)") %>% count(source)

# analysis on what was tweeted from the day of campaign announcement up to election day. 1st creating the table
campaign_tweets <- trump_tweets %>%
  extract(source, "source", "Twitter for (.*)") %>% 
  filter(source %in% c("Android", "iPhone") & created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>% 
  arrange(created_at)

# to test the possibility that 2 diff groups were tweeting from these devices --> extract the hours and see the distribution
ds_theme_set()
campaign_tweets %>% 
  mutate(hour = hour(with_tz(created_at, "EST"))) %>% 
  count(source, hour) %>% 
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>% 
  ungroup %>% 
  ggplot(aes(hour, percent, color = source)) + 
  geom_line() + geom_point() + scale_y_continuous(labels = percent_format()) + 
  labs(x = "Hours of the day (EST)", y = "% of tweets", color = "")

# clearly 2 diff trends are showing up there. Now analyse the text inside tweets
library(tidytext)

# 1st, an example of how unnest_token works
example <- data_frame(line = c(1, 2, 3, 4), 
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example %>% unnest_tokens(word, text)
# back to trump, example seeing a particular tweet
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% unnest_tokens(word, text) %>% select(word)
# selecting a pattern to extract tweeter characters too as # @
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>% unnest_tokens(word, text, token = "regex", pattern = pattern) %>% select(word)
# removing links to pictures
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>% select(word)
# extracting words from all tweets
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern)

# extracting the most commons words
tweet_words %>% count(word) %>% arrange(desc(n))
# very common words, not saying anything, stop_words helps to filter common words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>% 
  filter(!word %in% stop_words$word)

tweet_words %>% count(word) %>% top_n(10, n) %>% mutate(word = reorder(word, n)) %>% arrange(desc(n))
# removing words that are numbers and quote marks
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# calculating the chance that each single word came from android or from iphone
android_iphone_or <- tweet_words %>% 
  count(word, source) %>% 
  spread(source, n, fill = 0) %>% # spread diff sources into diff columns
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) /
           ((iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)
# imposing a filter based on total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>% arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>% arrange(or)

# Sentiments Analysis
sentiments
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>% select(word, sentiment)

# keeping only words with a sentiment associated
tweet_words %>% inner_join(nrc, by = "word") %>%
  select(source, word, sentiment) %>% sample_n(10)
# so now we can compare the frequency of each sentiment appears in each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>% 
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))

# since there are more words in android than iphone
tweet_words %>% group_by(source) %>% summarise(n = n())
# for each sentiment we can compute the odds of being in each device: proportion of words with sentiment vs proportion of words without and then compute the odds ratio comparing both devices
sentiment_counts %>% 
  mutate(Android = Android / (sum(Android) - Android), 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android / iPhone) %>%
  arrange(desc(or))

# how does this sentiment compares if we were just assigning sentiments at random?
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

# a graphical representation show some sentiments that are clearly overrepresented
log_or %>% 
  mutate(sentiment = reorder(sentiment, log_or),) %>% 
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) + 
  geom_errorbar() + 
  geom_point(aes(sentiment, log_or)) + 
  ylab("Log odds ratio for association between Android and sentiment") + 
  coord_flip()
# where we can see 5 top negative sentiments are tied to Android whereas words without sentiments are asociated to iphone
# now getting back to android_or_iphone
android_iphone_or %>% inner_join(nrc) %>% 
  filter(sentiment == "disgust" & Android + iPhone > 10) %>% 
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# ----- 4.1.3. Assessment Par 1 Date, Time, Text mining -----------------
library(dslabs)
library(lubridate)
library(tidyverse)
library(dplyr)
options(digits = 3)

data("brexit_polls")
brexit_polls %>% filter(month(startdate) == 4) %>% count
brexit_polls %>% filter(round_date(enddate, unit = "week", week_start = 7) == "2016-06-12") %>% count
weekdays(brexit_polls$enddate) %>% as.data.frame() %>% table()

data("movielens")
year(as_datetime(movielens$timestamp)) %>% table() %>% max()
hour(as_datetime(movielens$timestamp)) %>% table() %>% max()

# ----- 4.1.4. Assessment Par 2 Date, Time, Text mining -----------------
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

tmp2 <- gutenberg_metadata
tmp2 <- tmp2 %>% mutate(novel = (str_detect(title, "Pride and Prejudice")))
tmp2 %>% filter(novel) %>% count

tmp2 <- gutenberg_works()
tmp2 <- tmp2 %>% mutate(novel = (str_detect(title, "Pride and Prejudice")))
id_correct <- tmp2 %>% filter(novel & !is.na(author))
id_correct <- id_correct$gutenberg_id

book <- gutenberg_download(id_correct, mirror = "http://gutenberg.readingroo.ms/")
words <- book %>% unnest_tokens(word, text)
count(words)
words_tmp <- anti_join(words, stop_words, by = "word")
count(words_tmp)

words_tmp <- words_tmp %>% mutate(number = (str_detect(words_tmp$word, "\\d+"))) %>% filter(!number)
count(words_tmp)

words_tmp %>% count(word) %>% filter(n > 100) %>% arrange(desc(n))

afinn <- get_sentiments("afinn")
afinn_sentiments <- words_tmp %>% inner_join(afinn, by = "word")
total_words_afinn_sent <- count(afinn_sentiments)
positive_words_afinn_sent <- afinn_sentiments %>% filter(value > 0) %>% count
positive_words_afinn_sent / total_words_afinn_sent
afinn_sentiments %>% filter(value == 4) %>% count

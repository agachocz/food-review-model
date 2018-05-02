install.packages("tidytext")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("stringr")

library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

#Setting project directory
setwd("~/food-review-model")

#Loading dataset
reviews <- read_csv("food.csv")
reviews <- reviews %>%
  mutate(nr = row_number())

summary(reviews[1:3])

ggplot(reviews) +
  geom_histogram(aes(x=Score), bins=9) +
  ylab("Liczba ocen") +
  xlab("Punkty")

head(reviews)

#Splitting data into training and testing sets
splitPoint <- nrow(reviews) %/% 3 * 2
trainSet <- reviews %>% slice(1:splitPoint)
testSet <- reviews %>% slice((splitPoint + 1):n())

#Analizing words in Summary
textWords <- reviews %>%
  select(nr, Summary) %>%
  unnest_tokens(word, Summary)

#Structure
descWords %>%
  count(word, sort = T) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() + 
  xlab(NULL) +
  ylab("liczba wystąpień") +
  coord_flip()

descWords %>% left_join(reviews) %>%
  count(Score, word) %>%
  ggplot(aes(x=Score, y=n)) +
  geom_point(alpha = .5) +
  xlab("Liczba punktów") +
  ylab("Długość recenzji (w słowach)")

#Sentiment analysis

#Removing stop words
decscWords <- descWords %>% anti_join(stop_words)

#lexicon
#(-5 - very negative, 5 - very positive)
afinn <- get_sentiments("afinn")

#Including the impact of word "not"
descWords <- descWords %>%
  mutate(not = ifelse(lag(word, n=1) == "not", -1, 1))

descSentiment <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment*not))


#Plots
descSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=sum)) +
  geom_point(alpha = .1) +
  geom_smooth(method="lm")

descSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_point()





#Same for full text of review 

textWords <- reviews %>%
  select(nr, Text) %>%
  unnest_tokens(word, Text)

textWords <- textWords %>%
  mutate(not = ifelse(row_number() == 1, 1, ifelse(
    word[row_number()-1] == "not", -1, 1)))

#Removing stop words
textWords <- textWords %>% anti_join(stop_words)

textSentiment <- textWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment*not))

textSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=sum)) +
  geom_point(alpha = .1) +
  geom_smooth(method="lm")

textSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_point()


#Punctuation
descPunct <- reviews %>%
  mutate(excl = str_count(Summary, pattern="[!]"),
         quest = str_count(Summary, pattern="[?]"),
         ellip = str_count(Summary, pattern="[.]{3}"),
         cap = str_count(Summary, pattern = "[:upper:]{2,}"))

textPunct <- reviews %>%
  mutate(excl = str_count(Text, pattern="[!]"),
         quest = str_count(Text, pattern="[?]"),
         ellip = str_count(Text, pattern="[.]{3}"),
         cap = str_count(Text, pattern = "[:upper:]{2,}"))


  

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

head(reviews)

#Splitting data into training and testing sets
half <- nrow(reviews) %/% 2
trainSet <- reviews %>% slice(1:half)
testSet <- reviews %>% slice((half + 1):n())

#Analizing words in Summary
descWords <- trainSet %>%
  select(nr, Summary) %>%
  unnest_tokens(word, Summary)

#Structure
descWords %>%
  count(word, sort = T) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() + 
  xlab(NULL) +
  ylab("liczba wystąpień") +
  coord_flip()

#Sentiment analysis

#lexicon
#(-5 - very negative, 5 - very positive)
afinn <- get_sentiments("afinn")
head(afinn)

#Including the impact of word "not"
descWords <- descWords %>%
  mutate(not = ifelse(word == "not", -1, 1))

for(i in rev(2:nrow(descWords))){
  descWords$not[i] = descWords$not[i-1]
}

descSentiment <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment*not))

descSentiment2 <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(sum = sum(sentiment))

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
textWords <- trainSet %>%
  select(nr, Text) %>%
  unnest_tokens(word, Text)

textWords <- textWords %>%
  mutate(not = ifelse(word == "not", -1, 1))

for(i in rev(2:nrow(textWords))){
  textWords$not[i] = textWords$not[i-1]
}

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
  mutate(excl = str_detect(Summary, pattern="[!]"),
         quest = str_detect(Summary, pattern="[?]"),
         ellip = str_detect(Summary, pattern="[.]{3}"),
         cap = str_detect(Summary, pattern = "[:upper:]{2,}"))

descPunct %>% group_by(excl) %>%
  summarise(mean = mean(Score))

descPunct %>% group_by(quest) %>%
  summarise(mean = mean(Score))

descPunct %>% group_by(ellip) %>%
  summarise(mean = mean(Score))

descPunct %>% group_by(cap) %>%
  summarise(mean = mean(Score))

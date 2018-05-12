install.packages("tidytext")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("gridExtra")
install.packages("pROC")
install.packages("randomForest")

library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(gridExtra)
library(tidyr)

library(rpart)
library(rpart.plot)
library(class)
library(pROC)
library(randomForest)

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


#Analizing words in Summary
descWords <- reviews %>%
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
  group_by(Score) %>%
  summarise(srednia = mean(n),
            odchylenie  = sd(n),
            liczba_obs = n())


descWords %>% left_join(reviews) %>%
  count(Score, word) %>%
  filter(n < 10) %>%
  ggplot(aes(x = n)) +
  facet_wrap(~Score) +
  geom_histogram(bins = 10) +
  labs(title = "Rozkład liczby słów w recenzji w zależności od liczby punktów",
       x = "Liczba recenzji", y = "Liczba słów w recenzji")
  
#Sentiment analysis

#Removing stop words
decscWords <- descWords %>% anti_join(stop_words)

#lexicon
#(-5 - very negative, 5 - very positive)
afinn <- get_sentiments("afinn")

#Including the impact of words "not" and "don't"
descWords <- descWords %>%
  mutate(not = ifelse(lag(word, n=1) == "not" | lag(word, n=1) == "don't", -1, 1))
descWords$not[1] = 1

descSentiment <- descWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(Sum_sum = sum(sentiment*not))

#Plots
descSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=Sum_sum)) +
  geom_point(alpha = .01) +
  geom_smooth(method="lm", se = T)

descSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(Sum_sum), sd = sd(Sum_sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_pointrange(aes(y = mean, ymin = mean-sd, ymax = mean+sd), size = .9, alpha = .7) +
  labs(title = "Średnia wartość łacznego 'zabarwienia' słów w Podsumowaniu",
       x = "Ocena", y = "Średnia")

descSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=as.factor(Score), y=Sum_sum)) +
  geom_boxplot() +
  labs(title = "Średnia wartość łacznego 'zabarwienia' słów w Podsumowaniu",
       x = "Ocena", y = "Średnia")

#Same for full text of review 

textWords <- reviews %>%
  select(nr, Text) %>%
  unnest_tokens(word, Text)

#Structure
textWords %>% left_join(reviews) %>%
  count(Score, word) %>%
  group_by(Score) %>%
  summarise(srednia = mean(n),
            odchylenie  = sd(n),
            liczba_obs = n())

textWords %>% left_join(reviews) %>%
  count(Score, word) %>%
  filter(n < 30) %>%
  ggplot(aes(x = n)) +
  facet_wrap(~Score) +
  geom_histogram(bins = 10) +
  labs(title = "Rozkład liczby słów w recenzji w zależności od liczby punktów",
       x = "Liczba recenzji", y = "Liczba słów w recenzji")

#Including the impact of word "not"
textWords <- textWords %>%
  mutate(not = ifelse(lag(word, n=1) == "not" | lag(word, n=1) == "don't", -1, 1))
textWords$not[1] = 1

textSentiment <- textWords %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(Text_sum = sum(sentiment*not))

textSentiment %>% left_join(reviews) %>%
  ggplot(aes(x=Score, y=sum)) +
  geom_point(alpha = .1) +
  geom_smooth(method="lm")

textSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_point()

textSentiment %>% left_join(reviews) %>%
  group_by(Score) %>%
  summarise(mean = mean(Text_sum), sd = sd(Text_sum)) %>%
  ggplot(aes(x=Score, y=mean)) +
  geom_pointrange(aes(y = mean, ymin = mean-sd, ymax = mean+sd), size = .9, alpha = .7) +
  labs(title = "Średnia wartość łacznego 'zabarwienia' słów w recenzji",
       x = "Ocena", y = "Średnia")


#Punctuation
descPunct <- reviews %>%
  mutate(Sum_excl = str_count(Summary, pattern="[!]"),
         Sum_quest = str_count(Summary, pattern="[?]"),
         Sum_ellip = str_count(Summary, pattern="[.]{3}"),
         Sum_cap = str_count(Summary, pattern = "[:upper:]{2,}"))

textPunct <- reviews %>%
  mutate(Text_excl = str_count(Text, pattern="[!]"),
         Text_quest = str_count(Text, pattern="[?]"),
         Text_ellip = str_count(Text, pattern="[.]{3}"),
         Text_cap = str_count(Text, pattern = "[:upper:]{2,}"))


#Plots
descPunct %>% gather(non_verb_attr, quantity, Sum_excl:Sum_cap) %>%
  group_by(Score, non_verb_attr) %>%
  summarise(mean = mean(quantity)) %>%
  ggplot(aes(x = Score, y = mean, color = non_verb_attr)) +
  geom_line() +
  scale_color_discrete(name = "Czynnik", breaks = c("Sum_cap", "Sum_ellip", "Sum_excl", "Sum_quest"),
                       labels = c("wielkie litery", "wielokropki", "wykrzykniki", "pytajniki")) +
  labs(title = "Średnia liczba czynników niewerbalnych w podsumowaniach",
       x = "Ocena", y = "Średnia")  

textPunct %>% gather(non_verb_attr, quantity, Text_excl:Text_cap) %>%
  group_by(Score, non_verb_attr) %>%
  summarise(mean = mean(quantity)) %>%
  ggplot(aes(x = Score, y = mean, color = non_verb_attr)) +
  geom_line() +
  scale_color_discrete(name = "Czynnik", breaks = c("Text_cap", "Text_ellip", "Text_excl", "Text_quest"),
                       labels = c("wielkie litery", "wielokropki", "wykrzykniki", "pytajniki")) +
  labs(title = "Średnia liczba czynników niewerbalnych w recenzjach",
       x = "Ocena", y = "Średnia") 


# BUILDING MODELS

# Variables



# words count
Sum_words <- descWords %>% full_join(reviews) %>%
  group_by(nr) %>%
  summarise(Sum_words = n())

Text_words <- textWords %>% full_join(reviews) %>%
  group_by(nr) %>%
  summarise(Text_words = n())


# sentiment

Sum_sent <- descSentiment %>% inner_join(reviews) %>%
  select(nr, Sum_sum)

Text_sent <- textSentiment %>% inner_join(reviews) %>%
  select(nr, Text_sum)

# punctuation

Sum_punct <- descPunct %>%
  select(nr, Sum_excl, Sum_quest, Sum_ellip, Sum_cap)

Text_punct <- textPunct %>%
  select(nr, Text_excl, Text_quest, Text_ellip, Text_cap)


# Merging Summary and Text
words_count <- full_join(Sum_words, Text_words) %>%
  mutate(Words = Sum_words + Text_words,
         Sum_words = NULL,
         Text_words = NULL)

words_count %>% filter(is.na(Words))

sentiment <- inner_join(Sum_sent, Text_sent) %>%
  mutate(Sent = Sum_sum + Text_sum,
         Sum_sum = NULL,
         Text_sum = NULL)

punct <- inner_join(Sum_punct, Text_punct) %>%
  mutate(Excl = Sum_excl + Text_excl,
         Quest = Sum_quest + Text_quest,
         Ellip = Sum_ellip + Text_ellip,
         Cap = Sum_cap + Text_cap
         ) %>%
  select(nr, Excl, Quest, Ellip, Cap)

Merged_vars <- full_join(words_count, sentiment) %>%
  full_join(punct) %>%
  mutate(Sent = if_else(is.na(Sent), 0, Sent))


# Traing and testing datasets
data_df <- as.data.frame(Merged_vars)
y_vals <- as.factor(reviews$Score)

size <- nrow(data_df)
train_size <- round(size*2/3)
split_points <- sample(size, train_size, replace = F)

train_set <- data_df[split_points,]
test_set <- data_df[-split_points,]
train_y <- as.factor(y_vals[split_points])
test_y <- as.factor(y_vals[-split_points]) 




# KNN MODEL *******************************************************************

auc <- vector()

for(i in 1:5){
  knn_model <- knn(train = train_set, test = test_set, cl = train_y, k = i)
  auc[i] <- multiclass.roc(test_y, ordered(knn_model))$auc[1]
}

plot(auc)
acc

?multiclass.roc

#Najlepsza wartość jest o 5% lepsza od losowania

# DRZEWA

tree_model <- rpart(Score ~., data = cbind(train_set, Score = train_y), method = "class")
rpart.plot(tree_model)
tree_pred <- predict(tree_model, test_set, type = "class")
multiclass.roc(test_y, ordered(tree_pred))

# Equalization
score_5 <- reviews %>% filter(Score == 5) %>%
  sample_n(5000, replace = F) %>%
  select(nr)

other_score <- reviews %>% filter(Score != 5) %>%
  select(nr)

chosen_data <- as.vector(rbind(score_5, other_score))
data_df <- data_df[chosen_data$nr,]
y_vals <- y_vals[chosen_data$nr]

size <- nrow(chosen_data)
train_size <- round(size*2/3)
split_points <- sample(size, train_size, replace = F)

train_set <- data_df[split_points,]
test_set <- data_df[-split_points,]
train_y <- as.factor(y_vals[split_points])
test_y <- as.factor(y_vals[-split_points]) 

tree_model <- rpart(Score ~., data = cbind(train_set, Score = train_y), method = "class")
rpart.plot(tree_model)

tree_pred <- predict(tree_model, test_set, type = "class")
multiclass.roc(test_y, ordered(tree_pred))


# RANDOM FOREST

rf_model <- randomForest(Score ~., data = cbind(train_set, Score = train_y), ntree = 100)
rf_pred <- predict(rf_model, test_set)
multiclass.roc(test_y, ordered(rf_pred))

# TEST DATASET *************************************************************

test_set <- read_csv("food_test.csv") %>%
  transmute(nr = row_number(),
         review = paste(Summary, Text))

test_words <- test_set %>%
  unnest_tokens(word, review)

# words count
test_words_count <- test_words %>%
  group_by(nr) %>%
  summarise(Sum_words = n())

# sentiment

#Including the impact of word "not"
test_words <- test_words %>%
  mutate(not = ifelse(lag(word, n=1) == "not" | lag(word, n=1) == "not", -1, 1))
test_words$not[1] = 1

testSentiment <- test_words %>% inner_join(afinn) %>%
  rename(sentiment = score) %>%
  group_by(nr) %>%
  summarise(test_sum = sum(sentiment*not))

test_sent <- testSentiment %>%
  select(nr, test_sum)


#Punctuation
testPunct <- test_set %>%
  mutate(Excl = str_count(review, pattern="[!]"),
         Quest = str_count(review, pattern="[?]"),
         Ellip = str_count(review, pattern="[.]{3}"),
         Cap = str_count(review, pattern = "[:upper:]{2,}"))

test_punct <- testPunct %>%
  select(nr, Excl, Quest, Ellip, Cap)

Test_vars <- inner_join(test_words_count, test_sent) %>%
  inner_join(test_punct)

names(Test_vars) <- names(Merged_vars)

prediction <- predict(rf_model, Test_vars)
names(prediction) <- c("nr", "Score")
write.csv(prediction, "pred.csv")


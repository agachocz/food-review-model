install.packages("tidytext")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")

library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)

#Setting project directory
setwd("~/food-review-model")

#Loading dataset
reviews <- read_csv("food.csv")
head(reviews)

#Splitting data into training and testing sets
half <- nrow(reviews) %/% 2
trainSet <- reviews %>% slice(1:half)
testSet <- reviews %>% slice((half + 1):n())


# random forest
rm(list=ls())

library(randomForest)
library(tidyverse)
library("caTools")
library(stringr)

load('animesdados.RDATA')

Tema_total
animes_tema
summary(animes_tema)


colnames(animes_tema)=str_replace_all(colnames(animes_tema)," ",'_')
animes_tema

random_anime= animes_tema[,c(2,8,12,23:ncol(animes_tema))]

random_anime
random_anime=random_anime[,2:ncol(random_anime)]
random_anime

str(random_anime)

random_anime[1:ncol(random_anime)]= lapply(random_anime[,1:ncol(random_anime)], factor) 
random_anime=random_anime[,2:ncol(random_anime)]

random_anime

str(random_anime)
sample = sample.split(random_anime$Demographic, SplitRatio = .75)

train = subset(random_anime, sample == TRUE)
train
test  = subset(random_anime, sample == FALSE)
sum(is.na(train))

rf <- randomForest(
  Demographic ~ .,
  data=train
)

rf

table(random_anime$Demographic)

library(rsconnect)
library(rpart)
library(plyr)
library(ggplot2)
library(rpart.plot)

#Goal:  Plug in attributes and determine a cost with a level of variability.  Also, subset to see info on similar past data points
diamonds <- read.csv("diamonds.csv")
diamonds <- diamonds[sample(1:nrow(diamonds)), ] #randomize

str(diamonds) #view structure

#Use regression trees to model the price of diamonds based on all attributes in the dataframe
m.rpart <- rpart(price ~ .,data = diamonds,cp = .0001)

rpart.plot(m.rpart, type = 4, extra = 101, digits = -3)

prediction <-  data.frame(predict(m.rpart, diamonds))
diamonds$prediction <- prediction

diamonds$percent <- (diamonds$price - diamonds$prediction)/diamonds$price
#summary(diamonds$percent)

diamonds$correct <- ifelse(diamonds$percent < .25,1,0) #determine model accuracy
#summary(diamonds$correct)
#table(diamonds$correct)
sum(diamonds$correct) / nrow(diamonds) *100 #percent correct within 25% threshold

#Above proves the model holds weight. Now, let's find best deals in the training dataset
diamonds$bestDeal <- diamonds$prediction - diamonds$price
diamonds <- diamonds[order(diamonds$bestDeal),] #order dataframe

#Run the model on new diamonds found on the internet to determine which are undervalued
s <- read.csv("diamonds_test.csv")
t <- data.frame(predict(m.rpart, s))

s$prediction <- t
s$bestDeal <- s$prediction - s$price
s$percentDeal <- s$bestDeal / s$price *100
s <- s[order(s$bestDeal),]
s$Underpriced <- ifelse(s$percentDeal>0,"Good Deal","Bad Deal")
u <- data.frame(s$price,s$prediction,s$percentDeal,s$Underpriced)
u
# Biostat 682 Project

# Loading and fiddlign with the dataset.

setwd("/Users/noahgale/Desktop/UMich Classes and Clubs/Biostat 682/Project")
data1 = read.csv("character-deaths.csv", header = TRUE)
# data2 = read.csv("character-predictions.csv", header=TRUE)
data1 = subset(data1, select=c(Name, Allegiances, Book.of.Death, Death.Chapter, Book.Intro.Chapter, Gender, Nobility, GoT, CoK, SoS, FfC, DwD))
final_data = data1

GoT_chapter_total = 73
CoK_chapter_total = 70
Sos_chapter_total = 82
FfC_chapter_total = 46
DwD_chapter_total = 72

final_data$is.alive = 0
final_data$is.alive[is.na(final_data$Book.of.Death)] = 1
train = final_data[1:600,]
test = final_data[601:917,]



install.packages("randomForest")
require(randomForest)
fit = randomForest(as.factor(is.alive)~ Gender + Nobility + GoT + CoK + SoS + FfC + DwD, data= train, importance = TRUE, ntree = 1000)
summary(fit)
pred = predict(fit, test)
pred2 = predict(fit, train)
table(pred, test$is.alive)
table(pred2, train$is.alive)

varImpPlot(fit)
mean(pred != test$is.alive)
mean(pred2 != train$is.alive)


test1 = final_data[final_data$DwD == 1,]
train1 = final_data[final_data$DwD == 0,]
fit1 = randomForest(as.factor(is.alive)~ Gender + Nobility + GoT + CoK + SoS + FfC, data= train1, importance = TRUE, ntree = 1000)
summary(fit1)
pred1 = predict(fit1, test1)
pred21 = predict(fit1, train1)

table(pred1, test1$is.alive)
table(pred21, train1$is.alive)

varImpPlot(fit1)
mean(pred1 != test1$is.alive)
mean(pred21 != train1$is.alive)




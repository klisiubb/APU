install.packages("randomForest")
install.packages("e1071")
install.packages("party")
install.packages("mlr")
install.packages("rFerns")

library(randomForest)
library(e1071)
library(party)
library(mlr)
library(rFerns)


setwd("C:/Users/klism/Documents/GitHub/APU/lab3/")
lodowki_turystyczne=read.csv("lodowki.csv")
lodowki_turystyczne <- lodowki_turystyczne [1:6]
lodowki_turystyczne$nazwa = factor(lodowki_turystyczne$nazwa)
lodowki_turystyczne$ocena_klientow = factor(lodowki_turystyczne$ocena_klientow)

summarizeColumns(lodowki_turystyczne)

rdesc = makeResampleDesc("CV", iters = 10)

task = makeClassifTask(id = deparse(substitute(lodowki_turystyczne)), lodowki_turystyczne, "ocena_klientow",
                       weights = NULL, blocking = NULL, coordinates = NULL,
                       positive = NA_character_, fixup.data = "warn", check.data = TRUE)
lrns <- makeLearners(c("rpart", "C50", "ctree", "naiveBayes", "randomForest"), type = "classif")


bmr <- benchmark(learners = lrns, tasks = task, rdesc, models = TRUE, measures = list(acc, ber))
p = getBMRPredictions(bmr)
plotBMRSummary(bmr)


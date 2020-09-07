library(tidyverse)
library(lme4)

discrimination <- read.csv("AXresults.csv")
head(discrimination)
#Data set is of an AX discrimination task wherein pre-training and post-training accuracy are compared.
#There are trained and untrained items. Untrained items are there to measure generalization. 

discrimination$accuracy <-as.factor(discrimination$accuracy)


model1 <- glmer(accuracy~ Task * trained + (1 + Task|ID), data=discrimination, family= binomial)
model1

#Issues: I only have 10 participants so far and I get warning messages of a "singular fit" or that "the model does not converge".
#I also encounter this error in previous data sets and I am not sure how to address the warning. 


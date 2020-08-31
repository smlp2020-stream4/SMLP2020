# Christoph Aurnhammer
# aurnhammer@coli.uni-saarland.de
# 31.08.2020

# Load packages
library(data.table)
library(lme4)

# Load data
dt <- fread("chr_data.csv")

# z-score predictors
dt[,c("Cloze", "Assoc")] <- data.table(scale(dt[,c("Cloze","Assoc")])[,])

#### Model Reaction Times
# model linear relationship to log reaction times
rt_log <- lmer(log(ReactionTime) ~ 1 + Cloze * Assoc + (1 + Cloze * Assoc | Item) + (1 + Cloze * Assoc | Subject), dt)
summary(rt_log)

# model raw reaction times in glmer with gamma distribution
rt_glmer <- glmer(ReactionTime ~ 1 + Cloze * Assoc + (1 + Cloze * Assoc | Item) + (1 + Cloze * Assoc | Subject), dt, family=Gamma(link="identity"))
summary(rt_glmer)

#### Model Accuracy: logistic regression
acc_logistic <- glmer(Accuracy ~ 1 + Cloze * Assoc + (1 + Cloze * Assoc | Item) + (1 + Cloze * Assoc | Subject), dt, family=binomial(link="logit"))
summary(acc_logistic)

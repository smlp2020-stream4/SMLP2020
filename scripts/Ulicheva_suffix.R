# Data and model from Ana Ulicheva

## Background

# Participants saw nonwords with different types of endings and had to respond 
# whether these looked more like adjectives or nouns. 

# One analysis (see below), I did is looking at whether the probability of 
# responding Adjective (1) or Noun (0) (Response.coded) differed across conditions 
# (cond; AS - adjective suffixes, NS - noun suffixes, NE - noun endings).
#  Random intercepts are specified for subjects (Participant.Public.ID) and suffixes 
# (suf; all nonwords were different, only suffixes repeated)

## Setup and read data
library(lme4) # (g)lmer
library(tidyverse)

load("../data/data1.rda")

lc <- as_tibble(lc)
lc

## Model
m1 <- glmer(Response.coded~cond+(1|Participant.Public.ID)+(1|suf),
       data=lc, family=binomial())
print(summary(m1), corr=FALSE)

## Appendix
sessionInfo()       

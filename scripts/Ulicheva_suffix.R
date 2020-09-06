# Data and model from Ana Ulicheva
# Something with suffixes ... 

# Setup
library(lme4) # (g)lmer
library(tidyverse)

# README 



# Read data
load("../data/data1.rda")


lc <- as_tibble(lc)
lc

# Model
m1 <- glmer(Response.coded~cond+(1|Participant.Public.ID)+(1|suf),
       data=lc, family=binomial())

# Appendix
sessionInfo()       

library(readr)
smlp2020_SIQI <- read.csv("../data/smlp2020_SIQI_2_SPR.csv")
df = data.frame(smlp2020_SIQI)


library(plyr)
library(dplyr)
# con. -0.5, cau. 0.5
df$C <- mapvalues(df$condition, 
                  from = c('a','b','c','d','e','f','g','h'), 
                  to = c(-0.5,-0.5,-0.5,-0.5,0.5,0.5,0.5,0.5))
# pls. 0.5, imp. -0.5
df$P <- mapvalues(df$condition, 
                  from = c('a','b','c','d','e','f','g','h'),
                  to = c(0.5,0.5,-0.5,-0.5,0.5,0.5,-0.5,-0.5))
# neg. -0.5, aff. 0.5
df$N <- mapvalues(df$condition, 
                  from = c('a','b','c','d','e','f','g','h'),
                  to = c(0.5,-0.5,0.5,-0.5,0.5,-0.5,0.5,-0.5))



df$logRT = log(df$RT)



library(lme4)
library(lmerTest)
# critical regions: 6,7,8. 
# Run LMM for each region separately.
fullmodel = lmer(logRT ~ C * P * N + (1|subject) + (1|item),
                 data = df[(df$region==8),])
summary(fullmodel)
###





################# QUESTIONS ##################
# When it comes to a design of more than 3 variables (e.g., 2*2*2=8), I'm lost in model selection and interpretation.


# Q1. Model selection
# For example, in the current dataset, a full model on region 8 only shows a significant two-way P*N interaction. Is it the best model?
# What should I do next? Should I delete the fixed effects in the model one by one, one for each time, 
# e.g., first the N.S. three-way interaction, and then the N.S. C*N interaction, etc., and compare the model fit after each step of deletion?

# If there are four or more variables (and their interactions), there will be a long list of fixed effects from the full model.
# In which order should I choose the best model? This might be an old question, but I'm always confused when it comes to my own data.


# Q2. Model interpretation
# If the model output includes not only significant main effects, but also two-way or even three- or four-way interactions, how should I interpret all those significances?
# What we experimental linguists care about is always the interaction. For example, with the 2*2*2 design in the current dataset, I'll be expecting to find the three-way interactions on region 6, and will be interested to see the post-hoc results following that.

# My question is, can I ignore the other two-way interactions (if there's any, unfortunately not in the current dataset) while only reporting/focusing on the three-way interaction and post-hoc tests thereof?
# Are there any necessary relations between the higher-level interactions and the lower-level ones that cannot be neglected/need be reported or taken into account?

# I don't have a bigger dataset at hand, but I think the dataset you provided (MRK17_Exp1_xtra.csv) is a good example. 
# Do I have to discuss all significant effects in the model output? 
# Which ones should I keep and which can be left out? 
# Any relations between the four-way, three-way, and two-way interactions in the output?
# What's the 'correct'/suggested way to present results in the manuscript?

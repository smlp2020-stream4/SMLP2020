# rating-task data
# The experiment had four conditions. Sum contrast analysis have been performed.
# The intercepet have been changed twice.  In the first analysis intercept condition 'a'
# while in second analysis the intercept has been changed to condition 'd'.
# In this experiment we are checking participants awareness of grammaticality in conditions
# where we have either given local coherence ungrammatical parse or case-confusion ungrammatical parse.

#exp1
# a:Grammatical-baseline
# b:Local-coherence
# c:Case-confusion
# d:Ungramatical-baseline

# rk-comment: using tidyvers, you need factors,not strings
library(tidyverse)
data <- 
    read_tsv('../data/apurva-smlp-data.tsv') %>%
    transmute(Subj=as_factor(subj), 
              Exp=as_factor(exp), 
              Item=as_factor(item), 
              Cond=as_factor(cond),
              rating = rating, rt = rt)
data


# rk-comment: Sanity-data points
data %>%
    group_by(Subj, Cond) %>%
    tally()

data %>%
    group_by(Item, Cond) %>%
    tally()

# mean-plot
means.cond<-with(data,tapply(rating,Cond, mean,na.rm=T))
barplot(means.cond)
means.cond

#mean
# a:6.02
# b:5.95
# c:5.96
# d:1.51

# rk-comment: assuming you want treatment contrasts; note the specification of lmer()
# treatment contrast; condition 'a' as the intercept
library(lme4)
m1 <- lmer(rating~Cond+(1 | Subj) + (1 | Item), 
            REML = FALSE, data=data, control =lmerControl(calc.derivs=FALSE))
summary(m1)

# changing the intercept from 'a' to 'd'
contrasts(data$Cond) <- contr.treatment(4, base=4)

# treatment contrast; condition 'd' as the intercept
m2 <- lmer(rating ~ Cond + (1 | Subj) + (1 | Item),
    REML = FALSE, data = data, control = lmerControl(calc.derivs = FALSE)
)
summary(m2)

##Plotting mean ratings across conditions.

library('dplyr')
mean.rating <- 
    data %>% 
    group_by(Cond) %>%
    summarise(mean.rating=mean(rating,na.rm = TRUE), se.rating=sd(rating,na.rm = TRUE)/sqrt(n()))
mean.rating

names(mean.rating) <- c("Condition",  "Rating", "SE")

#barplot (rating)
ggplot(mean.rating, aes(x=Condition, y=Rating)) +
  geom_bar(stat="identity", colour='black',width=.75) +
  scale_fill_grey()+
  ylab("Mean ratings")+
  xlab("Conditions")+
  geom_errorbar(aes(ymin=Rating-SE, ymax=Rating+SE), width=.2)
  
  #appendix
  sessionInfo()
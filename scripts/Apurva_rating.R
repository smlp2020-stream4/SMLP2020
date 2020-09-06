#rating-task data
#The experiment had four conditions. Sum contrast analysis have been
#performed. The intercepet have been changed twice. In the first analysis intercept condition 'a' while in
# second analysis the intercept has been changed to condition 'd'.
# In this experiment we are checking participants awareness of grammaticality in conditions where we have either given 
# Local coherence ungrammatical parse of case-confusion ungrammatical pare.

#exp1
# a:Grammatical-baseline
# b:Local-coherence
# c:Case-confusion
# d:Ungramatical-baseline

library(stringr)
data<- read.table('../data/apurva-smlp-data.tsv',header = T, sep="\t")
head(data)
str(data)
dim(data)


#Sanity-data points
with(data, tapply( item, subj, length))
with(data, tapply( item, cond, length))


#mean-plot
library(ggplot2)
means.cond<-with(data,tapply(rating,cond, mean,na.rm=T))
barplot(means.cond)
means.cond

#mean
# a:6.02
# b:5.95
# c:5.96
# d:1.51

#sum contrast; condition 'a' as the intercept
library(lme4)
m1 <- lmer(scale(rating)~cond+(item|subj), data)
summary(m1)

#Changing the intercept from 'a' to 'd'
DF <- within(data, cond <- relevel(cond, ref = 'd'))
factor(DF$cond)

#sum contrast; condition 'd' as the intercept
m2 <- lmer(scale(rating)~cond+(cond|item), data = DF)
summary(m2)


##Plotting mean ratings across conditions.

data$cond <- as.factor(data$cond)
data$rating <- as.numeric(data$rating)

head(data)
library('dplyr')
mean.rating<-data%>%
  group_by(cond) %>%
  summarise(mean.rating=mean(rating,na.rm = TRUE), sd.rating=sd(rating,na.rm = TRUE)/sqrt(n()))
mean.rating
str(mean.rating)
colnames(mean.rating)[1] <- "Condition"
colnames(mean.rating)[2] <- "Rating"
colnames(mean.rating)[3] <- "SE"

#barplot (rating)
ggplot(mean.rating, aes(x=Condition, y=Rating)) +
  geom_bar(stat="identity", colour='black',width=.75) +
  scale_fill_grey()+
  ylab("Mean ratings")+
  xlab("Conditions")+
  geom_errorbar(aes(ymin=Rating-SE, ymax=Rating+SE), width=.2)


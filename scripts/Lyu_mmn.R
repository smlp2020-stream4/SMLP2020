library(readr)
smlp2020_SIQI <- read_csv("../data/smlp2020_SIQI_1_MMN.csv", #change the path if necessary
                          col_types = cols(area = col_character())) 

data = data.frame(smlp2020_SIQI)


library(lme4)
library(lmerTest)
# The statistical analysis included two parts. In each part, LMM was run for each 'interval' separately.
# There were ten intervals in total, i.e., '1-10' (100ms-600ms, every 50ms).



# Part 1
# effect of 'role' on the mean amplitude: dev1 vs. standard
model1 = lmer(meanamplitude ~ role + (1|Subject),
             data = data[(data$interval==6),])
summary(model1)
###



# Part 2
# main effect of 'group' and 'area', as well as their interaction, on the mismatch negativity (MMN).
# MMN is calculated by subtracting the standard from the deviant (dev1), i.e., dev1 minus standard.

# factors: 
## group (EE vs. CH)
## area (F-frontal, T-temporal)
model2 = lmer(MMN ~ group * area + (1|Subject), 
           data = data[(data$interval==7) & (data$MMN!=0),])
summary(model2)
###





################# QUESTION ##################
# EEG always comes with a large amount of data points.

# In this dataset, I have one data point, which is the mean amplitude of the EEG,
# for each subject's (n=8) each event (n=~100) in each condition (role, n=2), in each topographic region (area, n=2) at each time interval (n=10).

# Some of my colleagues first average the EVENT data for each SUBJECT when they preprocess the data in Matlab before importing it into R, 
# and then they run LMM on the averaged dataset, e.g., for main effects of CONDITION and REGION, in different time windows (INTERVALS).

# My question is, which way is "correct"/suggested - running LMM on the very raw dataset or the (preliminarily) averaged data?



#####PRE-REGISTRATION BALANCE ######


library (data.table)
library (ggplot2)
library (lme4)
library (stringr)
library(tidyverse)
library(dplyr)
library(readxl)
library(car)
library(lmerTest)
library(ggeffects)
options( scipen=999)


#### Experiment 2-- preprocessing #####
setwd("/Volumes/PANI CASADO/ALBA/Uniwestytet Jagiellonski/pre-registration/pre-registration:balance")
results<-read_excel("RES_prereg.xlsx", sheet = "res")
#reading the table with chronset results
chronset<-read_excel("RES_prereg.xlsx", sheet="chronset")
#reading the table for decodint the counterbalance
counterbalance<-read_excel("RES_prereg.xlsx", sheet="counterbalance")
#reading the table with the results of CRT (for calculating the estimate for each session)
crt<-read_excel("RES_prereg.xlsx", sheet="crt")

res<-results %>% mutate(session=as.numeric(session)) %>% left_join(counterbalance, by=c("task_version", "session")) 
res<-res %>% left_join(chronset, by="file_name")

res<-res %>% filter(n_block!="TR") %>% 
  select(sub_id, trial, item_id, session, cond, n_block, lang, cond, pic_type, rt, ACC, problem, trial, lang)

exp2<-res%>%rename(pic_id= item_id, 
                   accuracy= `ACC`,
                   RT= rt)%>%
  mutate(prior.lang.f= as.factor(cond),
         prior.lang.f= as.factor(prior.lang.f),
         sub_id= as.factor(sub_id),
         pic_id= as.factor(pic_id),
         RT= as.numeric(RT))


exp2.total<-exp2%>% mutate(count = 1) %>% group_by(sub_id) %>% summarise(total= sum(count)) %>% 
  filter(total==342) %>% distinct(sub_id) %>% pull

datos.filtered <- exp2 %>% filter(sub_id %in% exp2.total)

exp2_session1<- datos.filtered %>%filter (session==1) %>%group_by(sub_id)%>%mutate(trial= 1:171)
exp2_session2<- datos.filtered %>% filter (session ==2) %>%group_by(sub_id)%>%mutate(trial= 1:171)

datos2<-exp2_session1%>%full_join (exp2_session2)


datos2<- datos2 %>%filter(sub_id!="KR1701", #L2 (>60%)
                          sub_id!="KS0903", #L1 (>90%)
                          sub_id!="NB1103", #L1 (>90%)
                          sub_id!="WN0606", # L1 (>90%)
                          sub_id!="AG2506") # L2 (>60%)
datos2 <- datos2 %>% filter(accuracy == 1, problem==0, RT>300,RT<2000, !is.na(RT), n_block!= 1) %>%
  mutate(trial.l = as.numeric(log(trial)),
         trial.c = trial.l- mean(trial.l))


##filter 2.5sd for RT
filter2 <- datos2 %>% group_by(sub_id) %>% summarise (sd(RT), mean(RT))
filter2 = filter2 %>% mutate(desv= 2.5*`sd(RT)`, up= `mean(RT)`+desv, down= `mean(RT)`-desv)
datos2<- datos2  %>% left_join(filter2, by=c("sub_id"))
datos2<- datos2 %>% mutate(RT.up= ifelse(up<RT, "no", "si"))%>%mutate( RT.down= ifelse(down>RT, "no", "si"))
datos2<- datos2 %>% filter(RT.down== "si", RT.up == "si") %>% 
  mutate(rt_n = -1000/RT,
         lang= as.factor(lang),
         n_block.f= as.factor(n_block),
         prior.lang = ifelse(prior.lang.f=="PL", -0.5, 0.5),
         session =ifelse(session==1, -0.5, 0.5))

dat2<- datos2 %>%select(sub_id, pic_id, prior.lang, prior.lang.f, session,lang, n_block, n_block.f, rt_n, trial.c)
summary(dat2)

## calculate the estimates according to the pre-registration document: 

## we select only the data from block2 - language:L1
block2.1 <- dat2 %>%filter(n_block == 2, prior.lang== -0.5) %>%
  select(sub_id, trial.c, pic_id, prior.lang, rt_n)
#est.naming.RT.L1.block2
mod1.1 = lmer(rt_n ~ 1 + (1|sub_id) + (1|pic_id), block2.1)
fe1.1.intercept= fixef(mod1.1)["(Intercept)"]
rand1.1.inter= ranef(mod1.1)$sub_id["(Intercept)"]
est.B1L1 = fe1.1.intercept + rand1.1.inter

## we select only the data from block2 - language:L2
block2.2 <- dat2 %>%filter(n_block == 2, prior.lang== 0.5) %>%
  select(sub_id, trial.c, pic_id, prior.lang, rt_n)

#est.naming.RT.L2.block2
mod1.2 = lmer(rt_n ~ 1 + (1|sub_id) + (1|pic_id), block2.2)
fe1.2.intercept= fixef(mod1.2)["(Intercept)"]
rand1.2.inter= ranef(mod1.2)$sub_id["(Intercept)"]
est.B1L2 = fe1.2.intercept + rand1.2.inter

## we create a table for the estimates

estimates2= data.frame (cbind(est.B1L1, est.B1L2))
estimates2<-cbind(rownames(estimates2), estimates2)
estimates2 <- data.frame(setNames(estimates2, c("sub_id", "est.B1L1", "est.B1L2")))
estimates2<-as.tibble(estimates2)  

##select the data from block3 only
block3 <- dat2 %>%filter (n_block == 3) %>% select(sub_id, trial.c, pic_id, prior.lang, session, rt_n, n_block)

##joining the estimate table with the data from block2

data2<-block3 %>% left_join(estimates2, by=c("sub_id")) 

#we center the estimates
data2<-data2%>%mutate(est.B1L1.c = est.B1L1-mean(data2$est.B1L1),
                      est.B1L2.c = est.B1L2 -mean(data2$est.B1L2))

summary(data2)


#####CRT estimate experiment 2#####
#estimate for block 1
crt_mod<-crt %>% mutate(rt_n=-1000/rt) %>% filter(TR==0, acc==1) %>% mutate(session = ifelse(session==1, -0.5, 0.5))
m1_crt<-lmer(rt_n ~ session + (1 + session | sub_id), data=crt_mod)
summary(m1_crt)
#singular model: problem z random effectami (random slope + random intercept) dla typu triala - po wywaleniu tego w calosci jest ok, ale czy tak mozna?
#session 1 estimate crt RTs
est.crt.s1 <- fixef(m1_crt)["(Intercept)"] + ranef(m1_crt)$sub_id["(Intercept)"] + (fixef(m1_crt)["session"] + ranef(m1_crt)$sub_id["session"] ) * -0.5
est.crt.s1 <- cbind(sub_id = rownames(est.crt.s1), est.crt.s1)
colnames(est.crt.s1)<-c("sub_id", "est.crt.s1")
#session 2 estimate crt RTs
est.crt.s2 <- fixef(m1_crt)["(Intercept)"] + ranef(m1_crt)$sub_id["(Intercept)"] + (fixef(m1_crt)["session"] + ranef(m1_crt)$sub_id["session"] ) * 0.5
est.crt.s2 <- cbind(sub_id = rownames(est.crt.s2), est.crt.s2)
colnames(est.crt.s2)<-c("sub_id", "est.crt.s2")
#merging two estimates so that they could be added to the "dat" tibble
est.choiceRT<-est.crt.s1 %>% left_join(est.crt.s2, by="sub_id") %>% 
  gather(est.crt.s1,est.crt.s2, key = "session", value = "est.choiceRT") %>% 
  mutate(session=ifelse(session=="est.crt.s2", 0.5,-0.5))
#block2 naming - factor equal to the formerly used "cond"
data2<-data2 %>% left_join(est.choiceRT, by=c("sub_id", "session")) 
summary(data2)

####model experiment 2 ######

dat2<-data2 %>% filter(n_block==3)%>% mutate(block2.lang=prior.lang,
                                             est.choiceRT =est.choiceRT-mean(est.choiceRT))

model2<-lmer(rt_n ~ block2.lang + est.B1L1.c + est.B1L2.c + 
               block2.lang:est.B1L1.c + block2.lang:est.B1L2.c + est.choiceRT + trial.c + session +
               (1 + block2.lang + est.choiceRT + trial.c + session || sub_id) + 
               (1 + block2.lang + est.B1L1.c + est.B1L2.c + block2.lang:est.B1L1.c + 
                  block2.lang:est.B1L2.c + trial.c + session || pic_id), dat2)
summary(model2)

model2a<-lmer(rt_n ~ block2.lang + est.B1L1.c + est.B1L2.c + 
                block2.lang:est.B1L1.c + block2.lang:est.B1L2.c + est.choiceRT + trial.c + session +
                (1 + block2.lang + est.choiceRT + trial.c + session || sub_id) + 
                (1 + block2.lang + est.B1L1.c + est.B1L2.c + block2.lang:est.B1L1.c + est.choiceRT+
                   block2.lang:est.B1L2.c + trial.c + session || pic_id), dat2)
summary(model2a)


model2.1<-lmer(rt_n ~ block2.lang +est.B1L1.c + est.B1L2.c + 
                 block2.lang:est.B1L1.c + block2.lang:est.B1L2.c + est.choiceRT + trial.c + session +
                 (1| sub_id) + 
                 (1 + est.B1L1.c + block2.lang:est.B1L1.c|| pic_id), dat2)
summary(model2.1)



pred2.L1 <- ggpredict(model2.1, terms = c("est.B1L1.c", "block2.lang"))
plot(pred2.L1, colors = "system")+ 
  scale_x_continuous(name= "Estimated L1 naming latencies")+
  scale_y_continuous(name= "Predicted L1 naming latencies (ms)",breaks=(-1000/seq(100, 1500, 100)), labels=seq(100, 1500, 100))+
  scale_color_manual (name ="preceding language", labels=c("L1", "L2"), values= c("black", "red"))+
  theme(text = element_text(size=24))+
  ggtitle(" ")

pred2.L2 <- ggpredict(model2.1, terms = c("est.B1L2.c", "block2.lang"))
plot(pred2.L2, colors = "system")+ 
  scale_x_continuous(name= "Estimated L2 naming latencies")+
  scale_y_continuous(name= "Predicted L1 naming latencies (ms)",  breaks=(-1000/seq(100, 2000, 100)), labels=seq(100, 2000, 100))+
  scale_color_manual (name ="preceding language", labels=c("L1", "L2"), values= c("black", "red"))+
  theme(text = element_text(size=24))+
  ggtitle(" ")

pred2.crt <- ggpredict(model2.1, terms = c("est.choiceRT"))
plot(pred2.crt, colors = "system")

#####PLOTS ####
#plot for est.naming.RT.block2.L1
dat2 %>% mutate(block2.lang=as.factor(block2.lang), session=as.factor(session)) %>% 
  group_by(est.naming.RT.block2.L1, block2.lang, sub_id, session) %>% summarize(mean_rt=mean(rt)) %>%
  ggplot(aes(x=est.naming.RT.block2.L1, y=mean_rt, color=block2.lang)) +
  geom_smooth(method="lm") +
  geom_point() 

#plot for est.naming.RT.block2.L2
plot1= dat2 %>% mutate(block2.lang=as.factor(block2.lang), session=as.factor(session)) %>% 
  group_by(est.naming.RT.block2.L2, block2.lang, sub_id, session) %>% summarize(mean_rt=mean(rt)) %>%
  ggplot(aes(x=est.naming.RT.block2.L2, y=mean_rt, color=block2.lang)) +
  geom_smooth(method="lm") +
  geom_point() 
plot1

####PLOT estimates based in predicitons --library(plyr), BEWARE! #####
pred2.L1= pred2.L1%>%mutate(estimate= "L1")
pred2.L2= pred2.L2%>%mutate(estimate= "L2")
#preceding lang= L1
dat_pred_estL1_precL1=pred2.L1%>%filter(group== -0.5)%>% 
  mutate(est1= x, rt= -1000/predicted,  conf.high= -1000/conf.high, conf.low= -1000/conf.low)

dat_pred_estL2_precL1=pred2.L2%>%filter(group== -0.5)%>% 
  mutate(est2= x, rt= -1000/predicted, conf.high= -1000/conf.high, conf.low= -1000/conf.low)
library(plyr)

value_estL1= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL1$est1, dat_pred_estL2_precL1$est2))
rtL1= as.numeric((rbind.fill.matrix (dat_pred_estL1_precL1$rt, dat_pred_estL2_precL1$rt)))
std.errorL1= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL1$std.error, dat_pred_estL2_precL1$std.error))
conf.lowL1= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL1$conf.low, dat_pred_estL2_precL1$conf.low))
conf.highL1=  as.numeric(rbind.fill.matrix(dat_pred_estL1_precL1$conf.high, dat_pred_estL2_precL1$conf.high))
nameL1= as.factor(rbind.fill.matrix(dat_pred_estL1_precL1$estimate, dat_pred_estL2_precL1$estimate))
detach("package:plyr", unload=TRUE)
estimates2.L1= data.frame (cbind(value_estL1, rtL1, std.errorL1, conf.lowL1, conf.highL1, nameL1))

summary(estimates2.L1)

plot_estimates2.L1= estimates2.L1 %>% mutate(nameL1=as.factor(nameL1)) %>%
  ggplot(aes(x=value_estL1, y=rtL1, color=nameL1)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=estimates2.L1$conf.lowL1, ymax=estimates2.L1$conf.highL1), alpha=0.2)+
  scale_x_continuous(name= "Baseline naming latencies")+
  scale_y_continuous(name= "L1 naming latencies 3rd block (ms)", limits = c(600, 1300))+
  scale_color_manual (name ="", labels=c("L1", "L2"), values= c("black", "red"))+
  theme(text = element_text(size=16))+
  ggtitle("Preceding language= L1 ")

plot_estimates2.L1

#preceding lang= L2
dat_pred_estL1_precL2=pred2.L1%>%filter(group== 0.5)%>% 
  mutate(est1= x, rt= -1000/predicted, conf.high= -1000/conf.high, conf.low= -1000/conf.low)
dat_pred_estL2_precL2=pred2.L2%>%filter(group== 0.5)%>% 
  mutate(est2= x, rt= -1000/predicted, conf.high= -1000/conf.high, conf.low= -1000/conf.low)

library(plyr)
value_estL2= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL2$est1, dat_pred_estL2_precL2$est2))
rtL2= as.numeric((rbind.fill.matrix (dat_pred_estL1_precL2$rt, dat_pred_estL2_precL2$rt)))
std.errorL2= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL2$std.error, dat_pred_estL2_precL2$std.error))
conf.lowL2= as.numeric(rbind.fill.matrix(dat_pred_estL1_precL2$conf.low, dat_pred_estL2_precL2$conf.low))
conf.highL2=  as.numeric(rbind.fill.matrix(dat_pred_estL1_precL2$conf.high, dat_pred_estL2_precL2$conf.high))
nameL2= as.factor(rbind.fill.matrix(dat_pred_estL1_precL2$estimate, dat_pred_estL2_precL2$estimate))
detach("package:plyr", unload=TRUE)
estimates2.L2= data.frame (cbind(value_estL2, rtL2, std.errorL2, conf.lowL2, conf.highL2, nameL2))

summary(estimates2.L2)

plot_estimates2.L2= estimates2.L2 %>% mutate(nameL2=as.factor(nameL2)) %>%
  ggplot(aes(x=value_estL2, y=rtL2, color=nameL2)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=estimates2.L2$conf.lowL2, ymax=estimates2.L2$conf.highL2), alpha=0.2)+
  scale_x_continuous(name= "Baseline naming latencies")+
  scale_y_continuous(name= "L1 naming latencies 3rd block (ms)", limits = c(600, 1300))+
  scale_color_manual (name ="", labels=c("L1", "L2"), values= c("black", "red"))+
  theme(text = element_text(size=16))+
  ggtitle("Preceding language= L2 ")

plot_estimates2.L2



####plot index balance

dat14=dat2%>%mutate(balance= est.B1L2 - est.B1L1)%>% 
  group_by(sub_id)%>%summarise(balance= mean(balance), rt= -1000/mean(rt_n))
summary(dat14)

plot_balance2= dat14 %>%
  ggplot(aes(x=balance, y=rt)) +
  geom_smooth(method="lm") +
  geom_point() +
  scale_x_continuous(name= "Balance index")+
  scale_y_continuous(name= "L1 naming latencies 3rd block (ms)" , limits = c(650, 1250))+
  theme(text = element_text(size=16))+
  ggtitle("Balance- Experiment 2")
plot_balance2





###############################
#this is the shit script fr dont look at this 
#############################


library(tidyverse)
library(readxl)
library(ggthemes)
library(lme4)
library(performance)
library(DHARMa)
library(StatisticalModels)
library(ggeffects)
#load data
plantago_ID<-read_excel("spreadsheets/P_lanceolata.xlsx", sheet = "pollinator_data_raw")
plantago_abundance<- read_excel("spreadsheets/P_lanceolata.xlsx", sheet = "number_of_pollinators")

#still need to fix illuminance :( 
#^ BRUH I FORGOT TO DO THIS 
#REMEMBER TO DO THIS!!!!!!!!!!!!!!


#sort out spreadhseets 
plantago_abundance <- plantago_abundance %>% 
  mutate(time_period = as.factor(time_period), 
         Date_numberic = as.Date(Date, format = "%d%b%Y")- as.Date("2026/06/22"), #column for how late in year
         pollinators_fm = pollinators_flower_minute) #fixing the annoyingly long name for pollinator per flower per minute


#viasualise change in abundance over time of day 
(abundance_time<-ggplot(plantago_abundance, aes(x=time_period, y=pollinators_fm))+
  geom_boxplot()+
  geom_point(col="blue")+
  theme_few()+
  labs)
#definitely have more in the morning, but is this significant???? need anova type thing



##############################################
#testing if time period has effect on abundance
##############################################



#basic linear model
abund1<- lm(pollinators_fm ~ time_period, data = plantago_abundance)

par(mfrow=c(2,2))
plot(abund1) #asumptions
par(mfrow=c(1,1))

#residuals fitted line is not the best but could be acceptable? not 100% sure. q-q is good enough 
#scale location is clearly going up - voilates constant variance 
#constant leverage is fine

summary(abund1)
#r sq = 0.154 


#show model
predict_ab1<- ggpredict(abund1, c("time_period"))
plot(predict_ab1)
######
predict_ab1<- predict_ab1 %>% 
  rename(time_period=x, pollinators_fm = predicted) #rename parameters

(predictplot1<- ggplot()+ 
  geom_ribbon(data=predict_ab1, aes(x=time_period, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
  geom_point(data=plantago_abundance, aes(x=time_period, y=pollinators_fm))+ #plot raw data
  geom_line(data=predict_ab1, aes(x=time_period, y=pollinators_fm)))
#WAIT IDK HOW TO DO THIS FOR CATEGORICAL CUZ THE LINE DOESNT WORK OFC 
#HOW DO I EVEN FIGURE OUT HOW TO FIX THIS 
#is this even that important? 


##########################
#poisson model
#########################

abund2<-glm(pollinators_fm ~ time_period, data = plantago_abundance, family="poisson")

summary(abund2)
#check assumptions
simulateResiduals(fittedModel = abund2, plot = T)
#SO SCUFFED!!!!!
#bad fit for qq - poisson isnt a good fit for our data. was just plain linear model better? 
#not fully sure how to interpret resuduals vs fitted here? 

#get r2. Approx R2 = (NullDeviance - ResidualDeviance)/NullDeviance
#r2
(0.044282-0.033111)/0.044282
# = 0.2522695
summary(abund2)

exp(-6.297) #mean A
# 0.001841822
exp(-6.297-1.707 ) #mean B
#0.0003341235
exp(-6.297-1.116) #mean c
# 0.0006033579
exp(-6.297-1.456 )#mean D
#0.0004294522

#anova
car::Anova(abund2)
#p value being 0.9997 seems suspiciously good... has something gone wrong? 

#r2 for poisson model seems better, however the lm might fit the data better







########################
##plot and make models for all the abiotic factos on abundance, see if any look like they might be important 
#########################

#solar power
(abundance_time<-ggplot(plantago_abundance, aes(x=Solar_power, y=pollinators_flower_minute))+
   geom_point())

enviro1<-lm(pollinators_fm~Solar_power, data=plantago_abundance)

summary(enviro1)
#rsq is 0.02 -only 2 percent of variation explained 
#p not sig tho 
par(mfrow=c(2,2))
plot(enviro1) #asumptions
par(mfrow=c(1,1))
#not bad 

#poisson 
enviro2<- glm(pollinators_fm~Solar_power, data=plantago_abundance, family = "poisson")
summary(enviro2)

#r2 
(0.044282-0.043429)/0.044282
#0.01, so still awful

#solar power not an important factor 

#humidity
(abundance_time<-ggplot(plantago_abundance, aes(x=Humidity, y=pollinators_flower_minute))+
    geom_point())



enviro3<-lm(pollinators_fm ~Humidity, data=plantago_abundance)

par(mfrow=c(2,2))
plot(enviro3) #asumptions
par(mfrow=c(1,1))
#not great, could be worse 

summary(enviro3)
#r square so bad it went negative 

enviro4<-glm(pollinators_fm~Humidity, data =plantago_abundance, family = "poisson")
summary(enviro4)

#r2
(0.044282-0.044274)/0.044282
#0.0001806603 insanley low 

#humidity not an important factor 


#temp sun
(abundance_time<-ggplot(plantago_abundance, aes(x=Temp_sun, y=pollinators_flower_minute))+
    geom_point())
#slight negative trend

enviro5<-lm(pollinators_fm~Temp_sun, data = plantago_abundance)

par(mfrow=c(2,2))
plot(enviro5) #asumptions
par(mfrow=c(1,1))
 
summary(enviro5)
#again negative r2
#temp sun not important


#temp shade 
(abundance_time<-ggplot(plantago_abundance, aes(x=Temp_shade, y=pollinators_flower_minute))+
    geom_point())
#also slight negative trend 

enviro6<-lm(pollinators_fm ~ Temp_shade, data = plantago_abundance)

par(mfrow=c(2,2))
plot(enviro6) #asumptions
par(mfrow=c(1,1))

summary(enviro6)
#negative r2, temp shade not important 


#wind max
(abundance_time<-ggplot(plantago_abundance, aes(x=Wind_max, y=pollinators_flower_minute))+
    geom_point())
enviro7<-lm(pollinators_fm~Wind_max, data=plantago_abundance)

par(mfrow=c(2,2))
plot(enviro7) #asumptions
par(mfrow=c(1,1))

summary(enviro7)
#negative r2, wind max not important 


#average wind
(abundance_time<-ggplot(plantago_abundance, aes(x=Wind_average_every_10, y=pollinators_flower_minute))+
    geom_point())
# wtf is going on here??????
enviro8<-lm(pollinators_fm~Wind_average_every_10, data=plantago_abundance)

par(mfrow=c(2,2))
plot(enviro8) #asumptions
par(mfrow=c(1,1))

summary(enviro8)
#negative r squared 

#############
#from this have discovered that the only abiotic factor that did not give a negative r squared was 
#solar power, however it was literally 0.01 or 0.02 which doesnt rly mean much 




##############
#trying to include solar power in model
##########

abund3<-glm(pollinators_fm~time_period + Solar_power, data = plantago_abundance, family="poisson")
simulateResiduals(fittedModel = abund3, plot = T)
#still shite
summary(abund3)
#r2
(0.044282-0.032997)/0.044282
#0.254844
#literally no difference


#just lm 
abund4<-lm(pollinators_fm~time_period + Solar_power, data = plantago_abundance)

par(mfrow=c(2,2))
plot(abund4) #asumptions
par(mfrow=c(1,1))

summary(abund4)
#rsq is 0.1196 which is actually WORSE than before. 










##################
# time periods continuous rather than categorical.


plantago_abundance <- plantago_abundance %>% mutate(time_period_numeric = as.numeric(time_period))

abund_num1<-lm(pollinators_flower_minute ~ time_period_numeric, data = plantago_abundance)

#check assumptions 
par(mfrow=c(2,2))
plot(abund_num1) #asumptions
par(mfrow=c(1,1))

summary(abund_num1)
confint(abund_num1)
#my brain still doesnt love that this is technically. need to visualise this
#r2 is 0.09. worse than when categorical
#p value bad tho

predict_abund_num1<- ggpredict(abund_num1, c("time_period_numeric"))

plot(predict_abund_num1) #plot predicted values 

#i want to see this with the data on top 

predict_abund_num1<- predict_abund_num1 %>% 
  rename(time_period_numeric=x, pollinators_flower_minute = predicted) #rename parameters

(modelplot1<- ggplot()+ 
    geom_ribbon(data=predict_abund_num1, aes(x=time_period_numeric, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
                 geom_point(data=plantago_abundance, aes(x=time_period_numeric, y=pollinators_flower_minute))+ #plot raw data
                 geom_line(data=predict_abund_num1, aes(x=time_period_numeric, y=pollinators_flower_minute))) #plotting regression line )

#ik this is just basic linear model and i need to try other stuff + whatever, 



#################
#with fixed effects or interactions terms 

model2<-lm(pollinators_flower_minute ~ time_period_numeric + Solar_power, data = plantago_abundance)

par(mfrow=c(2,2))
plot(model2) #asumptions
par(mfrow=c(1,1))

summary(model2)

#solar power actually makes a difference here
#p value scuffed 

model3<-lm(pollinators_flower_minute ~ time_period_numeric*Solar_power, data = plantago_abundance)
par(mfrow=c(2,2))
plot(model3) #asumptions
par(mfrow=c(1,1))

summary(model3)
#okay now time period is actually significant, but solar power isnt? sooo


#temperature
model4<-lm(pollinators_fm~time_period_numeric  + Temp_sun, data = plantago_abundance)
par(mfrow=c(2,2))
plot(model4) #asumptions
par(mfrow=c(1,1))

summary(model4)
#nah
################################
























#try this but poisson 
abund_num2<-glm(pollinators_flower_minute ~ time_period_numeric, data = plantago_abundance, family = "poisson")

simulateResiduals(fittedModel = abund_num2, plot = T)
#same scuffed fit 

summary(abund_num2)
#get r2 Approx R2 = (NullDeviance - ResidualDeviance)/NullDeviance 

(0.044282-0.037156)/0.044282
#0.1609232

#sooo r2 when i had time period as categorial for poisson was 0.2522695, this is way lower. 

#visualise:
predict_abund_num2<- ggpredict(abund_num2, c("time_period_numeric"))%>% 
  rename(time_period_numeric=x, pollinators_flower_minute = predicted)

(modelplot2<- ggplot()+ 
    geom_ribbon(data=predict_abund_num2, aes(x=time_period_numeric, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
    geom_point(data=plantago_abundance, aes(x=time_period_numeric, y=pollinators_flower_minute))+ #plot raw data
    geom_line(data=predict_abund_num2, aes(x=time_period_numeric, y=pollinators_flower_minute))+
    scale_y_log10(labels=scales::comma))#log the axis and raw data, and prevent scientific notation on y axis???) 
#okay i think i did this wrong cuz it looks weird asf 





























###############################
#time to mess abt with insect ID 
##############################


plantago_ID<- plantago_ID %>% 
  filter(insect_ID!="N/A") %>% #get rid of rows with no insects
  mutate(time_period = as.factor(time_period), 
         Date_numberic = as.Date(Date, format = "%d%b%Y")- as.Date("2026/06/22"), #column for how late in year
         insect_ID = as.factor(insect_ID),
         insect_ID=recode(insect_ID, 
           "unknown_coleoptera" = "unknown", 
           "unknown_dipteran" = "unknown", 
           "unknown_hoverfly"="unknown", 
           "unkown_hoverfly"="unknown"
         )) %>% #grouping unknowns together
  group_by(time_period, insect_ID) %>% 
  mutate(occurance_in_period = n()) %>% #getting how many times each pollinator occured in each time period
  ungroup() %>% #yeah that ended up with a fucked up graph, not what i was imagining
  mutate(occurance = 1) #this works instead lol 




#basically want to see who shows up where 
(whos_there<- ggplot(plantago_ID, aes(x=time_period, y=occurance, fill=insect_ID))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c(
    "Apis_meliffera" = "#FCEA28",
    "Bombus_terrestris" = "#FCB528",
    "Cheilosia_illustrata"= "black",
    "fly_morphospecies_1" = "#8A620E", 
    "fly_morphospecies_2" = "#8A180E",
    "hemiptera_morphospecies_1"= "#88DB4B",
    "hemiptera_morphospecies_2" = "#F57FED",
    "Melanostoma_scalare" = "#8CD6DB", 
    "Rhagonycha_fulva" = "#FF1414", 
    "Wasp_morphospecies_1" = "#2246E5",
    "Xanthandrus_comtus" = "#26572B", 
    "unknown" = "#8300C7"))+
  labs (y = "Number of Occurances", x = "Time period", fill = "Species")+
  theme_few())
#still need to ID species and stuff and make it less ugly, but is goood enough for right now

ggsave("plots/Pl_pollinator_occurance.png", width = 12, height = 9, dpi = 600)




  














##################
#SCRAPPED SECTIONS 
############################################
#SCRAP THIS WHOLE SECTION!!!!!!
#################################
#literally scrap this entire bit of code cuz i just realised that continuous variables cant be random effects. 

#model with solar power as random effect
abund3<- glmer(pollinators_fm ~ time_period +(1|Solar_power), data = plantago_abundance, family="poisson")

#check assumptions 
simulateResiduals(fittedModel = abund3, plot = T)
#equally as bad as without random effect? 

R2GLMER(abund3)

#reminding myself of what stuff means: 
#conditional R2 = variance explained by the fixed and random effect
#marginal = just fixed effects 
#so???? only 18% is explained by time, and 41% explained by solar radiation? 
#is that right? 

#will try with all the other parameters then 
abund4<- glmer(pollinators_fm ~ time_period +(1|Humidity), data = plantago_abundance, family="poisson")
simulateResiduals(fittedModel = abund4, plot = T)
R2GLMER(abund4)
#18 by time, 59 overall 

abund5<- glmer(pollinators_fm ~time_period + (1|Humidity) + (1|Solar_power), data=plantago_abundance, family = "poisson")
simulateResiduals(fittedModel = abund5, plot = T)
R2GLMER(abund5)
#12 by time, 59 overall what 



#normal linear model, with fixed effects?
abund6<-lmer(pollinators_fm~time_period + (1|Solar_power), data = plantago_abundance)







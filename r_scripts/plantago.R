
###############################
#TO DO NEXT TIME WORKING: 
#sort illuminance 
# make sure i understand exactly how all the assumptions plots and stuff mean. make sure im interpreting everything properly 
# figure out how to do ggpredict model on top of the data points but for categorical 
#does continuous or categorical actually make more sense?
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
         Date_numberic = as.Date(Date, format = "%d%b%Y")- as.Date("2026/06/22")) #column for how late in year


#viasualise change in abundance over time of day 
(abundance_time<-ggplot(plantago_abundance, aes(x=time_period, y=pollinators_flower_minute))+
  geom_boxplot()+
  geom_point(col="blue"))
#lich so ugly :(
#bascially for each time period, how many polllinators we typically had
#deffo have more in the morning, but is this significant???? need anova type thing 



##############################################
#testing if time period has effect on abundance
##############################################



#basic linear model
abund1<- lm(pollinators_flower_minute ~ time_period, data = plantago_abundance)


par(mfrow=c(2,2))
plot(abund1) #asumptions
par(mfrow=c(1,1))
summary(abund1)
#she's scuffed but not as scuffed as i expected???? 
#r sq = 0.154 
# poisson might be the vibe cuz is count data technically 




#adding this in after doing the same thing wayy later on in script for time period as continuous not categorial
predict_ab1<- ggpredict(abund1, c("time_period"))
plot(predict_ab1)

predict_ab1<- predict_ab1 %>% 
  rename(time_period=x, pollinators_flower_minute = predicted) #rename parameters

(predictplot1<- ggplot()+ 
  geom_ribbon(data=predict_ab1, aes(x=time_period, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
  geom_point(data=plantago_abundance, aes(x=time_period, y=pollinators_flower_minute))+ #plot raw data
  geom_line(data=predict_ab1, aes(x=time_period, y=pollinators_flower_minute)))
#WAIT IDK HOW TO DO THIS FOR CATEGORICAL CUZ THE LINE DOESNT WORK OFC 
#HOW DO I EVEN FIGURE OUT HOW TO FIX THIS 
#is this even that important? 


##########################
#poisson model
#########################

abund2<-glm(pollinators_flower_minute ~ time_period, data = plantago_abundance, family="poisson")

summary(abund2)




#check assumptions
simulateResiduals(fittedModel = abund2, plot = T)
#SO SCUFFED!!!!!
#try w/fixed effects or random effects


#get r2 (Approx R2 = (NullDeviance - ResidualDeviance)/NullDeviance )

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
#tbf the model doesnt fit the assumptions of the model very well... could be fake 
#also hannah told us specifically to not use ANOVA and just look at summary instead
#but its difficult to know what to take from there?????



################
##plot all the abiotic factos on abundance, see if any look like they might be important 

#solar power
(abundance_time<-ggplot(plantago_abundance, aes(x=Solar_power, y=pollinators_flower_minute))+
   geom_point())

#humidity
(abundance_time<-ggplot(plantago_abundance, aes(x=Humidity, y=pollinators_flower_minute))+
    geom_point())

#temp sun
(abundance_time<-ggplot(plantago_abundance, aes(x=Temp_sun, y=pollinators_flower_minute))+
    geom_point())
#slight negative trend

#temp shade 
(abundance_time<-ggplot(plantago_abundance, aes(x=Temp_shade, y=pollinators_flower_minute))+
    geom_point())
#also slight negative trend 


#wind max
(abundance_time<-ggplot(plantago_abundance, aes(x=Wind_max, y=pollinators_flower_minute))+
    geom_point())
#maybe??

#average wind
(abundance_time<-ggplot(plantago_abundance, aes(x=Wind_average_every_10, y=pollinators_flower_minute))+
    geom_point())
# wtf is going on here??????

#okay that didnt help much lol lets just test the models



####################################
#Generalised mixed effects linear model
#####################

#model with solar power as random effect
abund3<- glmer(pollinators_flower_minute ~ time_period +(1|Solar_power), data = plantago_abundance, family="poisson")

#check assumptions 
simulateResiduals(fittedModel = abund3, plot = T)
#still crap. is it all the 0s? 

R2GLMER(abund3)

#reminding myself of what stuff means: 
#conditional R2 = variance explained by the fixed and random effect
#marginal = just fixed effects 
#so???? only 18% is explained by time, and 41% explained by solar radiation? 



##################
#daniel was sying like to make the time periods continuous rather than categorical.
#feel like this could be bad cuz are we expecting a simple straightforwards linear regresssion in this context? 
#probably not? but ill try it anyways 
#also going to try car::ANOVA after looking up what it does - remember to do this !!!!

#make column for numeric time period 

plantago_abundance <- plantago_abundance %>% mutate(time_period_numeric = as.numeric(time_period))

abund_num1<-lm(pollinators_flower_minute ~ time_period_numeric, data = plantago_abundance)


#check assumptions 
par(mfrow=c(2,2))
plot(abund_num1) #asumptions
par(mfrow=c(1,1))

summary(abund_num1)
confint(abund_num1)
#my brain still doesnt love that this is technically. need to visualise this

predict_abund_num1<- ggpredict(abund_num1, c("time_period_numeric"))
plot(predict_abund_num1) #plot predicted values 

#i want to see this with the data on top because im not convinced

predict_abund_num1<- predict_abund_num1 %>% 
  rename(time_period_numeric=x, pollinators_flower_minute = predicted) #rename parameters

(modelplot1<- ggplot()+ 
    geom_ribbon(data=predict_abund_num1, aes(x=time_period_numeric, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
                 geom_point(data=plantago_abundance, aes(x=time_period_numeric, y=pollinators_flower_minute))+ #plot raw data
                 geom_line(data=predict_abund_num1, aes(x=time_period_numeric, y=pollinators_flower_minute))) #plotting regression line )

#ik this is just basic linear model and i need to try other stuff + whatever, 
#but im not sure if im convinced by considering them continuous 
#i would if we had recorded constantly all day, and recorded the times each time one visited rather than such big time periods
#like its not really a linear relationship in the same way that an actual conditional variable is (rather than ordinal variable with bigg group in it)
#like a lot can change in 3 hours, its a very wide window


#try this but poisson 
abund_num2<-glm(pollinators_flower_minute ~ time_period_numeric, data = plantago_abundance, family = "poisson")



par(mfrow=c(2,2))
plot(abund_num2) #asumptions
par(mfrow=c(1,1))


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
    scale_y_log10(labels=scales::comma))#log the axis and raw data, and prevent scientific notation on y axis) 
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




  







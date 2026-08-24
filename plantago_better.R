library(tidyverse)
library(readxl)
library(ggthemes)
library(performance)
library(DHARMa)
library(StatisticalModels)
library(ggeffects)
#some in here im not acc using, cant remember which

#load data
plantago_ID<-read_excel("spreadsheets/P_lanceolata.xlsx", sheet = "pollinator_data_raw")
plantago_abundance<- read_excel("spreadsheets/P_lanceolata.xlsx", sheet = "number_of_pollinators")

#sort out spradsheets
plantago_abundance <- plantago_abundance %>% 
  mutate(time_period = as.factor(time_period), 
         Date_numeric = as.Date(Date, format = "%d%b%Y")- as.Date("2026/06/22"), #column for how late in year
         pollinators_fm = pollinators_flower_minute, #fixing the annoyingly long name for pollinator per flower per minute
         time_period_numeric = as.numeric(time_period)) #making time period numeric, so is continuous 


#viasualise change in abundance over time of day 
(abundance_time<-ggplot(plantago_abundance, aes(x=time_period, y=pollinators_fm))+
    geom_boxplot()+
    geom_point(col="#2246E5")+ #unsure if i want datapoints on this
    theme_few()+
    labs(y = "Pollinators per flower per minute", x = "Time period")+
    scale_x_discrete(labels= c("8:00-11:00","11:00-14:00","14:00 -17:00","17:00-20:00")))

ggsave("plots/Pl_daily_pattern.png", width = 9, height = 6, dpi = 600)


#create linear model 
model1<-lm(pollinators_fm~ time_period_numeric, data = plantago_abundance)

#check assumptions 
par(mfrow=c(2,2))
plot(model1) 

summary(model1)
#not significant

#plot model onto data points 
predict1<- ggpredict(model1, c("time_period_numeric"))

plot(predict1) #plot predicted values 

#with the data on top:
predict1<- predict1 %>% 
  rename(time_period_numeric=x, pollinators_fm = predicted) #rename parameters

(modelplot1<- ggplot()+ 
    geom_ribbon(data=predict1, aes(x=time_period_numeric, ymin = conf.low, ymax=conf.high), alpha=0.3)+ #create confidence bands 
    geom_point(data=plantago_abundance, aes(x=time_period_numeric, y=pollinators_fm))+ #plot raw data
    geom_line(data=predict1, aes(x=time_period_numeric, y=pollinators_fm))+#plotting regression line 
    theme_few()+
    labs(y = "Pollinators per flower per minute", x = "Time period")+
    scale_x_discrete(labels= c("8:00-11:00","11:00-14:00","14:00 -17:00","17:00-20:00")))

ggsave("plots/modelplot1.png", width = 9, height = 6, dpi = 600)


#testing linear model including some of the abiotic factors:
model2<-lm(pollinators_fm ~ time_period_numeric + Solar_power, data = plantago_abundance)
plot(model2) 

summary(model2)
#this makes time period now be significant, but solar power isnt, and the whole model isnt overall

model3<-lm(pollinators_fm~ time_period_numeric*Solar_power, data = plantago_abundance)
plot(model3)

summary(model3)
#same as before 

model4<- lm(pollinators_fm ~ time_period_numeric*Humidity, data = plantago_abundance)
plot(model4)

summary(model4)
#all crap 

model5<-lm(pollinators_fm ~ time_period_numeric*Temp_sun, data = plantago_abundance)
plot(model5)

summary(model5)
#not sig 

model6<-lm(pollinators_fm ~ time_period_numeric*Temp_shade, data = plantago_abundance)
plot(model6)

summary(model6)
#not sig

model7<-lm(pollinators_fm ~ time_period_numeric*Illuminance, data = plantago_abundance)
plot(model7)

summary(model7)
#again where itll make time period significant, but isnt significant itself 

model8<-lm(pollinators_fm ~ time_period_numeric*Wind_max, data = plantago_abundance)
plot(model8) 
summary(model8)
#same as above

model9<-lm(pollinators_fm ~ time_period_numeric*Wind_average_every_10, data = plantago_abundance)
plot(model9)
summary(model9)
#same as above

model10<-lm(pollinators_fm ~ time_period_numeric+Solar_power+Temp_shade+Temp_sun+Humidity+Illuminance+Wind_max+Wind_min+Wind_average_every_10, data = plantago_abundance)
plot(model10)
summary(model10)

model11<-lm(pollinators_fm ~ time_period_numeric + Humidity*Solar_power, data = plantago_abundance)
plot(model11)
summary(model11)
###tried a bunch of diff combos there



#so basucally theres no pattern lol. nothing explained nothing 


###########################
#plot for which insects showed up when 
#############################


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
    scale_y_continuous(breaks = c(0,2,4,6,8,10,12))+
    theme_few())
#still need to ID species and stuff and make it less ugly, but is goood enough for right now
#need to fix names of spp. 

ggsave("plots/Pl_pollinator_occurance.png", width = 9, height = 6, dpi = 600)



######################
#other things wanting to look at: did it change according to floral units
(visit_month<-ggplot(plantago_abundance, aes(x=Date_numeric, y=pollinators_fm))+
  geom_point())
#not significant. is there point making graph? 

datemod<- lm(pollinators_fm~ Date_numeric, data = plantago_abundance)
summary(datemod)
#nope

datemod2<- lm(pollinators_fm~ Date_numeric*Illuminance, data = plantago_abundance)
summary(datemod2)
#tried all the options. doesnt rly work ever 
#not significant 








###############
#now adding in floral units

plantago_floral<-read_excel("spreadsheets/P_lanceolata.xlsx", sheet = "floral_units")



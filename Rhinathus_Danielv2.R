library(tidyverse)
library(stringr)

#Load data
RM2 <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor.csv")

#Remove empty rows
RM2 = RM2[-c(159:160),] 

##########################
###                    ###
###    Part 1: GLMs    ###
###                    ###
##########################

### Simplest way to analyze your data using a GLM would be to convert your pollinator data into something numerical, rather than categorical 
### In this case, I'm converting it into just pollinator abundance per time interval

#Convert time into datetime object
RM2$time_24h <-strptime(RM2$Start_time, format = "%H:%M:%S")

#Break up data into time intervals
RM2$interval <- ifelse(RM2$time_24h >= strptime("08:00:00", format = "%H:%M:%S") & RM2$time_24h < strptime("11:00:00", format = "%H:%M:%S"), "Morning", 
                                     ifelse(RM2$time_24h >= strptime("11:00:00", format = "%H:%M:%S") & RM2$time_24h < strptime("14:00:00", format = "%H:%M:%S"), "Afternoon", 
                                            ifelse(RM2$time_24h >= strptime("14:00:00", format = "%H:%M:%S") & RM2$time_24h <= strptime("17:00:00", format = "%H:%M:%S"), "Evening", "Night")))

#Create column for each time interval per day, and also per patch(used for grouping later)
RM2$unique_id <- paste(RM2$Date, RM2$interval, RM2$Meadow, RM2$Patch)

###Calculate average of weather conditions in each row

# Function to convert data
convert_data <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, ">", "")
  x <- str_replace(x, "N/A", "NA")
  x <- as.numeric(x)
  return(x)
}

# Convert data
RM2$Temp_shade_start <- convert_data(RM2$Temp_shade_start)
RM2$Temp_shade_end <- convert_data(RM2$Temp_shade_end)
RM2$Temp_open_start <- convert_data(RM2$Temp_open_start)
RM2$Temp_open_end <- convert_data(RM2$Temp_open_end)
RM2$Wind_speed_start <- convert_data(RM2$Wind_speed_start)
RM2$Wind_speed_end <- convert_data(RM2$Wind_speed_end)
RM2$Humidity_start <- convert_data(RM2$Humidity_start)
RM2$Humidity_end <- convert_data(RM2$Humidity_end)
RM2$Solar_start <- convert_data(RM2$Solar_start)
RM2$Solar_end <- convert_data(RM2$Solar_end)

# Average the columns
RM2 <- RM2 %>%
  mutate(
    Temp_open = ifelse(is.na(Temp_open_start) | is.na(Temp_open_end), NA, (Temp_open_start + Temp_open_end) / 2),
    Temp_shade = ifelse(is.na(Temp_shade_start) | is.na(Temp_shade_end), NA, (Temp_shade_start + Temp_shade_end) / 2),
    Wind_speed = ifelse(is.na(Wind_speed_start) | is.na(Wind_speed_end), NA, (Wind_speed_start + Wind_speed_end) / 2),
    Humidity = ifelse(is.na(Humidity_start) | is.na(Humidity_end), NA, (Humidity_start + Humidity_end) / 2),
    Solar_radiation = ifelse(is.na(Solar_start) | is.na(Solar_end), NA, (Solar_start + Solar_end) / 2),
    Illuminance = ifelse(Illuminance_start == ">20000" | Illuminance_end == ">20000", 20000, 
                         ifelse(as.numeric(Illuminance_start) < as.numeric(Illuminance_end), ##changed this to have illuminance set to 20000, because otherwise it's not usable
                                Illuminance_start, 20000))
  )
#added a convert_data function to handle the conversion of the data
#function replaces any ">" characters and any "N/A" values with NA
#converts the result to numeric



### Find pollinator abundance in each time interval in each day
RM_abun <- RM2 %>%
  filter(Visual_insect_ID != "N/A") %>% 
  group_by(unique_id) %>%
  summarise(
    Pollinator_abundance = n(),
    Temp_open = mean(Temp_open, na.rm = TRUE),
    Temp_shade = mean(Temp_shade, na.rm = TRUE),
    Wind_speed = mean(Wind_speed, na.rm = TRUE),
    Humidity = mean(Humidity, na.rm = TRUE),
    Solar_radiation = mean(Solar_radiation, na.rm = TRUE),
    Illuminance = mean(as.numeric(Illuminance), na.rm = TRUE)
  )

#Split unique_id back into separate columns
RM_abun$Date <- str_split_i(RM_abun$unique_id, " ", 1)

RM_abun$Time <- str_split_i(RM_abun$unique_id, " ", 2)

RM_abun$Meadow <- str_split_i(RM_abun$unique_id, " ", 3)

#Add a column measuring how late in the year it is (in case you want to use it later)
RM_abun$Date_numeric <- as.numeric(as.Date(RM_abun$Date, format = "%d/%m/%Y") - as.Date("2026/06/22"))

#Add a column to convert time intervals into numbers (to reflect that there is progression between them in the GLM)
RM_abun$Time_numeric <- ifelse(RM_abun$Time == "Morning", 1,
                               ifelse(RM_abun$Time == "Afternoon", 2,
                                      ifelse(RM_abun$Time == "Evening", 3, 4)))

##We can plot pollinator abundance based on date
RM_date = RM_abun %>%
  group_by(Date) %>%
  summarise(
    Pollinator_abundance = sum(Pollinator_abundance))

# Time series plot of pollinator activity
ggplot(RM_date, aes(x = as.Date(Date, format = "%d/%m/%Y"), y = Pollinator_abundance)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Time Series of Pollinator Activity", x = "Date", y = "Activity")


### Now you can start building your GLM 
## Good rule of thumb is to start simple and build up, and only include interactions you expect a priori

model1 = glm(Pollinator_abundance ~ Time_numeric + Date_numeric + Meadow, family = poisson(), data = RM_abun) #Using a poisson distribution because it's count data; could also use a negative binomial distribution with glm.nb()
summary(model1)
car::Anova(model1)
#Both these indicate that as time increase (i.e. goes from morning to night) and date increases (goes more into July), the number of pollinators decrease (though only date is significant)
#Also shows that Meadow B has significantly less pollinators than Meadow 8

#Now we can add weather parameters and see what happens
model2 = glm(Pollinator_abundance ~ Time_numeric + Date_numeric +Meadow + Temp_open + Temp_shade +
               Wind_speed + Humidity + Solar_radiation + Illuminance, 
             family = poisson(), data = RM_abun)
summary(model2)
car::Anova(model2)
#The majority of weather parameters seem to have minimal impact on pollinator abundance, only potential exception being Temp_open and Temp_shade 

#Try a model with just date, time and temp_shade
model3 = glm(Pollinator_abundance ~ Time_numeric + Date_numeric + Meadow + Temp_open + Temp_shade, 
             family = poisson(), data = RM_abun) 
summary(model3)
car::Anova(model3)
#Based on this I think we can say that date, meadow and temperature are the biggest factors


### You can also do this for just one species, e.g. Bombus pascuorum
RM_pascuorum <- RM2 %>%
  filter(Visual_insect_ID == "Bombus pascuorum" | 
           Visual_insect_ID == "Bombus pascuorum ") %>% 
  group_by(unique_id) %>%
  summarise(
    Pollinator_abundance = n(),
    Temp_open = mean(Temp_open, na.rm = TRUE),
    Temp_shade = mean(Temp_shade, na.rm = TRUE),
    Wind_speed = mean(Wind_speed, na.rm = TRUE),
    Humidity = mean(Humidity, na.rm = TRUE),
    Solar_radiation = mean(Solar_radiation, na.rm = TRUE),
    Illuminance = mean(as.numeric(Illuminance), na.rm = TRUE)
  )

#You can try it yourself!


##################################
###                            ###
###    Part 2: Flower Units    ###
###                            ###
##################################

###There are numerous ways to quantify the diversity of an ecological community (in your free time, look up Simpsons index or NMDS)
###For the sake of simplicity we will do the quickest way to do it, a total sum of no. of species found

floral_units <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor_floral_units.csv")

#Add column showing number of plant species in each row
floral_units$Diversity <- 16 - rowSums(floral_units[, 5:20] == 0)

#Add in datetime values (to match with earlier dataset)
floral_units$time_24h <-strptime(floral_units$Start_time, format = "%H:%M")

#Break up data into time intervals
floral_units$interval <- ifelse(floral_units$time_24h >= strptime("08:00", format = "%H:%M") & floral_units$time_24h < strptime("11:00", format = "%H:%M"), "Morning", 
                       ifelse(floral_units$time_24h >= strptime("11:00", format = "%H:%M") & floral_units$time_24h < strptime("14:00", format = "%H:%M"), "Afternoon", 
                              ifelse(floral_units$time_24h >= strptime("14:00", format = "%H:%M") & floral_units$time_24h <= strptime("17:00", format = "%H:%M"), "Evening", "Night")))

#Make a unique id to match with earlier dataset
floral_units$unique_id <- paste(floral_units$Date, floral_units$interval, floral_units$Meadow, floral_units$Patch)

#Merge datasets
merged_data = merge(floral_units, RM_abun, by="unique_id")

#Plot floral diversity against pollinator abundance
ggplot(merged_data, aes(x=Pollinator_abundance, y=Diversity))+
  geom_point()

##Hmm no strong relationship it seems



##################################
###                            ###
###   Part 3: New Dataframe    ###
###                            ###
##################################

library(tidyverse)

#Load main dataset
RM_main <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor.csv")
#Remove empty rows
RM_main = RM_main[-c(159:160),] 

### Will need to do all the cleanup and averaging of weather conditions first
# Function to convert data
convert_data <- function(x) {
  x <- as.character(x)
  x <- str_replace(x, ">", "")
  x <- str_replace(x, "N/A", "NA")
  x <- as.numeric(x)
  return(x)
}

# Convert data
RM_main$Temp_shade_start <- convert_data(RM_main$Temp_shade_start)
RM_main$Temp_shade_end <- convert_data(RM_main$Temp_shade_end)
RM_main$Temp_open_start <- convert_data(RM_main$Temp_open_start)
RM_main$Temp_open_end <- convert_data(RM_main$Temp_open_end)
RM_main$Wind_speed_start <- convert_data(RM_main$Wind_speed_start)
RM_main$Wind_speed_end <- convert_data(RM_main$Wind_speed_end)
RM_main$Humidity_start <- convert_data(RM_main$Humidity_start)
RM_main$Humidity_end <- convert_data(RM_main$Humidity_end)
RM_main$Solar_start <- convert_data(RM_main$Solar_start)
RM_main$Solar_end <- convert_data(RM_main$Solar_end)

# Average the columns
RM_main <- RM_main %>%
  mutate(
    Temp_open = ifelse(is.na(Temp_open_start) | is.na(Temp_open_end), NA, (Temp_open_start + Temp_open_end) / 2),
    Temp_shade = ifelse(is.na(Temp_shade_start) | is.na(Temp_shade_end), NA, (Temp_shade_start + Temp_shade_end) / 2),
    Wind_speed = ifelse(is.na(Wind_speed_start) | is.na(Wind_speed_end), NA, (Wind_speed_start + Wind_speed_end) / 2),
    Humidity = ifelse(is.na(Humidity_start) | is.na(Humidity_end), NA, (Humidity_start + Humidity_end) / 2),
    Solar_radiation = ifelse(is.na(Solar_start) | is.na(Solar_end), NA, (Solar_start + Solar_end) / 2),
    Illuminance = ifelse(Illuminance_start == ">20000" | Illuminance_end == ">20000", 20000, 
                         ifelse(as.numeric(Illuminance_start) < as.numeric(Illuminance_end), ##changed this to have illuminance set to 20000, because otherwise it's not usable
                                Illuminance_start, 20000))
  )

#Load floral unit data
RM_floral <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor_floral_units.csv")

### Now we need to make the unique identifier code to match the two datasets

#Convert time into datetime object
RM_main$time_24h <-strptime(RM_main$Start_time, format = "%H:%M:%S")

#Break up data into time intervals
RM_main$interval <- ifelse(RM_main$time_24h >= strptime("08:00:00", format = "%H:%M:%S") & RM_main$time_24h < strptime("11:00:00", format = "%H:%M:%S"), "Morning", 
                       ifelse(RM_main$time_24h >= strptime("11:00:00", format = "%H:%M:%S") & RM_main$time_24h < strptime("14:00:00", format = "%H:%M:%S"), "Afternoon", 
                              ifelse(RM_main$time_24h >= strptime("14:00:00", format = "%H:%M:%S") & RM_main$time_24h <= strptime("17:00:00", format = "%H:%M:%S"), "Evening", "Night")))

#Create column for unique ID
RM_main$unique_id <- paste(RM_main$Date, RM_main$interval, RM_main$Meadow, RM_main$Patch)


## Now do the same for RM_floral
#Add in datetime values (to match with earlier dataset)
RM_floral$time_24h <-strptime(RM_floral$Start_time, format = "%H:%M")

#Break up data into time intervals
RM_floral$interval <- ifelse(RM_floral$time_24h >= strptime("08:00", format = "%H:%M") & RM_floral$time_24h < strptime("11:00", format = "%H:%M"), "Morning", 
                                ifelse(RM_floral$time_24h >= strptime("11:00", format = "%H:%M") & RM_floral$time_24h < strptime("14:00", format = "%H:%M"), "Afternoon", 
                                       ifelse(RM_floral$time_24h >= strptime("14:00", format = "%H:%M") & RM_floral$time_24h <= strptime("17:00", format = "%H:%M"), "Evening", "Night")))

#Make a unique id to match with earlier dataset
RM_floral$unique_id <- paste(RM_floral$Date, RM_floral$interval, RM_floral$Meadow, RM_floral$Patch)

###Merge datasets
merged_data = merge(floral_units[c("unique_id", "Rhinanthus_minor","Diversity")], RM_main, by="unique_id") 
#You could merge both full datasets, but this just selects the columns we want from RM_floral

#Check for any spelling errors in Visual_insect_ID (because that's what we're gonna use to sort things together)
unique(merged_data$Visual_insect_ID)

#Fix errors
merged_data$Visual_insect_ID <- gsub("Bombus pascuorum ", "Bombus pascuorum", merged_data$Visual_insect_ID)
merged_data$Visual_insect_ID <- gsub("Bombus lapidarius ", "Bombus lapidarius", merged_data$Visual_insect_ID)
merged_data$Visual_insect_ID <- gsub("Bombus terrestris/leucorum ", "Bombus terrestris/leucorum", merged_data$Visual_insect_ID)
merged_data$Visual_insect_ID <- gsub("Bombus terrestris leucorum ", "Bombus terrestris/leucorum", merged_data$Visual_insect_ID)
merged_data$Visual_insect_ID <- gsub("Pipiza spp.", "Syrphidae", merged_data$Visual_insect_ID) #In this case it doesn't really make sense to single out Pipiza

#Check again
unique(merged_data$Visual_insect_ID)

#Now group by unique_ID and pollinator species
RM_rate <- merged_data %>%
  filter(Visual_insect_ID != "N/A") %>% 
  group_by(unique_id) %>%
  summarise(
    Abundance = n(),
    Floral_units = mean(Rhinanthus_minor),
    Temp_open = mean(Temp_open, na.rm = TRUE),
    Temp_shade = mean(Temp_shade, na.rm = TRUE),
    Wind_speed = mean(Wind_speed, na.rm = TRUE),
    Humidity = mean(Humidity, na.rm = TRUE),
    Solar_radiation = mean(Solar_radiation, na.rm = TRUE),
    Illuminance = mean(as.numeric(Illuminance), na.rm = TRUE),
    Diversity = mean(Diversity)
  )

#Now generate rate of pollinator visitation, per pollinator per flower per minute
RM_rate$Rate = RM_rate$Abundance/RM_rate$Floral_units/30 #30 minute observation periods so divide by 30

#We can now split up unique_ID to get back time, date, meadow and patch
RM_rate$Date <- as.Date(str_split_i(RM_rate$unique_id, " ", 1), format = "%d/%m/%Y")
RM_rate$Time <- str_split_i(RM_rate$unique_id, " ", 2)

RM_rate$Meadow <- str_split_i(RM_rate$unique_id, " ", 3)

#At this point you can export it
write.csv(RM_rate, "C:/Users/grace/Downloads/Rhinanthus_pollinator_rate.csv", row.names = FALSE)


#If you want to use Date and Time as variables it's best to convert them into 
RM_rate$Date_numeric <- as.numeric(RM_rate$Date - as.Date("2026/06/22"))

RM_rate$Time_numeric <- ifelse(RM_rate$Time == "Morning", 1,
                               ifelse(RM_rate$Time == "Afternoon", 2,
                                      ifelse(RM_rate$Time == "Evening", 3, 4)))

#The GLM will probably be unhappy with the really small values of Rate, so log it 
RM_rate$Log_rate <- log(RM_rate$Rate)

##Cool now you're ready to test for which factors affect pollinator visitation rate!

##Really simple ones to try out

model_r1 <- glm(Log_rate ~ Time_numeric + Date_numeric, 
                family=gaussian(), data=RM_rate ) 
summary(model_r1)
car::Anova(model_r1)
drop1(model_r1, test='Chisq')
#we're using a Gaussian here because in theory after converting to rate it's no longer count data and thus no longer poisson
#And also in any case poisson doesn't allow for negative values

#without visual insect ID
model_r2 <- glm(Log_rate ~ Time_numeric + Date_numeric + Meadow, 
                family=gaussian(), data=RM_rate ) 
summary(model_r2)
anova(model_r1, model_r2)

#with visual insect ID
model_r3 <- glm(Log_rate ~ Time_numeric + Date_numeric + Meadow + Visual_insect_ID,
                family=gaussian(), data=RM_rate)
summary(model_r3)

#with ALL weather components
model_r4 <- glm(Log_rate ~ Time_numeric + Date_numeric + Meadow + Visual_insect_ID +
                  Temp_open + Temp_shade + Humidity + Wind_speed + Illuminance + Solar_radiation,
                family=gaussian(), data=RM_rate)
summary(model_r4)
drop1(model_r4, test="Chisq")

#without visual insect id, without weather components besides temperature
model_r5 <- glm(Log_rate ~ Time_numeric + Date_numeric + Meadow +
                  Temp_open + Temp_shade,
                family=gaussian(), data=RM_rate)
summary(model_r5)

#fixing bombus pascuorum activity time interval plot

#from barplot to boxplot (with error bars)

# Filter the data for Bombus pascuorum
Rhinanthus_minor_pascuorum <- RM_rate %>%
  filter(Visual_insect_ID == "Bombus pascuorum")

# Count the number of visits in each time interval
visit_counts <- Rhinanthus_minor_pascuorum %>%
  group_by(time_interval) %>%
  summarise(count = n())

# Create a box plot
ggplot(Rhinanthus_minor_pascuorum, aes(x = Time, y = Rate)) + 
  geom_boxplot(fill = c("#FFFF99", "#FFFF66", "#FFFF33", "#FFFF00"), color = "black") + 
  scale_x_discrete(labels = c("08:00-11:00", "11:00-14:00", "14:00-17:00", "17:00-20:00")) +
  labs(x = "Time Interval", y = "Rate of B. pascuorum visitation") +
  theme_classic() +
  geom_jitter()

#certifying no relationship between floral diversity and pollinator rate
model_r6 = model_r5 <- glm(Log_rate ~ Time_numeric + Date_numeric + Meadow + Diversity +
                             Temp_open + Temp_shade,
                           family=gaussian(), data=RM_rate)

summary(model_r6)

#coloncolon used to specify function from specific package, i.e. car :: anova to use the 'anova' function from the 'car' package

car::Anova(model_r6)

#plot to show variation between meadows
ggplot(data = RM_rate,  aes(x=Meadow, y=Rate))+
  geom_boxplot(fill="yellow")+
  geom_jitter()

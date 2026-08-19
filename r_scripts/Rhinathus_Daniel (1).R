library(tidyverse)
library(stringr)

#Load data
RM2 <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor.csv")

#Remove empty rows
RM2 = RM2[-c(159:160),] #To remove the previous B-east traps

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

#model incorporating curvilinear response to temperature
#fitting an x^2 term to do so
model3 = glm(Pollinator_abundance ~ Date_numeric + Meadow + I(Temp_open^2), 
             family = poisson(), data = RM_abun) 
summary(model3)
car::Anova(model3)
#numbering models to compare them, hence anova1, anova2, anova3
# Create a line plot
RM_abun %>% 
  ggplot(aes(x = Date_numeric, y = Pollinator_abundance, group = 1)) +
  geom_line() +
  labs(title = "Pollinator Abundance Over Time", 
       x = "Date", 
       y = "Pollinator Abundance")

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

floral_units <- read.csv("Rhinanthus_minor_floral_units.csv")

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


#all the code i used to show off my internship data!
#first , i wanted to see what pollinators had visited yellow rattle
#so i made a histogram plotting total visits by the species against the species diversity

# Load the necessary libraries
library(dplyr)
library(ggplot2)

# Load your data
Rhinanthus_minor <- read.csv("C:/Users/grace/Downloads/Rhinanthus_minor.csv")

# Standardize the species names and remove rows with N/A or blank species
Rhinanthus_minor <- Rhinanthus_minor %>%
  mutate(Visual_insect_ID = trimws(as.character(Visual_insect_ID))) %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID == "Bombus pascuorum ", "Bombus pascuorum", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(grepl("Bombus terrestris", Visual_insect_ID), "Bombus terrestris/leucorum", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID == "Bombus lapidarius", "Bombus lapidarius", Visual_insect_ID)) %>%
  filter(Visual_insect_ID != "N/A" & Visual_insect_ID != "")

# Count the number of occurrences of each species
species_counts <- Rhinanthus_minor %>%
  group_by(Visual_insect_ID) %>%
  summarise(count = n())

# using the commands ifelse and mutate to combine counts for Bombus lapidarius, Bombus pascuorum, and Bombus terrestris/leucorum
species_counts <- species_counts %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID == "Bombus lapidarius" |
                                     Visual_insect_ID == "Bombus lapidarius ", 
                                   "Bombus lapidarius", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID == "Bombus pascuorum" | 
                                     Visual_insect_ID == "Bombus pascuorum ", 
                                   "Bombus pascuorum", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(grepl("Bombus terrestris", Visual_insect_ID) | 
                                     Visual_insect_ID == "Bombus terrestris/leucorum", 
                                   "Bombus terrestris/leucorum", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID %in% c("Syrphidae", "Pipiza spp."), 
                                   "Syrphidae", Visual_insect_ID)) %>%
  mutate(Visual_insect_ID = ifelse(Visual_insect_ID == "Diptera", 
                                   "Non-Syrphid Diptera", Visual_insect_ID)) %>%
  group_by(Visual_insect_ID) %>%
  summarise(count = sum(count))

# Plot the result
ggplot(species_counts, aes(x = Visual_insect_ID, y = count, fill = Visual_insect_ID)) + 
  geom_col(color = "black", size = 0.5) + 
  scale_fill_manual(values = c(
    "Bombus lapidarius" = "red",
    "Bombus pascuorum" = "#FFD700",  
    "Bombus terrestris/leucorum" = "#FFFF00",  
    "Non-Syrphid Diptera" = "black",
    "Syrphidae" = "#AAAAAA",
    "Coleoptera" = "#AAAAAA"
  ), guide = "none") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), 
        axis.ticks.x = element_blank()) +
  labs(x = "Species", y = "Total number of pollinator visits") +
  theme(axis.line.x = element_blank(), 
        axis.title.x = element_text(margin = margin(t = 10)))



#bar plot showing the visitation by bombus 
#initially had wanted to use a box and whisker plot
#given each interval only had a single count, did a bar chart instead

# Load the necessary libraries
library(dplyr)
library(ggplot2)
library(stringr)

# Extract the hour of the day from the Time_of_observation column
Rhinanthus_minor$time_of_day <- sapply(strsplit(as.character(Rhinanthus_minor$Time_of_observation), ":"), function(x) as.integer(x[1]))

# Assign a time interval to each visit
Rhinanthus_minor$time_interval <- ifelse(Rhinanthus_minor$time_of_day >= 8 & Rhinanthus_minor$time_of_day < 11, "1",
                                         ifelse(Rhinanthus_minor$time_of_day >= 11 & Rhinanthus_minor$time_of_day < 14, "2", 
                                                ifelse(Rhinanthus_minor$time_of_day >= 14 & Rhinanthus_minor$time_of_day < 17, "3", "4")))

# Filter the data for Bombus pascuorum
Rhinanthus_minor_pascuorum <- Rhinanthus_minor %>%
  filter(Visual_insect_ID == "Bombus pascuorum")

# Count the number of visits in each time interval
visit_counts <- Rhinanthus_minor_pascuorum %>%
  group_by(time_interval) %>%
  summarise(count = n())

# Create a bar plot
ggplot(visit_counts, aes(x = time, y = rate)) + 
  geom_boxplot(fill = c("#FFFF99", "#FFFF66", "#FFFF33", "#FFFF00"), color = "black") + 
  scale_x_discrete(labels = c("08:00-11:00", "11:00-14:00", "14:00-17:00", "17:00-20:00")) +
  labs(x = "Time Interval", y = "Total number of B. pascuorum visits") +
  theme_classic()


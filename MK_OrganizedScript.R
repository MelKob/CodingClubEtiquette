# Making a nity script:

# This is a way of making an outline

# General structure ----

# Introduction: 
  #este script serve para?; 
  #autores; 
  #contactos e data

# Packages
# Load data
# Inspect data
# Format data for analysis
# Data manipulation
# Modelling populaiton change over time
# Visualising model outputs


#### Exemplo ####
# Analysing vertebrate population change based on the Living Planet Index
# Data available from http://www.livingplanetindex.org/

# Gergana Daskalova ourcodingclub@gmail.com
# 25-04-2017

# Packages ----
library(tidyr)  # Formatting data for analysis
library(dplyr)  # Manipulating data
library(ggplot2)  # Visualising results
library(readr)  # Manipulating data

# Defining functions ----
# A custom ggplot2 function
theme.LPI <- function(){
  theme_bw()+
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size=12, face="italic"),          
          legend.title = element_blank(),                              
          legend.position=c(0.9, 0.9))
}

# Import data ----
LPI <- read.csv("LPIdata_CC.csv")

# Formatting data ----
LPI2 <- gather(LPI, "year", "abundance", 9:53)  # Transforming the data from wide to long format, some blank cells may disappear
# gather function requires tidyr package
LPI2$year <- parse_number(LPI2$year)  # Do you see awkward Xs before all the years? This gets rid of them.
names(LPI2)  # Check what the different variables are called
names(LPI2) <- tolower(names(LPI2))  # Make all variable names lower case

# When manipulating data it's always good check if the variables have stayed how we want them
# Use the str() function
str(LPI2)

# Abundance is a character variable, when it should be numeric, let's fix that
LPI2$abundance <- as.numeric(LPI2$abundance)

# Calc summary stats for each biome in the LPI database ----
levels(LPI2$biome)  # list all biomes

LPI_biome_summ <- LPI2 %>%  # use of pipe operator
  group_by(biome) %>%  # Group by biome
  summarise(populations = n())  # Create columns, number of populations

# Visualising the number of populations in each biome with ggplot2 package ---- 
(barplot <- ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +  
   theme.LPI() +                     # Use of personal theme function
   ylab("Number of populations") +
   xlab("Biome") +
   theme(legend.position = "none"))  # Removal of legend for simplicity

# Saving plots ----
png(file="img/biome_pop.png", width = 1000, height = 2000)  # Note that png() uses pixel values for width and height
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()  # This tells R you are done with the plotting and it can save the file

pdf(file="img/biome_pop.pdf",  width = 13.33, height = 26.66)  # pdf() uses inches
ggplot(LPI_biome_summ, aes(biome, color = biome, y = populations)) + geom_bar(stat = "identity") +
  theme.LPI() +
  ylab("Number of populations") +
  xlab("Biome") +
  theme(legend.position = "none")
dev.off()

# Other coding etiquettes ----

# Object, variable (e.g.object$variable) and function names should be lowercase. 

# MinPrecip_august is confusing to remember, min.precip.aug is a bit long, but informative and easier to type.

# Variables should be nouns: abundance; richness
# and separated by _ : species_abundances

# Functions should be verbs: calc.sp.richness
# and separated by .

# Sound like too many things to remember?
# But this way it is clear what is as object, and what is an external file

# Examples:
# Object names
avg_clicks  # Good.
avg.clicks  # Acceptable.
avg_Clicks  # Not okay.

# Function names
calculate.avg.clicks  # This is what we are aiming for.
CalculateAvgClicks  # Not that bad, but mixing capital and lowercase letters can lead to typos
calculate_avg_clicks , calculateAvgClicks  # Bad. The convention is that functions are defined using dots, not underscores.

# Replacing ----
names(dataframe) <- gsub(".", "_", names(dataframe), fixed = TRUE)
# This code takes all of the variable names in the imaginary dataset `dataframe` and replaces `.` with `_`
# Depending on the naming style you are using, you might want to go the other way around and use `.` in all variable names

names(dataframe) <- tolower(names(dataframe))
# This code makes all of the variable names in the imaginary dataset lowercase

colnames(dataframe)[colnames(dataframe) == 'Old_Complicated_Name'] <- 'new.simple.name'
# Renaming an individual column in the imaginary dataset
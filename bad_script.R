#
# Creating plots on LPI population frequency by biome
# LPI: Living Planet Index
#
# Authors: Coding Club
# Edited by Melanie Köbel
#
# 2020-03-26 (last edit)
#

# Libraries ----
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)

# Functions ----
theme.for.lpi <- function()
{
  theme_bw() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),
          axis.title.y = element_text(size = 14, face = "plain"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size=20, vjust=1, hjust=0.5),
          legend.text = element_text(size = 12, face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9))}


# Load data ----
getwd()
LPI <- read.csv("LPIdata_CC.csv") # remember to put meaninfull names to objects


# Clean data ----
# long format, so abundance record are in one column:
LPI_long <- gather(data, "year", "abundance", 9:53)
#this function gathers values of columns and puts them into one variable, here var "abundance". The column names will go to "year".

# forces year to be numeric
LPI_long$year <- parse_number(LPI_long$year)

# all variables lowercase
names(LPI_long)
names(LPI_long) <- tolower(names(LPI_long))

# forces abundance to be numeric
LPI_long$abundance <- as.numeric(LPI_long$abundance)


# Summary dataframe of population n per biome ----
LPI_biomes_summ <- LPI_long %>% 
  group_by(biome) %>% 
  summarise(num_pop = n()) 
# creates new dataframe where each line is a "biome", and creates new column called "num_pop" with number of observations per "biome" in LPI_long

# Check data frame
LPI_biomes_summ[1:5, 1:2]


# Plot population frequency by biome ----

# bar plot
type = "bar"
biome_barplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type=="bar") geom_bar() else geom_point(stat="count")
  } +
	theme.for.lpi() + 
  ylab("Number of populations") + 
  xlab("Biome") +
	theme(legend.position = "none")

# dot plot
type = "point"
biome_dotplot <- ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "point") geom_bar() else geom_point(stat = "count")
  } +
	theme.for.lpi() + 
  ylab("Number of populations") + 
  xlab("Biome") +
	theme(legend.position = "none")

# compare plots
biome_barplot
biome_dotplot

#save bar plot, it looks better
type = "bar"
pdf(file="Img/plot1.pdf",  width = 13.33, height = 26.66)
ggplot(LPI_long, aes(biome, color = biome)) + {
  if(type == "bar") geom_bar() else geom_point(stat = "count")
  } +
	theme.for.lpi() + 
  ylab("Number of populations") + 
  xlab("Biome") +
	theme(legend.position = "none") 
dev.off()

## The End :)
#############################################
# WaDE-SFA Synoptic Sampling Data Processing
# Summaries per plot
#
# Matthew Berens
# Apr 2023
#############################################

#Load libraries and set directory
library(lubridate)
library(ggplot2)
library(plyr)
library(magrittr)
library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(sf)

setwd("~/Documents/WaDE-SFA")


#List of sites, samples, and field chemistry data
sample_data <- read.csv("data/processed/SYNOPTIC_2023-06-23.csv")  %>% 
  arrange(factor(network, levels = c("N Boundary", "Montana", "McArthur", "Willow Brook", "Bissell", "EFK")))

theme_er1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "right",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1, 'lines'),
          legend.spacing.y = unit(0.5, 'cm'),
          panel.border = element_rect(color="black",size=1, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          axis.ticks.length.x = unit(-0.2, "cm"),
          
          # formatting for facets
          panel.background = element_blank(),
          panel.grid.major = element_line(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          #panel.spacing.y = unit(1, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

#Data Visualization
##############################################
labels <- c(pH = "pH", SpC = "Conductivity (uS/cm)", temp = "Temperature (C)", TOC = "DOC (mg/L) ")

sample_data %>%
  subset(parameter == "pH") %>%
  subset(result_value != "NA")%>%
  ggplot() +
  geom_point(aes(x = developed, y = result_value, fill = network), shape = 21, size = 5) +
  geom_smooth(aes(x = developed, y = result_value), method = "lm") +
  #scale_y_continuous(oob = rescale_none) +
  facet_wrap(~parameter, ncol = 1, scales = "free", labeller = as_labeller(labels)) +
  theme_er1() 
  #theme(axis.text.x = element_text(size = 10,angle =90, vjust=0.5),
   #     axis.title = element_blank()) 

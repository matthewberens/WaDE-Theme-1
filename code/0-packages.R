#Functions and Packages
#WaDE SFA
#Matthew J Berens

# packages
library(tidyverse)
library(readr)
library(purrr)
library(stats)
library(cowplot)
library(lubridate)
library(cowplot)
library(knitr)
library(reshape2)
library(ggExtra)
library(stringi)
library(sf)
library(magrittr)
library(ggnewscale)
library(fs)
library(exactextractr)
library(bayestestR)


# functions 
theme_mb1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(text = element_text(family = "Trebuchet MS"),
          legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.box.background = element_blank(),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",linewidth =1, fill = NA),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_blank(), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=10, face="bold"), #facet labels
          strip.text.y = element_text(size=10, face="bold", angle = 270) #facet labels
    )
}


#Functions and Packages
#WaDE SFA
#Matthew J Berens

# packages
library(tidyverse)
library(ggthemes)
library(stats)
library(cowplot)
library(janitor)
library(lubridate)     # 1.6.0
library(cowplot)
library(knitr)
library(reshape2)
library(ggExtra)
library(stringi)
library(sf)
library(raster)
library(nhdplusTools)
library(FedData)
library(magrittr)
library(mapview)
library(ggnewscale)
mapviewOptions(
  basemaps = c("CartoDB.Positron", "OpenTopoMap", "Esri.WorldImagery", "Esri.WorldShadedRelief"),
  homebutton = FALSE,
  query.position = "topright",
  query.digits = 2,
  query.prefix = "",
  legend.pos = "bottomright"
)

library(exactextractr)
library(leafsync)


# functions 
theme_mb1 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          panel.grid = element_blank(),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
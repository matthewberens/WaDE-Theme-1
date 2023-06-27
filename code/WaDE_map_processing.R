#MJBERENS
#January 20 2023

library(sf)
library(terra)
library(dplyr)
library(rgdal)
library(riverdist)
library(raster)
library(ggspatial)
library(ggplot2)

setwd("~/Documents/Wade-SFA")

###### LOAD BOUNDARIES #############
sf_use_s2(FALSE)
EFPC_outline <- read_sf("mapping/raw_data/EFPC Watershed.shp")
Catchments_all <- read_sf("mapping/raw_data/Catchments_all.shp")
EFPC_catchments <- st_intersection(Catchments_all, EFPC_outline)
EFPC_LandUse <- read.csv("mapping/raw_data/EFPC_LandUseCalcs.csv")
EFPC_flowlines <- read_sf("mapping/raw_data/EFPC Flowlines All.shp") %>%
  st_zm()

map_LandUse <- merge(EFPC_catchments, EFPC_LandUse, by = "OBJECTID") %>%
  st_transform(4326) %>%
  mutate(developed = developed_0_19 + developed_20_49 + developed_50_79 + developed_80_100,
         forested = deciduous_forest + evergreen_forest + mixed_forest,
         wetland = herbaceous_wetland + woody_wetland,
         grassy = pasture_hay + shrub_scrub + pasture_hay + grass_herbaceous)

#########################################
#Import Synoptic Sampling Data.
synoptic_data <- read.csv("data/processed/SYNOPTIC_2023-06-23.csv")

#Convert synoptic coordinates to sf points
data_geoformatted <- 
  synoptic_data %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

#################################################################
ggplot() +
  
  geom_sf(data = EFPC_catchments, fill = NA, col = "black", linetype = "dotted") +
  #geom_sf(data = map_LandUse, aes(fill = developed)) +
  geom_sf(data = EFPC_flowlines, color = "blue", linetype = "solid", size =5) +
  geom_sf(data = subset(data_geoformatted, parameter == "TOC" & !is.na(sample_id)),
                        aes(fill = result_value), shape = 21, size = 6) +
  #geom_sf(data = data_geoformatted, aes(fill = flow_status), shape = 23, size = 4) +
  geom_sf(data = EFPC_outline, fill = NA, col = "#0E4C92", linewidth = 2) +
  #scale_fill_manual(values = c("red", "lightgreen", "#F4CA4E", "black")) +
  #scale_fill_gradientn(colours = terrain.colors(10), trans = 'reverse') +
  scale_fill_distiller(palette  = "RdYlBu", direction = -1) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey20", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "black")) +
  labs(fill = "DOC (mg/L)")
#########################################



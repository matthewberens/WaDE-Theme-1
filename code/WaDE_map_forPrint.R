#############################################
# WaDE-SFA Synoptic Maps for Printing
# Matthew Berens
# Updated June 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Geospatial Data -------------------------------------------------------------
EFPC_HUC <- st_set_crs(get_huc(id = "060102070302", buffer = 0.5, type = "huc12"), 4326) #EFPC Boundary
EFPC_catchments_landuse <- st_set_crs(read_sf("GIS_data/EFPC_catchments_landuse.gpkg"), 4326) #EFPC Catchment polygons
EFPC_fl <- st_set_crs(read_sf("GIS_data/EFPC Flowlines.shp"), 4326) #EFPC NHD flowlines
EFPC_main <- st_set_crs(read_sf("GIS_data/East Fork Poplar Creek.shp"), 4326) #EFPC main branch

#Synoptic sampling locations - set correct crs
WaDE_sites <- read.csv("raw/WaDE SYNOPTIC_locations.csv") %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

# Step 2. Load Synoptic Data, if desired ----------------------------------------------------
#synoptic_data <- read.csv("processed/SYNOPTIC_2023-06-23.csv")



#Merge the sampling locations with the synoptic resuts.
merged_siteData <- merge(WaDE_sites, synoptic_data, by = "site_name")




# Step 3. Create map of locations -----------------------------------------------------------
#If you want to save the map, uncomment Line 29 and give the map a name.
#map_name <- 
  
ggplot() +

#Begin with the EFPC watershed outline 
  geom_sf(data = EFPC_HUC, fill = NA, col = "#0E4C92", linewidth = 2) +
  
#-------------------
#For lines 42-59, select the 2-line pair that corresponds to the land use that you want to show.  
 
#Use this line for no fill
  geom_sf(data = EFPC_catchments_landuse, fill = NA, col = "black", linetype = "dotted") +
  
#Use these two lines for forested land cover
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Forest), col = "black", linetype = "dotted") +
  #scale_fill_gradientn(colours = terrain.colors(50), trans = 'reverse') +

#Use these two lines for developed land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Developed), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Reds", direction = 1) +
  
#Use these two lines for wetland land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Wetland), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Blues", direction = 1) +
  
#Use these two lines for herbaceous land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Herbaceous), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Oranges", direction = 1) +

#--------------------
#Add the EFPC streamlines
  geom_sf(data = st_zm(EFPC_main), color = "blue", linewidth = 1) +
  geom_sf(data = st_zm(EFPC_fl), color = "blue") +
#--------------------

#Start new color scale and add the sampling locations 
  new_scale_fill() +
  
#Run line 71 to just plot sampling locations
  #geom_sf(data = WaDE_sites, aes(fill = network) , shape = 21, size = 4) +

#Run line 74-75 to show sampling results. Change "parameter" to the paramater of interest. 
  geom_sf(data = merged_siteData %>% subset(parameter == "pH"), aes(fill = result_value) , shape = 21, size = 4) +
  scale_fill_distiller(palette  = "RdYlBu", direction = -1) +
  
  labs(fill = "pH") + #Name the legend for sampling location fill colors
  
#Add map annotations, scales, and legends 
  theme_classic() +
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey20", "white")) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("grey40", "white"),
      line_col = "black"))

# Step 4. Save the map -----------------------------------------------------------
#If you want to output the map, uncomment Line 67 and replace "map_name" with the name from Line 29.
#ggsave("output/map_name.png", map = map_name, width = 12, height = 9, units = "in")
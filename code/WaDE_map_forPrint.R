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
EFPC_LU_catchments_binned <- st_set_crs(read_sf("GIS_data/EFPC_LU_catchments_binned.gpkg"), 4326)
geology <- st_set_crs(read_sf("GIS_data/TN/TN_geol_poly.shp"), 4326)
geology_crop <- st_intersection(geology, EFPC_HUC) 
geology_units <- read.csv("GIS_data/TN/TN_units.csv")
geology_merge <- merge(geology_crop, geology_units, by = "UNIT_LINK")

#Synoptic sampling locations - set correct crs
WaDE_sites <- read.csv("raw/WaDE SYNOPTIC_SITES.csv") %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

# Step 2. Load Synoptic Data, if desired ----------------------------------------------------
JUNsynoptic_data <- read.csv("processed/SYNOPTIC_2023-06-23.csv")
APRsynoptic_data <- read.csv("processed/SYNOPTIC_2023-04-12.csv")


#Merge the sampling locations with the synoptic resuts.
JUNmerged_siteData <- merge(WaDE_sites, JUNsynoptic_data, by = "site_name")
APRmerged_siteData <- merge(WaDE_sites, APRsynoptic_data, by = "site_name")

bind_data <- rbind(JUNmerged_siteData, APRmerged_siteData)

# Step 3. Create map of locations -----------------------------------------------------------
#If you want to save the map, uncomment Line 29 and give the map a name.
#April2023_flowstatus <- 
  
ggplot() +

#Begin with the EFPC watershed outline 
  geom_sf(data = EFPC_HUC, fill = NA, col = "#0E4C92", linewidth = 2) +
  new_scale_fill() +
  
#-------------------
#For lines 42-59, select the 2-line pair that corresponds to the land use that you want to show.  
 
#Use this line for no fill
  #geom_sf(data = EFPC_catchments_landuse, fill = NA, col = "black", linetype = "dotted") +
  
#Use these two lines for forested land cover
  geom_sf(data = EFPC_catchments_landuse, aes(fill = Forest), col = "black", linetype = "dotted") +
  scale_fill_gradientn(colours = terrain.colors(50), trans = 'reverse') +
  #scale_fill_distiller(palette  = "Greens", direction = 1) +
  
#Use these two lines for developed land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Developed), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Reds", direction = 1) +
  #scale_fill_gradient(low = "#4A9E48", high = "red") +
  
#Use these two lines for wetland land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Wetland), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Blues", direction = 1) +
  
#Use these two lines for wetland land cover 
  #geom_sf(data = EFPC_LU_catchments_binned %>% subset(site_name %in% c("BSL3.1")), aes(fill = site_name), col = NA, linetype = "dotted") +
  
#Use these two lines for herbaceous land cover 
  #geom_sf(data = EFPC_catchments_landuse, aes(fill = Herbaceous), col = "black", linetype = "dotted") +
  #scale_fill_distiller(palette  = "Oranges", direction = 1) +

#Use these two lines for geology
  #geom_sf(data = geology_merge, aes(fill = map_sym1), col = "black", linewidth = 0.5) +
  #scale_fill_viridis_d() +

#--------------------
#Add the EFPC streamlines
  geom_sf(data = st_zm(EFPC_main), color = "blue", linewidth = 1) +
  geom_sf(data = st_zm(EFPC_fl), color = "blue") +
#--------------------

#Start new color scale and add the sampling locations 
  new_scale_fill() +
  
#Run line 71 to just plot sampling locations
  geom_sf(data = WaDE_sites,  aes(shape = EFK), fill = "#90e0ef", size = 4) +
  #scale_fill_brewer(palette  = "Set1") +
  scale_shape_manual(values = c(21,23)) +
  
#Run line 74-75 to show sampling results. Change "parameter" to the paramater of interest. 
  #geom_sf(data = bind_data %>% subset(parameter == "NO3" & DOY == 102 & !is.na(result_value)), aes(fill = result_value, shape = EFK), size = 6) +
  #geom_sf(data = bind_data %>% subset(DOY == 102), aes(fill = flow_status, shape = EFK) , size = 6) +
  #scale_fill_distiller(palette  = "RdYlBu", direction = -1) +
  #scale_shape_manual(values = c(21,24)) +
  #scale_fill_gradient2(low="#4575b4", mid="#ffffbf", high="#d73027", midpoint = 800/2,
   #                    limits = c(0, 800)) +
  #scale_fill_manual(values = c("#FF0505", "#9AC75D", "#FFCD05")) +
  
  #labs(fill = NULL) + #Name the legend for sampling location fill colors
  
#Add map annotations, scales, and legends 
  theme_classic() +
  theme(legend.position = "none") +
  guides(shape = "none") +
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
#If you want to output the map, un-comment Line 67 and replace "map_name" with the name from Line 29.
ggsave(plot = last_plot(), "output/overview_map.png", width = 12, height = 9, units = "in")

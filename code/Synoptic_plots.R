#############################################
# WaDE-SFA Synoptic Analysis
# Matthew Berens
# Updated June 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Watershed Data -------------------------------------------------------------
# Catchment Land Use Data
EFPC_LU_catchments_binned <- st_set_crs(read_sf("GIS_data/EFPC_LU_catchments_binned.gpkg"), 4326)



# Step 2. Load Synoptic Data, if desired ----------------------------------------------------
#Processed survey results
setwd("~/Documents/GitHub/Wade-Theme-1/processed")
alldata <- do.call(rbind, lapply(dir(),read.csv))
setwd("~/Documents/GitHub/Wade-Theme-1")


WaDE_sites <- read.csv("raw/WaDE SYNOPTIC_SITES.csv")
alldata <- merge(alldata, WaDE_sites, by = "site_name")


#Merge with catchment bins
synoptic_bins <- merge(alldata, st_drop_geometry(EFPC_LU_catchments_binned))


# EFK vs tributary comparisons  ----------------------------------------------------
bind_rows(alldata) %>%
  subset(parameter == "SpC") %>%
  ggplot() +
  #geom_boxplot(aes(x = EFK, y = Cd, fill = EFK), width = 0.5, outlier.shape = NA) +
  #geom_jitter(aes(x = EFK, y = Cd, color = sample_date), size =1, width = 0.1) +
  #scale_fill_manual(values = c("#90e0ef", "#0077b6")) +
  #scale_color_manual(values = c("red", "blue")) +
  #scale_y_log10() +
  geom_boxplot(aes(x = sample_date, y = result_value, fill = EFK)) +
  #geom_point(aes(x = as.factor(site_name), y = result_value, fill = sample_date), size = 2, shape = 21) +
  #geom_point(aes(x = pH, y = SpC), shape = 21, fill = "blue") +
  theme_mb1() +
  #expand_limits(y=0) +
  theme(legend.position = "none") +
  labs(x = NULL, y = "DOC (mg/L)")

ggsave(plot = last_plot(), "output/EFK_Cd.png", width = 3, height = 3, units = "in")



`%notin%` <- Negate(`%in%`)


synoptic_bins %>%  
  subset(parameter %in% c("DOC")) %>%
  subset(network %notin% c("EFK")) %>%
  ggplot(aes(y = result_value)) +
  geom_smooth(data = subset(synoptic_bins, network %notin% c("N Boundary") & parameter %in% c("DOC")), aes(x = Developed*100, color = sample_date), method = lm, se = TRUE) +
  geom_smooth(data = subset(synoptic_bins, network %notin% c("N Boundary") & parameter %in% c("DOC")), aes(x = Developed*100, color = sample_date), method = lm, se = FALSE) +
  geom_point(aes(x = Developed*100, fill = sample_date), shape = 21, size = 4) +
  scale_color_manual(values = c("red", "#0077b6", "yellow")) +
  scale_fill_manual(values = c("red", "#0077b6", "yellow")) +
  labs(x = "Developed Land Cover (%)", y = NULL) +
  theme_mb1() +
  facet_wrap(~parameter, scales = "free_y", nrow = 2) +
  theme(aspect.ratio = 1, axis.title = element_text(size = 18), axis.text = element_text(size = 18))


ggsave(plot = last_plot(), "output/Synoptic_comparisons.png", width =12, height = 9, units = "in")









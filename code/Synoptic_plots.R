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

#Synoptic sampling locations - set correct crs
WaDE_sites <- read.csv("raw/WaDE SYNOPTIC_SITES.csv") %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)


# Step 2. Load Synoptic Data, if desired ----------------------------------------------------
#Processed survey results
JUNsynoptic_data <- read.csv("processed/SYNOPTIC_2023-06-23.csv")
APRsynoptic_data <- read.csv("processed/SYNOPTIC_2023-04-12.csv")

#Merge with catchment bins
June2023_synoptic_bins <- merge(read.csv("processed/SYNOPTIC_2023-06-23.csv"), st_drop_geometry(EFPC_LU_catchments_binned))
Apr2023_synoptic_bins <- merge(read.csv("processed/SYNOPTIC_2023-04-12.csv"), st_drop_geometry(EFPC_LU_catchments_binned))

#Merge with NLCD data
JUN2023_LC <- merge(read.csv("raw/WaDE SYNOPTIC_2023-06-23.csv"), WaDE_sites)
APR2023_LC <- merge(read.csv("raw/WaDE SYNOPTIC_2023-04-12.csv"), WaDE_sites)



# EFK vs tributary comparisons  ----------------------------------------------------
bind_rows(JUN2023_LC, APR2023_LC) %>%
  ggplot() +
  #geom_boxplot(aes(x = EFK, y = Cd, fill = EFK), width = 0.5, outlier.shape = NA) +
  #geom_jitter(aes(x = EFK, y = Cd, color = sample_date), size =1, width = 0.1) +
  scale_fill_manual(values = c("#90e0ef", "#0077b6")) +
  scale_color_manual(values = c("red", "blue")) +
  #scale_y_log10() +
  geom_point(aes(x = (DOC/12)*1000, y = Fe/55.845, fill = EFK), size = 2, shape = 21) +
  #geom_point(aes(x = pH, y = SpC), shape = 21, fill = "blue") +
  theme_mb1() +
  expand_limits(y=0) +
  scale_y_continuous(limits = c(0,1)) +
  theme(aspect.ratio = 1, legend.position = "none") +
  labs(x = "DOC (mmol/L)", y = "Fe (mmol/L)")

ggsave(plot = last_plot(), "output/EFK_Cd.png", width = 3, height = 3, units = "in")



`%notin%` <- Negate(`%in%`)

bine <- rbind(Apr2023_synoptic_bins, June2023_synoptic_bins)
bine %>%  
  subset(parameter %in% c("Mg", "Ca", "SpC", "DIC", "DOC", "Cl")) %>%
 #subset((site_name %notin% c("NBO2", "NBO3", "NBO3", "NBO4", "NBO5", "NBO6"))) %>%
  #subset(parameter == "SpC") %>%
  ggplot(aes(y = result_value)) +
  geom_smooth(data = subset(bine, site_name %notin% c("NBO2", "NBO3", "NBO3", "NBO4", "NBO5", "NBO6") & parameter %in% c("Mg", "Ca", "SpC", "DIC", "DOC", "Cl")), aes(x = Developed*100, color = sample_date), method = lm, se = TRUE) +
  geom_smooth(data = subset(bine, site_name %in% c("NBO2", "NBO3", "NBO3", "NBO4", "NBO5", "NBO6") & parameter %in% c("Mg", "Ca", "SpC", "DIC", "DOC", "Cl")), aes(x = Developed*100, color = sample_date), method = lm, se = FALSE) +
  geom_point(aes(x = Developed*100, fill = sample_date), shape = 21, size = 4) +
  scale_color_manual(values = c("red", "#0077b6")) +
  scale_fill_manual(values = c("red", "#0077b6")) +
  labs(x = "Developed Land Cover (%)", y = NULL) +
  theme_mb1() +
  facet_wrap(~parameter, scales = "free_y", nrow = 2) +
  theme(aspect.ratio = 1, legend.position = "none", axis.title = element_text(size = 18), axis.text = element_text(size = 18))


ggsave(plot = last_plot(), "output/Synoptic_comparisons.png", width =12, height = 9, units = "in")









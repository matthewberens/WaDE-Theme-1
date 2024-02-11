#############################################
# HOBO Soil Moisture Processing
# Matthew Berens
# Updated Nov 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Data ---------------------------------------------------------------------
DATA_STIC <- "STIC Data" %>% 
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", show_col_types = FALSE) %>% 
  dplyr::select(-"#") %>%
  set_colnames(c("FileID", "DateTime", "Temp_degC", "Intensity_Lux")) %>%
  mutate(DateTime = mdy_hm(DateTime)) %>%
  mutate(Site = str_match(FileID, "/\\s*(.*?)\\s*_")[,2],
         STIC_ID = str_match(FileID, "_\\s*(.*?)\\s*_")[,2],
         Group = substr(Site, 1, 4)) %>%
  dplyr::select(-"FileID") %>%
  rename("INTENSITY" = "Intensity_Lux", "TEMP" = "Temp_degC") %>%
  dplyr::select(Group, Site, DateTime, INTENSITY, TEMP) %>%
  pivot_longer(c(INTENSITY:TEMP), names_to = "Parameter", values_to = "Value") %>%
  mutate(Unit = ifelse(Parameter == "INTENSITY", "LUX", "DEG_C"))

# Step 3. Save Formatted Data as a .csv  ---------------------------------------------------------
write.csv(DATA_STIC, "processed/WaDE_STIC_EFPC_PROCESSED.csv", row.names = FALSE)

#ggsave(plot = last_plot(), "STIC Data/STIC Figures/MCA2_20231020.jpg", width = 7, height = 3, units = "in")






  



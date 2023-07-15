#############################################
# WaDE-SFA Synoptic Maps for Printing
# Matthew Berens
# Updated June 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
source("code/0-packages.R")


# Step 2. Load Synoptic Data, if desired ----------------------------------------------------
synoptic_data <- read.csv("processed/SYNOPTIC_2023-06-23.csv")


synoptic_data %>%
  dplyr::select(-c("ddl", "constituent", "detection_FLAG", "unit")) %>%
  pivot_wider(names_from = "parameter", values_from = "result_value", names_expand = TRUE) %>%
  ggplot() +
  geom_point(aes(x = Ca,  y = Mg)) +
  theme_mb1()

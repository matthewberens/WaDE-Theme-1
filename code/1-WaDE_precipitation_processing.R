#############################################
# WaDE Precipitation Data
# Matthew Berens
# Updated Feb 2024
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

#load packages
source("code/0-packages.R")

PRECIP <- readRDS("Synoptic Data/HourlyPrecip_ORNL.rds") %>%
  select(-c("DATE", "TIME")) %>%
  subset(DateTime > ymd_hms("2023-01-01 00:00:00")) %>%
  dplyr::rename("Hour" = "DateTime")

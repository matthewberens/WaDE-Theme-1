#############################################
# WaDE-SFA Synoptic Sampling Data Processing
# Matthew Berens
# Updated June 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Data ---------------------------------------------------------------------

sampling_data = read.csv("raw/WaDE SYNOPTIC_2023-04-12.csv") 

# Step 2. Load Sampling Locations -------------------------------------------------------

synoptic_sites <- read.csv("raw/WaDE SYNOPTIC_locations.csv") 

# Step 3. Merge site info and sampling data ---------------------------------------------

merged_data <- merge(synoptic_sites, sampling_data, by = "site_name")


# Step 4. Transform data and prepare for export -----------------------------------------
data_formatted <- merged_data %>%
  pivot_longer(c("depth":"K"), values_to = "result_value", names_to = "parameter") %>%
  mutate(unit = ifelse(parameter == "depth", "CM",
                ifelse(parameter == "temp", "C",
                ifelse(parameter == "pH", "UNITS",
                ifelse(parameter == "SpC", "uS/CM", "MG/L")))),
         constituent = ifelse(parameter == "depth", "PHYS",
                       ifelse(parameter %in% c("pH", "SpC", "temp"), "CHEM",
                       ifelse(parameter %in% c("DOC", "DIC"), "C",
                       ifelse(parameter %in% c("SO4", "Cl", "NO3", "PO4"), "ANION",
                       ifelse(parameter %in% c("Mg", "Ca", "Na", "K"), "CATION", ""))))),
         sample_date = mdy(sample_date),
         sample_time = hm(sample_time))

#Save formatted data as a .csv
write.csv(data_formatted, "processed/SYNOPTIC_2023-04-12.csv", row.names = FALSE)
  



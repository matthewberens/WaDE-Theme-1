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

sampling_data = read.csv("raw/WaDE SYNOPTIC_2023-06-23.csv") 



# Step 2. Calculate TIC  ----------------------------------------------------------------

sampling_data$DIC = sampling_data$TC - sampling_data$DOC


# Step 3. Transform data and prepare for export -----------------------------------------
data_formatted <- sampling_data %>%
  pivot_longer(c("depth":"DIC"), values_to = "result_value", names_to = "parameter") %>%
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
write.csv(data_formatted, "processed/SYNOPTIC_2023-06-23.csv", row.names = FALSE)
  



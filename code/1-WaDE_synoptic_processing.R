#############################################
# WaDE-SFA Synoptic Sampling Data Processing
# Matthew Berens
# Updated January 2024
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Data ---------------------------------------------------------------------

DATA = read.csv("Synoptic Data/WaDE SYNOPTIC_SUMMARY.csv") %>%
  select(-c(Date, Time)) %>%
  mutate(DateTime = mdy_hm(DateTime)) %>%
  mutate(across(.cols = -c(Sample_ID:Flow_state), .fns = function(x) ifelse(x == "ND", as.numeric(-100), x))) %>%
  mutate(across(.cols = -c(Sample_ID:Flow_state), .fns = function(x) ifelse(x == "<DL", -200, x))) %>%
  mutate(across(.cols = -c(Sample_ID:Flow_state), .fns = function(x) ifelse(x == "TBD", -300, x))) %>%
  mutate(across(.cols = -c(Sample_ID:Flow_state), .fns = function(x) x = as.numeric(as.character(x)))) %>%
  pivot_longer(-c(Sample_ID:Flow_state), names_to = "Parameter", values_to = "Value") %>%
  mutate(Unit  = sub("^[^_]*_", "", Parameter),
         Parameter = gsub( "_.*$", "", Parameter),
         Network = gsub("[^a-zA-Z]", "", Site),
         Network = ifelse(Network %in% c("BSL", "MCA", "NBO", "MTN", "WBK"), Network, "EFK"),
         Group = ifelse(Network == "EFK", "EFK", substr(Site, 1, 4)),
         Survey = my(paste(month(DateTime),"/",year(DateTime), sep = ""))) %>%
  mutate(Sort = ifelse(Flow_state == "D", "Dry",
                ifelse(Flow_state == "P", "Pooled",
                ifelse(Value == -200, "<DL",
                ifelse(Value == -300, "TBD", "Flowing")))))

# Step 2. Export processed data as csv --------------------------------------------------

#Save formatted data as a .csv
#write.csv(data_LOD, "processed/SYNOPTIC_2023Apr.csv", row.names = FALSE)









  



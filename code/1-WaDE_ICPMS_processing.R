#############################################
# WaDE ICP-MS Processing
# Matthew Berens
# Updated Nov 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Data ---------------------------------------------------------------------

raw_1_61 = read.csv("Synoptic Data/ICP-MS/WaDE1-61_ICPMS_RAW.csv", check.names = FALSE) 
raw_62_134 = read.csv("Synoptic Data/ICP-MS/WaDE62-134_ICPMS_RAW.csv", check.names = FALSE) 
raw_135_198 = read.csv("Synoptic Data/ICP-MS/WaDE135-198_ICPMS_RAW.csv", check.names = FALSE) 
raw_199_270 = read.csv("Synoptic Data/ICP-MS/WaDE199-270_ICPMS_RAW.csv", check.names = FALSE) 


# Step 2. Separate Detection Limits  ----------------------------------------------------------------

DL_1_61 <- filter(raw_1_61, sampleID %in% c("DL_Cal", "DL_Instrument"))
concentrations_1_61 <- filter(raw_1_61, !(sampleID %in% c("DL_Cal", "DL_Instrument")))

DL_62_134 <- filter(raw_62_134, sampleID %in% c("DL_Cal", "DL_Instrument"))
concentrations_62_134 <- filter(raw_62_134, !(sampleID %in% c("DL_Cal", "DL_Instrument")))

DL_135_198 <- filter(raw_135_198, sampleID %in% c("DL_Cal", "DL_Instrument"))
concentrations_135_198 <- filter(raw_135_198, !(sampleID %in% c("DL_Cal", "DL_Instrument")))

DL_199_270 <- filter(raw_199_270, sampleID %in% c("DL_Cal", "DL_Instrument"))
concentrations_199_270 <- filter(raw_199_270, !(sampleID %in% c("DL_Cal", "DL_Instrument")))


# Step 3. Transpose DL Data  ---------------------------------------------------------
DL_long_1_61 <- DL_1_61 %>%
  pivot_longer(cols=c(-sampleID),names_to="Parameter")%>%
  pivot_wider(names_from=c(sampleID))

DL_long_62_134 <- DL_62_134 %>%
  pivot_longer(cols=c(-sampleID),names_to="Parameter")%>%
  pivot_wider(names_from=c(sampleID))

DL_long_135_198 <- DL_135_198 %>%
  pivot_longer(cols=c(-sampleID),names_to="Parameter")%>%
  pivot_wider(names_from=c(sampleID))

DL_long_199_270 <- DL_199_270 %>%
  pivot_longer(cols=c(-sampleID),names_to="Parameter")%>%
  pivot_wider(names_from=c(sampleID))


# Step 4. Merge and Assign Flags  ---------------------------------------------------------
merged_1_61 <- merge(
  pivot_longer(data = concentrations_1_61, !sampleID, names_to = "Parameter", values_to = "Concentration"),
  DL_long_1_61,
  by = "Parameter") %>%
  mutate(Unit = "PPB",
         Instrument = "ICP-MS",
         Parameter = sub("\\_.*", "", Parameter),
         Element = sub("\\ .*", "", Parameter),
         Cal_FLAG = ifelse(Concentration < DL_Cal, "FAIL", "PASS"),
         Instrument_FLAG = ifelse(Concentration < DL_Instrument, "FAIL", "PASS"))

merged_62_134 <- merge(
  pivot_longer(data = concentrations_62_134, !sampleID, names_to = "Parameter", values_to = "Concentration"),
  DL_long_62_134,
  by = "Parameter") %>%
  mutate(Unit = "PPB",
         Instrument = "ICP-MS",
         Parameter = sub("\\_.*", "", Parameter),
         Element = sub("\\ .*", "", Parameter),
         Cal_FLAG = ifelse(Concentration < DL_Cal, "FAIL", "PASS"),
         Instrument_FLAG = ifelse(Concentration < DL_Instrument, "FAIL", "PASS"))

merged_135_198 <- merge(
  pivot_longer(data = concentrations_135_198, !sampleID, names_to = "Parameter", values_to = "Concentration"),
  DL_long_135_198,
  by = "Parameter") %>%
  mutate(Unit = "PPB",
         Instrument = "ICP-MS",
         Parameter = sub("\\_.*", "", Parameter),
         Element = sub("\\ .*", "", Parameter),
         Cal_FLAG = ifelse(Concentration < DL_Cal, "FAIL", "PASS"),
         Instrument_FLAG = ifelse(Concentration < DL_Instrument, "FAIL", "PASS"))

merged_199_270 <- merge(
  pivot_longer(data = concentrations_199_270, !sampleID, names_to = "Parameter", values_to = "Concentration"),
  DL_long_199_270,
  by = "Parameter") %>%
  mutate(Unit = "PPB",
         Instrument = "ICP-MS",
         Parameter = sub("\\_.*", "", Parameter),
         Element = sub("\\ .*", "", Parameter),
         Cal_FLAG = ifelse(Concentration < DL_Cal, "FAIL", "PASS"),
         Instrument_FLAG = ifelse(Concentration < DL_Instrument, "FAIL", "PASS"))


# Step 5. Save Formatted Data as a .csv  ---------------------------------------------------------
all_ICPMS <- rbind(merged_1_61, merged_62_134, merged_135_198, merged_199_270) %>%
  dplyr::select(sampleID, Parameter, Element, Concentration, Unit, DL_Cal, DL_Instrument, Cal_FLAG, Instrument_FLAG, Instrument)

write.csv(all_ICPMS, "Synoptic Data/WaDE ICPMS_PROCESSED.csv", row.names = FALSE)
  



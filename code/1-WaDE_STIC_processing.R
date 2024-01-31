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
all_STIC <- "STIC Data" %>% 
  dir_ls(regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source") %>% 
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
write.csv(all_STIC, "processed/WaDE_STIC_EFPC_PROCESSED.csv", row.names = FALSE)



all_STIC %>% 
  subset(Group == "MCA2") %>%
  ggplot(aes(x = DateTime, y = Intensity_Lux)) +
  geom_line(color = "#283AB8") + 
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  facet_wrap(~Site, scales = "free_x", ncol = 1) +
  theme_mb1() +
  theme(text = element_text(family = "Trebuchet MS")) +
  labs(x = NULL, y = "Intensity") +
  #scale_color_brewer(palette = "PuOr", direction = 1) +
  guides(color = guide_legend(nrow = 1))

ggsave(plot = last_plot(), "STIC Data/STIC Figures/MCA2_20231020.jpg", width = 7, height = 3, units = "in")



all_SM %>%
  subset(Plot == "4") %>%
  subset(Parameter == "Temperature") %>%
  ggplot(aes(x = DateTime, y = as.numeric(Depth), z = Value)) +
  geom_contour_fill(na.fill = TRUE) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0,0)) +
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  labs(x = "2023", y = NULL, fill = "Temp (C)") +
  scale_y_reverse(expand = c(0,0))+
  #facet_wrap(~plot)+
  theme_mb1() +
  theme(axis.text.x = element_text(size = 10, vjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

clean %>%
  subset(Plot == "1") %>%
  subset(Parameter == "Water") %>%
  ggplot(aes(x = DateTime, y = as.numeric(Depth), z = Value)) +
  geom_contour_fill(na.fill = TRUE) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%b-%d", expand = c(0,0)) +
  scale_fill_gradientn(colors=pnw_palette("Bay"))+
  labs(x = "2023", y = NULL, fill = "VWC (m3/m3)") +
  scale_y_reverse(expand = c(0,0))+
  #facet_wrap(~plot)+
  theme_mb1() +
  theme(axis.text.x = element_text(size = 10, vjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())






  



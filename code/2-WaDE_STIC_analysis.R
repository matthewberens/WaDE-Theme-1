#############################################
# STIC Sensor Processing
# Matthew Berens
# Updated Nov 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

#load packages
source("code/0-packages.R")

# Step 1. Load Data ---------------------------------------------------------------------
STIC <-read.csv("processed/WaDE_STIC_EFPC_PROCESSED.csv") %>%
  mutate(DateTime = ymd_hms(DateTime))

EFPC_sites <- read.csv("GIS Data/EFPC_SiteData.csv")


# Step 2. Calculate Flow Time ---------------------------------------------------------
STIC_FLOW <- STIC %>% subset(Parameter == "INTENSITY") %>%
  mutate(Flow = ifelse(Value < 100, 0, 1),
         Hour = round_date(DateTime, unit = "10 minutes"),
                Network = gsub("[^a-zA-Z]", "", Site)) %>%
  select(c(Network, Group, Site, DateTime, Value, Flow, Hour))

# Step 3. STREAM FLOW PERMANENCE ---------------------------------------------------------
permanence <- STIC_FLOW %>%
  group_by(Network, Site) %>%
  summarise(Sum = sum(Flow),
            Permanence = Sum/length(Flow)) %>%
 merge(EFPC_sites)

permanence %>%
  ggplot() +
  geom_point(aes(x = DA_sqkm, y = Permanence, fill = Network), shape = 21, size = 3) +
  theme_mb1()

# Step 3. INSTANTANEOUS FLOW ---------------------------------------------------------
instantaneous <- STIC_FLOW %>%
  group_by(Hour, Network) %>%
  summarize(Inst_flow = sum(Flow)/length(Flow))

instantaneous %>%
  ggplot() +
  geom_line(aes(x = Hour, y = Inst_flow, color = Network)) +
  theme_mb1()




Flows <- STIC_FLOW %>% subset(Site == "BSL5.1") %>% select(c("DateTime","Flow"))




STIC_FLOW %>% 
  subset(Group == "BSL5") %>%
  ggplot(aes(x = DateTime, y = Value)) +
  geom_line(color = "#283AB8") + 
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  #facet_wrap(~Site, scales = "free_x", ncol = 1) +
  theme_mb1() +
  labs(x = NULL, y = "Intensity") +
  guides(color = guide_legend(nrow = 1)) +
  geom_hline(yintercept = 100)

  
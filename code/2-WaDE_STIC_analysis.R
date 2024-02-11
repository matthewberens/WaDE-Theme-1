#############################################
# STIC Sensor Processing
# Matthew Berens
# Updated Nov 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

#load packages
source("code/1-WaDE_STIC_processing.R")
source("code/1-WaDE_precipitation_processing.R")
# Step 1. Load Data ---------------------------------------------------------------------
SITES = read.csv("Synoptic Data/EFPC_SiteData.csv")

SURVEYS <- data.frame(xintercepts = c(ymd_hms("2023-10-25 12:00:00"),ymd_hms("2023-12-19 12:00:00")))

fills <- c("Flowing" = "#90be6d", "Pooled" = "#f9c74f", "Dry" = "#f94144", "<DL" = "black", "TBD" = "black")
networks <- c("BSL" = "#D7191C", "MCA" = "#FFFFBF", "NBO" = "#1A9641", "WBK" = "#FDAE61", "MTN" = "#A6D96A")


# Step 2. Calculate Flow Time ---------------------------------------------------------
STIC_FLOW <- DATA_STIC %>% subset(Parameter == "INTENSITY") %>%
  mutate(Flow = ifelse(Value < 100, 0, 1),
         Hour = round_date(DateTime, unit = "10 minutes"),
                Network = gsub("[^a-zA-Z]", "", Site)) %>%
  select(c(Network, Group, Site, DateTime, Value, Flow, Hour))

##STIC STREAM FLOW PERMANENCE ---------------------------------------------------------
STIC_PERM <- STIC_FLOW %>%
  group_by(Network, Site) %>%
  summarise(Sum = sum(Flow),
            STIC_Permanence = Sum/length(Flow)) %>%
  merge(SITES) %>%
  mutate(Network = factor(Network, levels = c("NBO", "MTN", "MCA", "WBK", "BSL"))) 

STIC_PERM %>%
  merge(SITES) %>%
  ggplot(aes(y = STIC_Permanence*100, x = Site)) +
  geom_point(aes(fill = Network), shape = 21, size = 3) +
  stat_smooth(method = "lm") +
  scale_fill_manual(values = networks) +
  labs(x= NULL, y = "Stream Flow Permamence (%)") +
  #guides(fill = "none") +
  theme_mb1()


##STIC INSTANTANEOUS STREAM FLOW -----------------------------------------------
STIC_INST <- STIC_FLOW %>%
  group_by(Group, Hour, Network) %>%
  summarise(Inst_flow = sum(Flow)/length(Flow))

STIC_INST %>%
  subset(Network %in% c("MTN", "MCA", "WBK", "BSL")) %>%
  ggplot() +
  geom_line(aes(x = Hour, y = Inst_flow*100, color = Group)) +
  labs(x = NULL, y = "Instaneous Network Flow (%)", title = "Instantaneous Network Flow") +
  theme_mb1() +
  theme(legend.title = element_blank())


STIC_FLOW %>% 
  subset(Group == "BSL8") %>%
  ggplot(aes(x = DateTime, y = Value)) +
  geom_line(color = "#283AB8") + 
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  facet_wrap(~Site, scales = "free_y", ncol = 1) +
  theme_mb1() +
  labs(x = NULL, y = "Intensity") +
  theme(text = element_text(family = "Trebuchet MS")) +
  guides(color = guide_legend(nrow = 1))


PRECIP %>% 
  merge(STIC_FLOW) %>%
  subset(Site == "BSL3.2") %>%
  ggplot(aes(x = Hour)) +
  geom_line(aes(y = KOQT*25.4)) + 
  geom_line(aes(y = -Value/20000+35), color = "#283AB8") +
  scale_y_continuous(trans = "reverse", limits = c(36, 0), breaks = seq(0,16, by = 2)) +
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  geom_vline(data = SURVEYS, aes(xintercept = xintercepts), linetype = "dashed") +
  theme_mb1() +
  labs(x = NULL, y = "hourly rainfall (mm)", title = "BSL3.2") +
  theme(text = element_text(family = "Trebuchet MS")) +
  guides(color = guide_legend(nrow = 1))

PRECIP %>% 
  merge(STIC_FLOW) %>%
  subset(Site == "BSL5.1") %>%
  ggplot(aes(x = Hour)) +
  geom_line(aes(y = KOQT*25.4)) + 
  geom_line(aes(y = -Value/8000+35), color = "#283AB8") +
  scale_y_continuous(trans = "reverse", limits = c(36, 0), breaks = seq(0,16, by = 2)) +
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  geom_vline(data = SURVEYS, aes(xintercept = xintercepts), linetype = "dashed") +
  theme_mb1() +
  labs(x = NULL, y = "hourly rainfall (mm)", title = "BSL5.1") +
  theme(text = element_text(family = "Trebuchet MS")) +
  guides(color = guide_legend(nrow = 1))

PRECIP %>% 
  merge(STIC_FLOW) %>%
  subset(Site == "NBO2") %>%
  subset(Hour > "2023-10-26 00:00:00") %>%
  ggplot(aes(x = Hour)) +
  geom_line(aes(y = KOQT*25.4)) + 
  geom_line(aes(y = -Value/6000+44), color = "#283AB8") +
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  scale_y_continuous(trans = "reverse", limits = c(36, 0), breaks = seq(0,16, by = 2)) +
  geom_vline(data = SURVEYS, aes(xintercept = xintercepts), linetype = "dashed") +
  theme_mb1() +
  labs(x = NULL, y = "hourly rainfall (mm)", title = "NBO2") +
  theme(text = element_text(family = "Trebuchet MS")) +
  guides(color = guide_legend(nrow = 1))

PRECIP %>% 
  merge(STIC_FLOW) %>%
  subset(Site == "BSL4.2") %>%
  #subset(Hour > "2023-10-24 00:00:00") %>%
  ggplot(aes(x = Hour)) +
  geom_line(aes(y = KOQT*25.4)) + 
  geom_line(aes(y = -Value/5000+35), color = "#283AB8") +
  #geom_line(aes(y = -Flow*10+32), color = "#283AB8") +
  scale_x_datetime(date_breaks = "1 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  scale_y_continuous(trans = "reverse", limits = c(36, 0), breaks = seq(0,16, by = 2)) +
  geom_vline(data = SURVEYS, aes(xintercept = xintercepts), linetype = "dashed") +
  theme_mb1() +
  labs(x = NULL, y = "hourly rainfall (mm)", title = "BSL4.2") +
  theme(axis.text.x = element_text(size = 8)) +
  guides(color = guide_legend(nrow = 1))

PRECIP %>% 
  merge(STIC_FLOW) %>%
  subset(Site == "BSL6") %>%
  ggplot(aes(x = Hour)) +
  geom_line(aes(y = KOQT*25.4)) + 
  geom_line(aes(y = -Value/8000+36), color = "blue") +
  scale_y_continuous(trans = "reverse", limits = c(36, 0), breaks = seq(0,16, by = 2)) +
  scale_x_datetime(date_breaks = "2 weeks", date_labels = "%b-%d", expand = c(0,0)) +
  theme_mb1() +
  theme(axis.text.x = element_text(size = 8)) +
  geom_vline(data = SURVEYS, aes(xintercept = xintercepts), linetype = "dashed") +
  labs(x = NULL, y = "hourly rainfall (mm)", title = "BSL6") +
  guides(color = guide_legend(nrow = 1))


  
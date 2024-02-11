#############################################
# STIC STIC Accuracy
# Matthew Berens
# Updated Nov 2023
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/WaDE-Theme-1")

# Step 1. Load Data ---------------------------------------------------------------------
source("code/1-WaDE_STIC_processing.R")
source("code/1-WaDE_synoptic_processing.R")

# Step 2. Calculate STIC PERMANENCE ---------------------------------------------------------
STIC_ACCURACY <- DATA_STIC %>% subset(Parameter == "INTENSITY") %>%
  mutate(Flow = ifelse(Value < 100, 0, 1),
         Hour = round_date(DateTime, unit = "10 minutes"),
         Network = gsub("[^a-zA-Z]", "", Site)) %>%
  select(c(Network, Group, Site, DateTime, Value, Flow, Hour)) %>%
  group_by(Network, Site) %>%
  summarise(STIC_permanence = sum(Flow)/length(Flow)) 

# Step 3. STREAM FLOW PERMANENCE ---------------------------------------------------------
RAW_SYNOPTIC <- DATA %>%
  select(-c("Sort", "Unit")) %>%
  pivot_wider(names_from = "Parameter", values_from = "Value") %>%
  select(c(Sample_ID:Trib)) %>%
  mutate(FLOW_synoptic = ifelse(Flow_state %in% c("F", "P"), 1, 0)) %>%
  group_by(Site) %>%
  summarise(SYNOPTIC_permanence = sum(FLOW_synoptic)/length(FLOW_synoptic))


COMPS <- merge(STIC_ACCURACY, RAW_SYNOPTIC, by = "Site") %>%
  merge(SITES) %>%
  mutate(Network = factor(Network, levels = c("NBO", "MTN", "MCA", "WBK", "BSL")))

COMPS %>%
  ggplot(aes(x = SYNOPTIC_permanence*100, y = STIC_permanence*100)) +
  geom_point(aes(fill = Network), shape = 21, size = 3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  geom_text(aes(label = Site), size = 3, check_overlap = TRUE)+
  scale_fill_manual(values = networks) +
  labs(x = "Synoptic Permanence (%)", y = "STIC permanence (%)", title = NULL, fill = "Developed Land (%)") +
  theme_mb1() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,100)) +
  theme(legend.title = element_blank(),
        legend.margin = margin(1,0,-5,0))



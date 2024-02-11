#############################################
# WaDE-SFA Synoptic Sampling Data Processing
# Matthew Berens
# Updated January 2024
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
#source("code/0-packages.R")
source("code/1-WaDE_synoptic_processing.R")

DATA_scatter <- DATA %>%
  mutate(Value = ifelse(Sort %in% c("Dry","<DL", "TBD"), 0,
                        ifelse(Sort == "ND", NA,
                               ifelse(Value %in% c(-300,-200), 0, Value)))) %>%
  merge(SITES)

shapes <- c("Flowing" = 21, "Pooled" = 21, "Dry" = 21, "<DL" = 8, "TBD" = 6)
fills <- c("Flowing" = "#90be6d", "Pooled" = "#f9c74f", "Dry" = "#f94144", "<DL" = "black", "TBD" = "black")

sub <- "DOC"
DATA_scatter %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  subset(Network == "BSL") %>%
  #subset(Sort != "Dry") %>%
  #subset(Survey == "2023-12-01") %>%
  subset(Parameter == "SO4IC") %>%
  ggplot() +
  geom_point(aes(x = Group, y = Value/0.09606, shape = Sort, fill = Sort), size = 3) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  theme_mb1() +
  labs(x = NULL, y = "SO4 (umol/L)") +
  facet_wrap(~Survey, labeller = function(x) format(x, "%B %Y"), nrow = 1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.title = element_blank(),
        legend.margin = margin(-5,0,-10,0),
        axis.text.x = element_text(size = 8))


Measurement = "Cd"

DATA_scatter %>%
  subset(Site != "FB") %>%
  subset(Sort != "Dry") %>%
  subset(Synoptic == "Y") %>%
  #subset(Sort %in% c("Flowing", "Pooled")) %>%
  subset(Trib == "TRIB") %>%
  subset(Parameter == Measurement) %>%
  ggplot(aes(label = Site)) +
  geom_point(aes(x = Developed_pct, y = Value, shape = Sort, fill = Sort), size = 3) +
  #stat_smooth(data = subset(DATA_scatter, Sort %in% c("Flowing", "Pooled") & Parameter == Measurement),
                             # aes(x = Impervious_pct, y = Value),  method = "lm") +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  theme_mb1() +
  theme(legend.title = element_blank())

  ggplotly(p)


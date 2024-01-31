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
                               ifelse(Value %in% c(-300,-200), 0, Value))))

shapes <- c("Flowing" = 21, "Pooled" = 21, "Dry" = 21, "<DL" = 8, "TBD" = 6)
fills <- c("Flowing" = "green", "Pooled" = "yellow", "Dry" = "red", "<DL" = "black", "TBD" = "black")

sub <- "DOC"
DATA_scatter %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  subset(Network == "BSL") %>%
  #subset(Survey == "2023-12-01") %>%
  subset(Parameter == sub) %>%
  ggplot() +
  geom_point(aes(x = Group, y = Value, shape = Sort, fill = Sort)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  theme_mb1() +
  labs(x = NULL, title = sub) +
  facet_wrap(~Survey, labeller = function(x) format(x, "%B %Y"), nrow = 1) +
  guides(color = guide_legend(nrow = 1)) 



DATA_scatter %>%
  subset(Site != "FB") %>%
  subset(Sort != "Dry") %>%
  subset(Synoptic == "Y") %>%
  subset(Trib == "TRIB") %>%
  #subset(Survey == "2023-12-01") %>%
  subset(Parameter == "SO4") %>%
  ggplot(aes(label = Site)) +
  geom_point(aes(x = Developed_pct, y = Value, shape = Sort, fill = Sort)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  theme_mb1() +
  theme(text = element_text(family = "Trebuchet MS"),
        legend.title = element_blank())
  


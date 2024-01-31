#############################################
# WaDE-SFA Synoptic Box Plots
# Matthew Berens
# Updated January 2024
#############################################

#Load libraries and set directory
setwd("~/Documents/GitHub/Wade-Theme-1")

#load packages
#source("code/0-packages.R")
source("code/1-WaDE_synoptic_processing.R")

DATA_box <- DATA %>%
  mutate(Value = ifelse(Sort == "<DL", 0,
                 ifelse(Sort %in% (c("TBD", "Dry")), NA,
                 ifelse(Value == -200, 0,
                 ifelse(Value == -300, NA, Value)))))


boxcolors <- c("TRIB" = "red", "EFPC" = "blue")
boxfills <- c("TRIB" = "salmon", "EFPC" = "lightblue")

sub <- "EC"
DATA_box %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  #subset(Network == "BSL") %>%
  #subset(Survey == "2023-12-01") %>%
  subset(Parameter == sub) %>%
  ggplot(aes(x = as.factor(Survey), y = Value, fill = Trib)) +
  geom_boxplot() +
  #scale_color_manual(values = colors) +
  scale_fill_manual(values = boxfills) +
  theme_mb1() +
  labs(x = NULL, title = sub) +
  #facet_wrap(~Survey, labeller = function(x) format(x, "%B %Y"), nrow = 1, scales = "free_x") +
  guides(color = guide_legend(nrow = 1)) 

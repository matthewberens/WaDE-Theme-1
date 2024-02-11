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

#LOAD IN RAW DATA AND REORGANIZE
RAW_SITES <- DATA %>%
  select(-c("Sort", "Unit")) %>%
  #subset(Parameter != "NO3" & Parameter != "DO") %>%
  pivot_wider(names_from = "Parameter", values_from = "Value") %>%
  mutate(across(.cols = -c(Sample_ID:Depth), .fns = function(x) ifelse(x == -100, NA, x))) %>%
  mutate(across(.cols = -c(Sample_ID:Depth), .fns = function(x) ifelse(x == -200, NA, x))) %>%
  mutate(across(.cols = -c(Sample_ID:Depth), .fns = function(x) ifelse(x == -300, NA, x))) %>%
  mutate(across(.cols = -c(Sample_ID:Depth), .fns = function(x) x = as.numeric(as.character(x)))) %>%
  mutate(Flow_state = case_when(Flow_state == "F" ~ "Flowing",
                                Flow_state == "P" ~ "Pooled",
                                Flow_state %in% c("D", "W") ~"Dry")) %>%
  mutate(Flow_state = factor(Flow_state, levels = c("Flowing", "Pooled", "Dry")))
  


#PLOTTING AESTHETHICS
shapes <- c("Flowing" = 21, "Pooled" = 21, "Dry" = 21, "<DL" = 8, "TBD" = 6)
fills <- c("Flowing" = "#90be6d", "Pooled" = "#f9c74f", "Dry" = "#f94144", "<DL" = "black", "TBD" = "black")
tribs <- c("EFPC" = "salmon", "TRIB" = "lightblue")




RAW_SITES %>% 
  merge(SITES) %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  subset(Fe<100) %>%
  #ggplot(aes(x = Ca/40.078 + Mg/24.305, y = EC)) +
  #ggplot(aes(x = Ca/40.078, y = SO4IC/.09606)) +
  ggplot(aes(x = DOC/0.012, y = Fe/55.485)) +
  #ggplot(aes(x = Impervious_pct, y = Zn/65.38, label = Site)) +
  stat_smooth(method = "lm", linetype = "dashed", color = "black") +
  #geom_point(data= subset(RAW_SITES, Developed_pct<0.50), shape = 21, size = 3) +
  geom_point(aes(fill = Flow_state),  shape = 21, size = 3) +
  #scale_x_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20)) +
  
  #scale_y_continuous(limits = c(0,500), breaks = seq(0, 500, by = 100)) +
  scale_shape_manual(values = shapes) +
  #scale_fill_distiller(palette = "Reds", direction = 1) +
  scale_fill_manual(values = fills) +
  theme_mb1() +
  expand_limits(y = 0) +
  labs(x = "Ca + Mg (umol/L)", y = "EC (uS/cm)", title = "Tributaries Only") +
  #facet_wrap(~Network) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(legend.title = element_blank(),
        legend.margin = margin(-5,0,-10,0)) 



RAW_SITES %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  subset(Fe < 100) %>%
  #ggplot(aes(x = Ca/40.078 + Mg/24.305, y = EC)) +
  ggplot(aes(x = Ca/40.078, y = SO4IC/96.06)) +
  #ggplot(aes(x = Ca/40.078, y = Zn/65.38)) +
  #geom_point(data= subset(RAW_SITES, Developed_pct<0.50), shape = 21, size = 3) +
  stat_smooth(aes(color = Trib), method = "lm", se = F) +
  geom_point(aes(fill = Trib),  shape = 21, size = 3) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = tribs) +
  scale_color_manual(values = tribs) +
  theme_mb1() +
  labs(x = "Ca (umol/L)", y = "Sulfate (umol/L)") +
  #facet_wrap(~Network) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.title = element_blank(),
        legend.margin = margin(-5,0,-10,0))  

RAW_SITES %>%
  subset(Site != "FB") %>%
  subset(Synoptic == "Y") %>%
  subset(Fe < 100) %>%
  #ggplot(aes(x = Ca/40.078 + Mg/24.305, y = EC)) +
  ggplot(aes(x = Trib, y = SO4IC/0.09606)) +
  geom_boxplot(aes(fill = Trib)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = tribs) +
  scale_color_manual(values = tribs) +
  theme_mb1() +
  labs(x = NULL, y = "SO4 (umol/L)") +
  theme(legend.position = "none")
  #facet_wrap(~Network) +



RAW_SITES %>%
  merge(SITES) %>%
  subset(Site != "FB") %>%
  #subset(Mn < 100) %>%
  subset(Synoptic == "Y") %>%
  #subset(Network == "BSL") %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(Survey), y = Ca/40.078, fill = Network)) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = fills) +
  #scale_fill_gradient( palette = "Reds") +
  #scale_y_continuous(limits = c(0,100), breaks = seq(0,100,by=20)) +
  theme_mb1() +
  labs(x = NULL, y="Ca (umol/L)") +
  #facet_wrap(~Survey, labeller = function(x) format(x, "%B %Y"), nrow = 1) +
  facet_wrap(~Network,  nrow = 1) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.title = element_blank(),
        legend.position = "none")

 

CORR <- RAW_SITES %>%
  select(-c(Site:Trib)) %>%
  as.matrix() %>%
  rcorr() %>%
  pluck(3) %>%
  as.data.frame() %>%
  mutate_all(funs(ifelse(.>0.05,"GOOD","BAD")))

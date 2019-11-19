#this script very unfinished. sorry about this. But do comment on the makefiles and other scripts
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)
UDES <- fread("UDES_m.csv", header=TRUE)
interval <- fread("interval.csv", header = TRUE)
#recode  some variables 
UDES_clean$Veteran <- ifelse(UDES_clean$`Client Veteran Status`=="Yes (HUD)", 1, 0)
UDES_clean$Insurance <- ifelse(UDES_clean$`Covered by Health Insurance(4376)`=="Yes (HUD)", 1, 0)
#veteran status
ggplot(UDES_clean, aes(Veteran))+geom_bar()
ggsave(filename = "vplot.png")
ggplot(interval, aes(interval))+geom_bar()

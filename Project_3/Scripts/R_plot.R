setwd("/Users/a11/Documents/GitHub/bios611-projects-fall-2019-fangcao730/Project_3/Data")
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)
UDES_clean <- fread("UDES_clean.csv", header = TRUE)
#recode
UDES_clean$Veteran <- ifelse(UDES_clean$`Client Veteran Status`=="Yes (HUD)", 1, 0)
plot <- ggplot(UDES_clean, aes(Veteran))+geom_bar()

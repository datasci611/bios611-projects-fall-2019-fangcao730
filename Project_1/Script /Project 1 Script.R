setwd("/Users/a11/Desktop/BIOS 611")
rm(list=ls())
library(readr)
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(tidyr)
raw <- read_tsv("UMD_Services.tsv")
raw$Date <- as.Date(raw$Date, "%m/%d/%Y")
#See how Number of clients served vary by month and year 
monthyear<-raw %>%
  separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019 & year >=1997) %>%group_by(year, month)%>%
  summarise(count = n_distinct(`Client File Number`))
##plot the count of clients by month and year
ggplot(monthyear, aes(month, year, fill=count)) + geom_tile()+ theme_classic()

##This part is counting type of services provided year each based on number of distinct clients served 
#x counting the hygiene product service overtime: the number of people who came in for this service every year (regardless of how many times)
 x <-raw %>%
  separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
   drop_na(`Hygiene Kits`) %>% filter(`Hygiene Kits`>0)%>% select(`Hygiene Kits`, `Client File Number`, year)%>%
   group_by(year)%>% filter(year>=1997)%>%summarise(hygiene = n_distinct(`Client File Number`))
#y is counting bus ticket service overtime 
  y <-raw %>%
   separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
   drop_na(`Bus Tickets (Number of)`) %>% filter(`Bus Tickets (Number of)`>0)%>% select(`Bus Tickets (Number of)`, `Client File Number`, year)%>%
   group_by(year)%>% filter(year>=1997)%>%summarise(bustix = n_distinct(`Client File Number`))
#f is counting the food services overtime 
  f <-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
    drop_na(`Food Pounds`) %>% filter(`Food Pounds`>0)%>% select(`Food Pounds`, `Client File Number`, year)%>%
    group_by(year)%>% filter(year>=1997)%>%summarise(foodpounds = n_distinct(`Client File Number`))
#m is counting the financial service overtime
  m <-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
    drop_na(`Financial Support`) %>% filter(`Financial Support`>0)%>% select(`Financial Support`, `Client File Number`, year)%>%
    group_by(year)%>% filter(year>=1997)%>%summarise(financialassistance = n_distinct(`Client File Number`))
#c is counting the clothing service 
  c <-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
    drop_na(`Clothing Items`) %>% filter(`Clothing Items`>0)%>% select(`Clothing Items`, `Client File Number`, year)%>%
    group_by(year)%>% filter(year>=1997)%>%summarise(clothing = n_distinct(`Client File Number`))
#s is counting the school kit service 
  s <-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%group_by(`Client File Number`)%>% 
    drop_na(`School Kits`) %>% filter(`School Kits`>0)%>% select(`School Kits`, `Client File Number`, year)%>%
    group_by(year)%>% filter(year>=1997)%>%summarise(school = n_distinct(`Client File Number`))
  #Number of unique client values for each year
  unique<-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019 & year >=1997) %>%group_by(year)%>%
    summarise(all = n_distinct(`Client File Number`))
  #z is counting diaper service overtime and then merged with all the services  
  z<-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019) %>%
    drop_na(`Diapers`)%>% filter(`Diapers` > 0) %>% select(`Diapers`, `Client File Number`, year)%>%
    group_by(year)%>% filter(year>=1997)%>%summarise(diapers = n_distinct(`Client File Number`))%>% full_join(unique, by='year') %>%
    full_join(y, by='year')%>% full_join(x, by='year')%>% full_join(f, by='year')%>% full_join(m, by='year')%>%full_join(c, by='year')%>%full_join(s, by='year')
  #Gather the services and put them in one column and other for quantity of service (ignoring the quantitiy for this graph)
  total <- z%>% gather(bustix, diapers, hygiene, financialassistance, foodpounds, school, key='Service', value='Quantity') %>% select("Service", "Quantity", "year")
  #Graphs  showing type of service people come in for each year 
  ggplot(total, aes(fill=Service, y=Quantity, x=year)) + 
    geom_bar(position="stack", stat="identity")+ xlab("Year") + ylab("Number of People/Families")
  
  
  #This section calculates the frequency and time intervals between each service for clients who come in more than once 
#freq counts the number of times each client came in over the years and then filter for those who came in more than once
  freq<-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019 & year >=1997) %>%group_by(`Client File Number`)%>%
    tally()%>%filter(n>1)
  names(freq)[1]<-"ID"
  #id is provides years and client ids for each client who came in service 
id<-raw %>%
    separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019 & year >=1997) %>%
    select(`Client File Number`, "year")%>%group_by(`Client File Number`)
  names(id)[1]<-"ID"
#this merge provides a table that records the year each person came in and how many times they came in
freqid <- merge(freq, id, by="ID")
freqid$year <- as.numeric(freqid$year)
freqid%>%aggregate(year ~ ID , freqid, FUN = function(i)max(i))
#byfreq counts the time interval each client came in for service (The year they last came in - the year they first came in) 
byfreq<- aggregate(year ~ ID , freqid, FUN = function(i)max(i) - min(i))
names(byfreq)[2]<-"Interval"
#Last year table lists the year each client last received service and joined by the time interval each person stayed in service above (byfreq) 
lastyear <-  aggregate(year ~ ID , freqid, FUN = function(i)max(i))%>%full_join(freq, by="ID")%>%full_join(byfreq, by="ID")
#plots the number of times people come in for service over the years for how long they stayed in service 
ggplot(lastyear, aes(year, Interval, fill=n)) + geom_tile()+ theme_classic()+scale_fill_distiller(palette = "Spectral")+
  ggtitle("Service Intervals by the last year clients came in for service")
## Total service provided each year
#(not doing them altogether because of all the NAs everywhere and dropped the services that were stopped midway
food <- raw%>% separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<2019 & year >=2005)%>% 
  drop_na( `Food Pounds`) %>% filter(`Food Pounds`>0 & `Food Pounds`<=1800)%>% select(`Food Pounds`, year, month)%>%
  group_by(year)%>%summarise(food=sum(`Food Pounds`))
diapers <- raw%>% separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<=2019 & year >=1997)%>% 
  drop_na(`Diapers`) %>% filter(`Diapers`>0 &`Diapers`<200)%>% select(`Diapers`, year, month)%>%
  group_by(year)%>%summarise(diapers=sum(`Diapers`))
cloth <- raw%>% separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<2019 & year >=2001)%>% 
  drop_na(`Clothing Items`) %>% filter(`Clothing Items`>0)%>% select(`Clothing Items`, year, month)%>%
  group_by(year)%>%summarise(clothing=sum(`Clothing Items`))
hygiene <- raw%>% separate(Date, sep="-", into = c("year", "month", "day"))%>% filter(year<2019 & year >=2002)%>% 
  drop_na(`Hygiene Kits`) %>% filter(`Hygiene Kits`>0)%>% select(`Hygiene Kits`, year, month)%>%
  group_by(year)%>%summarise(hygiene=sum(`Hygiene Kits`))
#plotting service by amount over the years arranged into one plot 
p1 <- ggplot(food, aes(x=year, y=food, group=1))+geom_point(color="RoyalBlue")+geom_smooth(se=FALSE)
 p2 <- ggplot(cloth, aes(x=year, y=clothing, group=1))+geom_point(color="RoyalBlue")+geom_smooth(se=FALSE)
 p3 <- ggplot(diapers, aes(x=year, y=diapers, group=1))+geom_point(color="RoyalBlue")+geom_smooth(se=FALSE)
 p4 <- ggplot(hygiene, aes(x=year, y=hygiene, group=1))+geom_point(color="RoyalBlue")+geom_smooth(se=FALSE)
 grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)

 







 
 
 
 
 
 
 
  
#setwd("/Users/a11/Documents/GitHub/bios611-projects-fall-2019-fangcao730/Project_2")
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
umd <-read_tsv("https://raw.githubusercontent.com/datasci611/bios611-projects-fall-2019-fangcao730/master/Project_2/Data/UMD_Services.tsv")
#umd <- read_tsv("/Users/a11/Documents/GitHub/bios611-projects-fall-2019-fangcao730/Project_2/UMD_Services.tsv")
colnames(umd) <- c("Date", "ID", "Client File Merge", "BusTickets", "Note", "FoodPersons","Foodpounds", "Clothing", 
                   "Diapers", "SchoolKits", "HygieneKits", "Ref", "Financial", "Bill", "Payer","1","2","3")
umd$Date <- as.Date(umd$Date, "%m/%d/%Y")
total <-umd[-c(16,17,18)]%>%separate(Date, sep="-", into = c("year", "month", "day"))
#sum service annually by input year range 
sumservice <- function(maxyear, minyear, input){
  yearly <- total%>% filter(year<maxyear & year>=minyear)%>%select(year, matches(input))%>%drop_na()%>%
    filter(get(input)<1800)%>%drop_na()%>%group_by(year)%>%summarise(service=sum(get(input)))
  yearly
}
#timeclient counts number of distinct clients receiving the input service at given years of input
yearclient <- function(maxyear, minyear, input){
  clients <- total%>% filter(year<maxyear & year>=minyear)%>%select(year, matches(input),ID)%>%drop_na()%>%
    filter(get(input)<1800)%>%drop_na()%>%group_by(year)%>%summarise(client=n())  
  clients
}
#shows  line graph with geom_points, easier for customers to use the hover function
linegraph <- function(df, x, y) {
  p <- ggplot(df, aes(x=x, y=y, group=1))+geom_point(color="RoyalBlue")+geom_line(color="RoyalBlue")+geom_point()
  p
}
#wordcloud plot
wordplot <- function(input) {
    textsb <- total%>% select(matches(input))%>%drop_na()
  docs <- Corpus(VectorSource(textsb))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  #separate to space by "/n" which shows up in the matrix sometimes
  docs <- tm_map(docs, toSpace, "\n")
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  set.seed(1234)
  wd<- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                 max.words=200, random.order=FALSE, rot.per=0.35, 
                 colors=brewer.pal(8, "Dark2"))
  wd
}
#ask for an year input and return the table with number of distinct families and the month
hhyear <- function(input) {
  household <- total%>% filter(year==input)%>%
    select(year, month, ID, Foodpounds, FoodPersons)%>%drop_na()%>%filter(FoodPersons>=2)%>%
    group_by(month)%>%summarise(n=n_distinct(ID))
  household
}

#ask for an integrer input and return the table with each families' IDs and the month they came in for help of that given year
#this is for calculating average size of families etc.
fam <- function(input) {
  families <- total%>% filter(year==input)%>%
    select(year, month, ID, Foodpounds, FoodPersons)%>%drop_na()%>%filter(FoodPersons>=2)%>%
    group_by(month)
  families
}
#bar chart of number of families served each year (by input) by month
hhgraph <- function(df,x,y) {
  ggplot(df,aes(x=x, y=y))+geom_bar(stat = "identity", fill="cornflowerblue")+xlab("Month")+
    ylab("Number of distinct Family served food")+ggtitle("Number of households served food of selected year")
}




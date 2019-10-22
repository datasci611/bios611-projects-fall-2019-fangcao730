setwd("/Users/a11/Documents/Github/bios611-projects-fall-2019-fangcao730/Project_2")
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
umd <-read_tsv("UMD_Services.tsv")
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
#timeclient counts number of distinct clients receiving the input service at given
yearclient <- function(maxyear, minyear, input){
  clients <- total%>% filter(year<maxyear & year>=minyear)%>%select(year, matches(input),ID)%>%drop_na()%>%
    filter(get(input)<1800)%>%drop_na()%>%group_by(year)%>%summarise(client=n())  
  clients
}
linegraph <- function(df, x, y) {
  p <- ggplot(df, aes(x=x, y=y, group=1))+geom_point(color="RoyalBlue")+geom_smooth(se=TRUE)+
    geom_point()+xlab("year")
  p
}

wordplot <- function(input) {
    textsb <- total%>% select(matches(input))%>%drop_na()
  docs <- Corpus(VectorSource(textsb))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
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




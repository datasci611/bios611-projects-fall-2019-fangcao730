setwd("/Users/a11/Documents/Github/bios611-projects-fall-2019-fangcao730/Project_2")
source("helper_function.R",local=F)
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidyr)
ui <- fluidPage( 
  titlePanel("UMD Service Visualization"),
  fluidRow(
  sidebarLayout( 
    sidebarPanel( 
      sliderInput("year", "Year:", 
                  min = 1990, max = 2019, 
                  step = 1, value=c(1998,2019)
      ),
      selectInput("variable", label = h3("Select the variable you want to explore"), 
                  choices = list("Food pounds" = "Foodpounds", 
                                 "Hygiene Kits" = "HygieneKits", 
                                 "Clothing" = "Clothing",
                                 "Bus Tickets"="BusTickets",
                                 "School Kits"="SchoolKits",
                                 "Financial Assistance"="Financial",
                                 "Diapers"="Diapers")),
      selectInput("selection", label = h3("Select the text variable you want to explore"),
                    choices = list("Bills Paid"="Bill",
                                   "Notes"="Note"))
    ),
    mainPanel( 
      column(12,
             verbatimTextOutput("text"),
             br(),
             br(),
             p("Plot 1: Amount of Service over time") ),
      column(12, 
             plotOutput("serviceplot"),
             p("Plot 2: Number of clients served of this product over time"),
             plotOutput("clientplot")
      ),
      p("Plot 3: Texts input: how are people assisted?"),
      plotOutput("words")
    ) 
  )
  )
)
server <- function(input, output) {
  #plot no. of service distributed against year
  output$serviceplot <- renderPlot({
    minyear <- input$year[1]
    maxyear <- input$year[2]
    df <- sumservice(maxyear, minyear, input$variable)
    linegraph(df,df$year,df$service)+ylab("Numer of products served")
  })
  #plot no. of clients against year receiving the input service
  output$clientplot <- renderPlot({ 
    minyear <- input$year[1]
    maxyear <- input$year[2]
    df <- yearclient(maxyear, minyear, input$variable)
    linegraph(df, df$year, df$client)+ylab("Number of Clients served")
  })
  output$words <- renderPlot({ 
   wordplot(input$selection)
  })
  
}
shinyApp(ui = ui, server = server)  

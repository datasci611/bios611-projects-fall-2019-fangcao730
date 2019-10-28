#setwd("https://github.com/datasci611/bios611-projects-fall-2019-fangcao730/tree/master/Project_2")
source("helper_function.R",local=F)
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidyr)
ui<- navbarPage("UMD service visualization",
                #introduction/background panel
                tabPanel("Background", 
                  strong(p("Data Source:"),style = "font-size:25px"),
                  p("The data comes from the Urban Ministries of Durham, which provides different services to homeless people over the years. 
                   This interface helps visualize the amount of different services provided as well as 
                    the number of clients it reached."), 
                  img(src='SoupCan.jpg', align = "right",width=300, height=300),
                  uiOutput("tab"),
                  strong(p("How to use this interface", style = "font-size:25px")),
                  tags$div(tags$ul(
                    tags$li("the first of which called Service by year shows the number of
                    people receiving this service or the amount of service given out each year by selected time range."),
                    tags$li("The second part called Text exploration visualizes the attached assistance notes 
                            into a word cloud"),
                    tags$li("The third part shows the size and number of households receiving food service 
                            over the years"),
                    tags$li("Limitation: this interface only provides visualization for years between 1998 
                            and 2019. The years earlier than 1998 have inconsistent data quality and sparse
                            data points and therefore clipped."),
                    tags$li("Here's for questions: fangcao@live.unc.edu")),  style = "font-size: 14px")
                   
                ),
                #this panel shows the amount of service and number of clients by selected type of service and year range
                tabPanel("Service by year",
                         fluidPage( 
                           titlePanel("Annual Service"),
                       #slider to select year range
                               sidebarPanel(
                                 sliderInput("year", "Slide the buttons to select Year range you want to explore:", 
                                             min = 1990, max = 2019, 
                                             step = 1, value=c(1998,2019)
                                 ),
                                 #select variable to be explored 
                                 selectInput("variable", label = h3("Select the variable you want to explore"), 
                                             choices = list("Food pounds" = "Foodpounds", 
                                                            "Hygiene Kits" = "HygieneKits", 
                                                            "Clothing" = "Clothing",
                                                            "Bus Tickets"="BusTickets",
                                                            "School Kits"="SchoolKits",
                                                            "Financial Assistance"="Financial",
                                                            "Diapers"="Diapers"))
                                 ),
                               mainPanel( 
                                 p("This tab serves to visualize the amount of selected service given out and the number of clients 
                                   served this service over the year range you want to explore. The black dots represent the total number 
                                   of products given out each year (for e.g. food in pounds or clothing) while the blue line is there to help
                                     you better visualize the trend in the data overtime. 
                                  "),
                                 p("To obtain specific numbers: 
                                  Hover over the black dots on the plots to see roughly the amount of total service/clients
                                   each year displayed in the grey bar below"),
                                        p("Plot 1: Amount of Service over time"),
                                 #hover  returns the value of y that the mouse is over 
                                        plotOutput("serviceplot", hover = "plot_hover"),
                                        verbatimTextOutput("info"),
                                        p("Plot 2: Number of clients served of this product over time"),
                                        plotOutput("clientplot", hover = "plot2_hover"),
                                        verbatimTextOutput("info2")
                           )
                         
                )
              ),#this tab explores the string variables in the dataset: notes and types of bills paid
                tabPanel("Text Exploration",
                         fluidPage(
                           fluidRow(
                               sidebarPanel ( p("From this drop down you can choose the text note/records to visualize
                                                what types of services or types of bills stand out"),
                                 selectInput("selection", label = h3("Select the text variable you want to explore"),
                                             choices = list("Bills Paid"="Bill",
                                                            "Notes"="Note"))
                               )
                             , mainPanel (
                               p("Texts input: how are people assisted?"),
                               plotOutput("words"),
                               p("Sometimes when people were assisted (for example, when their bills were paid) there 
                                 are notes or record of what types of bills were paid. Words are graphed as word cloud 
                                 in this graph above. Key words are clothing, food and electricity bills")
                             )
                           )
                         )
                         #this tab breaks down the number of families for each select input year
                ), tabPanel("Food service for Households",
                            fluidPage(
                              fluidRow(
                                sidebarPanel(
                                  numericInput("yearinput", "Year you want to explore:", 2019, min = 1998, max = 2019),
                                  p("click the up and down above or enter the year you want to obtain information, 
                                    or simply enter the year")
                                ),
                                mainPanel(
                                  p("This interactive plot aims to show how many distinct clients (families) were served
                                    in the select input year, to show how many families were reached each year and each month.
                                   The idea of Families is defined by food provided for more than one person.
                                    For individual usage please visit the second tab."),
                                  p("The same family coming back for service in the same month would only be counted once, 
                                    this plot is meant to show coverage instead of count."),
                                  
                                  plotOutput("hh"),
                                  verbatimTextOutput("averagesize")
                                )
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
  output$info<- renderText({
    paste0("Amount of Service=", input$plot_hover$y)
  })
  #plot no. of clients against year receiving the input service
  output$clientplot <- renderPlot({ 
    minyear <- input$year[1]
    maxyear <- input$year[2]
    df <- yearclient(maxyear, minyear, input$variable)
    linegraph(df, df$year, df$client)+ylab("Number of Clients served")
  })
  #hover values
  output$info2 <- renderText({
    paste0("Number Of Clients=", input$plot2_hover$y)
  })
  #paste UMD 's url 
  url <- a("Urban Ministries Durham: Food and Clothing Pantries", href="http://umdurham.org/what-we-do/food-pantry.html")
  output$tab <- renderUI({
    tagList("Link to UMD:", url)
  })
  #word cloud for the string variables 
  output$words <- renderPlot({ 
   wordplot(input$selection)
  })
  #plot the number of families served each year 
  output$hh <- renderPlot({
    household <- hhyear(input$yearinput)
    hhgraph(household, household$month, household$n)
  })
  #calculates the average size of families served each year 
  output$averagesize <- renderText({
    household <- hhyear(input$yearinput)
    families <- fam(input$yearinput)
    paste("The average number of families served in", input$yearinput, "is", mean(household$n), 
          "and the average size of families served this year is", mean(families$FoodPersons)
          )
  })
}
shinyApp(ui = ui, server = server)  

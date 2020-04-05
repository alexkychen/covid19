library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(reshape2)

URL_US_state <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
PAT_US_stpop <- "USpopulation.csv"

US_covid_data <- read.csv(URL_US_state, stringsAsFactors = T)
US_pop_data <- read.csv(PAT_US_stpop, stringsAsFactors = T)

#check if number of states/territories equal 
if(length(levels(US_covid_data$state)) != length(levels(US_pop_data$State))){
  stop("Number of states not equal in data sets")
}
#check state/territory names
StateNames <- levels(US_pop_data$State)
unmatch_statename <- NULL
for(i in levels(US_covid_data$state)){
  if(i %in% StateNames == F){
    unmatch_statename <- c(unmatch_statename, i)
  }
}
if(is.null(unmatch_statename) != T){
  stop(paste0("state name not matched:: ",paste(unmatch_statename, collapse=", ")))
}

ui <- fluidPage(
  title = "COVID-19 Dashboard by Alex Chen",
  
  h1('Time course of COVID-19 cases'),
  
  fluidRow(
    tabsetPanel(
      tabPanel(title = "U.S.",
               column(width = 6,
                      DTOutput("US_tb"),
                      helpText("Pop.Density unit: people/mile-square")),
               column(width = 6,
                      )),
      tabPanel(title = "Global",
               column(width = 6,
               ),
               column(width = 6,
               ))
    )
  )
)

server <- function(input, output){
  output$US_tb <- renderDT(cars, server = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
URL_Deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
URL_Recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

data_confirmed <- read.csv(URL_Confirmed)
data_deaths <- read.csv(URL_Deaths)
data_recovered <- read.csv(URL_Recovered)

countryList <- levels(data_confirmed$Country.Region)
countryList <- c("None", countryList)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("History of CONVID-19 cases by country"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange", "Date range", start="2020-01-22", format="mm-dd"),
            selectInput("country1", "Select a country", choices=countryList, selected="Taiwan*"),
            selectInput("country1", "Select a country", choices=countryList, selected="China"),
            selectInput("country1", "Select a country", choices=countryList, selected="None"),
            selectInput("country1", "Select a country", choices=countryList, selected="None"),
            actionButton("plot","Make plots"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("confirmed_accu"),
           plotOutput("deaths_accu"),
           plotOutput("mortality")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$confirmed_accu <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
URL_Deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
URL_Recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

data_confirmed <- read.csv(URL_Confirmed)
data_deaths <- read.csv(URL_Deaths)
data_recovered <- read.csv(URL_Recovered)

#Get date
date <- as.Date("2020-01-22") + 0:(ncol(data_confirmed)-5)

#Get country
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
            helpText("Earliest report date is January 22nd, 2020"),
            helpText(""),
            selectInput("country1", "Select a country", choices=countryList, selected="Taiwan*"),
            selectInput("country2", "Select a country", choices=countryList, selected="China"),
            selectInput("country3", "Select a country", choices=countryList, selected="None"),
            selectInput("country4", "Select a country", choices=countryList, selected="None"),
            actionButton("go",label="Make plots")
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

  res <- eventReactive(input$go, {
    
    #get selected country name
    countries <- c(input$country1,input$country2,input$country3,input$country4)
    
    #create empty data frame
    df <- data.frame(matrix(ncol=0, nrow=0), stringsAsFactors=T)
    
    #compile data for confirmed cases
    for(i in countries){
      if(i != "None"){
        #get confirmed cases
        one_country <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
        case_sum <- colSums(one_country)
        #get death cases
        one_country_death <- subset(data_deaths, Country.Region==i, select=5:ncol(data_deaths))
        death_sum <- colSums(one_country_death)
        
        if(i == "Taiwan*"){
          one_df <- data.frame(date=date, cases=case_sum, death=death_sum, country=rep("Taiwan", length(case_sum)), stringsAsFactors=T)
        }else{
          one_df <- data.frame(date=date, cases=case_sum, death=death_sum, country=rep(i, length(case_sum)), stringsAsFactors=T)
        }
        df <- rbind(df, one_df)
      }
    }
    
    #make confirmed case plot
    plot_confirmed <- ggplot(data=df, aes(x=date, y=cases, color=country))+
      geom_point(shape=19)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(input$daterange[1], input$daterange[2]))+
      ggtitle("Accumulated Cases of Confirmed CONVID-19")+
      xlab("Date")+ylab("Number of cases")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=16),legend.text=element_text(size=16))
    
    #make death case plot
    plot_deaths <- ggplot(data=df, aes(x=date, y=death, color=country))+
      geom_point(shape=19)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(input$daterange[1], input$daterange[2]))+
      ggtitle("Accumulated Deaths")+
      xlab("Date")+ylab("Number of deaths")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=16),legend.text=element_text(size=16))
    
    #output results
    list(plot_confirmed=plot_confirmed, plot_deaths=plot_deaths)
    
  })#for res <- eventReactive
  
  #render plot
  output$confirmed_accu <- renderPlot({
    res()$plot_confirmed
  })
  
  output$deaths_accu <- renderPlot({
    res()$plot_deaths
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

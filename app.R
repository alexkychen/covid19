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
#data_recovered <- read.csv(URL_Recovered)
data_population <- read.csv("population.csv")

#Get date
date <- as.Date("2020-01-22") + 0:(ncol(data_confirmed)-5)

#Get country
countryList <- levels(data_confirmed$Country.Region)
countryList <- c("None", countryList)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("History of CONVID-19 cases by country"),
    helpText("by Alex Chen"),
    
    fluidRow(
      column(width = 4,
         sidebarPanel( width=14,
           dateRangeInput("daterange", "Select date range", start="2020-01-22", format="mm-dd"),
           helpText("Earliest report date is January 22nd, 2020"),
           helpText(""),
           selectInput("country1", "Select a country", choices=countryList, selected="Taiwan*"),
           selectInput("country2", "Select a country", choices=countryList, selected="China"),
           selectInput("country3", "Select a country", choices=countryList, selected="Japan"),
           selectInput("country4", "Select a country", choices=countryList, selected="Korea, South"),
           actionButton("go",label="Show plots")
         ),
         p("Data updated by Johns Hopkins University's Center for Systems Science and Engineering ",a(href="https://github.com/CSSEGISandData/COVID-19","(Github repo)")),
         p("Taiwan*: Data from Taiwan are independently collected by Taiwan CDC and should not mix with data from China. TAIWAN is an INDEPENDENT country, not part of mainland China whatsoever.")
      
      ),
      column(width = 8,
         plotOutput("confirmed_accu"),
         plotOutput("confirmed_percapita"),
         plotOutput("deaths_accu"),
         plotOutput("mortality")
      )
    )#close for fluidRow
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
        #get pop size 
        one_population <- subset(data_population, name==i)
        #get confirmed cases
        one_country <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
        case_sum <- colSums(one_country)
        #calculate cases per capita 
        case_percapita <- case_sum / one_population$popsize2019
        #get death cases
        one_country_death <- subset(data_deaths, Country.Region==i, select=5:ncol(data_deaths))
        death_sum <- colSums(one_country_death)
        
        if(i == "Taiwan*"){
          one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, 
                               death=death_sum, country=rep("Taiwan", length(case_sum)), stringsAsFactors=T)
        }else{
          one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, 
                               death=death_sum, country=rep(i, length(case_sum)), stringsAsFactors=T)
        }
        df <- rbind(df, one_df)
      }
    }
    
    #make confirmed case plot
    plot_confirmed <- ggplot(data=df, aes(x=date, y=cases, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(input$daterange[1], input$daterange[2]))+
      ggtitle("Accumulated confirmed cases")+
      xlab("Date")+ylab("Number of cases")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=16),legend.text=element_text(size=16))
    
    #make confirmed case per capita plot
    plot_confirmed_pcap <- ggplot(data=df, aes(x=date, y=case_pcap, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(input$daterange[1], input$daterange[2]))+
      ggtitle("Accumulated confirmed cases divided by country population size")+
      xlab("Date")+ylab("Cases per capita")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=16),legend.text=element_text(size=16))
    
    #make death case plot
    plot_deaths <- ggplot(data=df, aes(x=date, y=death, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(input$daterange[1], input$daterange[2]))+
      ggtitle("Accumulated Deaths")+
      xlab("Date")+ylab("Number of deaths")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_text(size=16),legend.text=element_text(size=16))
    
    #output results
    list(plot_confirmed=plot_confirmed, plot_confirmed_pcap=plot_confirmed_pcap, plot_deaths=plot_deaths)
    
  })#for res <- eventReactive
  
  #render plot
  output$confirmed_accu <- renderPlot({
    res()$plot_confirmed
  })
  
  output$confirmed_percapita <- renderPlot({
    res()$plot_confirmed_pcap
  })
  
  output$deaths_accu <- renderPlot({
    res()$plot_deaths
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

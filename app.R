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
data_population <- read.csv("population.csv")

data_all <- list(data_confirmed, data_deaths, data_recovered)
#check data dimension and NA data
dfsize <- dim(data_confirmed)
for(df in data_all){
  #check no. of row
  if(nrow(df) != dfsize[1]){
    cat("Error: Imported data frame no. of row not identical")
  }
  #check no. of column
  if(ncol(df) != dfsize[2]){
    cat("Error: Imported data frame no. of column not identical ")
  }
}

#check if last column data is NA (or more than 200 data point is NA), if so, remove it
if(sum(is.na(data_confirmed[,ncol(data_confirmed)])) > 200){
  data_confirmed <- data_confirmed[,1:ncol(data_confirmed)-1]
  if(ncol(data_deaths) > ncol(data_confirmed)){
    data_deaths <- data_deaths[,1:nol(data_deaths)-1]
  }
  if(ncol(data_recovered) > ncol(data_confirmed)){
    data_recovered <- data_recovered[,1:ncol(data_recovered)-1]
  }
}
#get date
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
           dateRangeInput("daterange", "Select date range", start="2020-01-22", end=date[length(date)], format="mm-dd"),
           helpText("Earliest report date is January 22nd, 2020"),
           helpText(""),
           selectInput("country1", "Select a country", choices=countryList, selected="China"),
           selectInput("country2", "Select a country", choices=countryList, selected="US"),
           selectInput("country3", "Select a country", choices=countryList, selected="Italy"),
           selectInput("country4", "Select a country", choices=countryList, selected="Taiwan*"),
           actionButton("go",label="Show Me")
         ),
         p("Taiwan*: Data from Taiwan are independently collected by Taiwan CDC, and the country is still not a member of WHO."),
         p("CONVID-19 data updated by Johns Hopkins University's Center for Systems Science and Engineering ",
           a(href="https://github.com/CSSEGISandData/COVID-19","(Github repo)"), ". Country population size is based on 2019 data ",
           a(href="https://worldpopulationreview.com/","(worldpopulationreview)")),
         p("Powered by R Shiny. ",a(href="https://github.com/alexkychen/convid19","source code of this interface"))
      ),
      column(width = 8,
         strong(textOutput("summary_caption")),
         tableOutput("summary_table"),
         plotOutput("confirmed_accu"),
         plotOutput("confirmed_percapita"),
         plotOutput("deaths_accu")
        
      )
      
    )#close for fluidRow
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  res <- eventReactive(input$go, {
    
    #start progress bar
    withProgress(message="Start analyses",{  
    
    #get selected country name
    countries <- c(input$country1,input$country2,input$country3,input$country4)
    
    #select date range
    start_date <- input$daterange[1]
    end_date <- input$daterange[2]
    
    #create empty data frame
    df <- data.frame(matrix(ncol=0, nrow=0))#for all plots
    df_summary <- data.frame(matrix(ncol=0, nrow=0))#for summary table
    
    #compile data for confirmed cases
    for(i in countries){
      if(i != "None"){
        
        #get pop size 
        one_population <- subset(data_population, name==i)
        
        #get confirmed cases
        one_country <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
        case_sum <- colSums(one_country)#sum columns for a country that has multiple states/provinces
        
        #calculate cases per capita 
        case_percapita <- (case_sum / one_population$popsize2019)*100000
        
        #get death cases
        one_country_death <- subset(data_deaths, Country.Region==i, select=5:ncol(data_deaths))
        death_sum <- colSums(one_country_death)
        
        #create data frame for one country 
        if(i == "Taiwan*"){
          one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, 
                               death=death_sum, country=rep("Taiwan", length(case_sum)))
        }else{
          one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, 
                               death=death_sum, country=rep(i, length(case_sum)))
        }
        
        #subset data for selected last date 
        df_summary_one <- subset(one_df, date==end_date, select=c(country, cases, case_pcap, death))
        
        #add data together
        df <- rbind(df, one_df)
        df_summary <- rbind(df_summary, df_summary_one)
      }
    }#end for loop
    
    #create caption for each table
    caption1 <- paste("Summary as of ", end_date)

    
    #make confirmed case plot
    plot_confirmed <- ggplot(data=df, aes(x=date, y=cases, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      ggtitle("Accumulated Confirmed Cases")+
      xlab("Date")+ylab("Number of cases")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #make confirmed case per capita plot
    plot_prevalence <- ggplot(data=df, aes(x=date, y=case_pcap, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      labs(title="Prevalence Rate", subtitle="(confirmed cases / country population size)",
           x="Date", y="Cases per capita")+
      #ggtitle("Prevalence rate (confirmed cases / country population size)")+
      #xlab("Date")+ylab("Cases per capita")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #make death case plot
    plot_deaths <- ggplot(data=df, aes(x=date, y=death, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      ggtitle("Accumulated Deaths")+
      xlab("Date")+ylab("Number of deaths")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #adjust summary table format
    df_summary$cases <- format(df_summary$cases, digits=1)
    df_summary$case_pcap <- format(df_summary$case_pcap, digits=3)
    df_summary$death <- format(df_summary$death, digits=1)
    
    #rename column name of summary table
    names(df_summary) <- c("Country", "Total cases", "Prevalence rate", "Total deaths")
  
    #output results
    list(plot_confirmed=plot_confirmed, plot_prevalence=plot_prevalence, plot_deaths=plot_deaths, df_summary=df_summary, caption1=caption1)
  
    })#end of withProgress
  
  })#for res <- eventReactive
  
  #render caption and table
  output$summary_caption <- renderText({
    res()$caption1
  })
  
  output$summary_table <- renderTable({
    res()$df_summary
  },striped=T)
  
  #render plot
  output$confirmed_accu <- renderPlot({
    withProgress(message="Making 1st plot",{
      res()$plot_confirmed
    })
  })
  
  output$confirmed_percapita <- renderPlot({
    withProgress(message="Making 2nd plot",{
      res()$plot_prevalence
    })
  })
  
  output$deaths_accu <- renderPlot({
    withProgress(message="Making 3rd plot",{
      res()$plot_deaths
    })
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

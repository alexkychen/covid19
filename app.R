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
    titlePanel("Time course of COVID-19 epidemiological characteristics by country"),
    helpText("developed by Alex Chen"),
    
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
         p("COVID-19 data are updated by Johns Hopkins University's ", 
           a(href="https://github.com/CSSEGISandData/COVID-19","Center for Systems Science and Engineering"),
           ", and country population size is based on ",a(href="https://worldpopulationreview.com/","2019 global data.")),
         p("Powered by R Shiny. | ",a(href="https://github.com/alexkychen/covid19","Source code of interface"),
           " | ",a(href="https://github.com/alexkychen/covid19/issues","Report issue"))
      ),
      column(width = 8,
         h4(textOutput("summary_caption")),
         textOutput("summary_unit"),
         tableOutput("summary_table"),

         plotOutput("confirmed_accu"),
         plotOutput("confirmed_percapita"),
         plotOutput("deaths_accu"),
         plotOutput("dailycases")
        
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
    df_table <- data.frame(matrix(ncol=0, nrow=0))#for summary table
    
    #compile data for confirmed cases
    for(i in countries){
      if(i != "None"){
        
        #get population size
        country_data <- subset(data_population, name==i)
        country_population <- country_data$popsize2019
        country_pop10E6 <- country_population/1000000
        
        #get accumulative confirmed cases
        acc_confirmed_data <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
        #sum columns for multiple regions(rows)
        acc_confirmed_sum <- colSums(acc_confirmed_data)
        
        #get new cases of each day
        new_case_day <- acc_confirmed_sum[2:length(acc_confirmed_sum)] - acc_confirmed_sum[1:length(acc_confirmed_sum)-1]
        new_case_day <- c(NA, new_case_day) #add NA data to first date for 2020-1-22
        
        #get accumulative recovered cases
        acc_recovered_data <- subset(data_recovered, Country.Region==i, select=5:ncol(data_recovered))
        #sum columns for multiple regions(rows)
        acc_recovered_sum <- colSums(acc_recovered_data)
        
        #calculate prevalence rate
        preval <- (acc_confirmed_sum - acc_recovered_sum) / country_pop10E6
        
        #get accumulative deaths
        acc_deaths_data <- subset(data_deaths, Country.Region==i, select=5:ncol(data_deaths))
        acc_deaths_sum <- colSums(acc_deaths_data)
        
        #get new deaths of each day
        new_death_day <- acc_deaths_sum[2:length(acc_deaths_sum)] - acc_deaths_sum[1:length(acc_deaths_sum)-1]
        new_death_day <- c(NA, new_death_day)
        
        #calculate fatality rate
        fatal <- (acc_deaths_sum / acc_confirmed_sum) * 100
        
        #create data frame
        if(i == "Taiwan*"){
          df_one <- data.frame(country=rep("Taiwan", length(acc_confirmed_sum)), date=date, pop.size=country_pop10E6, 
                               accu.case=acc_confirmed_sum, new.case=new_case_day, prevalence=preval, accu.death=acc_deaths_sum,
                               new.death=new_death_day, fatality=fatal, row.names=NULL)
        }else{
          df_one <- data.frame(country=rep(i, length(acc_confirmed_sum)), date=date, pop.size=country_pop10E6, 
                               accu.case=acc_confirmed_sum, new.case=new_case_day, prevalence=preval, accu.death=acc_deaths_sum,
                               new.death=new_death_day, fatality=fatal, row.names=NULL)
        } 
        #extract data for summary table
        df_table_one <- subset(df_one, date==end_date, select=c(country, pop.size, accu.case, prevalence, accu.death, fatality))
        
        #concatenate data set
        df <- rbind(df, df_one)
        df_table <- rbind(df_table, df_table_one)
      }
    }#end for loop
    
    #create caption for each table
    caption1 <- paste("Summary of epidemiological characteristics as of ", end_date)
    caption2 <- "Population*: million; Prevalence*:cases per million people; Fatality*: %"

    
    #Accumulative confirmed case plot
    plot_confirmed <- ggplot(data=df, aes(x=date, y=accu.case, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      ggtitle("Accumulative Confirmed Cases")+
      xlab("Date")+ylab("Number of cases")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #Prevalence rate plot
    plot_prevalence <- ggplot(data=df, aes(x=date, y=prevalence, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      labs(title="Prevalence Rate", subtitle="[ (confirmed cases - recovered cases) / population size ]",
           x="Date", y="Infected cases per million people")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #Accumulative deaths plot
    plot_deaths <- ggplot(data=df, aes(x=date, y=accu.death, color=country))+
      geom_point(shape=19, size=3)+
      scale_colour_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      ggtitle("Accumulative Deaths")+
      xlab("Date")+ylab("Number of deaths")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    #Daily confirmed cases
    plot_newcase <- ggplot(data=df, aes(x=date, y=new.case, fill=country))+
      geom_bar(stat="identity",position=position_dodge())+
      scale_fill_hue(l=50)+
      scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
      labs(title="Daily Confirmed Cases", x="Date", y="Number of cases")+
      theme_bw()+
      theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
            plot.title=element_text(size=18,face="bold"),
            legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
    
    
    #adjust summary table format
    df_table$pop.size <- format(round(df_table$pop.size, digits=2))
    df_table$accu.case <- format(df_table$accu.case)
    df_table$prevalence <- format(round(df_table$prevalence, digits=2))
    df_table$accu.death <- format(df_table$accu.death)
    df_table$fatality <- format(round(df_table$fatality, digits=2))
    
    #change table column names
    names(df_table) <- c("Country","Population*", "Confirmed cases","Prevalence*", "Deaths","Fatality*")
    
    #output results
    list(df_table=df_table, caption1=caption1, caption2=caption2,
         plot_confirmed=plot_confirmed, plot_prevalence=plot_prevalence, plot_deaths=plot_deaths, plot_newcase=plot_newcase)
  
    })#end of withProgress
  
  })#for res <- eventReactive
  
  #render caption and table
  output$summary_caption <- renderText({
    res()$caption1
  })
  
  output$summary_table <- renderTable({
    res()$df_table
  },striped=T, align="r")
  
  output$summary_unit <- renderText({
    res()$caption2
  })
  
  
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
  
  output$dailycases <- renderPlot({
    withProgress(message="Making 4th plot",{
      res()$plot_newcase
    })
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

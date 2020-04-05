library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(reshape2)

####### Import data ###########
URL_US_state <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
PAT_US_stpop <- "USpopulation.csv"
URL_Global_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
URL_Global_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
PAT_Global_pop <- "population.csv"

US_covid_data <- read.csv(URL_US_state, stringsAsFactors = T)
US_pop_data <- read.csv(PAT_US_stpop, stringsAsFactors = T)
Global_cases <- read.csv(URL_Global_cases, stringsAsFactors = T)
Global_deaths <- read.csv(URL_Global_deaths, stringsAsFactors = T)
Global_pop <- read.csv(PAT_Global_pop, stringsAsFactors = T)

####### Check import data ##########
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
#check if number of column is equal between Global_cases vs. Global_deaths
if(ncol(Global_cases) != ncol(Global_deaths)){
    stop("Dataset column numbers not matched between Global cases and deaths")
}
#check country names between confirmed case and death data
if(length(levels(Global_cases$Country.Region)) != length(levels(Global_deaths$Country.Region))){
    stop("Country name between case and death data not match. Check unmatched country name.")
}
#check global country names
# CountryNames <- levels(Global_pop$name)
# unmatch_countryname <- NULL
# for(j in levels(Global_cases$Country.Region)){
#   if(j %in% CountryNames == F){
#     unmatch_countryname <- c(unmatch_countryname, j)
#   }
# }
# unmatch_countryname


########### Process US data ############
#US table 1 var: state, population, density, cases(cumulative), deaths(cumulative) 
US_tb1 <- data.frame(matrix(ncol=0, nrow=0))
#US table 2 var: state, date, cumu.cases, new.cases, cumu.deaths, new.deaths
US_tb2 <- data.frame(matrix(ncol=0, nrow=0))

for(s in StateNames){
    #extract state population and density (p/mi2) data
    state_pop <- subset(US_pop_data, State==s, select=c(Pop, density))
    #extract state COVID19 data
    state_covid19 <- subset(US_covid_data, state==s, select=c(date, cases, deaths))
    #get the last date of cumulative cases and deaths
    state_covid19_lastest <- state_covid19[nrow(state_covid19),] 
    #state_summary
    state_summary <- data.frame(State=s, Pop.Size=as.numeric(state_pop$Pop), Pop.Density=round(as.numeric(state_pop$density)),
                                Cases=state_covid19_lastest$cases, Deaths=state_covid19_lastest$deaths)
    #append state data
    US_tb1 <- rbind(US_tb1, state_summary)
    
    #get daily new cases
    num_dates <- nrow(state_covid19)
    daily_cases <- state_covid19$cases[2:num_dates] - state_covid19$cases[1:(num_dates-1)]
    daily_cases <- c(NA, daily_cases)
    #get daily new deaths
    daily_deaths <- state_covid19$deaths[2:num_dates] - state_covid19$deaths[1:(num_dates-1)]
    daily_deaths <- c(NA, daily_deaths)
    state_timecourse <- data.frame(State=rep(s, num_dates), Date=as.Date(state_covid19$date), 
                                   Cum.Cases=state_covid19$cases, Daily.Cases=daily_cases,
                                   Cum.Deaths=state_covid19$deaths, Daily.Deaths=daily_deaths)
    #append data
    US_tb2 <- rbind(US_tb2, state_timecourse)
}
#remove row names
row.names(US_tb1) <- NULL
row.names(US_tb2) <- NULL

#get row names in US_tb1 for top 3 cases
top3cases <- tail(sort(US_tb1$Cases),3)
top3index <- as.numeric(rownames(US_tb1[which(US_tb1$Cases >= min(top3cases)),]))

######### Process Global covid-19 data ############
#Global table 1 var: Country, population, density, cases and deaths
Global_tb1 <- data.frame(matrix(ncol=0, nrow=0))
#Global table 2 var: Country, date, cum.cases, new.cases, cum.deaths, new.deaths
Global_tb2 <- data.frame(matrix(ncol=0, nrow=0))

#get date stamp for global covid data
dates <- as.Date("2020-1-22") + 0:(ncol(Global_cases)-5)

CountryNames_case <- levels(Global_cases$Country.Region)
CountryNames_pop <- levels(Global_pop$name)
for(c in CountryNames_case){
    if(c %in% CountryNames_pop){
        #subset country population data
        country_pop <- subset(Global_pop, name==c, select=c("popsize2019", "Density"))
        #get country cumulative cases
        country_cases <- subset(Global_cases, Country.Region==c, select=5:ncol(Global_cases))
        #sum up column for country with multiple regions
        country_cases_sum <- colSums(country_cases)
        #get the latest cumulative cases
        country_cum_cases <- country_cases_sum[length(country_cases_sum)]
        #get daily new cases
        country_daily_cases <- country_cases_sum[2:length(country_cases_sum)] - country_cases_sum[1:length(country_cases_sum)-1]
        country_daily_cases <- c(NA, country_daily_cases)
        
        #get country cumulative deaths
        country_deaths <- subset(Global_deaths, Country.Region==c, select=5:ncol(Global_deaths))
        #combine multiple regions of a country
        country_deaths_sum <- colSums(country_deaths)
        #get the latest cumulative deaths
        country_cum_deaths <- country_deaths_sum[length(country_deaths_sum)]
        #get daily deaths
        country_daily_deaths <- country_deaths_sum[2:length(country_deaths_sum)] - country_deaths_sum[1:length(country_deaths_sum)-1]
        country_daily_deaths <- c(NA, country_daily_deaths)
        
        if(c == "Taiwan*"){
            country_summary <- data.frame(Country="Taiwan", Pop.Size=country_pop$popsize2019, Pop.Density=round(country_pop$Density),
                                          Cases=country_cum_cases, Deaths=country_cum_deaths)
            country_covid <- data.frame(Country=rep("Taiwan", length(dates)), Date = dates, Cum.Cases = country_cases_sum,
                                        Daily.Cases=country_daily_cases, Cum.Deaths=country_deaths_sum, Daily.Deaths=country_daily_deaths)
        }else{
            country_summary <- data.frame(Country=c, Pop.Size=country_pop$popsize2019, Pop.Density=round(country_pop$Density),
                                          Cases=country_cum_cases, Deaths=country_cum_deaths)
            country_covid <- data.frame(Country=rep(c, length(dates)), Date = dates, Cum.Cases = country_cases_sum,
                                        Daily.Cases=country_daily_cases, Cum.Deaths=country_deaths_sum, Daily.Deaths=country_daily_deaths)
        }
        #append summary data to table
        Global_tb1 <- rbind(Global_tb1, country_summary)
        Global_tb2 <- rbind(Global_tb2, country_covid)
    }
}
row.names(Global_tb1) <- NULL
row.names(Global_tb2) <- NULL

#get row names in Global_tb1 for top 5 countries
top5country <- tail(sort(Global_tb1$Cases),5)
top5ctryindex <- as.numeric(rownames(Global_tb1[which(Global_tb1$Cases >= min(top5country)),]))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    title = "COVID-19 Dashboard by Alex Chen",
    h1("Time course of COVID-19 cases"),
    
    tabsetPanel(
        tabPanel(title="U.S.",
                 column(5,
                        #dateRangeInput("daterange", "1. Select date range", start=min(US_tb2$Date), end=max(US_tb2$Date), format="mm-dd"),
                        #strong("2. Select states."),
                        h4(strong("Click to select/deselect each state")),
                        DTOutput("US_table"),
                        helpText("Pop.Density unit: people/mile-square"),
                        textOutput("US_text"),
                        p("US data from ",a(href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html","The New York Times"),
                          "based on reports from state and local health agencies.")),
                 column(7,
                        br(),
                        #actionButton("go1", "Show plots", icon=icon("chart-line")),
                        plotlyOutput("US_cum_cases"),
                        br(),
                        plotlyOutput("US_daily_cases"),
                        br(),
                        plotlyOutput("US_cum_deaths"),
                        br(),
                        plotlyOutput("US_daily_deaths"))),
        
        tabPanel(title="Global",
                 column(5,
                        h4(strong("Click to select/deselect each country")),
                        DTOutput("Global_table"),
                        helpText("Pop.Density unit: people/meter-sequare"),
                        textOutput("Global_text"),
                        p("Global data from",a(href="https://github.com/CSSEGISandData/COVID-19",
                                               "Johns Hopkins University Center for Systems Science and Engineering"))
                        ),
                 column(7,
                        br(),
                        plotlyOutput("Global_cum_cases"),
                        br(),
                        plotlyOutput("Global_daily_cases"),
                        br(),
                        plotlyOutput("Global_cum_deaths"),
                        br(),
                        plotlyOutput("Global_daily_deaths")
                        )
                 )#tabPanel
    ),
    br(),
    hr(),
    helpText("Developed by Alex Chen @ Copy Right Reserved | ",a(href="https://github.com/alexkychen/covid19","Github repository"))

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$US_text = renderText(paste0("US data last updated on ", max(US_tb2$Date)))
    output$Global_text = renderText(paste0("Global data last updated on ", max(Global_tb2$Date)))
    
    output$US_table = renderDT(US_tb1, server=F, 
                                options=list(order=list(list(3,"desc"))), #"3" indicates column "Cases"
                               selection=list(selected=top3index),
                               rownames=F)
    
    output$Global_table = renderDT(Global_tb1, server=F,
                                   options=list(order=list(list(3,"desc"))),
                                   selection=list(selected=top5ctryindex),
                                   rownames=F)
    #US cumulative case plot
    output$US_cum_cases = renderPlotly({
        withProgress(message="Generate plots",{
        SelectedStates <- as.character(US_tb1$State[input$US_table_rows_selected])
        USdf1 <- subset(US_tb2, State %in% SelectedStates, select=c(Date, State, Cum.Cases))
        plot_ly(data=USdf1, x=~Date, y=~Cum.Cases, color=~State, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Cumulative confirmed cases", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of case"))
        })
    })
    #US daily new case plot
    output$US_daily_cases = renderPlotly({
        SelectedStates <- as.character(US_tb1$State[input$US_table_rows_selected])
        USdf2 <- subset(US_tb2, State %in% SelectedStates, select=c(Date, State, Daily.Cases))
        plot_ly(data=USdf2, x=~Date, y=~Daily.Cases, color=~State, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Daily confirmed cases", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of case"))
    })
    #US cumulative death plot
    output$US_cum_deaths = renderPlotly({
        SelectedStates <- as.character(US_tb1$State[input$US_table_rows_selected])
        USdf3 <- subset(US_tb2, State %in% SelectedStates, select=c(Date, State, Cum.Deaths))
        plot_ly(data=USdf3, x=~Date, y=~Cum.Deaths, color=~State, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Cumulative deaths", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of deaths"))
    })
    #US daily death plot
    output$US_daily_deaths = renderPlotly({
        SelectedStates <- as.character(US_tb1$State[input$US_table_rows_selected])
        USdf4 <- subset(US_tb2, State %in% SelectedStates, select=c(Date, State, Daily.Deaths))
        plot_ly(data=USdf4, x=~Date, y=~Daily.Deaths, color=~State, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Daily deaths", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of deaths"))
    })
    
    #Global cumulative case plot
    output$Global_cum_cases = renderPlotly({
        withProgress(message="Generate plots",{
            SelectedCountry <- as.character(Global_tb1$Country[input$Global_table_rows_selected])
            GBdf1 <- subset(Global_tb2, Country %in% SelectedCountry, select=c(Date, Country, Cum.Cases))
            plot_ly(data=GBdf1, x=~Date, y=~Cum.Cases, color=~Country, colors="Set1", type="scatter", mode="lines") %>%
                layout(title = list(text="Cumulative confirmed cases", xanchor="right"),
                       xaxis=list(title="Date"),
                       yaxis=list(title="Number of case"))
        })
    })
    #Global daily new case plot
    output$Global_daily_cases = renderPlotly({
        SelectedCountry <- as.character(Global_tb1$Country[input$Global_table_rows_selected])
        GBdf2 <- subset(Global_tb2, Country %in% SelectedCountry, select=c(Date, Country, Daily.Cases))
        plot_ly(data=GBdf2, x=~Date, y=~Daily.Cases, color=~Country, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Daily confirmed cases", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of case"))
    })
    #Global cumulative death plot
    output$Global_cum_deaths = renderPlotly({
        SelectedCountry <- as.character(Global_tb1$Country[input$Global_table_rows_selected])
        GBdf3 <- subset(Global_tb2, Country %in% SelectedCountry, select=c(Date, Country, Cum.Deaths))
        plot_ly(data=GBdf3, x=~Date, y=~Cum.Deaths, color=~Country, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Cumulative deaths", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of deaths"))
    })
    #Global daily death plot
    output$Global_daily_deaths = renderPlotly({
        SelectedCountry <- as.character(Global_tb1$Country[input$Global_table_rows_selected])
        GBdf4 <- subset(Global_tb2, Country %in% SelectedCountry, select=c(Date, Country, Daily.Deaths))
        plot_ly(data=GBdf4, x=~Date, y=~Daily.Deaths, color=~Country, colors="Set1", type="scatter", mode="lines") %>%
            layout(title = list(text="Daily deaths", xanchor="right"),
                   xaxis=list(title="Date"),
                   yaxis=list(title="Number of deaths"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

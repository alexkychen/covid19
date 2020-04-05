library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(reshape2)

shinyServer(function(input, output, session) {
  
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
  
  #reshape df for Cum.Cases, Daily.cases, Cum.Deaths and Daily.Deaths (Date and each State as columns)
  USCumCaseDF <- dcast(US_tb2, Date~State, value.var="Cum.Cases")
  USDailyCaseDF <- dcast(US_tb2, Date~State, value.var="Daily.Cases")
  USCumDeathDF <- dcast(US_tb2, Date~State, value.var="Cum.Deaths")
  USDailyDeathDF <- dcast(US_tb2, Date~State, value.var="Daily.Deaths")
  DateStamp <- USCumCaseDF$Date
  
  #generate date/time stamp (no use)
  #ds <- length(numeric(max(US_tb2$Date) - min(US_tb2$Date)))
  #DateStamp <- min(US_tb2$Date) + 0:ds
  
  output$US_table1 = renderDT(US_tb1, server=F, 
                              options=list(order=list(list(4,"desc"))),
                              selection=list(selected=c(34,32,5,24,20,10)))
                              
  
  output$US_casevspop = renderPlotly({
    US_casevspop = plot_ly(data=US_tb1, x=~Pop.Size, y=~Cases, color=~State, colors="Set1", type="scatter", mode="markers") %>% 
      layout(title = list(text=" ", xanchor="right"),
             xaxis = list(title="Population size"), 
             yaxis = list(title="Cumulative cases"))
  })
  
  output$US_casevsdensity = renderPlotly({
    US_casevsdensity = plot_ly(data=US_tb1, x=~Pop.Density, y=~Cases, color=~State, colors="Set1", type="scatter", mode="markers") %>% 
      layout(title = list(text=" ", xanchor="right"),
             xaxis = list(title="Population density (people/mi-square)"), 
             yaxis = list(title="Cumulative cases"))
    
  })
  
  #Make time-series plots
  #Cumulative cases over time
  output$StateCumuCases = renderPlotly({
    #subset US_tb2 data for selected States
    SelectedStates <- as.character(US_tb1$State[input$US_table1_rows_selected])
    df <- USCumCaseDF[c(1, (input$US_table1_rows_selected) + 1)]
    
    for(state in SelectedStates){
      
    }
    plot1 <- plot_ly(data=USCumCaseDF, x=~Date)
    #for(state in SelectedStates){
      #plot1 <- plot1 %>% eval(parse(text=paste0("add_trace( y=~",state,", name=state, type='scatter', mode='lines')")))
      #statedf <- subset(US_tb2, State==state, select=c(State, Date, Cum.Cases))
      #df <- rbind(df, statedf)
    #}
    plot1
      # plot_ly(df, x=~Date, y=~Cum.Cases, color=~State, colors="Set1", type="scatter", mode="lines") %>% 
      # layout(title = list(text="Cumulative cases", xanchor="right"),
      #        xaxis=list(title="Date"),
      #        yaxis=list(title="Number of case"))
  })
  #Daily cases over time
  # output$StateDailyCases = renderPlotly({
  #   SelectedStates <- as.character(US_tb1$State[input$US_table1_rows_selected])
  #   df <- data.frame(matrix(ncol=0, nrow=0))
  #   for(state in SelectedStates){
  #     statedf <- subset(US_tb2, State==state, select=c(State, Date, Daily.Cases))
  #     df <- rbind(df, statedf)
  #   }
  #   plot_ly(df, x=~Date, y=~Daily.Cases, color=~State, colors="Set1", type="scatter", mode="lines") %>% 
  #     layout(title = list(text="Daily new cases", xanchor="right"),
  #            xaxis=list(title="Date"),
  #            yaxis=list(title="Number of case"))
  # })
  # #Cumulative deaths
  # output$StateCumuDeaths = renderPlotly({
  #   SelectedStates <- as.character(US_tb1$State[input$US_table1_rows_selected])
  #   df <- data.frame(matrix(ncol=0, nrow=0))
  #   for(state in SelectedStates){
  #     statedf <- subset(US_tb2, State==state, select=c(State, Date, Cum.Deaths))
  #     df <- rbind(df, statedf)
  #   }
  #   plot_ly(data=df, x=~Date, y=~Cum.Deaths, color=~State, colors="Set1", type="scatter", mode="lines") %>% 
  #     layout(title = list(text="Cumulative deaths", xanchor="right"),
  #            xaxis=list(title="Date"),
  #            yaxis=list(title="Number of deaths"))
  # })
  #Daily deaths
  # output$StateDailyDeaths = renderPlotly({
  #   SelectedStates <- as.character(US_tb1$State[input$US_table1_rows_selected])
  #   df <- data.frame(matrix(ncol=0, nrow=0))
  #   for(state in SelectedStates){
  #     statedf <- subset(US_tb2, State==state, select=c(State, Date, Daily.Deaths))
  #     df <- rbind(df, statedf)
  #   }
  #   plot_ly(data=df, x=~Date, y=~Daily.Deaths, color=~State, colors="Set1", type="scatter", mode="lines") %>% 
  #     layout(title = list(text="Daily deaths", xanchor="right"),
  #            xaxis=list(title="Date"),
  #            yaxis=list(title="Number of deaths"))
  # })
  # 
  
})
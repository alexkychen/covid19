################ Development for v2.0 ##############
library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(reshape2)

URL_US_state <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
PAT_US_stpop <- "covid19dash/USpopulation.csv"
URL_Global_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
URL_Global_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
PAT_Global_pop <- "covid19dash/population.csv"

US_covid_data <- read.csv(URL_US_state, stringsAsFactors = T)
US_pop_data <- read.csv(PAT_US_stpop, stringsAsFactors = T)
Global_cases <- read.csv(URL_Global_cases, stringsAsFactors = T)
Global_deaths <- read.csv(URL_Global_deaths, stringsAsFactors = T)
Global_pop <- read.csv(PAT_Global_pop, stringsAsFactors = T)

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




#Make cases vs. pop size plot
plot_ly(data=US_tb1, x=~Pop.Size, y=~Cases, color=~State, colors="Set1", type="scatter", mode="markers") %>% 
  layout(title = list(text="Cumulative confirmed cases", xanchor="right"),
          xaxis = list(title="Population size"), 
          yaxis = list(title="Cumulative cases"), 
         showlegend=FALSE)

plot_ly(data=US_tb1, x=~Pop.Density, y=~Cases, color=~State, colors="Set1", type="scatter", mode="markers") %>% 
  layout(title = list(text="Cumulative confirmed cases", xanchor="right"),
         xaxis = list(title="Population density (people/mi-square)"), 
         yaxis = list(title="Cumulative cases"))

#Cumulative cases over time
SelectedStates <- as.character(US_tb1$State[input$US_table1_rows_selected])
SelectedStates <- as.character(US_tb1$State[c(34,32,5,24)])
df <- data.frame(matrix(ncol=0, nrow=0))
for(state in SelectedStates){
  statedf <- subset(US_tb2, State==state, select=c(State, Date, Cum.Cases))
  df <- rbind(df, statedf)
}
plot_ly(df, x=~as.Date(Date), y=~Cum.Cases, color=~State, mode='lines') %>% 
  layout(xaxis=list(title="Date"),
         yaxis=list(title="Cumulative confirmed cases"))

plot_ly(data=US_tb2, x=~as.Date(Date), y=~Cum.Cases, color=~State, mode='lines') %>% 
  layout(xaxis=list(title="Date"),
         yaxis=list(title="Cumulative confirmed cases"))

#Cumulative deaths over time
plot_ly(data=US_tb2, x=~as.Date(Date), y=~Cum.Deaths, color=~State, mode='lines') %>% 
  layout(xaxis=list(title="Date"),
         yaxis=list(title="Cumulative deaths"))

#Daily new cases
plot_ly(data=US_tb2, x=~as.Date(Date), y=~Daily.Cases, color=~State, mode="lines") %>%
  layout(xaxis=list(title="Date"),
         yaxis=list(title="Daily confirmed cases"))

#Daily new deaths
plot_ly(data=US_tb2, x=~as.Date(Date), y=~Daily.Deaths, color=~State, mode="lines") %>%
  layout(xaxis=list(title="Date"),
         yaxis=list(title="Daily deaths"))























################ Development for v1.0 ##############

library(ggplot2)
library(directlabels)

URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
URL_Deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
URL_Recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

#updated link
URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
URL_Deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" 
URL_Recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"


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
#remove country names not in data_population
for(c in countryList){
  if(c %in% data_population$name==FALSE){
    countryList <- countryList[countryList!=c]
  }
}
countryList <- c("None", countryList)


#get selected country name
countries <- c("China","US","Italy","Taiwan*")

#select date range
start_date <- "2020-01-22"
end_date <- "2020-03-15"

#create empty data frame
df <- data.frame(matrix(ncol=0, nrow=0))#for all plots
df_table <- data.frame(matrix(ncol=0, nrow=0))#for summary table

i <- "China"

#for each selected country
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
        
        #get number of dates which cases are less/greater than 50
        case50greater <- length(acc_confirmed_sum[acc_confirmed_sum >= 50])
        case50less <- length(acc_confirmed_sum) - case50greater
        case50day <- c(rep(NA, case50less), 0:(case50greater-1))
        
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
                               accu.case=acc_confirmed_sum, case50day=case50day, new.case=new_case_day, prevalence=preval, accu.death=acc_deaths_sum,
                               new.death=new_death_day, fatality=fatal, row.names=NULL)
        }else{
          df_one <- data.frame(country=rep(i, length(acc_confirmed_sum)), date=date, pop.size=country_pop10E6, 
                               accu.case=acc_confirmed_sum, case50day=case50day, new.case=new_case_day, prevalence=preval, accu.death=acc_deaths_sum,
                               new.death=new_death_day, fatality=fatal, row.names=NULL)
        } 
        #extract data for summary table
        df_table_one <- subset(df_one, date==end_date, select=c(country, pop.size, accu.case, prevalence, accu.death, fatality))
        
        #concatenate data set
        df <- rbind(df, df_one)
        df_table <- rbind(df_table, df_table_one)
      }
    }#end for loop

#adjust summary table format
df_table$pop.size <- format(round(df_table$pop.size, digits=2))
df_table$accu.case <- format(df_table$accu.case)
df_table$prevalence <- format(round(df_table$prevalence, digits=2))
df_table$accu.death <- format(df_table$accu.death)
df_table$fatality <- format(round(df_table$fatality, digits=2))

#change table column names
names(df_table) <- c("Country","Population", "Confirmed cases*","Prevalence*", "Deaths*","Fatality*")

#make plot for new case each day

ggplot(data=df, aes(x=date, y=new.case, fill=country))+
  geom_bar(stat="identity",position=position_dodge())+
  scale_colour_hue(l=50)+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(start_date, end_date))+
  labs(title="Daily Confirmed Cases", x="Date", y="Number of cases")+
  theme_bw()+
  theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
        plot.title=element_text(size=18,face="bold"),
        legend.title=element_blank(),legend.text=element_text(size=16),legend.position=c(0.1,0.8))
  
#accumulative confirmed cases after 50th case 
#subset data which accu.case is greater than 50
df_case50 <- subset(df, accu.case>=50)
#get longest day of country
maxDay <- max(df_case50$case50day)
#count country name characters
country_maxDay <- as.character(df_case50[which(df_case50$case50day==maxDay),]$country)
country_maxChar <- max(nchar(country_maxDay))
x_max <- maxDay + country_maxChar

ggplot(data=df_case50, aes(x=case50day, y=accu.case, color=country))+
  geom_point(shape=19, size=2)+geom_line(size=1.3)+
  scale_colour_hue(l=50)+
  scale_x_continuous(limits=c(0,x_max), breaks=seq(0, x_max, 5))+
  labs(title="Accumulative Confirmed Cases After 50th Case",x="Day since 50th confirmed case",y="Number of cases")+
  theme_bw()+
  theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
        plot.title=element_text(size=18,face="bold"),
        legend.position="none")+
  geom_dl(aes(label = country), method = list(dl.trans(x = x + 0.2), "last.points", cex =1.5)) 

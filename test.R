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


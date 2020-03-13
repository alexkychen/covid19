URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data_confirmed <- read.csv(URL_Confirmed)


data_country1 <- subset(data_confirmed, Country.Region=="Taiwan*", select=5:ncol(data_confirmed))

data_country2 <- subset(data_confirmed, Country.Region=="China", select=5:ncol(data_confirmed))
col_sum <- colSums(data_country2)

data_countries <- subset(data_confirmed, Country.Region=="None")

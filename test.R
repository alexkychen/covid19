library(ggplot2)

URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
data_confirmed <- read.csv(URL_Confirmed)
date <- as.Date("2020-01-22") + 0:(ncol(data_confirmed)-5)


#compile data for accumulated confirmed cases
country1 <- "Taiwan*"
country2 <- "China"
country3 <- "None"
country4 <- "Iran"

countries <- c(country1,country2,country3,country4)

df <- data.frame(matrix(ncol=0, nrow=0), stringsAsFactors=T)

for(i in countries){
  if(i != "None"){
    one_country <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
    case_sum <- colSums(one_country)
    one_df <- data.frame(date=date, cases=case_sum, country=rep(i, length(case_sum)), stringsAsFactors=T)
    df <- rbind(df, one_df)
  }
}

ggplot(data=df, aes(x=date, y=cases, color=country))+
  geom_point(shape=19)+
  scale_colour_hue(l=50)+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week", limits = c(as.Date("2020-02-10"), as.Date("2020-03-12")))+
  xlab("Date")+ylab("Accumulated confirmed CONVID-19 cases")+
  theme_bw()



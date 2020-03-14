library(ggplot2)

URL_Confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
URL_Deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

data_confirmed <- read.csv(URL_Confirmed)
data_deaths <- read.csv(URL_Deaths)

date <- as.Date("2020-01-22") + 0:(ncol(data_confirmed)-5)
data_population <- read.csv("population.csv")

#compile data for accumulated confirmed cases
country1 <- "Taiwan*"
country2 <- "China"
country3 <- "US"
country4 <- "Iran"

countries <- c(country1,country2,country3,country4)

df <- data.frame(matrix(ncol=0, nrow=0), stringsAsFactors=T)

for(i in countries){
  if(i != "None"){
    #get pop size 
    one_population <- subset(data_population, name==i)
    
    #get confirmed cases
    one_country <- subset(data_confirmed, Country.Region==i, select=5:ncol(data_confirmed))
    case_sum <- colSums(one_country)
    case_percapita <- case_sum / one_population$popsize2019
    
    #get death cases
    one_country_death <- subset(data_deaths, Country.Region==i, select=5:ncol(data_deaths))
    death_sum <- colSums(one_country_death)
    
    
    if(i == "Taiwan*"){
      one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, death=death_sum, country=rep("Taiwan", length(case_sum)), stringsAsFactors=T)
    }else{
      one_df <- data.frame(date=date, cases=case_sum, case_pcap=case_percapita, death=death_sum, country=rep(i, length(case_sum)), stringsAsFactors=T)
    }
    df <- rbind(df, one_df)
  }
}

#make plot
ggplot(data=df, aes(x=date, y=cases, color=country))+
  geom_point(shape=19)+
  scale_colour_hue(l=50)+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  ggtitle("Accumulated cases")+
  xlab("Date")+ylab("Number of cases")+
  theme_bw()+
  theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
        plot.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=16),legend.text=element_text(size=16))

ggplot(data=df, aes(x=date, y=case_pcap, color=country))+
  geom_point(shape=19, size=3)+
  scale_colour_hue(l=50)+
  scale_x_date(date_labels = "%b-%d", date_breaks = "1 week")+
  ggtitle("Accumulated cases per capita")+
  xlab("Date")+ylab("cases per capita")+
  theme_bw()+
  theme(axis.title=element_text(size=16),axis.text=element_text(size=12),
        plot.title=element_text(size=18,face="bold"),
        legend.title=element_text(size=16),legend.text=element_text(size=16))

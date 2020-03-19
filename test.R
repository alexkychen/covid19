library(ggplot2)
library(directlabels)

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

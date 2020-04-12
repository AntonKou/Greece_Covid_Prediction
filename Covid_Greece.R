library(dplyr)
library("readxl")
library(lubridate)
library("tidyr")
library(arsenal)
library(httr)
library(ggplot2)
library(forecast)
library(xts)

#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")
url<-"https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"
#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
#Arima Modelling
data <- read_excel(tf)
gr_covid<-filter(data,geoId=="EL")
gr_covid<-mutate(arrange(gr_covid,dateRep),cumulative=cumsum(arrange(gr_covid,dateRep)$cases))
gr_covid<-filter(gr_covid,cumulative>0)
gr_covid<-mutate(gr_covid,day=seq(1,NROW(gr_covid),by=1))
gr_covid_test<-gr_covid[1:round(0.8*NROW(gr_covid),digits = 0),]
gr_covid_valid<-gr_covid[-seq_along(gr_covid_test$cumulative),]
arima_test<-auto.arima(gr_covid_test$cumulative)
gr_covid_f<-forecast(arima_test,9)
plot(gr_covid_f)
100*mean(abs((gr_covid_f$mean - gr_covid_valid$cumulative))/gr_covid_valid$cumulative)
test_set<-cbind(as.Date(gr_covid_valid$dateRep),gr_covid_valid$cumulative,gr_covid_f$mean)
colnames(test_set)<-c("date","actuals","predicted")
test_set<-as.data.frame(test_set)
test_set$date<-as_date(test_set$date)
ggplot(test_set,aes(x=date))+geom_line(aes(y=actuals),col="red")+geom_line(aes(y=predicted),col="blue")+ labs(title="Actual vs Predicted values")+ylab("Cases(acumulated)")
final<-cbind(as.Date(gr_covid$dateRep),gr_covid$cumulative,rbind(gr_covid[1:(NROW(gr_covid)-9),"cumulative"],gr_covid_f$mean))
#Linear Modelling

gr_covid_lm<-lm(cumulative~day,gr_covid_test)
gr_covid_flm<-predict(gr_covid_lm,gr_covid_test)
mean(abs((gr_covid_flm - gr_covid_valid$cumulative))/gr_covid_valid$cumulative)

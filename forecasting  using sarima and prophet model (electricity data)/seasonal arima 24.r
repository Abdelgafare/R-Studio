##sarima with only one period which is 24 run both data full and first week of july 
library(readxl)
full <- read_excel("C:/Users/pc/Desktop/full.xlsx")
ds<-full
View(full)
library(seastests)
library(forecast)
isSeasonal(ds$Demand,test = "combined",freq = 168)
combined_test(ds$Demand,freq = 24)
kw(ds$Demand,diff = T,residuals = F,freq = 168)
auto.arima(ds)
install.packages("DescTools")
library(DescTools)
install.packages("ie2misc")
library(ie2misc)
library(readxl)
USA_electricity_demand_including_first_week_of_july <- read_excel("C:/Users/pc/Desktop/USA electricity demand including first week of july.xlsx")
View(USA_electricity_demand_including_first_week_of_july)
ds<-USA_electricity_demand_including_first_week_of_july
ds<- ts(ds$Demand,frequency = 24)
plot(dsx,type= "l")
library(readxl)
datax <- read_excel("C:/Users/pc/Desktop/datax.xlsx")
ds<-datax
ggAcf(ds)
ds<- ts(ds,frequency = 24)
plot(Time,Electricity_demand_in_kilowatts,type = "l",ylab = "Electricitydemand in kilowatts" ,xlab = "Months")
Electricitydemandinkilowatts<- ds$Demand
View(datax)
auto.arima(ds)


fit1<-arima(ds,
            order=c(2,0,0),
            seasonal=c(0,1,1))
fit1
kw(ds$Demand,freq = 168)

ac
fit1$coef
checkresiduals(fit1)
x<-forecast(fit1,h=168)
x<-x$mean
x<- head(x,24)
actual<-full$`ac demand`[1:168]
last_168<- head(x,168)
# Calculate MAPE
acc_ <- accuracy(object = x, actual)
TheilU(actual,x)

c<-(1/168)*sum(abs(fit1$residuals))
c
acc_
full$`Time of ac`[1:168]
geom
accuracyData <- data.frame(Time= full$`Time of ac`[1:168],
                           actual = as.vector(actual) ,
                           sarima = as.vector(x))
accuracyData %>%
  ggplot() +
  geom_line(aes(x = full$`Time of ac`[1:168], y = actual, colour = "ACTUAL"),size=2)+
  geom_line(aes(x = full$`Time of ac`[1:168], y = sarima, colour = "SARIMA"),size=2)+
  
  labs(title = "Actual Values VS Sarima Values",x = "Date",y = "Electricity demand in kilowatts",colour = "")
shapiro.test(x = fit1$residuals)#Result : p-value = 6.452e-06 < 0.05 (alpha), which can be concluded that the residuals are not distributed normally. : It was found that the residuals are not distributed normally, this could happen since the size of our data if not large enough, but does not mean Forecast Performance is not good. We need to get more data to build and feed into models, so the residual more distributed normally and getting better result on Model Evaluation and Forecast
Box.test(fit1$residuals, type = "Ljung-Box")
#that the residuals has autocorrelation.

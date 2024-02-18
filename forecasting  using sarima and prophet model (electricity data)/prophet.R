## prophet
USA_electricity_demand_including_first_week_of_july <- read_excel("C:/Users/pc/Desktop/USA electricity demand including first week of july.xlsx")
install.packages("prophet")
library(readxl)
proph <- read_excel("C:/Users/pc/Desktop/proph.xlsx")
View(proph)
install.packages("readr")
# Load required libraries
install.packages("MASS") 
install.packages("reshape2") 
install.packages("openxlsx")
proph2 <- read_excel("C:/Users/pc/Desktop/proph2.xlsx")
View(proph2)
install.packages("reshape") 
library(openxlsx)
library(forecast)
library(dplyr)
library(lubridate) 
library(tidyr)
library(ggplot2)
library(TSstudio)
library(forecast)
library(readr)
library(zoo)
library(ggsci)
library(MASS) 
library(tseries)
install.packages("tseries")
library(reshape2) 
library(reshape) 
library(prophet)
library(readr)
library(rlang)
library(Rcpp)
library(tidyverse)
install.packages('tidyverse')
library(neuralnet)
install.packages("scales")
library(scales)
install.packages("neuralnet")
df <- proph
holiday <- data.frame(
  holiday = c("Memorial Day", "Juneteenth","indp"),
  ds = as.Date(c( "2021-5-31","2021-06-18","2021-7-5"))
)
holiday
class(df$Time)

df <- mutate (
  df,
  ds = Time,  # Create new ds column from date using mutate
  y = Value   # Create new y column from value using mutate
)

df<- df[,-1]
df
df<- df[,-1]
model1<- prophet(df,holidays = holiday,seasonality.mode = "multiplicative",weekly.seasonality = FALSE
)

future1<- make_future_dataframe(model1,periods =168,freq="hour")
forecast1<-predict(model1,future1)
tail(forecast1)

forecast1
as.data.frame(forecast1)
upper<-tail(forecast1$yhat_upper,168)
lower<-tail(forecast1$yhat_lower,168)

tail
last_1682 <- tail(forecast1$yhat,168)
last_1682
as.data.frame(last_168)

dyplot.prophet(model1,forecast1)
prophet_plot_components(model1,forecast1)
# R
prophetano <- data.frame(
  Time=c(full$`Time of ac`[1:168]),
  Forecasted=c(last_168),
  upper95 = c( upper),
  low95 = c(lower),
  Actual=c(actual)
)
actual
prophetano
write.xlsx(last1682, file = "prophetforecast.xlsx", rowNames = FALSE)

# Calculate MAPE
acc_ <- accuracy(object = last_1682, actual)
acc_
accuracyData <- data.frame(Time= full$`Time of ac`[1:168],
                           actual = as.vector(actual) ,
                           pro = as.vector(last_1682))

accuracyData
accuracyData %>%
  ggplot() +
  geom_point(aes(x = full$`Time of ac`[1:168], y = actual, colour = "Actual"),size=2)+
  geom_line(aes(x = full$`Time of ac`[1:168], y = last_1682, colour = "Prophet"),size=2)+
  geom_line(aes(x = full$`Time of ac`[1:168], y = last_168, colour = "Sarima"),size=2)+
  

  labs(title = "Actual  Vs prophet  VS Sarima ",x = "Date",y = "Electricity demand in kilowatts",colour = "")



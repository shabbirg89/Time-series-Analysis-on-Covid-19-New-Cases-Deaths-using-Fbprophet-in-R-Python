library("readxl")
library("dplyr")
library("lubridate")
library("ggplot2")
library("prophet")
df <- read_excel("C:/Users/sgove/OneDrive/Documents/R_ProgrammingDocs/25_onwards.xlsx")
View(df)
tsc <- df[,c(1,3)]

View(tsc)
tsc$Date <- ymd(tsc$Date)
str(tsc)

#Plot
attach(tsc)
qplot(Date, tsc$`New Cases`,
      main = "Covid-19 increasing number of cases in India")

ds <- tsc$Date
y <- tsc$`New Cases`
df1 <- data.frame(ds,y)

#forcasting
res <- prophet(df1)

#Predictions
future <- make_future_dataframe(res,periods = 60)
View(future)
forecast <- predict(res, future)

# Plot forecast
plot(res, forecast)
dyplot.prophet(res,forecast)

#Internal Components
prophet_plot_components(res, forecast)

# Model performance
pred <- forecast$yhat[1:169]
actual <- res$history$y
plot(actual, pred)
abline(lm(pred~actual),col='red')
summary(lm(pred~actual))

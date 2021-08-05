
require(forecast)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggforce)
library(zoom)

view(dataset)
Dealdataset<-tibble(dataset)
dataset<-na.omit(dataset)

dataset$Date1<-paste(dataset$Month,dataset$Day,dataset$Year)
dataset$Date<-mdy(dataset$Date1)
myts <- ts(dataset[,'On-time'], start = as.numeric(substr(dataset[1,'Date'],0,4)), frequency= 12)
myts1 <-ts(dataset['Volume'], start = as.numeric(substr(dataset[1,'Capacity'],0,5)), frequency=12)

plot(myts)
plot(myts1)
## Create the ets model with the right parameters, type ?ets in your R console after loading the forecast library for details

myets <- ets(myts, "ZZA")
myets1 <-ets(myts1,"ZZA")
plot(myets)
plot(myets1)
## The second argument in the function forecast() defines how many values you want to forecast. Since this is monthly dataset 12 is a whole year ahead

## Level determines which confidence intervals to plot, c(75,95) sets them to 75% and 95%.

myprediction <- forecast(myets, 12, level = c(75,95))
myprediction1 <- forecast(myets1, 12, level = c(75,95))

## Find the last row with actual values (to know which color to use for the plot)

lastValue = tail(myprediction$x,1)
lastValue1 = tail(myprediction1$x,1)
## Get the mean value for all future fitted dates

myprediction$mean=ts(c(lastValue,myprediction$mean), 
                     frequency = frequency(myprediction$mean), 
                     end=end(myprediction$mean))


myprediction1$mean=ts(c(lastValue1,myprediction1$mean), 
                      frequency = frequency(myprediction1$mean), 
                      end=end(myprediction1$mean))

myprediction$mean
myprediction1$mean


## Populate the upper values for all future fitted dates

myprediction$upper=ts(rbind(c(lastValue,lastValue),myprediction$upper), 
                      frequency = frequency(myprediction$upper), 
                      end=end(myprediction$upper))

myprediction1$upper=ts(rbind(c(lastValue1,lastValue1),myprediction1$upper), 
                       frequency = frequency(myprediction1$upper), 
                       end=end(myprediction1$upper))


## And the same for the the lower range values for all future fitted dates  

myprediction$lower=ts(rbind(c(lastValue,lastValue),myprediction$lower), 
                      frequency = frequency(myprediction$lower), 
                      end=end(myprediction$lower))

myprediction1$lower=ts(rbind(c(lastValue1,lastValue1),myprediction1$lower), 
                       frequency = frequency(myprediction1$lower), 
                       end=end(myprediction1$lower))

## Plot the results with historical values in blue and future values in red with shaded confidence intervals

plot(myprediction, lwd=2, col="blue", fcol="red", flwd=2, shaded=TRUE, col.sub = "gray50", cex.sub=0.75, main="On-Time Deal Forecast Q1FY20-Q4FY22",xlab = "QFY20-QFY22", ylab = "Deals",xaxt="n")
plot(myprediction1, lwd=2, col="blue", fcol="red", flwd=2, shaded=TRUE, col.sub = "gray50", cex.sub=0.75, main="Volume Forecast Q1FY20-Q4FY22",xlab = "QFY20-QFY22", ylab = "Volume",xaxt="n",)


accuracy(myprediction)
accuracy(myprediction1)
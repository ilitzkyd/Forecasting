library(forecast)

# Load the attendance dataset
dataset <- read.csv("attendance_data.csv")

# Preprocess the dataset
dataset$Date <- as.Date(with(dataset, paste(Year, Month, Day, sep = "-")))
dataset <- dataset[complete.cases(dataset), ]

# Create time series objects
myts <- ts(dataset$On.time, start = as.numeric(format(dataset$Date[1], "%Y")), frequency = 12)
myts1 <- ts(dataset$Volume, start = as.numeric(format(dataset$Capacity[1], "%Y")), frequency = 12)

# Create the ets models
myets <- ets(myts, "ZZA")
myets1 <- ets(myts1, "ZZA")

# Forecast future values
myprediction <- forecast(myets, h = 12, level = c(75, 95))
myprediction1 <- forecast(myets1, h = 12, level = c(75, 95))

# Get the mean value for all future fitted dates
myprediction$mean <- ts(c(tail(dataset$On.time, 1), myprediction$mean),
                        frequency = frequency(myprediction$mean),
                        end = end(myprediction$mean))
myprediction1$mean <- ts(c(tail(dataset$Volume, 1), myprediction1$mean),
                         frequency = frequency(myprediction1$mean),
                         end = end(myprediction1$mean))

# Populate the upper values for all future fitted dates
myprediction$upper <- ts(rbind(c(tail(dataset$On.time, 1), tail(dataset$On.time, 1)), myprediction$upper),
                         frequency = frequency(myprediction$upper),
                         end = end(myprediction$upper))
myprediction1$upper <- ts(rbind(c(tail(dataset$Volume, 1), tail(dataset$Volume, 1)), myprediction1$upper),
                          frequency = frequency(myprediction1$upper),
                          end = end(myprediction1$upper))

# And the same for the lower range values for all future fitted dates
myprediction$lower <- ts(rbind(c(tail(dataset$On.time, 1), tail(dataset$On.time, 1)), myprediction$lower),
                         frequency = frequency(myprediction$lower),
                         end = end(myprediction$lower))
myprediction1$lower <- ts(rbind(c(tail(dataset$Volume, 1), tail(dataset$Volume, 1)), myprediction1$lower),
                          frequency = frequency(myprediction1$lower),
                          end = end(myprediction1$lower))

# Plot the results with historical values in blue and future values in red with shaded confidence intervals
plot(myprediction, lwd = 2, col = "blue", fcol = "red", flwd = 2, shaded = TRUE,
     col.sub = "gray50", cex.sub = 0.75, main = "On-Time Deal Forecast Q1FY20-Q4FY22",
     xlab = "QFY20-QFY22", ylab = "Deals", xaxt = "n")
plot(myprediction1, lwd = 2, col = "blue", fcol = "red", flwd = 2, shaded = TRUE,
     col.sub = "gray50", cex.sub = 0.75, main = "Volume Forecast Q1FY20-Q4FY22",
     xlab = "QFY20-QFY22", ylab = "Volume", xaxt = "n")

# Calculate accuracy measures
accuracy(myprediction)
accuracy(myprediction1)

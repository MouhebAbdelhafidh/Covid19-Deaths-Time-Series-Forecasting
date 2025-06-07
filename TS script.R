data <- read.csv("C:/Users/asus/Desktop/TS COVID/COVID-19_Data.csv", header = TRUE, stringsAsFactors = FALSE)
summary(data)

data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
# Order by Date(ascending)
data <- data[order(data$Date), ]
# Creating the TS

ts <- ts(data)
class(ts)
summary(ts)
my_ts <- ts(rnorm(100), start = c(2000, 1), frequency = 12)
plot(my_ts, main = "Basic Time Series Plot", ylab = "Value")
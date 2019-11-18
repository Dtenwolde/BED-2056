library(lubridate)
library(dplyr)
library(forecast)

url <- "http://ansatte.uit.no/oystein.myrland/data/storedata.csv"
destfile <- "storedata.csv"
download.file(url, destfile)
data <- read.csv("storedata.csv")

data$Order_Date <- ymd(data$Order_Date)

sales_2017 <- function(data) {
  library(dplyr)
  data <- data %>% filter(Order_Date >= as.Date("2017-09-01") & (Region == "Region 1" | Region == "Region 9") & (Customer_Segment == "Corporate" | Customer_Segment == "Consumer"))
  result <- data %>% group_by(Month = month(Order_Date), Region, Customer_Segment) %>% summarise(sum = sum(Sales))
  return(result)
}

plot_sales <- function(data) {
  data <- monthly_sales(data)
  plot <- ggplot(data, aes(x=Date, y=sum, color=Region)) + 
    geom_line() +
    theme_classic() +
    labs(x="Date",y="Total Sales", title="Total sales per month")
}

monthly_sales <- function(data) {
  library(dplyr)
  library(ggplot2)
  library(scales)
  data <- data %>% filter(year(Order_Date) >= 2015 & year(Order_Date) <= 2017)
  data <- data %>% filter(Region == "Region 1" | Region == "Region 13")
  monthly_total_sales <- data %>% group_by(Year = year(Order_Date), Month = month(Order_Date), Region) %>% summarise(sum = sum(Sales))
  monthly_total_sales$Date <- ymd(paste(monthly_total_sales$Year, monthly_total_sales$Month, "01", sep="-"))
  return(monthly_total_sales)
}

greater_region <- function(data) {
  data <- monthly_sales(data)
  region_13 <- data %>% filter(Region == "Region 13")
  region_1 <- data %>% filter(Region == "Region 1")
  result <- subset(region_13, ((region_13$Date == region_1$Date) & (region_13$sum > region_1$sum)))
  print(result)
}

average_profit <- function(data) {
  library(dplyr)
  
  data <- data %>% filter(year(Order_Date) == 2017)
  data <- data %>% filter(Region != "Region 3" & Region != "Region 5" & Region != "Region 8")
  average <- data %>% group_by(Customer_Segment, Product_Category) %>% summarise(mean = mean(Profit))
  average_segment <- average %>% group_by(Customer_Segment) %>% summarise(mean = mean(mean))
}

sarima_models <- function(data) {
  
}

first_table <- sales_2017(data)
first_table

first_figure <- plot_sales(data)
first_figure

second_table <- greater_region(data)

third_table <- average_profit(data)

second_figure <- sarima_models(data)
model_data <- data %>% filter(year(Order_Date) >= 2014 & year(Order_Date) <= 2016)
model_data <- model_data %>% filter(Customer_Segment == "Small Business")
model_data <- model_data %>% filter(Product_Category == "Office Supplies")
model_data <- model_data %>% group_by(Year = year(Order_Date), Month = month(Order_Date)) %>% summarise(sum = sum(Order_Quantity))
model_data$Date <- ymd(paste(model_data$Year, model_data$Month, "01", sep="-"))
time_series <- ts(model_data$sum, start=c(2014, 1), end=c(2016,12), frequency = 12)
print(time_series)

p <- c(0:4)
q <- c(0:4)
d <- c(0:1)
P <- c(0:4)
Q <- c(0:4)
D <- c(0:1)
S <- 12

model <- time_series %>%
          auto.arima(max.d = 1, max.D = 1, max.p = 4, max.q = 4, max.P = 4, max.Q = 4, start.p = 0, start.q = 0, start.P = 0, start.Q = 0, seasonal=TRUE)



forecast <- forecast(model, h=12)

actual_2017 <- data %>% filter(year(Order_Date) == 2017) 
actual_2017 <- actual_2017 %>% filter(Customer_Segment == "Small Business")
actual_2017 <- actual_2017 %>% filter(Product_Category == "Office Supplies")
actual_2017 <- actual_2017 %>% group_by(Year = year(Order_Date), Month = month(Order_Date)) %>% summarise(sum = sum(Order_Quantity))
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(forecast$mean, actual_2017$sum)

autoplot(forecast(model, h=12)) +
  autolayer(ts(actual_2017$sum))
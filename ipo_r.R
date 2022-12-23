getwd()
setwd("C:/Users/payal")
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
options(scipen = 999)
data = read.csv("ipo_data.csv")



data = data[,-1]
data$Exchange..Market <- as.factor(data$Exchange..Market)
data$Date <- as.Date(data$Date)
data <- data[,-9]
data$sector <- as.factor(data$sector)
data$industry <- as.factor(data$industry)
colnames(data)[17] <- "Employees"
data$CEO_born <- as.Date(as.character(data$CEO_born), format = "%Y")
data$CEO_born <- year(data$CEO_born)

data <- data %>% mutate_all(na_if,"")


#apply(data, 2, function (x) sum(is.na(data$x)))
colSums(is.na(data))

original = read.csv("nasdaq.csv")
original$Date = as.Date(original$Date)
library(tidyverse)
original$year = format(original$Date, format = "%Y")
data$year <- format(data$Date, format = "%Y")

summary(data$sector)

data =data[complete.cases(data$sector),]
## EDA
## IPO (original dataset) by years

## Plotting number of IPOs since 2010 by year
year_data = original%>%
        group_by(year)%>%
        summarise(n = n())

ggplot(data = year_data, aes (x = year, y = n))+
  geom_bar(stat = "identity")

## Visualizing Price by sector and outliers 


ggplot(data = data, aes(y = sector, x = Price, fill = sector))+
  geom_boxplot()

ggplot(data = data, aes(x = year, y = Price))+
  geom_boxplot()+
  ggtitle("Box plot of Offer Price every year")

ggplot(data = data, aes(x = year, y = Price))+
  geom_boxplot()+
  ylim(0,36.8)+
  ggtitle("Box plot of Offer Price every year- Without Outliers")

# outliers in price
Q <- quantile(data$Price, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$Price)
up <-  Q[2]+1.5*iqr # Upper Range 
low<- Q[1]-1.5*iqr # Lower Range

outliers<- subset(data, data$Price < low | data$Price > up)

price_out  = data[(data$Price - mean(data$Price)) > 3 * sd(data$Price),]


##Offer amount: typical and outliers
ggplot(data = data, aes(x = year, y = Offer.Amount))+
  geom_boxplot()+
  ggtitle("Box plot of Offer amount every year ")

ggplot(data = data, aes(x = year, y = Offer.Amount))+
  geom_boxplot()+
  ylim(0,1000000000)+
  ggtitle("Box plot of Offer amount every year- Without Outliers")

##Top 5  offer amounts 
top_offer <- data %>%
  arrange(desc(Offer.Amount)) %>%
  slice(1:10, with_ties = FALSE)


ggplot(top_offer, aes(y= reorder(Company.Name, Offer.Amount), x = Offer.Amount,
                    fill = Company.Name))+
  geom_bar(stat = "identity")

## Bottom 10 offer amounts
bottom_offer <- data %>%
                arrange((Offer.Amount)) %>%
                slice(1:10, with_ties = FALSE)

ggplot(bottom_offer, aes(y= reorder(Company.Name, Offer.Amount), x = Offer.Amount,
                      fill = Company.Name))+
  geom_bar(stat = "identity")

##IPOs by sector and industry:
sec <- data%>%
        group_by(sector)%>%
        summarise(n = n())%>%
        mutate(pct = n/sum(n)*100)%>%
        arrange(desc(pct))

ggplot(sec, aes(x="", y=pct, fill=sector)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()  


##Employees: typical and outliers
summary(data$Employees)

emp_out <- drop_na(data[abs(data$Employees - mean(data$Employees, na.rm = TRUE)) >
                          3 * sd(data$Employees, na.rm = TRUE),])

ggplot(emp_out, aes(x = Employees, y = Company.Name))+
  geom_bar(stat="identity", width=1, color="white")

## CEO pay and yearbon:
pay = data[complete.cases(data$CEO_pay),]
summary(pay$CEO_pay)
ceopay_out <- (pay[abs(pay$CEO_pay - mean(pay$CEO_pay, na.rm = TRUE)) >
                             3 * sd(pay$CEO_pay, na.rm = TRUE),])

ggplot(ceopay_out, aes(x = CEO_pay, y = Company.Name))+
  geom_bar(stat="identity", width=1, color="white")


## CEO born
born =data[complete.cases(data$CEO_born),]
summary(born$CEO_born)
ceoborn_out <- (born[abs(born$CEO_born - mean(born$CEO_born, na.rm = TRUE)) >
                             3 * sd(born$CEO_born, na.rm = TRUE),])

ggplot(ceoborn_out, aes(x = CEO_born, y = Company.Name))+
  geom_bar(stat="identity", width=1, color="white")

#sort(abs(data$CEO_born - mean(data$CEO_born, na.rm = TRUE)), decreasing = TR)
bor <- subset(born, CEO_born < 1994 & CEO_born > 1942)
lm <- lm(bor$CEO_pay~ bor$CEO_born)

plot(bor$CEO_born, bor$CEO_pay, ylim = c(0,10000000))
abline(lm(bor$CEO_pay~ bor$CEO_born), col ='red')

summary(lm)

data$check <- data$firstday_open  /data$Price
sum(data$check >2 | data$check < -2)

tdata <- subset(data, data$check <2  & data$check > -2)
summary(tdata)

colSums(is.na(tdata))
tdata <- drop_na(tdata)


## Feature engineering
## Month and weekday of IPO

tdata$month <- format(tdata$Date,"%B")
tdata$weekday <- wday(tdata$Date, label=TRUE)



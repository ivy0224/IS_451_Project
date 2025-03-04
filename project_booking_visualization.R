library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggstats)
options(scipen = 999, digits = 3)

### load data
booking <- read.csv("booking.csv")
str(booking)
summary(booking)

### data cleaing and preprocessing
## factor categorical data 
booking$type.of.meal <- factor(booking$type.of.meal)
booking$room.type <- factor(booking$room.type)
booking$market.segment.type <- factor(booking$market.segment.type)
booking$booking.status <- factor(booking$booking.status)
str(booking)

## check missing values
na.rows <- booking %>% 
  filter(if_any(everything(), is.na))

## separate date --> day + month + year
## deal with format M/D/YYYY
mdy.df <- booking %>%
  separate(date.of.reservation, into = c("month", "day", "year"), 
           sep = "/", remove = FALSE)

## get 37 rows unmatched with M/D/YYYY but YYYY-M-D
ymd <- mdy.df %>%
  filter(if_any(everything(), is.na)) %>%
  pull(Booking_ID)

## split into 2 dataset
## get M/D/YYYY first
mdy.df <- mdy.df %>%
  filter(if_all(everything(), ~!is.na(.)))

## deal with YYYY-M-D then
ymd.df <- booking %>%
  filter(Booking_ID %in% ymd) %>%
  separate(date.of.reservation, into = c("year", "month", "day"),
           sep = "-", remove = FALSE)

## combine 2 dataset
booking <- bind_rows(mdy.df, ymd.df)

## transform from char to num
str(booking)
booking$month <- as.numeric(booking$month)
booking$day <- as.numeric(booking$day)
booking$year <- as.numeric(booking$year)

### data visualization
## exploratory (since we do not know detailed info about each room type)
## relationship between average price and room type?
p1.price.room <- ggplot(data = booking) +
  geom_boxplot(aes(x = room.type, 
                   y = average.price, 
                   fill = room.type)) +
  labs(title = "Price Variation by Room Type", 
       x = "Room Type", 
       y = "Average Price") +
  theme(plot.title = element_text(hjust = 0.5))
p1.price.room

## canceled vs not_canceled
status <- booking %>%
  group_by(booking.status) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

p2.status <- ggplot(data = status) +   
  aes(x = "", 
      fill = booking.status, 
      weight = proportion, 
      by = booking.status) +
  geom_bar(position = "fill", 
           width = 1, 
           color = "black") + 
  scale_fill_manual(values = c("Canceled" = "orange", 
                               "Not_Canceled" = "skyblue")) + 
  coord_polar(theta = "y") +
  labs(title = "Booking Status Distribution", fill = "Booking Status") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(stat = "prop", position = position_fill(0.5), 
            aes(label = paste0(round(proportion * 100, 1), "%")), 
            color = "white", size = 5)
p2.status

## lead time for canceled and not_canceled
p3.leadtime <- ggplot(data = booking) +
  geom_boxplot(aes(x = booking.status, 
                   y = lead.time, 
                   fill = booking.status),
               color = "black") + 
  scale_fill_manual(values = c("Canceled" = "orange", 
                               "Not_Canceled" = "skyblue")) +
  labs(title = "Lead Time Distribution by Booking Status", 
       x = "Booking Status", 
       y = "Lead Time (Days)") + 
  theme(plot.title = element_text(hjust = 0.5))
p3.leadtime

## average price: canceled vs. not_canceled
# bin the average price into 6 categories
# 0-50, 50-100, 100-150, 150-200, 200-250, and >250
avg.price <- booking [, c("Booking_ID", "average.price", "booking.status")]
avg.price$price_category <- cut(booking$average.price,
                                breaks = c(0, 50, 100, 150, 200, 250, Inf), 
                                labels = c("0-50", "50-100", "100-150", "150-200", 
                                           "200-250", ">250"),
                                right = FALSE)

# calculate the proportion for each booking status and price category
avg.price <- avg.price %>%
  group_by(booking.status, price_category) %>%
  summarise(count = n()) %>%
  group_by(price_category) %>%
  mutate(proportion = count / sum(count))

# bar chart
avg.price$booking.status <- factor(avg.price$booking.status, 
                                   levels = c("Not_Canceled", "Canceled"))

p4.avg.price <- ggplot(avg.price) +
  aes(x = price_category, 
      fill = booking.status, 
      weight = proportion, 
      by = price_category) +
  geom_bar(position = "fill",
           color = "black") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            stat = "prop", 
            position = position_fill(0.5), 
            color = "white", 
            size = 4) +
  scale_fill_manual(values = c("Canceled" = "orange", 
                               "Not_Canceled" = "skyblue")) +  
  labs(title = "Average Price Distribution by Booking Status", 
       x = "Price Category", 
       y = "Proportion", 
       fill = "Booking Status") +  
  theme(plot.title = element_text(hjust = 0.5))
p4.avg.price

## reservation.time: month --> holiday season?
# count total booking by month
monthly.total <- booking %>%
  group_by(month) %>%
  summarise(count.booking = n())

# count total cancellation by month
monthly.cancel <- booking %>%
  filter(booking.status == "Canceled") %>%
  group_by(month) %>%
  summarise(count.cancel = n())

# merge two datasets on month
monthly.cancel.rate <- left_join(monthly.cancel, monthly.total, 
                                 by = "month") %>%
  mutate(cancel.rate = count.cancel / count.booking)

# plot bar chart
p5.reservation.month <- ggplot(data = monthly.cancel.rate) +
  geom_bar(aes(x = month,
               y = cancel.rate, 
               fill = "Canceled"),
           stat = "identity", 
           position = "dodge",
           color = "black") +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  scale_fill_manual(values = "orange") +
  labs(title = "Cancellation Rate by Reservation Month", 
       x = "Month", 
       y = "Cancellation Rate",
       fill = "Booking Status") +  
  theme(plot.title = element_text(hjust = 0.5))
p5.reservation.month

## special requests: 
## more requests --> more effort put in --> fewer cancel?
# get summarized number of requests
requests <- booking %>% 
  group_by(booking.status, special.requests) %>%
  summarise(count = n())

# plot lines
p6.request <- ggplot(data = requests) +
  aes(x = special.requests, 
      y = count, 
      color = booking.status, 
      group = booking.status) +
  geom_line(size = 1.5) +
  labs(title = "Number of Special Requests by Booking Status",
       x = "Number of Special Requests",
       y = "Count of Bookings",
       color = "Booking Status") +
  scale_color_manual(values = c("Canceled" = "orange", 
                                "Not_Canceled" = "skyblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p6.request

## market segment: 
## online, offline, aviation, corporate, complementary
market.segment <- booking %>%
  group_by(booking.status, market.segment.type) %>%
  summarise(count = n())

p7.market.segment <- ggplot(data = market.segment) +
  geom_bar(aes(x = count, 
               y = market.segment.type, 
               fill = booking.status),
           stat = "identity", 
           position = "dodge",
           color = "black") +
  scale_fill_manual(values = c("Canceled" = "orange", 
                               "Not_Canceled" = "skyblue")) +
  labs(title = "Booking Status by Market Segment",
       x = "Count of Bookings",
       y = "Market Segment",
       fill = "Booking Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
p7.market.segment
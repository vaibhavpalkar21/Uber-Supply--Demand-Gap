# Assignment_uber_supply_gap_demand_gap
# Read Data
library(ggplot2)
library(lubridate)
library(stringr)
library(dplyr)
options(max.print=100000)


Uber <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)
View(Uber)
str(Uber)

## Data cleaning
# look for duplicate values
duplicate_values <- Uber[duplicated(Uber), ] # No duplicate values

# missing values
na_values <- is.na(Uber)
summary(na_values)
# there is duplicate values in driver id and drop.timestamp but dont need to clean it as we will not use that columns for analysis

# blank values
length(which(Uber$Request.id == ""))
length(which(Uber$Pickup.point == ""))
length(which(Uber$Driver.id == ""))
length(which(Uber$Status == ""))
length(which(Uber$Request.timestamp == ""))
length(which(Uber$Request.id == ""))
# blank values not available

# R is case sensitive so for safety lets convert strings to lower format
Uber$Pickup.point <- tolower(Uber$Pickup.point)
Uber$Status <- tolower(Uber$Status)

# remove "/" and add "-"
Uber$Request.timestamp <- str_replace_all(Uber$Request.timestamp, "/", "-")
Uber$Drop.timestamp <- str_replace_all(Uber$Drop.timestamp, "/", "-")

typeof(Uber$Request.timestamp)
typeof(Uber$Drop.timestamp)

# standard date format
Uber$Request.timestamp <- parse_date_time(Uber$Request.timestamp, c("%d-%m-%Y %H:%M", "%d-%m-%Y %H:%M:%S"))

Uber$Drop.timestamp <- parse_date_time(Uber$Drop.timestamp, c("%d-%m-%Y %H:%M", "%d-%m-%Y %H:%M:%S"))

typeof(Uber$Request.timestamp)
typeof(Uber$Drop.timestamp)

# split date and time into 2 column
#for request date and time
Uber$request_date <- format(Uber$Request.timestamp, "%Y-%m-%d")
Uber$request_time <- format(Uber$Request.timestamp, "%H:%M:%S")

typeof(Uber$request_time)
typeof(Uber$request_date)

#for drop date and time
Uber$Drop_date <- format(Uber$Drop.timestamp, "%Y-%m-%d")
Uber$Drop_time <- format(Uber$Drop.timestamp, "%H:%M:%S")

typeof(Uber$Drop_time)
typeof(Uber$Drop_date)

# separate hours on request and drop time
# hours for request time
Uber$request_time_hour <- format(strptime(Uber$request_time,"%H:%M:%S"),'%H')
Uber$request_time_hour <- as.numeric(Uber$request_time_hour)
typeof(Uber$request_time_hour)

# for request time
Uber$Drop_time_hour <- format(strptime(Uber$Drop_time, "%H:%M:%S"), "%H")
Uber$Drop_time_hour <- as.numeric(Uber$Drop_time_hour)
typeof(Uber$Drop_time_hour)

# Create time slots for request time
# Uber$uber_time_slots <- sapply(Uber$request_time_hour, function(x) ifelse(x >= 4 & x <= 8, "early morning", 0))

Uber$request_uber_time_slots <- sapply(Uber$request_time_hour, function(x) if(x >= 4 & x <= 8) {print("early morning")} 
                                                                      else if (x >= 8 & x <= 12) {print("morning")}
                                                                               else if(x >= 12 & x <= 16) {print("afternoon")}
                                                                                        else if(x >= 16 & x <= 20) {print("evening")}
                                                                                                 else if(x >= 20 & x <= 24) {print("night")}
                                                                                                         else {print("late night")})

# save file
write.csv(Uber, "clean_uber_data.csv", row.names = FALSE)

## Question-1
# Plot Create plots to visualise the frequency of requests that get cancelled or show 'no cars available

#A. create plot for the most problematic time slots (early mornings, late evenings etc.) using plots

ggplot(Uber, aes(x=factor(request_uber_time_slots), fill = Status)) + geom_bar() + labs(x="request_uber_time_slots", y="count") + geom_text(stat = "count", aes(label=request_uber_time_slots))

# Answer
# identify most problematic request
# A. so main problematic time slot is early moring and evening
# at early morning maximum number of request status is cancelled and then next is no cars available
# also at evening time cars are not available and very few times request is cancelled

#B. identify the most problematic types of requests (city to airport / airport to city etc.)

ggplot(Uber, aes(x=Pickup.point, fill=Status)) + geom_bar()

# Answer
# B. most of the time request is cancelled at city so most problematic pickup point is city

## Question-2 
# Find out the gap between supply and demand and show the same using plots.
supply <- filter(Uber, Status == "trip completed")
nrow(supply)

demand <- filter(Uber, Status == "cancelled" | Status == "no cars available")
nrow(demand)
# the count for demand is 3914 and supply is 2831 so the difference gap is 1083 (extra request than supply)

# A. Find the time slots when the highest gap exists
ggplot(Uber, aes(x=request_time_hour, fill = Status)) + geom_bar() + labs(x="Time slot", y="No. of booking requests") + ggtitle("Time slots Vs Booking request for trip status")

# Answer
# highest gap is between 16 Hr to 24 hour means in evening and night slots the supply and demand gap is highest

# B. Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
ggplot(Uber, aes(x=request_time_hour, fill = Pickup.point)) + geom_bar(position = "stack") + labs(x="Time slot",y="No. of booking requests") + ggtitle("Pickup point frequency for airport and city")

# Answer
# As per graph less pickup is from city so for request from city-airport gap is more severe in identified time slots

## Question-3 
# What do you think is the reason for this issue for the supply-demand gap?

# Answer
# As per our graph the supply and demand gap is high for evening(16Hr-20Hr) and night(20Hr-24Hr) time slots
# most of the flights arrival time at airport is 24Hr to 4Hr so if driver take passenger request
# between 16Hr to 24Hr then for driver need to wait till late night so he will get new passenger ride
# like avg trip time is 1hr 20min so if drive take trip at 16hr then he will reach at 17hr 20min
# so he need to wait 4-5 hour for next trip.

## Question-4
# Recommend some ways to resolve the supply-demand gap.
# We can give bonus to driver who will complete maximum ride in this gap, like set rule that if
# driver complete more than 12 or 15 trip in that severe time slot we calculate above then driver
# will get bonus amount

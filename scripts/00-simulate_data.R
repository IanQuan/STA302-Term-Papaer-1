#### Preamble ####
# Purpose: Simulates neighbourhood's crime rate data in Toronto
# Author: Ian Quan
# Date: 18 Janurary 2023
# Contact: ian.quan@mail.utoronto.ca 
# License: MIT


#### Workspace setup ####
library(tidyverse)


#### Simulate data ####

# Set seed for reproducibility
set.seed(123)

# Simulate the different crime rates in Toronto in different years
crime_data <- data.frame(
  year = seq(2014, 2023),
  assault_rate = rnorm(10, mean = 100, sd = 20),
  auto_theft_rate = rnorm(10, mean = 50, sd = 10),
  bike_theft_rate = rnorm(10, mean = 20, sd = 5),
  break_enter_rate = rnorm(10, mean = 80, sd = 15),
  homicide_rate = rnorm(10, mean = 5, sd = 2),
  robbery_rate = rnorm(10, mean = 40, sd = 10),
  shooting_rate = rnorm(10, mean = 10, sd = 5),
  theft_from_motor_vehicle_rate = rnorm(10, mean = 60, sd = 12),
  theft_over_rate = rnorm(10, mean = 30, sd = 8)
)

print(crime_data)


# A list of Toronto neighborhoods
neighborhoods <- c("Downtown", "Scarborough", "North York", "Etobicoke", "East York", "York", "Toronto Islands", "Old Toronto", "West Toronto", "East Toronto")

# Simulate the neighborhoods's average crime rates
neighborhood_crime_data <- data.frame(
  neighborhood = neighborhoods,
  assault_rate = rnorm(10, mean = 80, sd = 15),
  auto_theft_rate = rnorm(10, mean = 40, sd = 10),
  bike_theft_rate = rnorm(10, mean = 15, sd = 5),
  break_enter_rate = rnorm(10, mean = 70, sd = 12),
  homicide_rate = rnorm(10, mean = 3, sd = 1.5),
  robbery_rate = rnorm(10, mean = 30, sd = 8),
  shooting_rate = rnorm(10, mean = 8, sd = 4),
  theft_from_motor_vehicle_rate = rnorm(10, mean = 50, sd = 10),
  theft_over_rate = rnorm(10, mean = 25, sd = 6)
)

print(neighborhood_crime_data)




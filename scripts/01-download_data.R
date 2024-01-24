#### Preamble ####
# Purpose: Downloads and saves the data from https://open.toronto.ca/dataset/neighbourhood-crime-rates/
# Author: Ian Quan
# Date: 18 Janurary 2023
# Contact: ian.quan@mail.utoronto.ca 
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(sf)
library(sp)
library(RColorBrewer)
library(classInt)

#### Download data ####
# get package
package <- show_package("neighbourhood-crime-rates")
package

# get all resources for this package
resources <- list_package_resources("neighbourhood-crime-rates")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
toronto_crime_rates <- filter(datastore_resources, row_number()==1) %>% get_resource()
head(toronto_crime_rates)


# The following handles the spatial data, which is included in this script because 
# storing spatial data in a csv file will cause data instability
crime_rates_2023_clean <- clean_names(toronto_crime_rates) |>
  select(area_name, population_2023, assault_rate_2023, autotheft_rate_2023,
         biketheft_rate_2023, breakenter_rate_2023, homicide_rate_2023,
         robbery_rate_2023, shooting_rate_2023, theftfrommv_rate_2023, 
         theftover_rate_2023
  ) |>
  mutate(across(-c(area_name, population_2023), ~./1000))


pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette

# Create a list of crime rate column names in 2023
crime_rate_columns <- c(
  "assault_rate_2023",
  "autotheft_rate_2023",
  "biketheft_rate_2023",
  "breakenter_rate_2023",
  "homicide_rate_2023",
  "robbery_rate_2023",
  "shooting_rate_2023",
  "theftfrommv_rate_2023",
  "theftover_rate_2023"
)

crime_rate_name_columns <- c(
  "assault_rate",
  "auto_theft_rate",
  "bike_theft_rate",
  "break_enter_rate",
  "homicide_rate",
  "robbery_rate",
  "shooting_rate",
  "theft_from_motor_vehicle_rate",
  "theft_over_rate"
)

# Set up the layout for multiple plots
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Iterate through each crime rate column and plot
lapply(seq_along(crime_rate_columns), function(i) {
  crime_rate_column <- crime_rate_columns[i]
  crime_rate_name_column <- crime_rate_name_columns[i]
  
  png(file.path("outputs/plots", paste0("plot_", crime_rate_column, ".png")), width = 600, height = 400)
  
  # Plot the crime rate map
  plot(crime_rates_2023_clean[crime_rate_column], 
       main = paste("Toronto", gsub("_", " ", crime_rate_name_column), "density per square km"), 
       breaks = "quantile", nbreaks = 7, 
       pal = pal,
       cex.main = 1.5)
  # Save the plot as a PNG file
  dev.off()
})

png(file.path("outputs/plots", paste0("plot_", "population_2023", ".png")), width = 600, height = 400)
plot(crime_rates_2023_clean["population_2023"], 
     main = paste("Toronto population density per square km in 2023"), 
     breaks = "quantile", nbreaks = 7, 
     pal = pal,
     cex.main = 1.5)
dev.off()

#### Save data ####
# [...UPDATE THIS...]
# change the_raw_data to whatever name you assigned when you downloaded it.
write_csv(toronto_crime_rates, "inputs/data/raw_data.csv") 


         

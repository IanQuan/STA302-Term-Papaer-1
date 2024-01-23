#### Preamble ####
# Purpose: Simulates... [...UPDATE THIS...]
# Author: Ian Quan
# Date: 18 Janurary 2023
# Contact: ian.quan@mail.utoronto.ca 
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(janitor)
library(knitr)
library(ggplot2)
library(dplyr)


#### Simulate data ####

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

toronto_crime_rates_clean <-
  clean_names(toronto_crime_rates) |>
  select(area_name, population_2023, 
           assault_rate_2014, assault_rate_2015, assault_rate_2016, assault_rate_2017, assault_rate_2018, assault_rate_2019, assault_rate_2020, assault_rate_2021, assault_rate_2022, assault_rate_2023, 
           autotheft_rate_2014, autotheft_rate_2015, autotheft_rate_2016, autotheft_rate_2017, autotheft_rate_2018, autotheft_rate_2019, autotheft_rate_2020, autotheft_rate_2021, autotheft_rate_2022, autotheft_rate_2023,
           biketheft_rate_2014, biketheft_rate_2015, biketheft_rate_2016, biketheft_rate_2017, biketheft_rate_2018, biketheft_rate_2019, biketheft_rate_2020, biketheft_rate_2021, biketheft_rate_2022, biketheft_rate_2023,
           breakenter_rate_2014, breakenter_rate_2015, breakenter_rate_2016, breakenter_rate_2017, breakenter_rate_2018, breakenter_rate_2019, breakenter_rate_2020, breakenter_rate_2021, breakenter_rate_2022, breakenter_rate_2023,
           homicide_rate_2014, homicide_rate_2015, homicide_rate_2016, homicide_rate_2017, homicide_rate_2018, homicide_rate_2019, homicide_rate_2020, homicide_rate_2021, homicide_rate_2022, homicide_rate_2023,
           robbery_rate_2014, robbery_rate_2015, robbery_rate_2016, robbery_rate_2017, robbery_rate_2018, robbery_rate_2019, robbery_rate_2020, robbery_rate_2021, robbery_rate_2022, robbery_rate_2023,
           shooting_rate_2014, shooting_rate_2015, shooting_rate_2016, shooting_rate_2017, shooting_rate_2018, shooting_rate_2019, shooting_rate_2020, shooting_rate_2021, shooting_rate_2022, shooting_rate_2023,
           theftfrommv_rate_2014, theftfrommv_rate_2015, theftfrommv_rate_2016, theftfrommv_rate_2017, theftfrommv_rate_2018, theftfrommv_rate_2019, theftfrommv_rate_2020, theftfrommv_rate_2021, theftfrommv_rate_2022, theftfrommv_rate_2023,
           theftover_rate_2014, theftover_rate_2015, theftover_rate_2016, theftover_rate_2017, theftover_rate_2018, theftover_rate_2019, theftover_rate_2020, theftover_rate_2021, theftover_rate_2022, theftover_rate_2023
         )
         

head(toronto_crime_rates_clean)


write_csv(
  x = toronto_crime_rates_clean,
  file = "toronto_crime_rates_clean.csv"
)




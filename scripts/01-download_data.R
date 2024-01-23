#### Preamble ####
# Purpose: Downloads and saves the data from [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


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



crime_rates_clean <-
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
str(crime_rates_clean)

average_crime_rate <- 
  crime_rates_clean |>
  summarise(across(
    starts_with("assault_rate") |
      starts_with("autotheft_rate") |
      starts_with("biketheft_rate") |
      starts_with("breakenter_rate") |
      starts_with("homicide_rate") |
      starts_with("robbery_rate") |
      starts_with("shooting_rate") |
      starts_with("theftfrommv_rate") |
      starts_with("theftover_rate"),
    ~ mean(. / 1000, na.rm = TRUE),
    .names = "avg_{.col}"
  ))

average_crime_rate <- 
  average_crime_rate %>%
  st_drop_geometry()

average_crime_rate <- average_crime_rate %>%
  gather(key = "crime_type_year", value = "crime_rate") %>%
  separate(col = "crime_type_year", into = c("prefix", "crime_type", "year"), sep = "_", extra = "merge")%>% 
  mutate(year = sub("rate_", "", year)) %>%  # Remove "rate" from the "year" column
  select(-prefix) %>%  # Remove the "prefix" column
  spread(key = "crime_type", value = "crime_rate")

average_crime_rate$year <- as.numeric(average_crime_rate$year)

head(average_crime_rate)

######################################################################################################

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

######################################################################################
head(crime_rates_2023_clean)

crime_rates_2023_no_geometry_clean <- 
  crime_rates_2023_clean %>%
  st_drop_geometry()

crime_rates_2023_no_geometry_clean[, c("assault_rate_2023", "autotheft_rate_2023", "biketheft_rate_2023", "breakenter_rate_2023", "homicide_rate_2023", "robbery_rate_2023", "shooting_rate_2023", "theftfrommv_rate_2023", "theftover_rate_2023")] <- 
  lapply(crime_rates_2023_no_geometry_clean[, c("assault_rate_2023", "autotheft_rate_2023", "biketheft_rate_2023", "breakenter_rate_2023", "homicide_rate_2023", "robbery_rate_2023", "shooting_rate_2023", "theftfrommv_rate_2023", "theftover_rate_2023")], as.numeric)

crime_rates_2023_no_geometry_clean$avg_crime_rates <- 
  rowSums(crime_rates_2023_no_geometry_clean[, c("assault_rate_2023", "autotheft_rate_2023", 
                                     "biketheft_rate_2023", "breakenter_rate_2023", 
                                     "homicide_rate_2023", "robbery_rate_2023", 
                                     "shooting_rate_2023", "theftfrommv_rate_2023", 
                                     "theftover_rate_2023")], na.rm = TRUE) / 9

# Sort the dataframe by avg_crime_rates in descending order
sorted_data <- 
  crime_rates_2023_no_geometry_clean[order(crime_rates_2023_no_geometry_clean$avg_crime_rates, 
                                           decreasing = TRUE), ]

# Select the top 20 rows
top_20_areas <- head(sorted_data, 20)

png(file.path("outputs/plots", paste0("plot_top_20_neighbourhood.png")), width = 600, height = 400)
ggplot(top_20_areas, aes(x = reorder(area_name, -avg_crime_rates), y = avg_crime_rates)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Top 20 Neighbourhood's Average Crime Rates", x = "Area Name", y = "Average Crime Rates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0)) +  
  coord_flip()  
dev.off()



#### Save data ####
# [...UPDATE THIS...]
# change the_raw_data to whatever name you assigned when you downloaded it.
write_csv(toronto_crime_rates, "inputs/data/raw_data.csv") 
write_csv(crime_rates_clean, "outputs/data/crime_rates_clean.csv") 
write_csv(average_crime_rate, "outputs/data/average_crime_rate.csv")
write_csv(crime_rates_2023_clean, "outputs/data/crime_rates_2023_clean.csv")

         

#### Preamble ####
# Purpose: Cleans the raw data into average crime rates in 2023 and crime rates over time
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

#### Clean data ####

# Read the raw data
toronto_crime_rates <- read_csv("inputs/data/raw_data.csv")

# Clean the dataset into average crime rates from 2014 to 2023
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

# Calculate average crime rates from 2014 to 2023
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

# Reshape data for better visualization
average_crime_rate <- average_crime_rate %>%
  gather(key = "crime_type_year", value = "crime_rate") %>%
  separate(col = "crime_type_year", into = c("prefix", "crime_type", "year"), sep = "_", extra = "merge")%>% 
  mutate(year = sub("rate_", "", year)) %>%  # Remove "rate" from the "year" column
  select(-prefix) %>%  # Remove the "prefix" column
  spread(key = "crime_type", value = "crime_rate")

average_crime_rate$year <- as.numeric(average_crime_rate$year)

head(average_crime_rate)


######################################################################################
# Process 2023 crime rates data
head(crime_rates_2023_clean)

# Drop geometry column for non-geospatial analysis
crime_rates_2023_no_geometry_clean <- 
  crime_rates_2023_clean %>%
  st_drop_geometry()

# Convert specific columns to numeric
crime_rates_2023_no_geometry_clean[, c("assault_rate_2023", "autotheft_rate_2023", "biketheft_rate_2023", "breakenter_rate_2023", "homicide_rate_2023", "robbery_rate_2023", "shooting_rate_2023", "theftfrommv_rate_2023", "theftover_rate_2023")] <- 
  lapply(crime_rates_2023_no_geometry_clean[, c("assault_rate_2023", "autotheft_rate_2023", "biketheft_rate_2023", "breakenter_rate_2023", "homicide_rate_2023", "robbery_rate_2023", "shooting_rate_2023", "theftfrommv_rate_2023", "theftover_rate_2023")], as.numeric)

# Calculate average crime rates for 2023
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
write_csv(average_crime_rate, "outputs/data/average_crime_rate.csv")
write_csv(crime_rates_clean, "outputs/data/crime_rates_clean.csv") 
write_csv(crime_rates_2023_clean, "outputs/data/crime_rates_2023_clean.csv")


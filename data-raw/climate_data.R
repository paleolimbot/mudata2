
library(tidyverse)
library(mudata2)

ecdata <- function(locs, ...) {
  rclimateca::getClimateData(locs, ..., nicenames = TRUE) %>%
    left_join(rclimateca::ecclimatelocs %>% 
                select(station_name = Name, stationid = `Station ID`),
              by = "stationid") %>%
    select(station_name, year, month, date = parseddate, contains("temp"), contains("precip")) %>%
    set_names(., stringr::str_replace(names(.), "(temp|precip)$", "_\\1") %>%
                stringr::str_replace("(max|min)", "_\\1")) %>%
    mutate(station_name = stringr::str_to_title(station_name)) %>%
    as_tibble()
}

eclocs <- function(locs) {
  # get locations
  rclimateca::ecclimatelocs %>%
    filter(`Station ID` %in% locs) %>%
    select(station_id = `Station ID`, location = Name, province = Province,
           elevation_m = `Elevation (m)`, latitude = `Latitude (Decimal Degrees)`,
           longitude = `Longitude (Decimal Degrees)`) %>%
    mutate(location = stringr::str_to_title(location)) %>%
    as_tibble()
}

# get data for longest running climate stations in each province, write to canada_climate.csv
canada_climate <- ecdata(c(5051, 5345, 735L, 3912, 2925, 6206, 2205, 6454, 
                         1586, 1650, 6693, 1786, 6527)) %>%
  group_by(station_name, year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE), 
            extr_max_temp = max(extr_max_temp, na.rm = TRUE),
            extr_min_temp = min(extr_min_temp, na.rm = TRUE),
            total_precip = mean(total_precip, na.rm = TRUE) * 12)

canada_climate_locations <- eclocs(c(5051, 5345, 735L, 3912, 2925, 6206, 2205, 6454, 
                                     1586, 1650, 6693, 1786, 6527))

canada_climate_long <- canada_climate %>%
  ungroup() %>%
  rename(location = station_name) %>%
  gather(-location, -year, key = "param", value = "value") %>%
  filter(is.finite(value))

canada_climate_params <- canada_climate_long %>%
  ungroup() %>%
  distinct(param) %>%
  mutate(unit = if_else(endsWith(param, "temp"), "degrees Celcius", "mm"))
  
canada_climate <- mudata(canada_climate_long, locations = canada_climate_locations,
                         params = canada_climate_params, dataset_id = "ecclimate_yearly",
                         x_columns = "year") %>%
  update_datasets("ecclimate_yearly", url = "http://climate.weather.gc.ca/") %>%
  validate_mudata()




library(tidyverse)
library(mudata2)
library(readxl)
library(lubridate)

temp_data <- list.files("data-raw/second_lake_temp/", "\\.xlsx$", full.names = TRUE) %>%
  set_names() %>%
  map_dfr(read_excel, skip = 1, .id = "file") %>%
  mutate(location = "Second Lake", param = "temp") %>%
  extract(file, "depth", "([0-9.]+)\\.xlsx$") %>%
  mutate(depth = as.numeric(gsub("\\.$", "", depth))) %>%
  select(location, param, depth, datetime = 3, value = 4) %>%
  filter(datetime >= as.POSIXct("2013-07-10"), datetime <= as.POSIXct("2013-11-10")) %>%
  mutate(datetime = with_tz(datetime + dhours(3), "UTC")) %>%
  # remove values that are the same as the previous to save size (there are many)
  mutate(is_prev = value == lag(value)) %>%
  filter(!is_prev | is.na(is_prev)) %>%
  select(-is_prev)

# ggplot(temp_data, aes(datetime, value)) +
#   geom_line() +
#   facet_wrap(~depth, ncol = 1)

second_lake_temp <- mudata(temp_data, dataset_id = "second_lake_temp", 
                           x_columns = c("datetime", "depth")) %>%
  update_datasets("second_lake_temp",
                  title = "A multi-proxy comparative paleolimnological study of anthropogenic impact between First and Second Lake, Lower Sackville, Nova Scotia",
                  type = "B.Sc.H. Thesis",
                  author = "Misiuk, Benjamin",
                  address = "Wolfville, NS",
                  year = 2014,
                  school = "Acadia University",
                  url = "http://openarchive.acadiau.ca/cdm/singleitem/collection/HTheses/id/1010/rec/1") %>%
  update_locations("Second Lake", latitude = 44.78510913, longitude = -63.66417714) %>%
  update_params("temp", method = "Thermistor", unit = "degees Celcius") %>%
  update_columns_table() %>%
  update_columns("datetime", description = "Date/time in UTC") %>%
  update_columns("depth", description = "Depth in the water column (meters)")

devtools::use_data(second_lake_temp, overwrite = TRUE)


library(magrittr)

# get sample climate data using rclimateca package

kv_locations <- rclimateca::ecclimatelocs %>% 
  dplyr::filter(`Station ID` %in% c(27141, 6354)) %>%
  dplyr::mutate(dataset = "ecclimate") %>%
  dplyr::select(dataset, location = Name, stationid = `Station ID`,
                latitude = `Latitude (Decimal Degrees)`,
                longitude = `Longitude (Decimal Degrees)`, 
                province = Province)

kentvillegreenwood_raw <- rclimateca::getClimateData(c(27141, 6354), year=1999, 
                                                     month=7:8, timeframe="daily",
                                                     format = "long")

kv_params <- kentvillegreenwood_raw %>%
  dplyr::mutate(dataset = "ecclimate") %>%
  dplyr::distinct(dataset, param) %>%
  dplyr::mutate(param_nice = rclimateca:::nice.names(param)) %>%
  dplyr::mutate(label = gsub("°", "", param)) %>%
  dplyr::select(dataset, param = param_nice, label)

kv_data <- kentvillegreenwood_raw %>%
  dplyr::mutate(dataset = "ecclimate") %>% 
  dplyr::mutate(param = rclimateca:::nice.names(as.character(param))) %>%
  dplyr::left_join(kv_params, by = "param") %>%
  dplyr::left_join(kv_locations, by = c("stationID" = "stationid")) %>%
  dplyr::select(dataset, location, param, date = parsedDate,
                value, flags)

kv_datasets <- tibble::tibble(dataset = "ecclimate", url = "http://climate.weather.gc.ca/")

kentvillegreenwood <- mudata(kv_data, locations = kv_locations, params = kv_params,
                             datasets = kv_datasets)

# plot data to check
# qualifierplot(kentvillegreenwood$data, 
#               id.vars = c("dataset", "location", "param", "date"), 
#                measure.var = "value")

devtools::use_data(kentvillegreenwood, overwrite = TRUE)
unlink("ec.cache/", recursive = TRUE)

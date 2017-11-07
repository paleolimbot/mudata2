
library(tidyverse)
library(mudata2)
library(rclimateca)
library(stringr)

locs_tidy <- ecclimatelocs %>%
  set_names(., tolower(names(.)) %>% str_replace("\\s\\(.*?\\)$", "") %>% str_replace_all("\\s", "_")) %>%
  .[unique(names(.))] %>%
  as_tibble() 

ns_locs <- locs_tidy %>%
  mutate(total_years = last_year - first_year) %>%
  select(name, province, total_years, station_id, latitude, longitude, first_year, last_year) %>%
  arrange(desc(total_years)) %>%
  filter(province == "NOVA SCOTIA", total_years > 80)

ns_data_monthly <- getClimateData(ns_locs$station_id, timeframe = "monthly")
ns_data_monthly_flags <- tibble(station_id = ns_locs$station_id) %>%
  mutate(raw = map(station_id, getClimateDataRaw, timeframe = "monthly", 
                   flag.info = TRUE, .cache = "ec.cache", .quiet = TRUE)) %>%
  mutate(flags = map(raw, "flags")) %>%
  pull(flags) %>%
  bind_rows() %>%
  distinct()

ns_data_monthly_tidy <- ns_data_monthly %>%
  set_names(., tolower(names(.)) %>% str_replace("\\s\\(.*?\\)$", "") %>% str_replace_all("[^a-z0-9]+", "_")) %>%
  .[unique(names(.))] %>%
  as_tibble()

flags <- names(ns_data_monthly_tidy) %>% str_subset("flag$")
vals <- str_replace(flags, "_flag$", "")
nslong <- ns_data_monthly_tidy %>%
  select(stationid, date = parseddate, one_of(vals), one_of(flags)) %>%
  parallel_gather(value = one_of(vals), flag = one_of(flags), key = "param") %>%
  mutate(flag = na_if(flag, ""), value = as.numeric(str_replace(value, "^<", ""))) %>%
  filter(!(is.na(value) & is.na(flag))) %>%
  rename(station_id = stationid)

# ggplot(nslong, aes(date, value, col = factor(station_id))) +
#   geom_line() +
#   facet_wrap(~param, scales = "free_y")

ns_climate_locs <- locs_tidy %>%
  filter(station_id %in% unique(nslong$station_id)) %>%
  mutate(location = paste(name, station_id))

ns_climate_params <- tibble(
  label = names(ns_data_monthly),
  param = tolower(label) %>% 
    str_replace("\\s\\(.*?\\)$", "") %>% 
    str_replace_all("[^a-z0-9]+", "_")
) %>%
  filter(param %in% unique(nslong$param)) %>%
  mutate(label = str_replace_all(label, "[^A-Za-z0-9'/() ]", "")) %>%
  extract(label, "unit", "\\((.*?)\\)", remove = FALSE)

ns_climate_data <- nslong %>%
  left_join(ns_climate_locs %>% select(station_id, location), by = "station_id") %>%
  left_join(ns_data_monthly_flags, by = "flag") %>%
  select(-station_id) %>%
  rename(flag_text = description)

ns_climate <- mudata(
  data = ns_climate_data,
  locations = ns_climate_locs,
  params = ns_climate_params,
  dataset_id = "ecclimate_monthly",
  x_columns = "date"
) %>%
  update_datasets("ecclimate_monthly", url = "http://climate.weather.gc.ca/") %>%
  update_columns_table() %>%
  update_columns("date", description = "First day of the month for which data are representative") %>%
  update_columns(c("flag", "flag_text"), description = "Flags provide extra data to qualify value") %>%
  update_columns("label", description = "Original Environment Canada column label") %>%
  update_columns("unit", description = "Unit of measurement for values with this param")

devtools::use_data(ns_climate, overwrite = TRUE)
unlink("ec.cache", recursive = TRUE)


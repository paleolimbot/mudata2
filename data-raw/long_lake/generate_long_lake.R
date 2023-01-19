
library(tidyverse)
library(mudata2)

# read geochem
# blank values were not measured, ND is non-detect
geochem <- bind_rows(
  read_csv("data-raw/long_lake/longlake_gc10.csv", col_types = cols(.default = col_character())),
  read_csv("data-raw/long_lake/longlake_pc2_nounits.csv", col_types = cols(.default = col_character()))
)

# assign non ppm units
units <- c(
  "LOI" = "%", "C" = "%", "N" = "%", "C/N" = NA_character_,
  "Hg" = "ppb", "d13C" = "per mille", "d15N" = "per mille",
  "Fe/Mn" = NA_character_
)

geochem_long <- geochem %>%
  gather(-(location_id:sample_id), key = "param", value = "value") %>%
  filter(!is.na(value)) %>%
  mutate(non_detect = value == "ND") %>%
  mutate(
    value = na_if(value, "ND") %>% as.numeric(),
    depth = as.numeric(depth)
  ) %>%
  group_by(location_id, Unit, param, depth) %>%
  summarise(
    val = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
    n = n(), n_detect = sum(!non_detect)
  ) %>%
  rename(value = val, location = location_id, unit = Unit) %>%
  mutate(units = if_else(param %in% names(units), units[param], "ppm"))

# put in 14C ages
c14_ages <- read_csv("data-raw/long_lake/LL-PC1.csv",
  col_types = cols(
    labID = col_character(),
    age = col_integer(),
    error = col_integer(),
    depth = col_double(),
    cc = col_integer()
  )
)

c14_ages_long <- c14_ages %>%
  mutate(location = "LL PC2", param = "14C_age", units = "14C Years") %>%
  select(location, param, depth, value = age, sd = error, units)

# put in bacon ages
bacon_ages <- read_delim("data-raw/long_lake/LL-PC1_43_ages.txt",
  delim = "\t",
  col_types = cols(
    depth = col_integer(),
    min = col_double(),
    max = col_double(),
    median = col_double(),
    wmean = col_double()
  )
)

bacon_ages_long <- bacon_ages %>%
  mutate(location = "LL PC2", param = "bacon_age", units = "years before 1950") %>%
  select(location, param, depth, value = wmean, units, min_value = min, max_value = max)


data_long <- bind_rows(geochem_long, c14_ages_long, bacon_ages_long) %>%
  ungroup() %>%
  filter(param %in% c(
    "bacon_age", "14C_age", "LOI", "C/N", "Ti", "Zr", "Fe/Mn",
    "Zn", "As", "S", "Cl", "Pb", "Hg", "Cr", "Y", "d15N", "d13C"
  )) %>%
  arrange(location, param, depth) %>%
  select(location, param, depth, value, sd, units, zone = unit, everything())

long_lake <- mudata(data_long, dataset_id = "long_lake17", x_columns = "depth") %>%
  # add param metadata
  update_params("bacon_age", method = "BACON (Blaauw and Christen 2011)") %>%
  update_params("14C_age", method = "NSF Arizona/Lalonde AMS Ottawa") %>%
  update_params("LOI", method = "Muffle furnace/550 degrees") %>%
  update_params(c("C/N", "d15N", "d13C"), method = "SINLAB/University of New Brunswick") %>%
  update_params(c("Ti", "Zr", "Zn", "As", "S", "Cl", "Pb", "Cr", "Y", "Fe/Mn"),
    method = "Portable XRF/Olympus X-50"
  ) %>%
  update_params("Hg", method = "Thermal decomposition/gold amalgamation atomic absorbance/Nippon MA-2000") %>%
  # add location metadata
  # PC2: 45°55′33′′N, 64°9′42′′W; GC10: 45°54′30′′N, 64°9′48′′W
  update_locations("LL PC2", latitude = 45.92583, longitude = 64.161667) %>%
  update_locations("LL GC10", latitude = 45.90833, longitude = 64.16333) %>%
  # add dataset metadata
  update_datasets("long_lake17",
    doi = "10.1139/facets-2017-0004",
    year = 2017,
    url = "http://www.facetsjournal.com/article/facets-2017-0004/",
    title = "A paleolimnological archive of metal sequestration and release in the Cumberland Basin Marshes, Atlantic Canada",
    authors = "Dunnington, Dewey W. and White, Hilary and Spooner, Ian S. and Mallory, Mark L. and White, Chris and O'Driscoll, Nelson J. and McLellan, Nic R.",
    journal = "FACETS",
    volume = 2, pages = "440-460"
  ) %>%
  # add column metadata
  update_columns_table() %>%
  update_columns("sd", description = "standard deviation of value") %>%
  update_columns("units", description = "units of value, sd, min_value, and max_value") %>%
  update_columns("n", description = "number of values measured for parameter") %>%
  update_columns("n_detect", description = "number of values contributing to value") %>%
  update_columns("zone", description = "geochemical zone/unit as described in the article")

devtools::use_data(long_lake, overwrite = TRUE)

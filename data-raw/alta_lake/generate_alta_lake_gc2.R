
library(tidyverse)
library(mudata2)

gc2data <- read_delim("data-raw/dunnington16a_alta_lake.txt",
  delim = "\t",
  col_types = cols(
    .default = col_double(),
    zone = col_character(),
    n_As = col_integer(),
    n_Cu = col_integer(),
    n_Pb = col_integer(),
    n_Zn = col_integer(),
    `n_Fe/Mn` = col_integer(),
    n_Mo = col_integer(),
    n_K = col_integer(),
    n_Rb = col_integer(),
    n_Ti = col_integer(),
    n_C = col_integer(),
    `n_C/N` = col_integer(),
    n_d13C = col_integer(),
    n_d15N = col_integer(),
    n_SAR = col_integer()
  )
)
# assign units
units <- c(
  As = "ppm", C = "%", Cu = "ppm", "d13C" = "per mille",
  d15N = "per mille", K = "%", Mo = "ppm", Pb = "ppm",
  Rb = "ppm", SAR = "g/m2/yr", Ti = "%", Zn = "ppm"
)
gc2_long <- parallel_gather(gc2data,
  value = As:SAR, stdev = stdev_As:stdev_SAR, n = n_As:n_SAR,
  key = "param"
) %>%
  mutate(units = units[param]) %>%
  select(age, depth, param, value, stdev, units, n, zone, everything())


alta_lake <- mudata(gc2_long,
  dataset_id = "alta_lake16", location_id = "ALGC2",
  x_columns = c("depth", "age")
) %>%
  update_datasets("alta_lake16",
    doi = "10.1007/s10933-016-9919-x",
    year = 2016,
    url = "http://link.springer.com/article/10.1007/s10933-016-9919-x",
    title = "A geochemical perspective on the impact of development at Alta Lake, British Columbia, Canada",
    authors = "Dunnington, Dewey W. and Spooner, Ian S. and White, Chris E. and Cornett, R. Jack and Williamson, Dave and Nelson, Mike",
    journal = "Journal of Paleolimnology",
    volume = 56, pages = "315-330"
  ) %>%
  # document locations, params
  update_locations("ALGC2", longitude = -122.98125843, latitude = 50.11765695) %>%
  update_params(c("As", "Cu", "Fe/Mn", "K", "Mo", "Pb", "Rb", "Ti", "Zn"),
    method = "Portable XRF Olympus X-50"
  ) %>%
  update_params(c("C", "C/N", "d13C", "d15N"), method = "SINLAB/University of New Brunswick") %>%
  update_params("SAR", method = "210Po Assay/CRS Model (R.J. Cornett)") %>%
  # document columns
  update_columns_table() %>%
  update_columns("depth", description = "Depth in core (cm)") %>%
  update_columns("age", description = "210Pb/CRS Model ages in year AD (R.J. Cornett)") %>%
  update_columns("stdev", description = "Standard deviation of value") %>%
  update_columns("n", description = "Number of replicates contributing to value") %>%
  update_columns("zone", description = "Zone, as identified in article text") %>%
  update_columns("units", description = "Units of value, with NA as unitless")

devtools::use_data(alta_lake, overwrite = TRUE)

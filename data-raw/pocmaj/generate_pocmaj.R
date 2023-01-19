# generate pocmaj and pocmajsum datasets

library(dplyr)
library(reshape2)

pocmaj <- read.csv("data-raw/pocmaj.csv", stringsAsFactors = F)

pocmajsum <- pocmaj %>%
  melt(id.vars = c("core", "depth")) %>%
  group_by(core, variable, depth) %>%
  summarise(sd = sd(value), value = mean(value)) %>%
  melt(id.vars = c("core", "depth", "variable"), variable.name = "valtype") %>%
  dcast(core + depth ~ variable + valtype)
# fix names
names(pocmajsum) <- gsub("_value", "", names(pocmajsum))
# reorder columns
pocmajsum <- pocmajsum[c("core", "depth", "Ca", "Ca_sd", "Ti", "Ti_sd", "V", "V_sd")]

devtools::use_data(pocmaj, overwrite = TRUE)
devtools::use_data(pocmajsum, overwrite = TRUE)

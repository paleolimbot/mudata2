
# get sample climate data using rclimateca package

kentvillegreenwood <- rclimateca::getClimateMUData(c(27141, 6354), year=1999, month=7:8, timeframe="daily")

# remove degree symbols for 'label'
kentvillegreenwood$params$label <- gsub("°", "", kentvillegreenwood$params$label)

#plot(kentvillegreenwood)
#biplot(kentvillegreenwood, c("maxtemp", "meantemp", "mintemp"), col="location")

devtools::use_data(kentvillegreenwood, overwrite = TRUE)

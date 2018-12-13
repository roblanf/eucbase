library(dplyr)
library(raster)
library(sp)
#df <- read.csv("data/example_input.csv")
#temp <- filter(df, Species == "Eucalyptus dwyeri")
#data <- temp

gridMean <- function(data, km) {
  # Change class of species to chr
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  e <- setExt(data)
  out1 <- data %>%
    group_by(Species) %>%
    do(rMGrid(., km, e))
  return(as.data.frame(out1))
}

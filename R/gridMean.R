library(dplyr)
library(raster)
#df <- read.csv("data/example_input.csv")
#temp <- filter(df, Species == "Corymbia calophylla")
#data <- temp

gridMean <- function(data, km) {
  # Change class of species to chr
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  out1 <- data %>%
    group_by(Species) %>%
    do(rMGrid(., km))
  colnames(out1)[2] <- "cellID"
  colnames(out1)[3:ncol(out1)] <- colnames(data)[2:ncol(data)]
  return(as.data.frame(out1))
}
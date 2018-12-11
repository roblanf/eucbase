library(dplyr)
library(raster)
library(sp)
#df <- read.csv("data/example_input.csv")
#temp <- filter(df, Species == "Corymbia calophylla")
#data <- temp

gridMean <- function(data, km) {
  # Change class of species to chr
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  e <- setExt(data)
  out1 <- data %>%
    group_by(Species) %>%
    do(nTest(.)) %>%
    do(rMGrid(., km, e))
  colnames(out1)[2] <- "cellID"
  colnames(out1)[3:ncol(out1)] <- colnames(data)[2:ncol(data)]
  return(as.data.frame(out1))
}

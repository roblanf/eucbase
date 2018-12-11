library(dplyr)
library(raster)
#df <- read.csv("data/example_input.csv")
#temp <- filter(df, Species == "Corymbia calophylla")
#data <- temp

speciesMean <- function(data, km) {
  # Change class of species to chr
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  e <- setExt(data)
  out1 <- data %>%
    group_by(Species) %>%
    do(rMSpp(., km, e))
  colnames(out1) <- colnames(data[c(1, 4: ncol(data))])
  return(as.data.frame(out1))
}

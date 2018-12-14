library(dplyr)
library(raster)
#df <- read.csv("data/example_input.csv")
#temp <- filter(data, Species == "Eucalyptus amplifolia")
#data <- temp

speciesMean <- function(data, km) {
  # Change class of species to chr
  colnames(data)[1] <- "Species"
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  e <- setExt(data)
  colnames(data)[1] <- "Species"
  out1 <- data %>%
    group_by(Species) %>%
    do(rMSpp(., km, e))
  colnames(out1) <- colnames(data[c(1, 4: ncol(data))])
  return(as.data.frame(out1))
}

#' Grid-based means of species point occurrences
#'
#' Calculates species-level means of point occurrence-associated variables
#' (i.e. climate layers) of Australian distributions. Means are calculated 
#' based on the user-specified grid size (km^2).
#'
#' @param data Dataframe of point occurrence data and corresponding variables. 
#' First three columns must include species names, latitude and longitudes. All 
#' additional columns will contain the variables
#' @param km Numeric. Distance of grid size in km^2 to be applied
#'
#' @export
#'
#' @examples
#' data(eucs)
#' spMean <- speciesMean(eucs, 10)
#' 
#' @importFrom dplyr filter summarise_all group_by
#' @importFrom magrittr %>%
#' @import raster
#' @import sp
#' 

speciesMean <- function(data, km) {
  # Change class of species to chr
  colnames(data)[1] <- "Species"
  data[ ,1] <- sapply(data[ ,1], as.character)
  # Calculate gridded/rasterised means, grouped by species
  e <- setExt(data)
  out1 <- data %>%
    group_by(Species) %>%
    do(rMSpp(., km, e))
  return(as.data.frame(out1))
}

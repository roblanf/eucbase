#' ALA Layer Means by Species
#'
#' Calculates the mean of layer variables per specified grid size (km^2) of
#' compiled species downloaded from ALA.
#'
#' @param data ALA dataframe containing point occurrence data and layer values
#' @param km Numeric. Distance of grid size in km^2 to be sampled
#'
#' @export
#'
#' @examples
#' alaGridMean(df, 10)
#' alaGridMean(data = df, km = 10)
#'
#' @import raster
#' @import sp
#' @import dplyr
#'

alaGridMean <- function (data, km) {

  # Create species list
  species <- unique(data$Scientific.Name)
  # Remove unused columns
  #* Test that all downloads share the same format
  #* Test that all ALA downloads share the same format
  #* For future addition - look to specify columns to be calculated 
  #* so function can be appropriated to all point occurrrence data :)
  # Specify columns of values to be calculated
  nc <- ncol(data)-2
  df <- data[, c(4, 20, 21, 38:nc)]
  # Create blank df for output
  species_means <- data[NULL, c(4, 38:nc)]

  # Group by species
  for (i in species) {
    df.spp <- filter(df, Scientific.Name == i)
    # Create Spatial Dataframe and set projection
    df.spp <- SpatialPointsDataFrame(coords = df.spp[,3:2], data = df.spp,
                                proj4string = CRS("+init=epsg:4326")
    )
    # Convert projection to coordinate system
    df.spp <- spTransform(df.spp, CRS("+init=epsg:20353"))
    # Set raster
    r <- raster(ext = extent(df.spp@bbox),
                resolution = km*1000, crs ="+init=epsg:20353")

    # Calculate mean
    rMean <- rasterize(x = df.spp,
                       y = r,
                       field = df.spp@data[,4:ncol(df.spp)],
                         fun = mean,
                       na.rm = T)
    #* Calculate sd
    # Write SP | Mean | SD
    out <- summarise_all(as.data.frame(rMean@data@values), mean, na.rm = T)
    out <- cbind(i, out)
    colnames(out) <- colnames(species_means)
    species_means <- rbind(species_means, out)

  }
  species_means <<- species_means
  return(View(species_means))

}

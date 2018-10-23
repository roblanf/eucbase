#' ALA Layer Means by Species
#'
#' Calculates the mean of a layer variable per specified grid size (km^2) of
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
  nc <- ncol(data)-2
  df <- data[, c(4, 20, 21, 38:nc)]
  # Create blank df for output
  #* Change to add column per looped variable
  species_means <- data[NULL, c(4, 38)]

  # Group by species
  for (i in species) {
    q <- filter(df, Scientific.Name == i)
    # Create Spatial Dataframe and set projection
    q <- SpatialPointsDataFrame(coords = q[,3:2], data = q,
                                proj4string = CRS("+init=epsg:4326")
    )
    # Convert projection to coordinate system
    q <- spTransform(q, CRS("+init=epsg:20353"))
    # Set raster
    r <- raster(ext = extent(q@bbox),
                resolution = km*1000, crs ="+init=epsg:20353")

    # Calculate mean
    rMean <- rasterize(x = q,
                       y = r,
                       field = q$Temperature...annual.range..Bio07., # TO DO: loop for all variables / summarise_all
                       fun = mean,
                       na.rm= T)
    #* Calculate sd
    # Write SP | Mean | SD
    out <- data.frame(Name = i, Mean = mean(rMean@data@values, na.rm = T))

    species_means <- rbind(species_means, out)

  }
  species_means <<- species_means
  return(View(species_means))

}

#' ALA Layer Means by Species
#'
#' Calculates the species means of layer variables, downloaded from ALA. Means
#' are calculated as per the specified grid size in km^2. 
#'
#' @param data ALA dataframe containing point occurrence data and layer values
#' @param km Numeric. Distance of grid size in km^2 to be sampled
#' @param ncol1 The column number corresponding to the first layer. Check with colnames(data)
#' @param ncol2 The column number corresponding to the last layer. Check with colnames(data) 
#'
#' @export
#'
#' @examples
#' x <- alaSpeciesMean(df, 10, 38, 42)
#' x <- alaSpeciesMean(data = df, km = 10, ncol1 = 38, ncol2 = 42)
#' 
#' @import dplyr
#' @import raster
#' @import sp
#'

alaSpeciesMean <- function (data, km, ncol1, ncol2) {

  # Create species list
  species <- unique(data$Scientific.Name)
  # Remove unused columns and specify columns of values to be calculated
  df <- data[, c(4, 20, 21, ncol1:ncol2)]
  # Create blank df for output
  species_means <- data[NULL, c(4, ncol1:ncol2)]

  # Iterate mean calculations by species
  for (i in species) {
    # Group by species
    df.spp <- filter(df, Scientific.Name == i)
    # Create Spatial Dataframe and set projection
    df.spp <- SpatialPointsDataFrame(coords = df.spp[,3:2], data = df.spp,
                                proj4string = CRS("+init=epsg:4326"))
    # Convert to coordinate system to enable specification of grid size in km^2
    df.spp <- spTransform(df.spp, CRS("+init=epsg:20353"))
    # Set raster and grid size
    r <- raster(ext = extent(-1731010, 2642161, 5073122, 8912215),
                resolution = km*1000, crs ="+init=epsg:20353")

    # Calculate mean
    rMean <- rasterize(x = df.spp,
                       y = r,
                       field = df.spp@data[,4:ncol(df.spp)],
                         fun = mean,
                       na.rm = T)
    #* Calculate sd
    
    # Summarise mean across all grids and Write output dataframe
    out <- summarise_all(as.data.frame(rMean@data@values), mean, na.rm = T)
    out <- cbind(i, out)
    colnames(out) <- colnames(species_means)
    species_means <- rbind(species_means, out)

  }
  return(species_means)
}

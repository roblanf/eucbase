#' ALA Layer Means per Grid
#'
#' Calculates individual cell means of layer variables, downloaded from ALA.
#' Values are grouped by species.
#'
#' @param data ALA dataframe containing point occurrence data and layer values
#' @param km Numeric. Distance of grid size in km^2 to be sampled
#' @param ncol1 The column number corresponding to the first layer. Check with colnames(data)
#' @param ncol2 The column number corresponding to the last layer. Check with colnames(data) 
#'
#' @export
#'
#' @examples
#' x <- alaGridMean(df, 10, 38, 42)
#' x <- alaGridMean(data = df, km = 10, ncol1 = 38, ncol2 = 42)
#'
#' @import dplyr
#' @import raster
#' @import sp
#' 

alaGridMean <- function (data, km, ncol1, ncol2) {

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
    #* Change extent to aus extent
    r <- raster(ext = extent(-1730951, 2642161, 5093507, 8912546),
                resolution = km*1000, crs ="+init=epsg:20353")

    # Calculate mean
    rMean <- rasterize(x = df.spp,
                       y = r,
                       field = df.spp@data[,4:ncol(df.spp)],
                         fun = mean,
                       na.rm = T)
    #* Calculate sd
    
    # Export cell ID and coordinates
    out <- data.frame(1:nrow(rMean@data@values),
                      xyFromCell(rMean, 1:nrow(rMean@data@values)),
                      rMean@data@values)
    # Remove all cells absent of species
    nlay <- ncol2-ncol1+1
    out <- out[rowSums(is.na(out))<nlay,]
    # Write species names 
    out <- cbind(i, out)
    # Rename columns
    colnames(out) <- c("Scientific.Name","cellID", "x", "y", 
                       colnames(data[, ncol1 : ncol2]))
    species_means <- rbind(species_means, out)

  }
  return(species_means)
}

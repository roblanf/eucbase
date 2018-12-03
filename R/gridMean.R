#' Grid-based means of point occurrences
#'
#' Calculates species grid means of point occurrence-associated variables
#' (i.e. climate layers). Means are calculated based on the user-specified grid
#' size (km^2).
#'
#' @param data Dataframe of point occurrence data and corresponding variables. 
#' First three columns must include species names, latitude and longitudes. All 
#' additional columns will contain the variables
#' @param km Numeric. Distance of grid size in km^2 to be applied
#'
#' @export
#'
#' @examples 
#' df <- read.csv("example_input.csv")
#' x <- gridMean(df, 10)
#'
#' @import dplyr
#' @import raster
#' @import sp
#' 

gridMean <- function (data, km) {
  # Create species list
  species <- unique(data[,1])
  # Create blank df for output
  species_means <- data[NULL, c(1, 4:ncol(data))]

  # Iterate mean calculations by species
  for (i in species) {
    print(i)
    # Group by species
    df.spp <- filter(data, data[,1] == i)
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
    
    # Export cell ID and coordinates
    out <- data.frame(1:nrow(rMean@data@values),
                      xyFromCell(rMean, 1:nrow(rMean@data@values)),
                      rMean@data@values)
    # Remove all cells absent of species
    nlay <- ncol(data)-3
    out <- out[rowSums(is.na(out))<nlay,]
    # Write species names 
    out <- cbind(i, out)
    # Rename columns
    colnames(out) <- c("Species","cellID", "x", "y", 
                       colnames(data[, 4: ncol(data)]))
    species_means <- rbind(species_means, out)

  }
  print("Done!")
  return(species_means)
}

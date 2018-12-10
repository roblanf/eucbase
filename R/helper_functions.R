# Calculates rasterised means for speciesMean and gridMean
rasMean <- function(data, km) {
  # Create Spatial Dataframe and set projection
  df.spp <- SpatialPointsDataFrame(coords = data[,3:2], data = data,
                                   proj4string = CRS("+init=epsg:4326"))
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
  return(rMean)
}

# gridMean f(x) - Extracts cellID and coordinates of grids
rMGrid <- function(data, km) {
  rasMean <- rasMean(data, km)
  # Export cell ID and coordinates
  out2 <- data.frame(1:nrow(rasMean@data@values),
                     xyFromCell(rasMean, 1:nrow(rasMean@data@values)),
                     rasMean@data@values)
  # Remove all cells absent of species
  nlay <- ncol(data)-3
  out2 <- out2[rowSums(is.na(out2))<nlay,]
  return(out2)
}

# speciesMean f(x) - summarises all raster means
rMSpp <- function(data,km) {
  rasMean <- rasMean(data, km)
  out2 <- summarise_all(as.data.frame(rasMean@data@values), mean, na.rm = T)
  return(out2)
}

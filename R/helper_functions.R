# find min/max latlongs
setExt <- function(data) {
  ext <- extent(min(data[,3]), max(data[,3]),
                min(data[,2]), max(data[,2]))
  ext <- as(ext, "SpatialPolygons")
  proj4string(ext) <- CRS("+init=epsg:4326")
  ext <- spTransform(ext, CRS("+init=epsg:20353"))
  return(extent(ext))
}

# Calculates rasterised means for speciesMean and gridMean
rasMean <- function(data, km, e) {
    # Create Spatial Dataframe and set projection
    df.spp <- SpatialPointsDataFrame(coords = data[,3:2], data = data,
                                     proj4string = CRS("+init=epsg:4326"))
    df.spp <- spTransform(df.spp, CRS("+init=epsg:20353"))
    # Set raster and grid size
    r <- raster(ext = e,
                resolution = km*1000, crs ="+init=epsg:20353")
    # Calculate mean
    rMean <- rasterize(x = df.spp,
                       y = r,
                       field = df.spp@data[,4:ncol(df.spp)],
                       fun = mean,
                       na.rm = T)
    return(rMean)
}

# Check for n = 1 rows
nTest <- function(data) {
  if(nrow(data) < 2) {
    return(rbind(data, data))
#    return(rbind(data, data))
  } else {
    return(data)
  }
  
}
# gridMean f(x) - Extracts cellID and coordinates of grids
rMGrid <- function(data, km, e) {
  rasMean <- rasMean(data, km, e)
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
rMSpp <- function(data, km) {
  rasMean <- rasMean(data, km)
  out2 <- summarise_all(as.data.frame(rasMean@data@values), mean, na.rm = T)
  return(out2)
}

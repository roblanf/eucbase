# find min/max latlongs for extent
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

# Calculates rasterised SD for speciesMean and gridMean
rasSD <- function(data, km, e) {
  # Create Spatial Dataframe and set projection
  df.spp <- SpatialPointsDataFrame(coords = data[,3:2], data = data,
                                   proj4string = CRS("+init=epsg:4326"))
  df.spp <- spTransform(df.spp, CRS("+init=epsg:20353"))
  # Set raster and grid size
  r <- raster(ext = e,
              resolution = km*1000, crs ="+init=epsg:20353")
  # Calculate SD
  rSD <- rasterize(x = df.spp,
                   y = r,
                   field = df.spp@data[,4:ncol(df.spp)],
                   fun = sd,
                   na.rm = T)
  return(rSD)
}

# Check for n = 1 rows
nTest <- function(data) {
  if(nrow(data) < 2) {
    return(rbind(data, data))
  } else {
    return(data)
  }
}

# Check for layer NA's
checkNA <- function(data) {
  if (sum(is.na(data[,4:ncol(data)])) == (nrow(data) * ncol(data[,4:ncol(data)]))) {
    print(paste(unique(data$Species), " does not have layer values. Omitting from analysis.", sep = ""))
  } else {
  }
}

# gridMean f(x) - Extracts cellID and coordinates of grids
rMGrid <- function(data, km, e) {
  checkNA(data)
  rasMean <- rasMean(data, km, e)
  rasSD   <-   rasSD(data, km, e)
  # Export cell ID and coordinates
  outM <- data.frame(1:nrow(rasMean@data@values),
                     xyFromCell(rasMean, 1:nrow(rasMean@data@values)),
                     rasMean@data@values)
  outSD <- data.frame(1:nrow(rasSD@data@values),
                    xyFromCell(rasSD, 1:nrow(rasSD@data@values)),
                    rasSD@data@values)
  # Rename columns
  colnames(outM)[1] <- colnames(outSD)[1] <- "cellID"
  colnames(outM)[4:ncol(outM)] <- paste("mean", colnames(data)[4:ncol(data)], sep = "_")
  colnames(outSD)[4:ncol(outSD)] <- paste("SD", colnames(data)[4:ncol(data)], sep = "_")
  # Join mean and SD
  out2 <- left_join(outM, outSD[, c(1, 4:ncol(outSD))], "cellID")
  # Remove all cells absent of mean values
  nlay <- ncol(data)-3
  out2 <- out2[rowSums(is.na(out2)[,4:ncol(outM)])<nlay,]
  return(out2)
}

# speciesMean f(x) - summarises all raster means
rMSpp <- function(data, km, e) {
  rasMean <- rasMean(data, km, e)
  out2 <- summarise_all(as.data.frame(rasMean@data@values), mean, na.rm = T)
  return(out2)
}

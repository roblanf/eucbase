# Set vars
#rM   <- rasMean@data@values
#rSD  <- rasSD@data@values
#4:ncol(data)  <- 4:ncol(data)

### Primary functions (called directly in user-functions) ###
rMSpp <- function(data, km, e) {
  # Check for species with layers that are all NAs
  checkNA(data)
  
  # Convert to spatial dataframe
  spdf <- spdf(data)
  
  # Calculate raster means
  rasMean <- ras(spdf, mean, km, e)
  
  # Summarise values
  out2 <- summarise_all(as.data.frame(rasMean@data@values), mean, na.rm = T)
  out2 <- out2[rowSums(is.na(out2)) < ncol(out2),]
  return(out2)
}

rMGrid <- function(data, km, e) {
  # Check for species with layers that are all NAs
  checkNA(data)
  
  # Convert to spatial dataframe
  spdf <- spdf(data)
  
  # Calculate raster means and sds
  rasMean <- ras(spdf, mean, km, e)
  rasSD   <- ras(spdf, sd,   km, e)
  
  # Export cell ID and coordinates
  outM  <- data.frame(1:nrow(rasMean@data@values ), xyFromCell(rasMean, 1:nrow(rasMean@data@values )),  rasMean@data@values)
  outSD <- data.frame(1:nrow(rasSD@data@values), xyFromCell(rasSD,   1:nrow(rasSD@data@values)), rasSD@data@values)
  
  # Rename columns
  colnames(outM )[1]   <- colnames(outSD)[1] <- "cellID"
  colnames(outM )[4:ncol(data)] <- paste("mean", colnames(data)[4:ncol(data)], sep = "_")
  colnames(outSD)[4:ncol(data)] <- paste("SD"  , colnames(data)[4:ncol(data)], sep = "_")
  
  # Join mean and SD
  out2 <- left_join(outM, outSD[, c(1, 4:ncol(data))], "cellID")
  
  # Remove all cells absent of mean values
  out2 <- out2[rowSums(is.na(out2)[,4:ncol(data)]) < (max(4:ncol(data))-3),]
  return(out2)
  
}

setExt <- function(data) {
  ext <- extent((min(data[,3])-2), (max(data[,3])+2),
                (min(data[,2])-2), (max(data[,2])+2))
  ext <- as(ext, "SpatialPolygons")
  proj4string(ext) <- CRS("+init=epsg:4326")
  ext <- spTransform(ext, CRS("+init=epsg:20353"))
  return(extent(ext))
}

### Secondary functions (called in helper (primary) functions) ###
checkNA <- function(data) { # May be able to change this to ifelse()
  if (sum(is.na(data[,4:ncol(data)])) == (nrow(data) * ncol(data[,4:ncol(data)]))) {
    print(paste(unique(data$Species), " does not have layer values. Omitting from analysis.", sep = ""))
  } else {
  }
}

spdf <- function(data) {
  spdf <- SpatialPointsDataFrame(coords = data[,3:2], data = data,
                                   proj4string = CRS("+init=epsg:4326"))
  spdf <- spTransform(spdf, CRS("+init=epsg:20353"))
  return(spdf)
}

ras <- function(spdf, fun, km, e) {
  r   <- raster(ext = e, resolution = km*1000, crs ="+init=epsg:20353")
  ras <- rasterize(x = spdf, y = r, field = spdf@data[,4:ncol(spdf)],
                   fun = fun, na.rm = T)
  return(ras)
}

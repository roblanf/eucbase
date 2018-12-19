### Primary functions (called directly in user-functions) ###
rMSpp <- function(data, km, e) {
  # Check for species with layers that are all NAs
  checkNA(data)
  # Convert to spatial dataframe
  spdf <- spdf(data)
  # Calculate raster means
  rasMean <- ras(spdf, mean, km, e)
  # Summarise values
  if(all(is.na(rasMean@data@values))) {
    print(paste("Error in calculating mean values for ", unique(data$Species), ", please specify a larger grid size!", sep = ""))
    out2 <- data.frame((matrix(NA, ncol = length(rasMean@data@names))))
  } else {
    out2 <- summarise_all(as.data.frame(rasMean@data@values), mean, na.rm = T)
    out2 <- out2[rowSums(is.na(out2)) < ncol(out2),]
  }
  colnames(out2) <- colnames(data[c(4: ncol(data))])
  return(out2)
}

rMGrid <- function(data, km, e) {
  # Check for species with layers that are all NAs
  if(is.null(checkNA(data))){
    # Convert to spatial dataframe
    spdf <- spdf(data)
    # Calculate raster means and sds
    rasMean <- ras(spdf, mean, km, e)
    # Export cell ID and coordinates
    if(all(is.na(rasMean@data@values))) {
      print(paste("Error in calculating mean values for ", unique(data$Species), ", please specify a larger grid size!", sep = ""))
      NAs  <- data.frame((matrix(NA, ncol = length(rasMean@data@names))))
      outM <- data.frame(1:nrow(xyFromCell(rasMean, 1:nrow(rasMean@data@values ))), xyFromCell(rasMean, 1:nrow(rasMean@data@values )), NAs)
    } else {
      outM  <- data.frame(1:nrow(rasMean@data@values ), xyFromCell(rasMean, 1:nrow(rasMean@data@values )), rasMean@data@values)
    }
    # Remove all cells absent of mean values
    if(all(is.na(outM[,4:ncol(outM)]))) {
    } else {
      outM <- outM[rowSums(is.na(outM)[,4:ncol(data)]) < (max(4:ncol(data))-3),]
    }
    
  } else {
    outM  <- data.frame((matrix(NA, ncol = ncol(data))))
  }
  # Rename columns
  colnames(outM )[1:3] <- c("cellID", "x", "y")
  colnames(outM )[4:ncol(data)] <- paste("mean", colnames(data)[4:ncol(data)], sep = "_")
  
  return(outM)
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
    print(paste(unique(data$Species), " does not have layer values. NAs will be output.", sep = ""))
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

rMGrid <- function(data, km, e) {
  checkNA(data)
  spdf <- spdf(data)
  if (nrow(spdf@coords) == 1) {
    #n1 function
    print("n1")
    
  } else {
    # Calculate raster means and sds
    rasMean <- ras(spdf, mean, km, e)
    rasSD   <- ras(spdf, sd,   km, e)
    
    # Set vars
    rM   <- rasMean@data@values
    rSD  <- rasSD@data@values
    lay  <- 4:ncol(data)
    
    # Export cell ID and coordinates
    outM  <- data.frame(1:nrow(rM ), xyFromCell(rasMean, 1:nrow(rM )),  rM)
    outSD <- data.frame(1:nrow(rSD), xyFromCell(rasSD,   1:nrow(rSD)), rSD)
    
    # Rename columns
    colnames(outM )[1]   <- colnames(outSD)[1] <- "cellID"
    colnames(outM )[lay] <- paste("mean", colnames(data)[lay], sep = "_")
    colnames(outSD)[lay] <- paste("SD"  , colnames(data)[lay], sep = "_")
    
    # Join mean and SD
    out2 <- left_join(outM, outSD[, c(1, lay)], "cellID")
    
    # Remove all cells absent of mean values
    out2 <- out2[rowSums(is.na(out2)[,lay]) < (max(lay)-3),]
    return(out2)
  }

}

spdf <- function(data) {
  spdf <- SpatialPointsDataFrame(coords = data[,3:2], data = data,
                                   proj4string = CRS("+init=epsg:4326"))
  spdf <- spTransform(spdf, CRS("+init=epsg:20353"))
  return(spdf)
}

ras <- function(spdf, fun, km, e) {
r <- raster(ext = e, resolution = km*1000, crs ="+init=epsg:20353")
# Calculate mean
rMean <- rasterize(x = spdf,
                   y = r,
                   field = spdf@data[,4:ncol(spdf)],
                   fun = fun,
                   na.rm = T)
}

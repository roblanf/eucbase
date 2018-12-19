#' Distribution of Species-Level Traits
#'
#' Plots the distribution of Australian species-level traits. Three graphs are
#' generated (trait means, standard deviation and species richness), written as
#' one .png file. Values are calculated as per the specified grid size in km^2.
#'
#' @param points Dataframe containing species point occurrence data. First
#' three columns must be ordered by species names, latitudes and longitudes.
#' @param traits Two column dataframe containing species names and trait means.
#' @param trait_name Character string specifying the trait name and
#' corresponding units (if applicable). This will be incorporated in the graph title.
#' @param km Numeric. Distance of grid size in km^2 to be sampled.
#' 
#' @export
#'
#' @examples
#' # Plot the distribution of a subset of Eucalyptus species' genome sizes
#' data(eucs)
#' data(gs)
#' plotSpeciesTrait(eucs, gs, "genome size (pg/2C)", 100)
#' 
#' @import raster
#' @import rgeos
#' @import rgdal
#' @importFrom dplyr inner_join n_distinct
#' @importFrom grDevices dev.off png
#' @importFrom graphics par
#' @importFrom stats na.omit sd
#' @import viridis
#'

plotSpeciesTrait <- function(points, traits, trait_name, km) {
  ## Assign species traits values to occurrence points ##
  # Call occurrence points and species traits
  # Match species column name
  points[,1] <- sapply(points[,1], as.character)
  traits[,1] <- sapply(traits[,1], as.character)
  colnames(points)[1] <- colnames(traits)[1] <- "Species"
  # Assign traits to each species occurrence
  df <- inner_join(points, traits, by = "Species")
  # How many species retained? (i.e. present in both datasets)
  print(paste("Unmatched species removed. Proceeding with ",
              n_distinct(df$Species), "/", n_distinct(points$Species), " species.",
                  sep = ""))
  
  ## Prepare files for mapping ##
  # Create spatial dataframe from occurrence points and set lat/long projection
  df.spp <- SpatialPointsDataFrame(coords = df[,3:2], data = df,
                                   proj4string = CRS("+init=epsg:4326"))
  # Read map outline, crop and project to lat/longs
  print("Loading map...")
  aus <- getData('GADM', country = "AUS", level = 0)
  aus <- crop(aus, extent(113.2901, 157.131, -44.7779, -9.7203))
  projection(aus) <- CRS("+init=epsg:4326")
  # Convert projection to coordinate system (metres) to specify grid size (km)
  df.spp <- spTransform(df.spp, CRS("+init=epsg:20353"))
  aus    <- spTransform(aus, CRS("+init=epsg:20353"))
  # Create raster and set extents as map boundaries
  print(paste("Generating raster with ", km, "km^2 grids...", sep = ""))
  r <- raster(ext = extent(df.spp@bbox),
              resolution = km*1000, crs ="+init=epsg:20353")
  
  ## Calculate stats per grid cell, set plot titles and mask raster
  # Mean
  print("Calculating grid means... ")
  rMean <- rasterize(x = df.spp, y = r,
                     field = df.spp@data[,ncol(df.spp)],
                     fun = mean)
  rMean  <- mask(rMean, aus)
  rMeanT <- paste("Mean ", trait_name, " per ", km, "km^2 cell", sep ="")
  # SD
  print("Calculating grid standard deviations... ")
  rSD <- rasterize(x = df.spp, y = r,
                   field = df.spp@data[,ncol(df.spp)],
                   fun = sd)
  rSD <- mask(rSD, aus)
  rSDT <- paste("Sdev of ", trait_name, " per ", km, "km^2 cell", sep ="")
  # Species richness
  print("Calculating grid species richness... ")
  rRich <- rasterize(x = df.spp, y = r, 
                     field = df.spp@data$Species, 
                     fun = function(x, ...) {length(unique(na.omit(x)))})
  rRich <- mask(rRich, aus)
  rRichT <-  paste("Species richness per ", km, "km^2 cell", sep ="")
  
  # Function to prepare plots (plotting, add title and map outline)
  plotRas <- function(ras, rasT) {
    plot(ras, main = rasT, col = viridis(50))
    plot(aus, add = T) 
  }
  
  # Write maps in .png
  print("Writing plots to plot_species_trait.png...")
  png("plot_species_trait.png",
      width = 1000,
      height = 800
      )
  par(mfrow = c(2, 2))
  plotRas(rMean, rMeanT)
  plotRas(rSD, rSDT)
  plotRas(rRich, rRichT)
  dev.off()
  print("Done!")

}

# eucbase

A database of life history traits, occurrence data, and environmental traits for eucalypts.

# R Package
The following package takes species point occurrences, with associated environmental layers per point, to output mean values and plot species trait distributions. Aggregating points to a user-specified grid size (km) allows you to output mean values (i.e. environmental variables) for each species, or, each species by grid. By adding species-level life history traits, maps can be generated to visualise the distribution of your trait values.

We recommend using The Atlas of Living Australia's (ALA) [spatial portal](https://spatial.ala.org.au/) to generate your input point occurrence file. More detail below.

Currently, only Australian distributions can be used with the package. 

## Installation

```
# Install dependencies 
install.packages("devtools")
install.packages("dplyr")
install.packages("raster")
install.packages("rgeos")
install.packages("rgdal")
install.packages("viridis")

# Install eucbase
library(devtools)
install_github("roblanf/eucbase")
library(eucbase)
```

## Usage and Examples

Refer to the function documentation for usage and examples.
``` 
# Functions
?speciesMean
?gridMean
?plotSpeciesTrait

# Example datasets
data(eucs)
data(gs)
```

### Input file

Raw point occurrence data, named by species, can either be uploaded to or retrived from the ALA database. [Available layers](https://spatial.ala.org.au/layers) can be attached upon exporting your point data, which will be used as your input file (.csv) for the two functions above.

Due to existing bugs in the beta version, we recommended using the older (non-beta) version of the [ALA spatial portal](https://spatial.ala.org.au/). [Getting Started](https://support.ala.org.au/support/solutions/articles/6000208463-getting-started) outlines the usage of the spatial portal.

Please remove unused columns prior to running the functions and refer to `data(eucs)` for formatting. The neccessary columns to retain include the Scientific Name, Latitude, Longitude, and all layers onwards, in that order.

### Calculating grid-based species means of environmental traits
``` 
speciesMean() # Outputs mean values for all species
gridMean()    # Outputs gridded mean values for all species
```
Depending on the size of your input file, we suggest avoiding using a grid-size < 1km.

### Plotting species-level trait distributions

```
plotSpeciesTrait() # Generates maps for the mean, sd and spp. richness of your trait
```

To observe the distribution and variance of species traits, three gridded maps are generated of the mean, standard deviation and species richness.

This function may run slower on low-end machines caused when masking rasters to the map outlines and take longer as the grid size decreases.

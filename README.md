# eucbase

A database of life history traits, occurrence data, and environmental traits for eucalypts.

# R Package
The following package calculates point occurrence data by summarising points to a user-specified grid size (km). 
Currently, two primary uses are available. 
Firstly, raw species point data downloaded from the Atlas of Living Australia (ALA) act as an input to generate species-level means of environmental factors.
Secondly, point data and existing species life history traits are taken to generate (gridded) spatial distribution plots.

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

See the function documentation for usage.

Example input files (and corresponding outputs) are available in /data which are fully reproducible. 
Functions have been tested using alternate datasets which are not uploaded.

### ALA Point Occurrences and Environmental Layers 

``` 
alaSpeciesMeans()
alaGridMeans()
```

Point occurrence data, named by species, can either be uploaded to or retrived from the ALA database. Climate and substrate layers can be attached upon exporting your point data, which will be used as your raw input file (.csv) for the two functions above.

It's recommended to use the older (non-beta) version of the [ALA spatial portal](https://spatial.ala.org.au/) due to existing bugs in the beta. [Getting Started](https://support.ala.org.au/support/solutions/articles/6000208463-getting-started) outlines the usage of the spatial portal.

### Plotting Species-Level Trait Distributions

```
plotSpeciesTrait()
```

To observe the distribution and variance of species traits, three gridded maps are generated of the mean, standard deviation and species richness.

This function may run slower on low-end machines caused when masking rasters to the map outlines and take longer as the grid size decreases.

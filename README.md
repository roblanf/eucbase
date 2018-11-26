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

This function may run slowly on some computers, mainly when masking the raster to the map outline (there are three of these, one for each map).

For reference, it took 9-10 minutes to run this function on a low-end Windows 7 machine, using 100km^2 grids. It's expected to run slower as the specified grid size gets smaller.

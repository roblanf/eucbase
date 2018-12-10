# Based on the first 20 rows of /data/example_input.csv
Species <- c('Eucalyptus ebbanoensis', 'Eucalyptus dwyeri', 
             'Eucalyptus oleosa', 'Eucalyptus blakelyi', 'Eucalyptus dwyeri',
             'Eucalyptus oleosa', 'Eucalyptus blakelyi', 'Eucalyptus blakelyi',
             'Eucalyptus yalatensis','Corymbia calophylla',
             'Eucalyptus amplifolia', 'Eucalyptus blakelyi', 
             'Eucalyptus talyuberlup', 'Eucalyptus blakelyi', 
             'Corymbia ficifolia', 'Corymbia calophylla', 'Eucalyptus oleosa',
             'Eucalyptus oleosa', 'Eucalyptus oleosa', 'Corymbia calophylla')
Latitude <- c(-30.25356894, -34.18248292, -31.89143, -34.35921769, -33.3380001,
              -34.604235, -34.88506295, -31.3, -33.4789, -31.68255, 
              -33.01864764, -35.9689, -34.6, -34.74844308, -35.045, -31.961695,
              -34.41185, -34.12556, -35.32716, -31.822469)
Longitude <- c(119.266396, 146.2936765,  133.30399, 149.6778322, 147.3558321,
               141.383336, 149.3514572, 149.2833, 136.4318, 116.32628,
               151.1538875, 147.5135, 118.2, 148.8345347, 116.926, 115.839214,
               140.03915, 139.35683, 138.86204, 115.934974)
Precipitation...seasonality..Bio15. <- c(33, 14, 44, 13, 12, 31, 14, 25, 42,
                                         78, 35, 29, 39, 18, 62, 84, 28, 26,
                                         43, 84)
Temperature...annual.range..Bio07. <- c(31.5, 29.5, 22.3, 27.1, 30.7, 28, 27.7,
                                        28.9, 24.2, 26.6, 23.9, 29.2, 18.2,
                                        28.5, 17.2, 22.4, 28.2, 27.3, 20.8, 
                                        24.7)
Potassium <- c(0.020942, 0.059593, NA, 0.066611, 0.039794, NA, 0.030052,
               0.051346, 0.02626, NA, NA, 0.1742936, 0.036376044, 0.077844,
               0.038729, NA, 0.030151, 0.04423786,0.077699, 0.057652)
Phosphorous <- c(0.015637, 0.018021, 0.011322, 0.012155, 0.015012, NA, 
                 0.012967, 0.012452, 0.01277, NA, NA, 0.015572644, 0.013273467,
                 0.012294, 0.015586, NA, 0.01303, 0.013922676, 0.022155, 
                 0.017507)
Nitrogen <- c(0.018333, 0.029059, 0.007988, 0.034101, 0.014945, NA, 0.022085,
              0.040862, 0.013395, NA, NA, 0.035341777, 0.034206536, 0.034138,
              0.039873, NA, 0.018122, 0.0155317, 0.027626, 0.055819)

ex.in <- as.data.frame(cbind(Latitude, Longitude, 
                             Precipitation...seasonality..Bio15.,
                             Temperature...annual.range..Bio07., 
                             Potassium, Phosphorous, Nitrogen))

ex.in <- cbind(Species, ex.in)

rm(Species, Latitude, Longitude, 
   Precipitation...seasonality..Bio15.,
   Temperature...annual.range..Bio07., 
   Potassium, Phosphorous, Nitrogen)

ex.in.spp <- filter(ex.in, Species == 'Eucalyptus blakelyi')

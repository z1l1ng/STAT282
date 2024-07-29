# **Libraries:**
library(tidycensus)
library(sp)
library(sf)
library(tidyverse)
library(ggspatial)
library(spdep)
library(tmap)
library(spatialreg)
library(gstat) 
library(spatstat)
library(raster)
library(kableExtra)

#Load in Crime Data:** (remember at end cite data)
crime <- read_csv("CrimeData-2020.csv") 
#Working with Crime Data:
crime_sf <- crime |>
  drop_na(OpenDataX, OpenDataY) |>
  st_as_sf(coords = c("OpenDataX", "OpenDataY"), remove = FALSE) |>
  st_set_crs(2913)

crime_sf <- crime_sf |>
  drop_na(Neighborhood)

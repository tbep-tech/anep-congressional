library(tidyverse)
library(sf)
library(here)

prj <- 4326

# anep boundaries as sf object --------------------------------------------

anep <- st_read(here::here('data/data-raw/NEP_Boundaries10162018.shp')) %>%
  st_transform(crs = prj)

save(anep, file = here::here('data/anep.RData'), compress = 'xz')
                  

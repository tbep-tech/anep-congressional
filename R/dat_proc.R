library(tidyverse)
library(sf)
library(httr)
library(here)
library(lwgeom)

prj <- 4326

# anep boundaries as sf object --------------------------------------------

anep <- st_read(here::here('data/data-raw/NEP_Boundaries10162018.shp')) %>%
  st_transform(crs = prj) %>% 
  mutate(
    nep_area_sqkm = AREA_SQMI * 2.59, 
    nep = as.character(NEP_NAME)
    ) %>% 
  select(nep, nep_area_sqkm)

save(anep, file = here::here('data/anep.RData'), compress = 'xz')
                  
# congressional boundaries for NEP, not clipped ---------------------------

data(anep)

# download zipped shapefile to temp directory, unzip
tmp_dir <- tempdir()
zipurl<- 'https://www2.census.gov/geo/tiger/TIGER2018/CD/tl_2018_us_cd116.zip'
locpth <- paste0(tmp_dir, '\\fl.zip')
download.file(zipurl, locpth)
unzip(locpth, exdir = tmp_dir)

# sf object, select by anep boundaries
cgrs <- st_read(file.path(tmp_dir, 'tl_2018_us_cd116.shp')) %>% 
  st_transform(crs = prj) %>% 
  select(district = CD116FP, statefp = STATEFP) %>% 
  mutate(district = as.numeric(as.character(district))) %>% # one is 'ZZ'
  .[anep,]

# get area
sqm <- st_area(cgrs) %>% 
  as.numeric 

cgrs <- cgrs %>% 
  mutate(
    cgrs_area_sqkm = sqm / 1e6
  )
  
# add state abbreviation to cgrs using fips
state <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
           "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
           "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", 
           "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", 
           "UT", "VT", "VA", "WA", "WV", "WI", "WY", "AS", "GU", "MP", "PR", 
           "VI")
statefp <- c(1L, 2L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L, 
             19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 
             32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 44L, 45L, 
             46L, 47L, 48L, 49L, 50L, 51L, 53L, 54L, 55L, 56L, 60L, 66L, 69L, 
             72L, 78L)
fips <- data.frame(
  state = state, 
  statefp = statefp, 
  stringsAsFactors = F
)

cgrs <- cgrs %>% 
  mutate(statefp = as.numeric(as.character(statefp))) %>% 
  left_join(fips,by = 'statefp') %>% 
  select(district, state, cgrs_area_sqkm)

save(cgrs, file = here::here('data/cgrs.RData'), compress = 'xz')

# congressional district data by nep boundaries, 116th congress -----------

data(cgrs)
data(anep)

# propublica key, saved to .Renviron file 
# https://projects.propublica.org/api-docs/congress-api/
mykey <- Sys.getenv("propublica_key")

# # senator data
# url <- 'https://api.propublica.org/congress/v1/116/senate/members.json'
# req <- GET(
#   url = url,
#   content_type_json(),
#   add_headers(`X-API-Key` = mykey)
#   )
# sendat <- content(req) %>% 
#   .$results %>% 
#   .[[1]] %>% 
#   .$members %>% 
#   do.call('rbind', .) %>% 
#   data.frame(stringsAsFactors = F)

# house data, change url for house/senate and congress number
url <- 'https://api.propublica.org/congress/v1/116/house/members.json'
req <- GET(
  url = url,
  content_type_json(),
  add_headers(`X-API-Key` = mykey)
)
hsedat <- content(req) %>% 
  .$results %>% 
  .[[1]] %>% 
  .$members %>% 
  map(unlist) %>%
  map(as.list) %>% 
  map(data.frame, stringsAsFactors = F) %>% 
  bind_rows %>% 
  select(first_name, middle_name, last_name, state, district, office, party) %>% 
  unite('first', first_name, middle_name, sep = ' ', na.rm = T) %>% 
  unite('name', last_name, first, sep = ', ') %>% 
  mutate(district = as.numeric(district))

# get intersection of anep boundaries with congressional districts
ints <- st_intersection(st_make_valid(anep), cgrs) %>% 
  mutate(
    ints_sqm = as.numeric(st_area(.)), 
    ints_area_sqkm = ints_sqm / 1e6, 
    per_in_shed = 100 * ints_area_sqkm / cgrs_area_sqkm
  ) %>% 
  select(nep, district, state, per_in_shed) %>% 
  st_set_geometry(NULL)

# join with hsedat
hsedat <- ints %>% 
  left_join(hsedat, by = c('state', 'district'))

save(hsedat, file = here::here('data/hsedat.RData'), compress = 'xz')
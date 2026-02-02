library(tidyverse)
library(sf)
library(tigris)
library(janitor)

# Loading Louisiana county FIPS
parishes <- counties(state = "LA", cb = TRUE, year = 2024)

# Louisiana precinct data: https://redist.legis.la.gov/default_ShapeFiles2020
precincts <- st_read("data/2025 Precinct Shapefiles (07-03-2025)/2025 Precinct Shapefiles (07-03-2025).shp")

# Congressional districts downloaded here: https://www2.census.gov/geo/tiger/TIGER2025/CD/
congressional <- st_read("data/tl_2025_22_cd119/tl_2025_22_cd119.shp")

# Senate district
senate <- state_legislative_districts(state = "LA", house = "upper", cb = FALSE)

# House district
house <- state_legislative_districts(state = "LA", house = "lower", cb = FALSE)

## Public Service Commission disticts downloaded here: https://redist.legis.la.gov/2023_07/2023PSE (unsure if most up-to-date)
public_service_commission <- st_read("data/HB2_PSC_221ES/HB2_PSC_221ES.shp")

## Louisiana Supreme court disticts downloaded here: https://redist.legis.la.gov/2024_Files/2024LASSCAct7
supreme_court <- st_read("data/LASC7 - SB_255_Engrossed_(Fields)/SB_255_Engrossed_(Fields).shp")

# block_equivalency <- read_csv("https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2025_12_BLOCK_DATA.txt")

precinct_block_equivalency <- read_csv("data/https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2025_12_VTD_DATA.txt")

# CRS
planar_crs <- 3452

# Transforming to the planar projection
parishes <- st_transform(parishes, planar_crs) %>% st_make_valid() %>% st_buffer(0)
precincts <- st_transform(precincts, planar_crs) %>% st_make_valid() %>% st_buffer(0) %>%
  left_join(precinct_block_equivalency, by = "UNITNUM")
congressional <- st_transform(congressional, planar_crs) %>% st_make_valid() %>% st_buffer(0)
public_service_commission <- st_transform(public_service_commission, planar_crs) %>% st_make_valid() %>% st_buffer(0)
supreme_court <- st_transform(supreme_court, planar_crs) %>% st_make_valid() %>% st_buffer(0)
senate <- st_transform(senate, planar_crs) %>% st_make_valid() %>% st_buffer(0)
house <- st_transform(house, planar_crs) %>% st_make_valid() %>% st_buffer(0)

precincts_clean <- precincts %>%
  mutate(centroid = st_point_on_surface(geometry)) %>%
  st_join(
    congressional %>% select(congressional = NAMELSAD),
    join = function(x, y) st_within(x$centroid, y)
  ) %>%
  st_join(
    senate %>% select(senate = NAMELSAD),
    join = function(x, y) st_within(x$centroid, y)
  ) %>%
  st_join(
    house %>% select(house = NAMELSAD),
    join = function(x, y) st_within(x$centroid, y)
  ) %>%
  st_join(
    public_service_commission %>% select(public_service_commission = NAME),
    join = function(x, y) st_within(x$centroid, y)
  ) %>%
  st_join(
    supreme_court %>% select(supreme_court = NAME),
    join = function(x, y) st_within(x$centroid, y)
  ) %>%
  select(-centroid) %>%
  clean_names() %>%
  select(
    objectid,
    unit_name,
    countyname,
    `tot_pop_y`:`reg_oth_other_24_12`,
    congressional,
    senate,
    house,
    public_service_commission,
    supreme_court
  )
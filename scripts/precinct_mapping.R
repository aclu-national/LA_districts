# loading libraries
library(tidyverse)
library(sf)
library(tigris)
library(janitor)

# --------------------------- Loading data -------------------------------------
# Louisiana parishes
parishes <- counties(state = "LA", cb = TRUE, year = 2024)

# Louisiana precinct data
# Source: https://redist.legis.la.gov/default_ShapeFiles2020
precincts <- st_read("data/shapemaps/2026 Precinct Shapefiles (01-27-2026)/_2026_Precinct_Shapefiles__01_27_2026_ 2026-02-02.shp")

# Congressional districts
# Source: https://www2.census.gov/geo/tiger/TIGER2025/CD/
congressional <- st_read("data/shapemaps/tl_2025_22_cd119/tl_2025_22_cd119.shp")

# Senate district
senate <- state_legislative_districts(state = "LA", house = "upper", cb = FALSE, year = 2025)

# House district
house <- state_legislative_districts(state = "LA", house = "lower", cb = FALSE, year = 2025)

# Public Service Commission districts
# Source: https://redist.legis.la.gov/2023_07/2023PSE (unsure if most up-to-date)
public_service_commission <- st_read("data/shapemaps/HB2_PSC_221ES/HB2_PSC_221ES.shp")

# Louisiana Supreme Court districts
# Source: https://redist.legis.la.gov/2024_Files/2024LASSCAct7
supreme_court <- st_read("data/shapemaps/LASC7 - SB_255_Engrossed_(Fields)/SB_255_Engrossed_(Fields).shp")

# Precinct block equivalency file
precinct_block_equivalency <- read_csv("data/voting_data/LA_2026_01_VTD_DATA.txt")

# ----------------------------- Cleaning Geometries ----------------------------
# (EPSG:3452)
planar_crs <- 3452

# Transforming and making valid
parishes <- st_transform(parishes, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

precincts <- st_transform(precincts, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0) %>%
  left_join(precinct_block_equivalency, by = "UNITNUM") # Adding voting data

congressional <- st_transform(congressional, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

senate <- st_transform(senate, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

house <- st_transform(house, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

public_service_commission <- st_transform(public_service_commission, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

supreme_court <- st_transform(supreme_court, planar_crs) %>% 
  st_make_valid() %>% 
  st_buffer(0)

# ------------------------- Centroid Assignment --------------------------------
# Method 1: Centroid-based assignment
# This assigns district based on where precinct centroid falls

# Centroid combined data
precincts_clean_centroid <- precincts %>%
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
    `tot_pop_y`:`reg_oth_other_25_12`,
    congressional,
    senate,
    house,
    public_service_commission,
    supreme_court
  )

# ------------------------- Area-weighted Assignment ---------------------------
# Method 2: Area-weighted assignment
# This assigns district based on largest area overlap

# Function to assign based on overlap
assign_district_by_area <- function(precincts_sf, districts_sf, district_col) {
  
  # intersects the geometries
  intersections <- st_intersection(precincts_sf, districts_sf %>% select(all_of(district_col)))
  
  # Calculates the intersection area
  intersections <- intersections %>%
    mutate(intersection_area = st_area(geometry))
  
  # Finds the largest intersection
  result <- intersections %>%
    st_drop_geometry() %>%
    group_by(UNITNUM) %>%
    slice_max(intersection_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(UNITNUM, all_of(district_col))
  
  return(result)
}

# Assigns districts based on overlap
congressional_assignment <- assign_district_by_area(precincts, congressional, "NAMELSAD") %>%
  rename(congressional = NAMELSAD)

senate_assignment <- assign_district_by_area(precincts, senate, "NAMELSAD") %>%
  rename(senate = NAMELSAD)

house_assignment <- assign_district_by_area(precincts, house, "NAMELSAD") %>%
  rename(house = NAMELSAD)

psc_assignment <- assign_district_by_area(precincts, public_service_commission, "NAME") %>%
  rename(public_service_commission = NAME)

sc_assignment <- assign_district_by_area(precincts, supreme_court, "NAME") %>%
  rename(supreme_court = NAME)

# Join all assignments to precincts
precincts_clean_area <- precincts %>%
  left_join(congressional_assignment, by = "UNITNUM") %>%
  left_join(senate_assignment, by = "UNITNUM") %>%
  left_join(house_assignment, by = "UNITNUM") %>%
  left_join(psc_assignment, by = "UNITNUM") %>%
  left_join(sc_assignment, by = "UNITNUM") %>%
  clean_names() %>%
  select(
    objectid,
    unit_name,
    countyname,
    `tot_pop_y`:`reg_oth_other_25_12`,
    congressional,
    senate,
    house,
    public_service_commission,
    supreme_court
  )

# --------------------------- Check for Discrepancy ----------------------------

# Finds precincts where centroid and area methods disagree
discrepancies <- precincts_clean_area %>%
  left_join(
    precincts_clean_centroid %>%
      select(objectid, congressional, senate, house, public_service_commission, supreme_court) %>%
      st_drop_geometry(),
    by = "objectid",
    suffix = c("_area", "_centroid")
  ) %>%
  filter(
    congressional_area != congressional_centroid |
      senate_area != senate_centroid |
      house_area != house_centroid |
      public_service_commission_area != public_service_commission_centroid |
      supreme_court_area != supreme_court_centroid
  )
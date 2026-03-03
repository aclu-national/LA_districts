# --------------------------- Libraries ----------------------------------------

# Loading libraries
library(tidyverse)
library(sf)
library(tigris)
library(janitor)
library(lwgeom)
library(rmapshaper)

# --------------------- Defining Variables and Functions -----------------------

# Defining geometry
planar_crs <- 3452

# Cleaning geometries
clean_geom <- function(x, crs) {
  x %>%
    st_make_valid() %>%
    st_transform(crs) %>%
    
    # Minimal snapping to grid
    st_snap_to_grid(1) %>%
    st_make_valid()
}

# Getting difference in geometries safely
safe_difference <- function(x, y) {
  st_difference(x, y) %>% st_make_valid()
}

# Assigning districts by area
assign_district_by_area <- function(precincts_sf, districts_sf, district_col) {
  
  # Intersecting districts
  intersections <- st_intersection(precincts_sf, districts_sf %>% select(all_of(district_col))) %>%
    
    # Calculating the area of the intersection
    mutate(intersection_area = st_area(geometry))
  
  # Getting the highest area
  intersections %>%
    st_drop_geometry() %>%
    group_by(UNITNUM) %>%
    slice_max(intersection_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(UNITNUM, all_of(district_col))
}

# --------------------------- Loading data -------------------------------------

# Louisiana parishes (2024)
raw_parishes <- counties(state = "LA", cb = TRUE, year = 2024)

# Louisiana precinct data (2026)
# Source: https://redist.legis.la.gov/default_ShapeFiles2020
raw_precincts <- st_read("data/shapemaps/2026 Precinct Shapefiles (01-27-2026)/_2026_Precinct_Shapefiles__01_27_2026_ 2026-02-02.shp")

# Precinct block equivalency file (2026)
raw_precinct_block_equivalency <- read_csv("data/voting_data/LA_2026_01_VTD_DATA.txt")

# Congressional districts
# Source: https://www2.census.gov/geo/tiger/TIGER2025/CD/ (2025)
raw_congressional <- st_read("data/shapemaps/tl_2025_22_cd119/tl_2025_22_cd119.shp")

# Senate district (2025)
raw_senate <- state_legislative_districts(state = "LA", house = "upper", year = 2025)

# House district (2025)
raw_house <- state_legislative_districts(state = "LA", house = "lower", year = 2025)

# Public Service Commission districts (2023)
raw_public_service_commission <- st_read("data/shapemaps/HB2_PSC_221ES/HB2_PSC_221ES.shp")

# Louisiana Supreme Court districts (2024)
# Source: https://redist.legis.la.gov/2024_Files/2024LASSCAct7
raw_supreme_court <- st_read("data/shapemaps/LASC7 - SB_255_Engrossed_(Fields)/SB_255_Engrossed_(Fields).shp")

# Water (2025)
# Looping over parishes to extract all water geometries
raw_water <- lapply(raw_parishes$COUNTYFP, function(fips) area_water(state = "LA", county = fips, year = 2025))

# -------------------------- Clean Geometry ------------------------------------

# Simple
parishes <- clean_geom(raw_parishes, planar_crs)
house <- clean_geom(raw_house, planar_crs)
public_service_commission <- clean_geom(raw_public_service_commission, planar_crs)
supreme_court <- clean_geom(raw_supreme_court, planar_crs)
congressional <- clean_geom(raw_congressional, planar_crs)

# Complicated

## Precincts
precincts <- clean_geom(raw_precincts, planar_crs) %>%
  
  # Removing empty precincts
  filter(TOT_POP != 0) %>%
  
  select(-setdiff(intersect(names(raw_precincts), names(raw_precinct_block_equivalency)), "UNITNUM")) %>%
  
  # Joining to demographic data
  left_join(raw_precinct_block_equivalency, by = "UNITNUM")

## Senate
senate <- clean_geom(raw_senate, planar_crs) %>%
  
  # Removing all undefined senate districts
  filter(NAMELSAD != "State Senate Districts not defined")

## Water
water <- do.call(rbind, raw_water) %>%
  clean_geom(crs = planar_crs) %>%
  st_union() %>%
  st_make_valid() %>%
  st_buffer(0)

# ----------------------------- Saving the Water -------------------------------

save(
  water,
  file = "../shiny/clean_data/water.RData",
  compress = "xz"
)

# --------------------------- Remove water for area assignment -----------------

# Removing water
precincts_no_water <- safe_difference(precincts, water)
congressional_no_water <- safe_difference(congressional, water)
senate_no_water <- safe_difference(senate, water)
house_no_water <- safe_difference(house, water)
psc_no_water <- safe_difference(public_service_commission, water)
sc_no_water <- safe_difference(supreme_court, water)

# --------------------------- Area-weighted assignment -------------------------

# Getting assignments, with water removed
congressional_assignment_nw <- assign_district_by_area(precincts_no_water, congressional_no_water, "NAMELSAD") %>%
  rename(congressional = NAMELSAD)
senate_assignment_nw <- assign_district_by_area(precincts_no_water, senate_no_water, "NAMELSAD") %>%
  rename(senate = NAMELSAD)
house_assignment_nw <- assign_district_by_area(precincts_no_water, house_no_water, "NAMELSAD") %>%
  rename(house = NAMELSAD)
psc_assignment_nw <- assign_district_by_area(precincts_no_water, psc_no_water, "NAME") %>%
  rename(public_service_commission = NAME)
sc_assignment_nw <- assign_district_by_area(precincts_no_water, sc_no_water, "NAME") %>%
  rename(supreme_court = NAME)

# Getting assignments, without water removed
congressional_assignment_w <- assign_district_by_area(precincts, congressional, "NAMELSAD") %>%
  rename(congressional = NAMELSAD)
senate_assignment_w <- assign_district_by_area(precincts, senate, "NAMELSAD") %>%
  rename(senate = NAMELSAD)
house_assignment_w <- assign_district_by_area(precincts, house, "NAMELSAD") %>%
  rename(house = NAMELSAD)
psc_assignment_w <- assign_district_by_area(precincts, public_service_commission, "NAME") %>%
  rename(public_service_commission = NAME)
sc_assignment_w <- assign_district_by_area(precincts, supreme_court, "NAME") %>%
  rename(supreme_court = NAME)

# --------------------------- Comparing assignments ----------------------------

# Checking which precincts differ between the two approaches
congressional_diff <- congressional_assignment_nw %>%
  left_join(congressional_assignment_w, by = "UNITNUM", suffix = c("_no_water", "_with_water")) %>%
  filter(congressional_no_water != congressional_with_water)

senate_diff <- senate_assignment_nw %>%
  left_join(senate_assignment_w, by = "UNITNUM", suffix = c("_no_water", "_with_water")) %>%
  filter(senate_no_water != senate_with_water)

house_diff <- house_assignment_nw %>%
  left_join(house_assignment_w, by = "UNITNUM", suffix = c("_no_water", "_with_water")) %>%
  filter(house_no_water != house_with_water)

psc_diff <- psc_assignment_nw %>%
  left_join(psc_assignment_w, by = "UNITNUM", suffix = c("_no_water", "_with_water")) %>%
  filter(public_service_commission_no_water != public_service_commission_with_water)

sc_diff <- sc_assignment_nw %>%
  left_join(sc_assignment_w, by = "UNITNUM", suffix = c("_no_water", "_with_water")) %>%
  filter(supreme_court_no_water != supreme_court_with_water)

# Printing summary of differences
message("Congressional differences: ", nrow(congressional_diff))
message("Senate differences: ", nrow(senate_diff))
message("House differences: ", nrow(house_diff))
message("PSC differences: ", nrow(psc_diff))
message("Supreme Court differences: ", nrow(sc_diff))

# --------------------------- Join back to original precincts ------------------

# Joining precincts with other assignments (using no-water assignments)
precincts_clean_area <- precincts %>%
  left_join(congressional_assignment_nw, by = "UNITNUM") %>%
  left_join(senate_assignment_nw, by = "UNITNUM") %>%
  left_join(house_assignment_nw, by = "UNITNUM") %>%
  left_join(psc_assignment_nw, by = "UNITNUM") %>%
  left_join(sc_assignment_nw, by = "UNITNUM") %>%
  clean_names() %>%
  
  # Selecting important variables
  select(
    objectid, unit_name, countyname, `tot_pop`:`reg_oth_other_25_12`,
    congressional, senate, house, public_service_commission, supreme_court
  ) %>%
  
  # Creating better names
  mutate(
    public_service_commission = paste0("Public Service Commission ", public_service_commission),
    supreme_court = paste0("Louisiana Supreme Court ", supreme_court)
  ) %>%
  
  # Simplifying geometries
  ms_simplify(keep = 0.20, keep_shapes = TRUE)

# ----------------------------- Saving the Data --------------------------------

# Precincts
save(
  precincts_clean_area,
  file = "../shiny/clean_data/precincts_clean_area.RData",
  compress = "xz"
)
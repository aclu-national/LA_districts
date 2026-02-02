# Louisiana Precinct and District Mapping

This project processes Louisiana precinct shapefiles and assigns them to multiple political districts, combining geographic data with demographic and voter registration information for analysis and mapping.

## Overview

The workflow produces **two main datasets** for both blocks and precincts:

1. **Centroid-based assignment** – Assigns districts based on where the centroid of each block or precinct falls.
2. **Area-weighted assignment** – Assigns districts based on the largest area of overlap with each district.

The project also identifies **discrepancies** between centroid and area-based assignments, which can be used for quality control or further investigation.

## Data Sources

| Layer | Source | Notes |
|-------|--------|-------|
| **Parishes (Counties)** | `tigris` R package | 2024 boundaries |
| **Precincts (VTDs)** | [Louisiana Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) | 2025 shapefiles |
| **Congressional Districts** | [TIGER/Line 2025](https://www2.census.gov/geo/tiger/TIGER2025/CD/) | 2025 boundaries |
| **State Senate & House** | `tigris::state_legislative_districts` | 2025 boundaries |
| **Public Service Commission** | [Redistricting Portal PSC](https://redist.legis.la.gov/2023_07/2023PSE) | 2023 plan |
| **Louisiana Supreme Court** | [Redistricting Portal LASC](https://redist.legis.la.gov/2024_Files/2024LASSCAct7) | 2024 plan |
| **Block/Precinct Equivalency** | [Redistricting CSV](https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2025_12_VTD_DATA.txt) | Links census blocks to precincts |

## Workflow

1. **Load shapefiles and data**
   - Parish, precinct, block, and district shapefiles.
   - Voter registration and demographic CSVs.
2. **Standardize CRS and geometry**
   - Transform all layers to **planar CRS EPSG:3452**.
   - Validate geometries and apply `st_buffer(0)` to fix topology issues.
3. **Centroid-based district assignment**
   - Calculate centroids with `st_point_on_surface()`.
   - Assign districts using `st_within()` spatial joins.
4. **Area-weighted district assignment**
   - Compute `st_intersection()` between precincts/blocks and district layers.
   - Assign each unit to the district with the largest overlapping area.
5. **Merge demographic and voter registration data**
   - Join block/precinct-level population and registration variables.
6. **Identify discrepancies**
   - Compare centroid vs. area-weighted assignments.
   - Flag mismatches for review.

## Output

- `precinct_data.RData`  
  Contains `precincts_clean_centroid` and `precincts_clean_area`.
- `block_data.RData`  
  Contains `blocks_clean_centroid` and `blocks_clean_area`.
- `discrepancies` data frames  
  For both blocks and precincts, highlighting units with differing centroid and area-based assignments.

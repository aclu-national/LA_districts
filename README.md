# Louisiana District Intersections Tool

An interactive web application for exploring Louisiana's electoral districts and their demographic composition. This tool enables researchers, advocates, and policymakers to visualize and analyze how different political district boundaries overlap and intersect, with detailed demographic and voter registration data.

## Overview

This project processes Louisiana precinct and census block shapefiles to create a comprehensive mapping of political districts across multiple levels of government. Users can interactively select any combination of district types (Congressional, State Senate, State House, Public Service Commission, Supreme Court) to see their intersections and examine the demographic makeup of these overlapping areas.

The tool implements two district assignment methods—**centroid-based** and **area-weighted**—and compares their results to ensure accuracy. After analysis, the area-weighted method was selected as optimal for the application.

## Features

### Interactive Web Application
- **Dynamic district intersection**: Select multiple district types to see how they overlap
- **Dual visualizations**: 
  - Distinct colors for visual clarity
  - Data variable mapping (demographics, registration) with gradient coloring
- **Address and district search**: Find specific locations or zoom to particular districts
- **Detailed breakdowns**: Click any region for comprehensive statistics, including:
  - Precinct-level partisan mapping
  - Party registration charts
  - Demographic composition
  - District vs. statewide comparisons
- **Full data access**: Browse and download complete datasets as CSV

## Data Sources

| Layer | Source | Year |
|-------|--------|------|
| **Parishes (Counties)** | `tigris` R package | 2024 |
| **Precincts** | [LA Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) | 2026 |
| **Census Blocks** | [LA Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) | 2025 |
| **Congressional Districts** | [Census TIGER/Line](https://www2.census.gov/geo/tiger/TIGER2025/CD/) | 2025 |
| **State Senate** | [Census TIGER/Line](https://www2.census.gov/geo/tiger/TIGER2025/SLDU/) | 2025 |
| **State House** | [Census TIGER/Line](https://www2.census.gov/geo/tiger/TIGER2025/SLDL/) | 2025 |
| **Public Service Commission** | [LA Redistricting Portal](https://redist.legis.la.gov/2023_07/2023PSE) | 2023 |
| **Supreme Court Districts** | [LA Redistricting Portal](https://redist.legis.la.gov/2024_Files/2024LASSCAct7) | 2024 |
| **Voter Registration Data** | LA Redistricting Portal | 2026 |

## Methodology

### Geographic Processing
1. **Projection**: All spatial data transformed to Louisiana State Plane South (EPSG:3452) for accurate area calculations
2. **Validation**: Geometries validated using `st_make_valid()` to ensure clean spatial operations
3. **Assignment Method Selection**: 
   - Centroid-based: Assigns units based on geometric center location
   - Area-weighted: Assigns based on the largest geographic overlap
   - **Finding**: Both methods produced identical results except for one precinct
   - **Decision**: Area-weighted selected as it better handles boundary-spanning units

### Data Aggregation
When multiple district types are selected, the application:
1. Identifies all unique intersections between selected districts
2. Groups precincts by district combinations
3. Aggregates demographic and registration data to intersection areas
4. Recalculates percentages based on aggregated totals
5. Creates unified geometries from spatial unions

### Performance Optimization
- Geometries simplified to 10% of the original detail using `rmapshaper` for web display
- Data compressed using `xz` compression for faster loading
- Both full and simplified datasets maintained for analysis vs. display

## Project Structure
```
├── data_processing/
│   ├── scripts/
│   │   ├── precinct_mapping.R    # Precinct-level processing
│   │   ├── block_mapping.R       # Block-level processing
│   │   └── save_data.R           # Data export and compression
│   ├── data/
│   │   ├── shapemaps/            # District shapefiles
│   │   └── voting_data/          # Voter registration CSVs
│   └── master_script.R           # Main processing pipeline
├── shiny/
│   ├── app.R                     # Shiny application
│   ├── clean_data/               # Processed datasets
│   └── www/                      # Fonts, CSS, images
└── README.md
```

## Output Datasets

The processing pipeline generates eight datasets:

**Precinct-level** (selected for application):
- `precinct_centroid_data.RData` / `precinct_centroid_data_simple.RData`
- `precinct_area_data.RData` / `precinct_area_data_simple.RData`

**Block-level** (available for alternative analysis):
- `block_centroid_data.RData` / `block_centroid_data_simple.RData`
- `block_area_data.RData` / `block_area_data_simple.RData`

Each dataset includes:
- District assignments (Congressional, Senate, House, PSC, Supreme Court)
- Total population and racial/ethnic breakdowns
- Voting age population (VAP) statistics
- Voter registration by party and demographics
- Spatial geometries (full resolution and simplified)

## Contact

Developed by the ACLU of Louisiana. For questions or feedback, contact: eappelson@laaclu.org

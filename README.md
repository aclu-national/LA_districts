# Louisiana Precinct and District Mapping

This project takes Louisiana precinct and block shapefiles and links them to multiple political districts. It combines geographic boundaries with demographic and voter registration data, creating a clean dataset that can be used for mapping, analysis, and reporting. The goal is to produce both **centroid-based** and **area-weighted** district assignments, and to identify where the two methods disagree.

## Overview

For each block and precinct, two different assignment methods are used. The **centroid-based assignment** determines the district containing the geographic center of the unit, while the **area-weighted assignment** assigns the unit to the district covering the largest portion of its area. Comparing these methods allows us to flag potential discrepancies, which are useful for quality control or edge-case analysis.

## Data Sources

The project uses a combination of shapefiles and CSVs. Parish (county) boundaries come from the `tigris` R package and reflect the 2024 boundaries. Precinct shapefiles (VTDs) are from the Louisiana Redistricting Portal (2026), while census blocks are from the same portal (2025). Congressional districts are from TIGER/Line 2025, and state legislative districts come from either `tigris::state_legislative_districts` or local shapefiles, covering both the upper (Senate) and lower (House) chambers. Public Service Commission districts are based on the 2023 plan from the Louisiana Redistricting Portal, and Supreme Court districts are from the 2024 plan. Finally, CSV files provide block-to-precinct equivalency and voting data.  

| Layer | Source | Notes |
|-------|--------|-------|
| **Parishes (Counties)** | `tigris` R package | 2024 boundaries |
| **Precincts (VTDs)** | [Louisiana Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) | 2026 shapefiles |
| **Census Blocks** | [Louisiana Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) | 2025 shapefiles |
| **Congressional Districts** | [TIGER/Line 2025](https://www2.census.gov/geo/tiger/TIGER2025/CD/) | 2025 boundaries |
| **State Senate & House** | `tigris::state_legislative_districts` or shapefiles | 2025 boundaries |
| **Public Service Commission** | [Redistricting Portal PSC](https://redist.legis.la.gov/2023_07/2023PSE) | 2023 plan |
| **Louisiana Supreme Court** | [Redistricting Portal LASC](https://redist.legis.la.gov/2024_Files/2024LASSCAct7) | 2024 plan |
| **Block/Precinct Equivalency** | [Redistricting CSV](https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2025_12_VTD_DATA.txt) | Links blocks or precincts to voting data |

## Workflow

The workflow begins by loading all shapefiles and CSVs, including parishes, precincts, blocks, and districts for congressional, state legislative, PSC, and Supreme Court boundaries. All layers are transformed to a planar coordinate reference system (EPSG:3452) and validated to fix any geometry issues.

Next, districts are assigned to each unit in two ways. The centroid-based approach calculates a centroid for each block or precinct and assigns the unit to the district containing that point. The area-weighted approach computes the geometric intersection with all overlapping districts and assigns the unit to the district covering the largest area. After assigning districts, demographic and voter registration data are merged with each unit to create a comprehensive dataset.

Finally, discrepancies between centroid-based and area-weighted assignments are identified. These discrepancies highlight units where the two methods disagree, which can indicate boundary edge cases or unusual shapes.

## Output

The final output includes four main datasets:

- `precinct_data.RData`, containing both `precincts_clean_centroid` and `precincts_clean_area`.
- `block_data.RData`, containing both `blocks_clean_centroid` and `blocks_clean_area`.
- Discrepancy tables for blocks and precincts, showing units where centroid and area-based assignments differ.  

These datasets provide a ready-to-use foundation for mapping, analysis, or further redistricting research.

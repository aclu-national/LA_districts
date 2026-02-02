# Louisiana Precinct and District Mapping

This project loads and processes Louisiana precinct shapefiles, adding congressional, state legislative, public service commission, and Supreme Court district data. The final dataset combines precinct-level geography with demographic and voter registration information for analysis and mapping. 

## Data Sources

- **Parishes (Counties):** `tigris` R package
- **Precincts:** [Louisiana Redistricting Portal](https://redist.legis.la.gov/default_ShapeFiles2020) (2025 shapefiles)  
- **Congressional Districts:** [TIGER/Line 2025](https://www2.census.gov/geo/tiger/TIGER2025/CD/)  
- **State Senate & House:** `tigris::state_legislative_districts`  
- **Public Service Commission:** [Redistricting Data](https://redist.legis.la.gov/2023_07/2023PSE)  
- **Louisiana Supreme Court:** [Redistricting Data](https://redist.legis.la.gov/2024_Files/2024LASSCAct7)  
- **Precinct Block Equivalency:** [Redistricting Data CSV](https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2025_12_VTD_DATA.txt)

## Workflow

1. Load parish, precinct, and district shapefiles.  
2. Transform all layers to a planar CRS (EPSG:3452) and validate geometries.  
3. Join precincts to higher-level districts using precinct centroids.  
4. Clean column names and select relevant demographic and district variables. 

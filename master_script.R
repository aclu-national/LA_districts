# Running the scripts
source("scripts/precinct_mapping.R")
source("scripts/block_mapping.R")

# Saving the precinct data
save(
  precincts_clean_centroid, 
  precincts_clean_area,
  file = "precinct_data.RData"
)

# Saving block data
save(
  blocks_clean_centroid,
  blocks_clean_area,
  file = "block_data.RData"
)
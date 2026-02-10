# Run preprocessing scripts
source("scripts/precinct_mapping.R")
source("scripts/block_mapping.R")

# Simplified geometries
precincts_centroid_simple <- precincts_clean_centroid %>%
  ms_simplify(keep = 0.10, keep_shapes = TRUE)

precincts_area_simple <- precincts_clean_area %>%
  ms_simplify(keep = 0.10, keep_shapes = TRUE)

blocks_centroid_simple <- blocks_clean_centroid %>%
  ms_simplify(keep = 0.10, keep_shapes = TRUE)

blocks_area_simple <- blocks_clean_area %>%
  ms_simplify(keep = 0.10, keep_shapes = TRUE)

# Precincts
save(
  precincts_clean_centroid,
  file = "../shiny/clean_data/precinct_centroid_data.RData",
  compress = "xz"
)

save(
  precincts_centroid_simple,
  file = "../shiny/clean_data/precinct_centroid_data_simple.RData",
  compress = "xz"
)

save(
  precincts_clean_area,
  file = "../shiny/clean_data/precinct_area_data.RData",
  compress = "xz"
)

save(
  precincts_area_simple,
  file = "../shiny/clean_data/precinct_area_data_simple.RData",
  compress = "xz"
)

# Blocks
save(
  blocks_clean_centroid,
  file = "../shiny/clean_data/block_centroid_data.RData",
  compress = "xz"
)

save(
  blocks_centroid_simple,
  file = "../shiny/clean_data/block_centroid_data_simple.RData",
  compress = "xz"
)

save(
  blocks_clean_area,
  file = "../shiny/clean_data/block_area_data.RData",
  compress = "xz"
)

save(
  blocks_area_simple,
  file = "../shiny/clean_data/block_area_data_simple.RData",
  compress = "xz"
)

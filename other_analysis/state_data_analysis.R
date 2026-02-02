library(readxl)
library(tidyverse)
library(janitor)
library(readr)

load_sheet <- function(file, sheet, skip = 0, col_map = NULL, filter_total = TRUE) {
  data <- read_excel(file, sheet = sheet, skip = skip) %>%
    clean_names()
  
  if (!is.null(col_map)) {
    data <- data %>% rename(!!!col_map)
    
    # convert numeric columns
    numeric_cols <- names(col_map)
    data <- data %>% mutate(across(all_of(numeric_cols), ~ parse_number(as.character(.x))))
  }
  
  if (filter_total) {
    data <- data %>% filter(!str_detect(.[[1]], "Total|Overall Total"))
  }
  
  return(data)
}


parish_names <- read_excel(file_path, sheet = 1, skip = 9) %>%
  clean_names() %>%
  select(state) %>%
  separate_wider_delim(
    state,
    delim = " - ",
    names = c("parish_name", "parish")
  ) %>%
  mutate(
    parish_name = str_to_lower(paste0(parish_name))
  )

pcs_map <- c(
  total = "total_2",
  registered_white = "white_3",
  registered_black = "black_4",
  registered_other = "other_5",
  dem_total        = "total_6",
  dem_white        = "white_7",
  dem_black        = "black_8",
  dem_other        = "other_9",
  rep_total        = "total_10",
  rep_white        = "white_11",
  rep_black        = "black_12",
  rep_other        = "other_13",
  other_total      = "total_14",
  other_white      = "white_15",
  other_black      = "black_16",
  other_other      = "other_17"
)

parish_map <- c(
  total = "total_3",
  registered_white = "white_4",
  registered_black = "black_5",
  registered_other = "other_6",
  dem_total        = "total_7",
  dem_white        = "white_8",
  dem_black        = "black_9",
  dem_other        = "other_10",
  rep_total        = "total_11",
  rep_white        = "white_12",
  rep_black        = "black_13",
  rep_other        = "other_14",
  other_total      = "total_15",
  other_white      = "white_16",
  other_black      = "black_17",
  other_other      = "other_18"
)

file_path <- "2026_0101_sta_comb.xls"

# Main sheets
datasets <- list(
  congress  = list(race_party_sheet = 3, other_demo_sheet = 5, join_key = c("cong","total")),
  senate    = list(race_party_sheet = 7, other_demo_sheet = 9, join_key = c("sen","total")),
  lacs      = list(race_party_sheet = 15, other_demo_sheet = 17, join_key = c("sct","total")),
  pcs       = list(race_party_sheet = 27, other_demo_sheet = 29, join_key = c("psc","total"))
)

# Parish-level sheets
parish_datasets <- list(
  congress_parish = list(race_party_sheet = 4, other_demo_sheet = 6, join_key = c("cong","parish","total")),
  senate_parish = list(race_party_sheet = 8, other_demo_sheet = 10, join_key = c("sen","parish","total")),
  lacs_parish = list(race_party_sheet = 16, other_demo_sheet = 18, join_key = c("sct","parish","total")),
  pcs_parish = list(race_party_sheet = 28, other_demo_sheet = 30, join_key = c("psc","parish","total"))
)

full_data <- map(datasets, ~ {
  rp <- load_sheet(file_path, sheet = .x$race_party_sheet, skip = 10, col_map = pcs_map)
  od <- load_sheet(file_path, sheet = .x$other_demo_sheet, skip = 8)
  rp %>% left_join(od, by = .x$join_key)
})

full_data_parish <- map(parish_datasets, ~ {
  rp <- load_sheet(file_path, sheet = .x$race_party_sheet, skip = 9, col_map = parish_map)
  od <- load_sheet(file_path, sheet = .x$other_demo_sheet, skip = 9)
  rp %>% left_join(od, by = .x$join_key) %>%
    left_join(parish_names, by = "parish") %>%
    mutate(parish = parish_name) %>%
    select(-parish_name)
})
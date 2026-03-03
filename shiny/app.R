# Loading libraries
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(scales)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(shinyjs)
library(tidyverse)
library(arrow)
library(showtext)
library(rmarkdown)
library(highcharter)
library(rmapshaper)
library(smoothr)
library(lwgeom)
library(waiter)
library(metathis)

# --------------------------- FONT SETUP -----------------------------

font_add(
  family = "gt-america",
  regular = "www/GT-America-Compressed-Bold.ttf"
)

font_add(
  family = "century",
  regular = "www/Century-Schoolbook.ttf"
)

showtext_auto()
showtext_opts(dpi = 96)

# --------------------------- DATA LOADING ---------------------------

load("precincts_clean_area.RData")
precincts_data <- precincts_clean_area %>%
  st_snap_to_grid(size = 0.00001) %>%
  st_make_valid()

precincts_data <- precincts_data %>%
  mutate(
    senate = factor(
      senate,
      levels = c(
        paste("State Senate District", unique(sort(as.numeric(str_extract(senate, "\\d+"))))),
        "State Senate Districts not defined"
      ),
      ordered = TRUE
    ),
    house = factor(
      house,
      levels = c(
        paste("State House District", unique(sort(as.numeric(str_extract(house, "\\d+"))))),
        "State House Districts not defined"
      ),
      ordered = TRUE
    )
  )

# --------------------------- LABEL MAPPINGS -------------------------

# Variable label mappings
VAR_LABELS <- c(
  "tot_pop" = "Total Population",
  "pct_white" = "% White",
  "pct_black" = "% Black",
  "pct_asian" = "% Asian",
  "pct_amind" = "% Native American",
  "pct_other" = "% Other Race",
  "pct_hispanic" = "% Hispanic",
  "tot_white" = "White Population",
  "tot_black" = "Black Population",
  "tot_asian" = "Asian Population",
  "tot_amind" = "Native American Population",
  "tot_other" = "Other Race Population",
  "tot_hispanic" = "Hispanic Population",
  "vap_total" = "VAP Total",
  "vap_pct_white" = "VAP % White",
  "vap_pct_black" = "VAP % Black",
  "vap_pct_asian" = "VAP % Asian",
  "vap_pct_amind" = "VAP % Native American",
  "vap_pct_other" = "VAP % Other",
  "vap_pct_hispanic" = "VAP % Hispanic",
  "vap_white" = "VAP White",
  "vap_black" = "VAP Black",
  "vap_asian" = "VAP Asian",
  "vap_amind" = "VAP Native American",
  "vap_other" = "VAP Other",
  "vap_hispanic" = "VAP Hispanic",
  "reg_total_25_12" = "Total Registered",
  "pct_reg_dem" = "% Democrat",
  "pct_reg_rep" = "% Republican",
  "pct_reg_oth" = "% Other Party",
  "reg_white_25_12" = "Registered White",
  "reg_black_25_12" = "Registered Black",
  "reg_other_25_12" = "Registered Other",
  "reg_dem_total_25_12" = "Registered Democrat",
  "reg_dem_white_25_12" = "Registered Democrat White",
  "reg_dem_black_25_12" = "Registered Democrat Black",
  "reg_dem_other_25_12" = "Registered Democrat Other",
  "reg_rep_total_25_12" = "Registered Republican",
  "reg_rep_white_25_12" = "Registered Republican White",
  "reg_rep_black_25_12" = "Registered Republican Black",
  "reg_rep_other_25_12" = "Registered Republican Other",
  "reg_oth_total_25_12" = "Registered Other Party",
  "reg_oth_white_25_12" = "Registered Other Party White",
  "reg_oth_black_25_12" = "Registered Other Party Black",
  "reg_oth_other_25_12" = "Registered Other Party Other"
)

# District name mappings
DISTRICT_NAMES <- c(
  "congressional" = "Congressional",
  "senate" = "State Senate",
  "house" = "State House",
  "public_service_commission" = "Public Service Commission",
  "supreme_court" = "Supreme Court"
)

# --------------------------- COLOR PALETTES -------------------------

PARTY_COLORS <- c(
  "Democrat" = "#0055AA", 
  "Republican" = "#D93A3F", 
  "Other" = "#888888"
)

DEMOGRAPHIC_COLORS <- c(
  "White" = "#0055AA",
  "Black" = "#D93A3F",
  "Asian" = "#FCAA17",
  "Hispanic" = "#552564",
  "Native American" = "#235564"
)

PARTISAN_GRADIENT <- c(
  low = "#D93A3F", 
  mid = "#552564",
  high = "#0055AA"
)

DATA_VARIABLE_PALETTE <- c("#FEE5D8", "#FCC2AA", "#F03F2E", "#CC191E", "#B91419", "#67000D")

# Reversed map colors
map_colors <- c(
  "#fEE5D8", "#FCC2AA", "#F03F2E", "#CC191E", "#B91419", "#67000D"
) %>% rev()

# --------------------------- MAP SETTINGS ---------------------------

# Map boundaries
MAP_INITIAL_VIEW <- list(lng = -92.5, lat = 30.5, zoom = 6.5)
MAP_ZOOM_LIMITS <- list(min = 6, max = 18)

# --------------------------- FUNCTIONS ------------------------------

# Assigns unique colors to adjacent map polygons using graph coloring
assign_map_colors <- function(sf_data, n_colors = 5) {
  if(nrow(sf_data) < 2) return(rep(1, nrow(sf_data)))
  neighbors <- tryCatch(st_touches(sf_data), error = function(e) NULL)
  if(is.null(neighbors)) return(rep(1, nrow(sf_data)))
  n <- nrow(sf_data)
  colors <- rep(0, n)
  for (i in 1:n) {
    neighbor_colors <- unique(colors[neighbors[[i]]])
    neighbor_colors <- neighbor_colors[neighbor_colors > 0]
    available_colors <- setdiff(1:n_colors, neighbor_colors)
    colors[i] <- if(length(available_colors) > 0) available_colors[1] else 1
  }
  return(colors)
}

# Creates hover tooltip labels for map polygons
create_hover_labels <- function(data, selected_districts, color_mode, data_variable) {
  lapply(1:nrow(data), function(i) {
    row <- data[i, ]
    
    # Builds district name lines
    district_parts <- character(0)
    if ("congressional" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("<b>Congressional:</b> ", row$congressional))
    if ("senate" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("<b>Senate:</b> ", row$senate))
    if ("house" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("<b>House:</b> ", row$house))
    if ("public_service_commission" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("<b>PSC:</b> ", row$public_service_commission))
    if ("supreme_court" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("<b>Supreme Court:</b> ", row$supreme_court))
    
    # Append data variable value if in variable color mode
    variable_display <- ""
    if (color_mode == "variable" && !is.null(data_variable)) {
      var_value <- row[[data_variable]]
      var_label <- VAR_LABELS[data_variable]
      
      var_formatted <- if (grepl("pct_", data_variable)) {
        paste0(round(var_value, 1), "%")
      } else {
        scales::comma(var_value)
      }
      
      variable_display <- paste0(
        '<br><br>',
        '<div style="font-size: 13px; margin: 4px 0; display: flex;">',
        '<span style="color: black; font-weight: 500; font-weight: bold; font-family: Century-Schoolbook, serif; margin-right: 6px;">',
        var_label, ':</span>',
        '<span style="color: black !important; font-family: "GT-America-Compressed-Bold", sans-serif !important; font-size: 22px !important;">',
        var_formatted, '</span>',
        '</div>'
      )
    }
    
    HTML(paste0(
      '<div class="popup-title">District</div>',
      paste(district_parts, collapse = "<br>"),
      variable_display
    ))
  })
}

# Builds display-friendly district name strings for the right sidebar
build_district_names <- function(district_data, selected_districts) {
  district_parts <- character(0)
  if ("congressional" %in% selected_districts) district_parts <- c(district_parts, paste0("Congressional: ", district_data$congressional))
  if ("senate" %in% selected_districts) district_parts <- c(district_parts, paste0("Senate: ", district_data$senate))
  if ("house" %in% selected_districts) district_parts <- c(district_parts, paste0("House: ", district_data$house))
  if ("public_service_commission" %in% selected_districts) district_parts <- c(district_parts, paste0("PSC: ", district_data$public_service_commission))
  if ("supreme_court" %in% selected_districts) district_parts <- c(district_parts, paste0("Supreme Court: ", district_data$supreme_court))
  return(district_parts)
}

# --------------------------- COLUMN RENAMES -------------------------

# Centralized rename dictionary used by both the data table and CSV download
column_renames <- c(
  "State Senate District" = "senate",
  "Congressional District" = "congressional",
  "House District" = "house",
  "Public Service Commission District" = "public_service_commission",
  "Supreme Court District" = "supreme_court",
  "Precinct" = "unit_name",
  "Parish" = "countyname",
  "Total Population" = "tot_pop",
  "% White" = "pct_white",
  "% Black" = "pct_black",
  "% Asian" = "pct_asian",
  "% Native American" = "pct_amind",
  "% Other Race" = "pct_other",
  "% Hispanic" = "pct_hispanic",
  "White Population" = "tot_white",
  "Black Population" = "tot_black",
  "Asian Population" = "tot_asian",
  "Native American Population" = "tot_amind",
  "Other Race Population" = "tot_other",
  "Hispanic Population" = "tot_hispanic",
  "VAP Total" = "vap_total",
  "VAP % White" = "vap_pct_white",
  "VAP % Black" = "vap_pct_black",
  "VAP % Asian" = "vap_pct_asian",
  "VAP % Native American" = "vap_pct_amind",
  "VAP % Other" = "vap_pct_other",
  "VAP % Hispanic" = "vap_pct_hispanic",
  "VAP White" = "vap_white",
  "VAP Black" = "vap_black",
  "VAP Asian" = "vap_asian",
  "VAP Native American" = "vap_amind",
  "VAP Other" = "vap_other",
  "VAP Hispanic" = "vap_hispanic",
  "Total Registered" = "reg_total_25_12",
  "% Democrat" = "pct_reg_dem",
  "% Republican" = "pct_reg_rep",
  "% Other Party" = "pct_reg_oth",
  "Registered White" = "reg_white_25_12",
  "Registered Black" = "reg_black_25_12",
  "Registered Other" = "reg_other_25_12",
  "Registered Democrat" = "reg_dem_total_25_12",
  "Registered Democrat White" = "reg_dem_white_25_12",
  "Registered Democrat Black" = "reg_dem_black_25_12",
  "Registered Democrat Other" = "reg_dem_other_25_12",
  "Registered Republican" = "reg_rep_total_25_12",
  "Registered Republican White" = "reg_rep_white_25_12",
  "Registered Republican Black" = "reg_rep_black_25_12",
  "Registered Republican Other" = "reg_rep_other_25_12",
  "Registered Other Party" = "reg_oth_total_25_12",
  "Registered Other Party White" = "reg_oth_white_25_12",
  "Registered Other Party Black" = "reg_oth_black_25_12",
  "Registered Other Party Other" = "reg_oth_other_25_12"
)

# ----------------------------- SHINY UI -----------------------------

ui <- fluidPage(

  useWaiter(),
  useShinyjs(),
  
  # External CSS/JS libraries and custom stylesheet
  tags$head(
    tags$title("Louisiana District Intersections | ACLU of Louisiana"),
    
    # Favicon (the little icon in the tab)
    tags$link(rel = "shortcut icon", href = "https://aclujusticelab.org/wp-content/themes/aclu-la-justice-lab/assets/images/favicon-16x16.png"),
    tags$link(rel = "icon", type = "image/x-icon", href = "https://aclujusticelab.org/wp-content/themes/aclu-la-justice-lab/assets/images/favicon-16x16.png"),
    
    # Open Graph / social sharing meta tags
    tags$meta(property = "og:title", content = "Louisiana District Intersections"),
    tags$meta(property = "og:description", content = "Intersect districts, get breakdowns, download answers"),
    tags$meta(property = "og:image", content = "https://www.aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png"),
    tags$meta(property = "og:url", content = "https://laaclu.shinyapps.io/districts/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:author", content = "Elijah Appelson"),
    
    # Twitter card meta tags
    tags$meta(name = "twitter:card", content = "summary_large_image"),
    tags$meta(name = "twitter:title", content = "Louisiana District Intersections | ACLU of Louisiana"),
    tags$meta(name = "twitter:description", content = "Intersect districts, get breakdowns, download answers"),
    tags$meta(name = "twitter:image", content = "https://www.aclujusticelab.org/wp-content/uploads/2020/12/ACLULA_JusticeLabStyleGuide-02.png"),
    
    
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.css"
    ),
    tags$script(
      src = "https://unpkg.com/leaflet-control-geocoder/dist/Control.Geocoder.js"
    ),
    includeCSS("www/style.css"),
    tags$script(HTML("
    $(document).ready(function() {
        
        // ----- DRAGGABLE RIGHT SIDEBAR -----
        var isDragging = false;
        var currentSidebar = null;
        var startX, startY, startLeft, startTop;
        
        $(document).on('mousedown', '.right-sidebar .sidebar-header', function(e) {
          if ($(e.target).closest('.close-sidebar').length > 0) return;
          
          currentSidebar = $(this).closest('.right-sidebar');
          isDragging = true;
          currentSidebar.addClass('dragging');
          
          startX = e.clientX;
          startY = e.clientY;
          
          var offset = currentSidebar.offset();
          startLeft = offset.left;
          startTop = offset.top;
          
          e.preventDefault();
          e.stopPropagation();
        });
        
        $(document).on('mousemove', function(e) {
          if (!isDragging || !currentSidebar) return;
          
          var deltaX = e.clientX - startX;
          var deltaY = e.clientY - startY;
          
          var newLeft = startLeft + deltaX;
          var newTop = startTop + deltaY;
          
          var maxLeft = $(window).width() - currentSidebar.outerWidth();
          var maxTop = $(window).height() - currentSidebar.outerHeight();
          
          newLeft = Math.max(0, Math.min(newLeft, maxLeft));
          newTop = Math.max(56, Math.min(newTop, maxTop));
          
          currentSidebar.css({
            left: newLeft + 'px',
            top: newTop + 'px',
            right: 'auto'
          });
          
          e.preventDefault();
        });
        
        $(document).on('mouseup', function(e) {
          if (isDragging) {
            isDragging = false;
            if (currentSidebar) {
              currentSidebar.removeClass('dragging');
              currentSidebar = null;
            }
          }
        });
        
        // ----- SIDEBAR TOGGLES -----
        
        $('#toggleLeft').click(function() {
          $('.left-sidebar').toggleClass('collapsed');
          if ($('.left-sidebar').hasClass('collapsed')) {
            $(this).html('Show Controls ▶'); 
          } else {
            $(this).html('Hide Controls ◀'); 
          }
        });
        
        $('#toggleSearch').click(function() {
          $('.search-container').toggleClass('collapsed');
          if ($('.search-container').hasClass('collapsed')) {
            $(this).html('Show Search ◀'); 
          } else {
            $(this).html('Hide Search ▶'); 
          }
        });
        
        // ----- MAP CLICK HANDLERS -----
        
        // Clicking empty map collapses left sidebar, search, and bottom drawer
        // Note: right sidebar only closes via its own 'X' button
        $('#map').on('click', function(e) {
          if ($(e.target).closest('.leaflet-control-container, .leaflet-popup, .left-sidebar, .search-container, .sidebar-toggle, .search-toggle, .right-sidebar').length === 0) {
            if (!$('.left-sidebar').hasClass('collapsed')) {
              $('.left-sidebar').addClass('collapsed');
              $('#toggleLeft').html('Show Controls ▶');
            }
            if (!$('.search-container').hasClass('collapsed')) {
              $('.search-container').addClass('collapsed');
              $('#toggleSearch').html('Show Search ◀');
            }
            if ($('#bottom-drawer').hasClass('open')) {
              $('#bottom-drawer').removeClass('open');
              $('#table-backdrop').removeClass('active');
              $('#toggle-drawer').text('Show Data Table ▲');
              $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
            }
          }
        });
        
        // Clicking the backdrop closes the data table drawer
        $('#table-backdrop').on('click', function() {
          $('#bottom-drawer').removeClass('open');
          $(this).removeClass('active');
          $('#toggle-drawer').text('Show Data Table ▲');
          $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
        });
        
        // Close button on the right sidebar
        $(document).on('click', '.close-sidebar', function() {
          $('.right-sidebar').removeClass('active');
        });
        
        // ----- DATA TABLE DRAWER -----
        
        function toggleDataTable() {
          $('#bottom-drawer').toggleClass('open');
          $('#table-backdrop').toggleClass('active');
          var isOpen = $('#bottom-drawer').hasClass('open');
          
          var handleText = isOpen ? 'Hide Data Table ▼' : 'Show Data Table ▲';
          $('#toggle-drawer').text(handleText);
          
          var btnHtml = isOpen ? '<i class=\"fas fa-table\"></i> Hide Data Table' : '<i class=\"fas fa-table\"></i> Show Data Table';
          $('#btn-show-table').html(btnHtml);
          
          if(isOpen) {
            setTimeout(function() { $(window).trigger('resize'); }, 300);
          }
        }
        
        $('#toggle-drawer').click(toggleDataTable);
        $('#btn-show-table').click(toggleDataTable);
        
        // ----- FIND INTERSECTION BUTTON -----
        
        // Show loading overlay and collapse sidebars when running intersection
        $(document).on('click', '#find_ab', function() {
          $('#loading-overlay .loading-text').text('Finding Intersection');
          $('#loading-overlay .loading-subtext').text('Calculating district overlap...');
          $('#loading-overlay').addClass('active');
          
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          // Note: right sidebar intentionally left open for results
        });
        
        // ----- UPDATE MAP BUTTON -----
        
        $('#update_map').on('click', function() {
          $('#loading-overlay .loading-text').text('Updating Map');
          $('#loading-overlay .loading-subtext').text('Processing district intersections...');
          $('#loading-overlay').addClass('active');
          
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
        
        // ----- ADDRESS SEARCH BUTTON -----
        
        $('#search_address').on('click', function() {
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
        
        // ----- DISTRICT SEARCH BUTTON -----
        
        $('#search_district').on('click', function() {
          if (!$('.left-sidebar').hasClass('collapsed')) {
            $('.left-sidebar').addClass('collapsed');
            $('#toggleLeft').html('Show Controls ▶');
          }
          if (!$('.search-container').hasClass('collapsed')) {
            $('.search-container').addClass('collapsed');
            $('#toggleSearch').html('Show Search ◀');
          }
          $('.right-sidebar').removeClass('active');
        });
        
        // ----- ABOUT BUTTON -----
        
        $('#about_btn').click(function() {
          Shiny.setInputValue('show_about', Math.random());
        });
        
      });
      
      // ----- LOADING OVERLAY -----
      
      // Hide loading overlay (called from server via sendCustomMessage)
      Shiny.addCustomMessageHandler('hide_loading', function(message) {
        $('#loading-overlay').removeClass('active');
      });
    ")
    )),
  
  # ----- HEADER -----
  
  div(class = "main-header",
      div(class = "header-content",
          h1("Louisiana District Intersections"),
          span(class = "aclu-badge", "ACLU of Louisiana • VOTING RIGHTS")
      ),
      tags$button(
        id = "about_btn",
        class = "about-btn",
        HTML('<i class="fas fa-info-circle"></i> About')
      )
  ),
  
  # ----- LOADING OVERLAY -----
  
  div(id = "loading-overlay", class = "loading-overlay",
      div(class = "loading-content",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Updating Map"),
          div(class = "loading-subtext", "Processing district intersections...")
      )
  ),
  
  # ----- MAP -----
  
  div(class = "map-section",
      leafletOutput("map") %>% withSpinner(color = "#0d47a1")
  ),
  
  # ----- FLOATING UI -----
  
  div(class = "app-container",
      
      # Sidebar toggle buttons
      tags$button(id = "toggleLeft", class = "sidebar-toggle left-toggle", "Hide Controls ◀"),
      tags$button(id = "toggleSearch", class = "search-toggle", "Hide Search ▶"),
      
      # Search panel (address + district find)
      div(class = "search-container",
          div(class = "search-header",
              tags$i(class = "fas fa-search"),
              "Search"
          ),
          div(class = "search-content",
              div(class = "search-section",
                  div(class = "search-section-title", "Search Address"),
                  div(class = "search-input-wrapper",
                      textInput("address_input", NULL, 
                                placeholder = "Enter Louisiana address...",
                                width = "100%"),
                      actionButton("search_address", 
                                   HTML('<i class="fas fa-search"></i>'), 
                                   class = "search-icon-btn")
                  )
              ),
              div(class = "search-section",
                  div(class = "search-section-title", "Find District"),
                  selectInput("district_type_search", 
                              NULL,
                              choices = c(
                                "Select district type..." = "",
                                "Congressional" = "congressional",
                                "State Senate" = "senate",
                                "State House" = "house",
                                "Public Service Commission" = "public_service_commission",
                                "Supreme Court" = "supreme_court"
                              ),
                              width = "100%"),
                  uiOutput("district_number_ui"),
                  actionButton("search_district", 
                               HTML('<i class="fas fa-map-marker-alt"></i> Find District'), 
                               class = "search-btn")
              )
          )
      ),
      
      # Left control sidebar
      div(class = "left-sidebar",
          div(class = "sidebar-header",
              div(class = "sidebar-header-content",
                  h3("Controls")
              )
          ),
          div(class = "sidebar-content",
              
              # Mode selector
              div(class = "control-panel", style="margin-bottom: 10px;",
                  radioButtons(
                    "app_mode", div(class = "section-header", "Select Analysis Method"),
                    choices = c(
                      "Statewide Grouping" = "statewide",
                      "Individual Districts" = "ab_compare"
                    ),
                    selected = "statewide"
                  )
              ),
              
              # ---- MODE 1: STATEWIDE GROUPING ----
              conditionalPanel(
                condition = "input.app_mode == 'statewide'",
                
                # District selection
                div(class = "control-panel",
                    div(class = "section-header", "Select Districts to Intersect"),
                    checkboxGroupInput(
                      "districts",
                      NULL,
                      choices = c(
                        "Congressional" = "congressional",
                        "State Senate" = "senate",
                        "State House" = "house",
                        "Public Service Commission" = "public_service_commission",
                        "Supreme Court" = "supreme_court"
                      ),
                      selected = "congressional"
                    ),
                    actionButton(
                      "update_map",
                      HTML('<i class="fas fa-sync-alt"></i> Update Map'),
                      class = "update-btn"
                    ),
                    p(class = "control-hint", 
                      "Click 'Update Map' to apply district selection changes")
                ),
                
                # Color mode
                div(class = "control-panel",
                    div(class = "section-header", "Choose Map Colors"),
                    radioButtons(
                      "color_mode",
                      NULL,
                      choices = c(
                        "Distinct Colors" = "distinct",
                        "Data Variable" = "variable"
                      ),
                      selected = "distinct"
                    ),
                    conditionalPanel(
                      condition = "input.color_mode == 'variable'",
                      selectInput(
                        "data_variable",
                        "Variable:",
                        choices = c(
                          "Total Population" = "tot_pop",
                          "% White" = "pct_white",
                          "% Black" = "pct_black",
                          "% Asian" = "pct_asian",
                          "% Native American" = "pct_amind",
                          "% Other Race" = "pct_other",
                          "% Hispanic" = "pct_hispanic",
                          "White Population" = "tot_white",
                          "Black Population" = "tot_black",
                          "Asian Population" = "tot_asian",
                          "Native American Population" = "tot_amind",
                          "Other Race Population" = "tot_other",
                          "Hispanic Population" = "tot_hispanic",
                          "VAP Total" = "vap_total",
                          "VAP % White" = "vap_pct_white",
                          "VAP % Black" = "vap_pct_black",
                          "VAP % Asian" = "vap_pct_asian",
                          "VAP % Native American" = "vap_pct_amind",
                          "VAP % Other" = "vap_pct_other",
                          "VAP % Hispanic" = "vap_pct_hispanic",
                          "VAP White" = "vap_white",
                          "VAP Black" = "vap_black",
                          "VAP Asian" = "vap_asian",
                          "VAP Native American" = "vap_amind",
                          "VAP Other" = "vap_other",
                          "VAP Hispanic" = "vap_hispanic",
                          "Total Registered" = "reg_total_25_12",
                          "% Democrat" = "pct_reg_dem",
                          "% Republican" = "pct_reg_rep",
                          "% Other Party" = "pct_reg_oth",
                          "Registered White" = "reg_white_25_12",
                          "Registered Black" = "reg_black_25_12",
                          "Registered Other" = "reg_other_25_12",
                          "Registered Democrat" = "reg_dem_total_25_12",
                          "Registered Democrat White" = "reg_dem_white_25_12",
                          "Registered Democrat Black" = "reg_dem_black_25_12",
                          "Registered Democrat Other" = "reg_dem_other_25_12",
                          "Registered Republican" = "reg_rep_total_25_12",
                          "Registered Republican White" = "reg_rep_white_25_12",
                          "Registered Republican Black" = "reg_rep_black_25_12",
                          "Registered Republican Other" = "reg_rep_other_25_12",
                          "Registered Other Party" = "reg_oth_total_25_12",
                          "Registered Other Party White" = "reg_oth_white_25_12",
                          "Registered Other Party Black" = "reg_oth_black_25_12",
                          "Registered Other Party Other" = "reg_oth_other_25_12"
                        ),
                        selected = "tot_pop"
                      ),
                      p(class = "color-scale-note", "Scale: White (Low) ➝ Red (High)")
                    ),
                    p(class = "control-hint", "Colors update instantly")
                ),
                
                # District outline overlay
                div(class = "control-panel",
                    div(class = "section-header", "Add District Outlines"),
                    checkboxInput(
                      "show_outline",
                      "Show District Outline",
                      value = FALSE
                    ),
                    conditionalPanel(
                      condition = "input.show_outline == true",
                      selectInput(
                        "outline_district",
                        "Outline District:",
                        choices = c(
                          "Congressional" = "congressional",
                          "State Senate" = "senate",
                          "State House" = "house",
                          "Public Service Commission" = "public_service_commission",
                          "Supreme Court" = "supreme_court"
                        ),
                        selected = "congressional"
                      )
                    ),
                    p(class = "control-hint", "Outlines update instantly")
                ),
                
                div(class = "info-box",
                    h4("HOW TO USE"),
                    p("1. Select district types and click 'Update Map'"),
                    p("2. Instantly adjust colors and variables"),
                    p("3. Add district outlines for reference"),
                    p("4. Search addresses (top-right) or districts"),
                    p("5. Hover over areas for quick info"),
                    p("6. Click regions for detailed breakdowns"),
                    p("7. View and download full data table")
                )
              ),
              
              # ---- MODE 2: A VS B DISTRICT COMPARISON ----
              conditionalPanel(
                condition = "input.app_mode == 'ab_compare'",
                div(class = "control-panel",
                    p(class = "ab-sec-label", "First District"),
                    selectInput("type_a", "Type",
                                choices = c("Congressional" = "congressional", "State Senate" = "senate",
                                            "State House" = "house",
                                            "Public Service Commission" = "public_service_commission",
                                            "Supreme Court" = "supreme_court"),
                                selected = "congressional", width = "100%"),
                    uiOutput("district_a_ui"),
                    
                    div(class = "ab-district-sep", "⬇"),
                    
                    p(class = "ab-sec-label", "Second District"),
                    selectInput("type_b", "Type",
                                choices = c("Congressional" = "congressional", "State Senate" = "senate",
                                            "State House" = "house",
                                            "Public Service Commission" = "public_service_commission",
                                            "Supreme Court" = "supreme_court"),
                                selected = "senate", width = "100%"),
                    uiOutput("district_b_ui"),
                    
                    p(class = "ab-hint", "The second district filters to precincts overlapping the first district."),
                    
                    tags$button(
                      id = "find_ab", class = "ab-find-btn",
                      onclick = "Shiny.setInputValue('find_ab', Math.random())",
                      HTML('<i class="fas fa-compress-arrows-alt"></i>  Find Intersection')
                    )
                ),
                div(class = "info-box",
                    h4("HOW TO USE"),
                    p("1. Select a first district type and district"),
                    p("2. Select a second district type and district"),
                    p("3. Press 'Find Intersection'"),
                    p("4. Click the map for a district intersection breakdown")
                )
              )
          )
      ),
      
      # Right floating sidebars (rendered dynamically by mode)
      uiOutput("info_panel"),
      uiOutput("ab_floating_panel")
  ),
  
  # ----- DATA TABLE -----
  
  div(class = "table-backdrop", id = "table-backdrop"),
  
  div(id = "bottom-drawer", class = "bottom-drawer",
      div(id = "toggle-drawer", class = "drawer-handle", "Show Data Table ▲"),
      div(class = "drawer-content",
          div(class = "table-controls",
              h3(class = "table-header", "District Data"),
              p(style = "margin: 0 0 8px 0; font-size: 12px; color: #888; font-style: italic;",
                "Estimates only — precinct assignments are area-weighted and may not be 100% accurate. Not official data."),
              downloadButton("download_data", "Download CSV", class = "download-btn")
          ),
          DTOutput("district_table")
      )
  ),
  
  # ----- FOOTER -----
  
  tags$a(
    href = "https://www.laaclu.org/",
    target = "_blank",
    class = "aclu-logo",
    tags$img(
      src = "https://www.laaclu.org/app/themes/aclu-parent/global/bundles/common/images/affiliate-logos/ACLU_LA.svg",
      alt = "ACLU of Louisiana"
    )
  )
)

# --------------------------- SERVER ---------------------------------

server <- function(input, output, session) {
  
  # Tracks the currently clicked district polygon (statewide mode)
  clicked_district <- reactiveVal(NULL)
  
  # --------------------------- MODALS -----------------------------
  
  # Welcome modal on app load
  showModal(modalDialog(
    title = div(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; font-size: 24px; color: #0d47a1;", 
                "WELCOME TO THE LOUISIANA DISTRICT INTERSECTIONS TOOL"),
    div(style = "font-family: 'Century-Schoolbook', serif;",
        h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
           "What This Tool Does"),
        p("This interactive map allows you to explore Louisiana's electoral districts by selecting and intersecting different district types. You can visualize demographic and voter registration data for any combination of overlapping districts."),
        p(strong("Perfect for:"), " Analyzing voting patterns, understanding district demographics, and exploring how different district boundaries interact."),
        
        h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
           "How to Use"),
        tags$ol(
          tags$li(strong("Select Districts:"), " Use the left sidebar to choose which district types to intersect (Congressional, State Senate, State House, Public Service Commission, Supreme Court)"),
          tags$li(strong("Update Map:"), " Click the 'Update Map' button to apply your district selections"),
          tags$li(strong("Explore Data:"), " Hover over areas for quick info, click regions for detailed breakdowns"),
          tags$li(strong("Customize Colors:"), " Choose between distinct colors or color by demographic/registration variables"),
          tags$li(strong("Search:"), " Use the search panel (top-right) to find specific addresses or districts"),
          tags$li(strong("View Data:"), " Open the data table at the bottom to see all statistics and download as a CSV")
        ),
        
        h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
           "Quick Tips"),
        tags$ul(
          tags$li("Start with one district type, then add more to see intersections"),
          tags$li("Color changes (mode and variable) update instantly"),
          tags$li("District outline overlays help visualize boundaries"),
          tags$li("Click the map to close all sidebars and get a clear view")
        ),
        
        h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
           "Questions"),
        p(HTML("For questions or feedback, contact: <strong>eappelson@laaclu.org</strong><br>Click the <strong>About</strong> button in the header for data sources and methodology.")),
        
        div(style = "margin-top: 20px; padding: 12px 16px; background: #fff8e1; border-left: 4px solid #f9a825; border-radius: 4px;",
            p(style = "margin: 0; font-size: 13px; color: #555;",
              HTML("<strong>Disclaimer:</strong> Precinct-to-district assignments are based on area-weighted overlap and may not be 100% accurate in all cases. All data aggregations in this tool are estimates and are <strong>not official figures</strong>. Results should not be used as a substitute for official election or census data.")
            )
        )
    ),
    easyClose = TRUE,
    footer = tagList(
      actionButton("welcome_close", "Get Started", class = "modal-btn")
    )
  ))
  
  observeEvent(input$welcome_close, {
    removeModal()
  })
  
  shinyjs::runjs("$('.modal-dialog').css('margin-top', '100px');")
  
  # About modal (triggered from header button)
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = div(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; font-size: 24px; color: #0d47a1;", 
                  "ABOUT"),
      div(style = "font-family: 'Century-Schoolbook', serif;",
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "What This Tool Does"),
          p("This interactive map allows you to explore Louisiana's electoral districts by selecting and intersecting different district types (Congressional, State Senate, State House, Public Service Commission, and Supreme Court). You can visualize demographic and voting registration data for any combination of overlapping districts."),
          
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "Data Sources"),
          tags$ul(
            tags$li(HTML("<b>Precinct Shapefiles</b> — <a href='https://redist.legis.la.gov/2025%201RS/Shapefiles/2026%20Precinct%20Shapefiles%20(01-27-2026).zip' target='_blank'>Download ZIP</a> (01/27/2026)")),
            tags$li(HTML("<b>Precinct Voting Data</b> — <a href='https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2026_01_VTD_DATA.zip' target='_blank'>Download ZIP</a> (01/27/2026)")),
            tags$li(HTML("<b>Congressional Shapefiles</b> — <a href='https://www2.census.gov/geo/tiger/TIGER2025/CD' target='_blank'>Census TIGER Files</a> (09/22/2025)")),
            tags$li(HTML("<b>State Senate Shapefiles</b> — <a href='https://www2.census.gov/geo/tiger/TIGER2025/SLDL/' target='_blank'>Census TIGER Files</a> (09/22/2025)")),
            tags$li(HTML("<b>State House Shapefiles</b> — <a href='https://www2.census.gov/geo/tiger/TIGER2025/SLDU/' target='_blank'>Census TIGER Files</a> (09/22/2025)")),
            tags$li(HTML("<b>Public Service Commission Shapefiles</b> — <a href='https://redist.legis.la.gov/2023_07/Adopted%20Plans%20From%20the%202022%201st%20Extraordinary%20Session/Public%20Service%20Commssion/Shapefiles%20and%20KML%20Files/HB2_PSC_221ES.zip' target='_blank'>Download ZIP</a> (01/01/2023)")),
            tags$li(HTML("<b>Supreme Court Shapefiles</b> — <a href='https://redist.legis.la.gov/2024_Files/2024_RS/2024LASSCAct7/Shapefile/Act_7_-__RS_(2024)_-_LASC.zip' target='_blank'>Download ZIP</a> (05/01/2024)"))
          ),
          
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "Methodology"),
          p(strong("Data Analysis Level:"), " This analysis was conducted at both the census block level and precinct level. After comparing results from both geographic units, precinct-level data was selected as optimal for this tool."),
          p(strong("Geometric Processing:"), " All spatial data is transformed to Louisiana State Plane South (EPSG:3452) for accurate area calculations. Geometries are validated using st_make_valid() and snapped to a grid to ensure clean intersections and proper spatial operations. Final geometries are simplified using ms_simplify() (retaining 20% of vertices) to optimize rendering performance while preserving shape integrity."),
          p(strong("Water Removal:"), " To improve the accuracy of district assignments, water geometries are extracted from Census TIGER area water files for all Louisiana parishes and subtracted from precinct and district boundaries prior to area calculations. This ensures that large water bodies do not distort overlap measurements for precincts situated along coastlines, rivers, or lakes."),
          p(strong("District Assignment Method Selection:"), " Two assignment methods were tested: centroid-based (assigning precincts based on where their geometric center falls) and area-weighted (assigning based on largest geographic overlap). Both methods were compared across all precincts and produced identical results except for a single precinct. The area-weighted method was selected as the optimal approach because it better handles precincts that span district boundaries."),
          p(strong("Water Removal Comparison:"), " Area-weighted assignments were computed both with and without water removed from geometries, and results were compared across all five district types. The two approaches produced nearly identical assignments, differing for only a small number of precincts. The water-removed assignments were selected as the final method, as they more accurately reflect land-based population distribution."),
          p(strong("Area-Weighted Assignment:"), " For each precinct, spatial intersections with all district types are calculated using water-removed geometries. The intersection area between each precinct and overlapping districts is computed, and the precinct is assigned to the district with which it shares the largest geographic overlap. Another approach is to distribute precinct populations across the districts it intersects; however, we chose not to use this approach."),
          p(strong("Data Aggregation:"), " When multiple district types are selected, the tool identifies all unique intersections between those districts by grouping precincts. Demographic data and voter registration statistics are aggregated from precinct-level data to these intersection areas. Percentages are recalculated based on the aggregated totals."),
          p(strong("Visualization:"), " Final geometries are created by performing spatial unions of precinct boundaries grouped by selected district combinations, then transformed to WGS84 (EPSG:4326) for web mapping. The map offers two coloring modes: 'Distinct Colors' assigns different colors to adjacent regions for visual clarity, while 'Data Variable' uses a white-to-red gradient to show demographic or registration patterns."),
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "Questions"),
          p("Should you have any questions or concerns, please contact: eappelson@laaclu.org."),
          
          div(style = "margin-top: 20px; padding: 12px 16px; background: #fff8e1; border-left: 4px solid #f9a825; border-radius: 4px;",
              p(style = "margin: 0; font-size: 13px; color: #555;",
                HTML("<strong>Disclaimer:</strong> Precinct-to-district assignments are based on area-weighted overlap and a small number of precincts may be incorrectly assigned, particularly those straddling district boundaries. All aggregations in this tool are estimates and are <strong>not official figures</strong>. Results should not be used as a substitute for official election or census data.")
              )
          )
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton("welcome_close", "Close", class = "modal-btn")
      )
    ))
    
    shinyjs::runjs("$('.modal-dialog').css('margin-top', '100px');")
  })
  
  # ----------------------- INTERSECTION CALCULATIONS ------------------
  
  # Aggregates precinct data and creates geometry unions for selected district combination
  intersection_data <- eventReactive(input$update_map, {
    req(length(input$districts) > 0)
    
    aggregated <- precincts_data %>%
      st_drop_geometry() %>%
      group_by(across(all_of(input$districts))) %>%
      summarize(
        # Population totals
        tot_pop = sum(tot_pop, na.rm = TRUE),
        tot_white = sum(tot_white, na.rm = TRUE),
        tot_black = sum(tot_black, na.rm = TRUE),
        tot_asian = sum(tot_asian, na.rm = TRUE),
        tot_amind = sum(tot_amind, na.rm = TRUE),
        tot_other = sum(tot_other, na.rm = TRUE),
        tot_hispanic = sum(tot_hispanic, na.rm = TRUE),
        
        # VAP totals
        vap_total = sum(vap_total, na.rm = TRUE),
        vap_white = sum(vap_white, na.rm = TRUE),
        vap_black = sum(vap_black, na.rm = TRUE),
        vap_asian = sum(vap_asian, na.rm = TRUE),
        vap_amind = sum(vap_amind, na.rm = TRUE),
        vap_other = sum(vap_other, na.rm = TRUE),
        vap_hispanic = sum(vap_hispanic, na.rm = TRUE),
        
        # Registration totals
        reg_total_25_12 = sum(reg_total_25_12, na.rm = TRUE),
        reg_white_25_12 = sum(reg_white_25_12, na.rm = TRUE),
        reg_black_25_12 = sum(reg_black_25_12, na.rm = TRUE),
        reg_other_25_12 = sum(reg_other_25_12, na.rm = TRUE),
        
        # Democrat registration totals
        reg_dem_total_25_12 = sum(reg_dem_total_25_12, na.rm = TRUE),
        reg_dem_white_25_12 = sum(reg_dem_white_25_12, na.rm = TRUE),
        reg_dem_black_25_12 = sum(reg_dem_black_25_12, na.rm = TRUE),
        reg_dem_other_25_12 = sum(reg_dem_other_25_12, na.rm = TRUE),
        
        # Republican registration totals
        reg_rep_total_25_12 = sum(reg_rep_total_25_12, na.rm = TRUE),
        reg_rep_white_25_12 = sum(reg_rep_white_25_12, na.rm = TRUE),
        reg_rep_black_25_12 = sum(reg_rep_black_25_12, na.rm = TRUE),
        reg_rep_other_25_12 = sum(reg_rep_other_25_12, na.rm = TRUE),
        
        # Other party registration totals
        reg_oth_total_25_12 = sum(reg_oth_total_25_12, na.rm = TRUE),
        reg_oth_white_25_12 = sum(reg_oth_white_25_12, na.rm = TRUE),
        reg_oth_black_25_12 = sum(reg_oth_black_25_12, na.rm = TRUE),
        reg_oth_other_25_12 = sum(reg_oth_other_25_12, na.rm = TRUE),
        
        .groups = "drop"
      ) %>%
      mutate(
        # Population percentages
        pct_white = tot_white / tot_pop * 100,
        pct_black = tot_black / tot_pop * 100,
        pct_asian = tot_asian / tot_pop * 100,
        pct_amind = tot_amind / tot_pop * 100,
        pct_other = tot_other / tot_pop * 100,
        pct_hispanic = tot_hispanic / tot_pop * 100,
        
        # VAP percentages
        vap_pct_white = vap_white / vap_total * 100,
        vap_pct_black = vap_black / vap_total * 100,
        vap_pct_asian = vap_asian / vap_total * 100,
        vap_pct_amind = vap_amind / vap_total * 100,
        vap_pct_other = vap_other / vap_total * 100,
        vap_pct_hispanic = vap_hispanic / vap_total * 100,
        
        # Registration percentages
        pct_reg_dem = reg_dem_total_25_12 / reg_total_25_12 * 100,
        pct_reg_rep = reg_rep_total_25_12 / reg_total_25_12 * 100,
        pct_reg_oth = reg_oth_total_25_12 / reg_total_25_12 * 100
      )
    
    # Create geometry unions grouped by selected districts
    result <- precincts_data %>%
      group_by(across(all_of(input$districts))) %>%
      summarize(geometry = st_union(geometry), .groups = "drop") %>%
      st_make_valid()
    
    if(is.na(st_crs(result))) {
      result <- st_set_crs(result, st_crs(precincts_data))
    }
    
    result <- result %>%
      left_join(aggregated, by = input$districts)
    
    # Assign distinct colors and unique polygon IDs
    result$color_group <- assign_map_colors(result)
    result$distinct_color <- map_colors[result$color_group]
    result$poly_id <- 1:nrow(result)
    
    st_transform(result, 4326)
  }, ignoreNULL = FALSE)
  
  # Dissolves precinct boundaries into a single district outline for overlay
  outline_data <- reactive({
    if (!input$show_outline) return(NULL)
    req(input$outline_district)
    
    precincts_data %>%
      group_by(!!sym(input$outline_district)) %>%
      summarize(geometry = st_union(geometry), .groups = "drop") %>%
      st_make_valid() %>%
      st_transform(4326)
  })
  
  # ----------------------- DISTRICT SELECTION -------------------------
  
  # Clear clicked district when map updates or district selections change
  observeEvent(input$update_map, { clicked_district(NULL) })
  observeEvent(input$districts, { clicked_district(NULL) }, ignoreInit = TRUE)
  
  # Handle polygon clicks: load breakdown in statewide mode or reopen sidebar in A&B mode
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    if (input$app_mode == "ab_compare") {
      shinyjs::runjs("$('.right-sidebar').addClass('active');")
    } else {
      data <- intersection_data()
      if (!is.null(click$id) && !is.null(data)) {
        clicked_district(data[data$poly_id == click$id, ])
      }
    }
  })
  
  # Re-open right sidebar when clicking empty map in A&B mode
  observeEvent(input$map_click, {
    if (input$app_mode == "ab_compare") {
      shinyjs::runjs("$('.right-sidebar').addClass('active');")
    }
  })
  
  # ----------------------- SEARCH FUNCTIONALITY -----------------------
  
  # Address search via Nominatim geocoding
  observeEvent(input$search_address, {
    req(input$address_input, nchar(trimws(input$address_input)) > 0)
    
    address_full <- trimws(input$address_input)
    
    tryCatch({
      url <- paste0(
        "https://nominatim.openstreetmap.org/search?",
        "format=json",
        "&q=", URLencode(address_full),
        "&countrycodes=us",
        "&limit=1",
        "&addressdetails=1"
      )
      
      response <- jsonlite::fromJSON(url, simplifyVector = TRUE)
      
      if (length(response) > 0 && nrow(response) > 0) {
        lat <- as.numeric(response$lat[1])
        lon <- as.numeric(response$lon[1])
        display_name <- response$display_name[1]
        
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(
            lng = lon, 
            lat = lat,
            popup = paste0(
              "<div style='font-family: Century-Schoolbook, serif;'>",
              "<b style='font-family: GT-America-Compressed-Bold, sans-serif; color: #0d47a1;'>",
              address_full,
              "</b><br>",
              "<span style='font-size: 12px;'>", display_name, "</span>",
              "</div>"
            )
          ) %>%
          setView(lng = lon, lat = lat, zoom = 15)
        
        showNotification("Address found!", type = "message", duration = 3)
        
      } else {
        showNotification(
          "Address not found. Try including city name or zip code.", 
          type = "warning", 
          duration = 5
        )
      }
      
    }, error = function(e) {
      showNotification(
        "Error searching address. Please try again.", 
        type = "error", 
        duration = 5
      )
    })
  })
  
  # Dynamic dropdown for district number search
  output$district_number_ui <- renderUI({
    req(input$district_type_search, input$district_type_search != "")
    
    vals <- precincts_data[[input$district_type_search]]
    vals <- stringr::str_sort(unique(vals[!is.na(vals)]), numeric = TRUE)
    
    selectInput("district_number_search", 
                NULL,
                choices = c("Select district..." = "", vals),
                width = "100%")
  })
  
  # Zoom to and briefly highlight the searched district
  observeEvent(input$search_district, {
    req(input$district_type_search, input$district_type_search != "")
    req(input$district_number_search, input$district_number_search != "")
    
    district_col <- input$district_type_search
    district_num <- input$district_number_search
    
    district_geom <- precincts_data %>%
      filter(!!sym(district_col) == district_num) %>%
      st_union() %>%
      st_make_valid() %>%
      st_transform(4326)
    
    if (length(district_geom) > 0) {
      bbox <- st_bbox(district_geom)
      
      leafletProxy("map") %>%
        clearGroup("search_highlight") %>%
        clearMarkers() %>%
        addPolygons(
          data = district_geom,
          fillColor = "#0d47a1",
          fillOpacity = 0.4,
          color = "#0d47a1",
          weight = 2,
          opacity = 1,
          group = "search_highlight",
          options = pathOptions(pane = "highlights")
        ) %>%
        fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
          options = list(padding = c(50, 50))
        )
      
      showNotification(
        paste("Found", DISTRICT_NAMES[district_col], "District", district_num), 
        type = "message",
        duration = 3
      )
      
      # Remove highlight after 6 seconds
      shinyjs::delay(6000, {
        leafletProxy("map") %>% clearGroup("search_highlight")
      })
    } else {
      showNotification("District not found.", type = "warning", duration = 5)
    }
  })
  
  # ----------------------- MAP RENDERING ------------------------------
  
  # Color palette for data variable mode (white-to-red gradient)
  color_pal <- reactive({
    req(input$color_mode == "variable", input$data_variable)
    data <- intersection_data()
    if (input$data_variable %in% names(data)) {
      values <- data[[input$data_variable]]
      values <- values[!is.na(values) & is.finite(values)]
      if (length(values) > 0) {
        colorNumeric(palette = DATA_VARIABLE_PALETTE, domain = range(values), na.color = "#ccc")
      } else NULL
    } else NULL
  })
  
  # Base map initialization with custom panes for proper z-index layering
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = TRUE, minZoom = MAP_ZOOM_LIMITS$min, maxZoom = MAP_ZOOM_LIMITS$max
    )) %>%
      addTiles() %>%
      setView(lng = MAP_INITIAL_VIEW$lng, lat = MAP_INITIAL_VIEW$lat, zoom = MAP_INITIAL_VIEW$zoom) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        map.getPane('tilePane').style.zIndex = 200;
        
        // Panes for A vs B background zones
        map.createPane('bg_a');     map.getPane('bg_a').style.zIndex     = 380;
        map.createPane('bg_b');     map.getPane('bg_b').style.zIndex     = 390;
        
        // Main statewide polygons
        map.createPane('polygons'); map.getPane('polygons').style.zIndex = 400;
        
        // A vs B precinct layers
        map.createPane('p_a');      map.getPane('p_a').style.zIndex      = 410;
        map.createPane('p_b');      map.getPane('p_b').style.zIndex      = 420;
        map.createPane('p_int');    map.getPane('p_int').style.zIndex    = 440;
        
        // District outline overlays
        map.createPane('outlines'); map.getPane('outlines').style.zIndex = 450;
        map.createPane('outline');  map.getPane('outline').style.zIndex  = 460;
        
        // Search highlights
        map.createPane('highlights'); map.getPane('highlights').style.zIndex = 500;
        
        // CartoDB label overlay (non-interactive, sits above polygons)
        map.createPane('mapOverlay'); map.getPane('mapOverlay').style.zIndex = 600;
        map.getPane('mapOverlay').style.pointerEvents = 'none';
        
        // Tooltips always on top
        map.getPane('tooltipPane').style.zIndex = 700;
        
        // Add CartoDB voyager labels only
        L.tileLayer('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png', {
          pane: 'mapOverlay', attribution: '&copy; OpenStreetMap, &copy; CartoDB'
        }).addTo(map);
      }
    ")
  })
  
  # ---- MAP UPDATE: STATEWIDE GROUPING (triggered by Update Map button) ----
  observeEvent(input$update_map, {
    req(input$app_mode == "statewide")
    data <- intersection_data()
    
    fill_colors <- if (input$color_mode == "variable" && !is.null(color_pal())) {
      color_pal()(data[[input$data_variable]])
    } else {
      data$distinct_color
    }
    
    labels <- create_hover_labels(data, input$districts, input$color_mode, input$data_variable)
    
    leafletProxy("map") %>%
      clearGroup("A only") %>% clearGroup("B only") %>% clearGroup("Intersection") %>% clearControls() %>%
      clearGroup("main_polygons") %>%
      addPolygons(
        data = data, fillColor = fill_colors, fillOpacity = 0.7, color = "white",
        weight = 2.5, layerId = ~poly_id, group = "main_polygons",
        options = pathOptions(pane = "polygons"),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px", direction = "auto"
        ),
        highlightOptions = highlightOptions(weight = 3, color = "#0d47a1", fillOpacity = 0.85, bringToFront = TRUE)
      )
    
    session$sendCustomMessage(type = 'hide_loading', message = list())
  })
  
  # ---- MAP UPDATE: COLOR TOGGLE (instant, no button press required) ----
  observe({
    req(input$app_mode == "statewide")
    if (is.null(intersection_data())) return()
    
    data <- intersection_data()
    fill_colors <- if (input$color_mode == "variable" && !is.null(color_pal())) {
      color_pal()(data[[input$data_variable]])
    } else {
      data$distinct_color
    }
    
    labels <- create_hover_labels(data, input$districts, input$color_mode, input$data_variable)
    
    leafletProxy("map") %>%
      clearGroup("main_polygons") %>%
      addPolygons(
        data = data, fillColor = fill_colors, fillOpacity = 0.7, color = "white",
        weight = 2.5, layerId = ~poly_id, group = "main_polygons",
        options = pathOptions(pane = "polygons"),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px", direction = "auto"
        ),
        highlightOptions = highlightOptions(weight = 3, color = "#0d47a1", fillOpacity = 0.85, bringToFront = TRUE)
      )
  }) %>% bindEvent(input$color_mode, input$data_variable, ignoreNULL = FALSE)
  
  # ---- MAP UPDATE: DISTRICT OUTLINE OVERLAY ----
  observe({
    leafletProxy("map") %>% clearGroup("district_outline")
    if (input$show_outline && input$app_mode == "statewide") {
      outline <- outline_data()
      if (!is.null(outline)) {
        leafletProxy("map") %>%
          addPolygons(
            data = outline, fillColor = "transparent", fillOpacity = 0, color = "#000000",
            weight = 3, opacity = 1, group = "district_outline",
            options = pathOptions(interactive = FALSE, pane = "outlines")
          )
      }
    }
  })
  
  # ---- MAP UPDATE: A VS B COMPARISON ----
  observeEvent(ab_result(), {
    req(input$app_mode == "ab_compare")
    res <- ab_result()
    session$sendCustomMessage(type = 'hide_loading', message = list())
    if (is.null(res) || !is.null(res$error)) return()
    
    bbox <- res$bbox
    
    lbl_opts <- labelOptions(
      style = list("padding" = "6px 10px", "background" = "rgba(255,255,255,0.96)",
                   "border" = "1px solid #ddd", "border-radius" = "4px", "box-shadow" = "0 2px 6px rgba(0,0,0,0.1)"),
      direction = "auto"
    )
    hi_int <- highlightOptions(weight = 2.5, color = "#222", fillOpacity = 0.95, bringToFront = TRUE)
    hi_a   <- highlightOptions(weight = 2.5, color = "#0d47a1", fillOpacity = 0.8, bringToFront = TRUE)
    hi_b   <- highlightOptions(weight = 2.5, color = "#b71c1c", fillOpacity = 0.8, bringToFront = TRUE)
    
    proxy <- leafletProxy("map") %>% 
      clearGroup("main_polygons") %>% clearGroup("district_outline") %>% clearControls() %>%
      clearGroup("A only") %>% clearGroup("B only") %>% clearGroup("Intersection")
    
    # A-only zone (Blue)
    if (!is.null(res$g_a_only)) {
      proxy <- proxy %>%
        addPolygons(data = res$g_a_only, group = "A only",
                    fillColor = "#1565c0", fillOpacity = 0.12, color = "#0d47a1", weight = 2, dashArray = "5,4",
                    options = pathOptions(pane = "bg_a", interactive = FALSE)) %>%
        addPolygons(data = res$p_a_only, group = "A only",
                    fillColor = "#0d47a1", fillOpacity = 0.55, color = "#0d47a1", weight = 0.5,
                    options = pathOptions(pane = "p_a"), label = lapply(res$p_a_only$hover, HTML), 
                    labelOptions = lbl_opts, highlightOptions = hi_a)
    }
    
    # B-only zone (Red)
    if (!is.null(res$g_b_only)) {
      proxy <- proxy %>%
        addPolygons(data = res$g_b_only, group = "B only",
                    fillColor = "#b71c1c", fillOpacity = 0.12, color = "#c62828", weight = 2, dashArray = "5,4",
                    options = pathOptions(pane = "bg_b", interactive = FALSE)) %>%
        addPolygons(data = res$p_b_only, group = "B only",
                    fillColor = "#b71c1c", fillOpacity = 0.55, color = "#c62828", weight = 0.5,
                    options = pathOptions(pane = "p_b"), label = lapply(res$p_b_only$hover, HTML), 
                    labelOptions = lbl_opts, highlightOptions = hi_b)
    }
    
    # Intersection zone (Purple)
    proxy <- proxy %>%
      addPolygons(data = res$p_int, group = "Intersection",
                  fillColor = "#800080", fillOpacity = 0.75, color = "white", weight = 0.7,
                  options = pathOptions(pane = "p_int"), label = lapply(res$p_int$hover, HTML), 
                  labelOptions = lbl_opts, highlightOptions = hi_int) %>%
      addPolygons(data = res$g_int, group = "Intersection",
                  fillColor = "transparent", fillOpacity = 0, color = "#222", weight = 3,
                  options = pathOptions(pane = "outline", interactive = FALSE))
    
    # Zone legend
    zone_colors <- c("#800080")
    zone_labels <- c("Intersection")
    if (!is.null(res$g_a_only)) { zone_colors <- c(zone_colors, "#0d47a1"); zone_labels <- c(zone_labels, paste0(res$label_a, " only")) }
    if (!is.null(res$g_b_only)) { zone_colors <- c(zone_colors, "#b71c1c"); zone_labels <- c(zone_labels, paste0(res$label_b, " only")) }
    
    proxy <- proxy %>% addLegend("bottomleft", 
                                 colors = zone_colors, 
                                 labels = zone_labels, 
                                 title = "<span style='font-family: GT-America-Compressed-Bold, sans-serif; letter-spacing: 1px;'>ZONE</span>", 
                                 opacity = 0.9)
    
    proxy %>% fitBounds(bbox$xmin - 0.05, bbox$ymin - 0.05, bbox$xmax + 0.05, bbox$ymax + 0.05)
  })
  
  # ----------------------- RIGHT SIDEBAR: STATEWIDE -------------------
  
  output$info_panel <- renderUI({
    if(input$app_mode == "ab_compare") return(NULL)
    
    d <- clicked_district()
    if (is.null(d)) return(NULL)
    
    current_data <- intersection_data()
    if (is.null(current_data)) return(NULL)
    
    # Validate that district still exists in the current intersection
    missing_districts <- setdiff(input$districts, names(d))
    if (length(missing_districts) > 0) {
      clicked_district(NULL)
      showNotification("District selections changed. Please click 'Update Map' to see new intersections.", type = "warning", duration = 5)
      return(NULL)
    }
    
    district_match <- current_data %>% st_drop_geometry()
    for (dist_type in input$districts) {
      district_match <- district_match %>% filter(!!sym(dist_type) == d[[dist_type]])
    }
    
    if (nrow(district_match) == 0) {
      clicked_district(NULL)
      showNotification("District selections changed. Please click 'Update Map' to see new intersections.", type = "warning", duration = 5)
      return(NULL)
    }
    
    district_parts <- build_district_names(d, input$districts)
    
    tags$div(class = "right-sidebar active",
             div(class = "sidebar-header",
                 div(class = "sidebar-header-content",
                     h3("District Breakdown"),
                     div(class = "district-names", HTML(paste(district_parts, collapse = "<br>")))
                 ),
                 tags$button(class = "close-sidebar", "×")
             ),
             div(class = "sidebar-content",
                 div(class = "stat-grid",
                     div(class = "stat-card", div(class = "label", "Population"), div(class = "value", scales::comma(d$tot_pop))),
                     div(class = "stat-card", div(class = "label", "Voting Age Population"), div(class = "value", scales::comma(d$vap_total))),
                     div(class = "stat-card", div(class = "label", "Registered"), div(class = "value", scales::comma(d$reg_total_25_12))),
                     div(class = "stat-card", div(class = "label", "% Democrat"), div(class = "value", paste0(round(d$pct_reg_dem, 1), "%")))
                 ),
                 div(class = "plot-section", h5("PRECINCT BREAKDOWN (% DEMOCRAT)"), leafletOutput("precinct_map", height = "200px")),
                 div(class = "plot-section", h5("PARTY REGISTRATION"), highchartOutput("party_plot", height = "200px")),
                 div(class = "plot-section", h5("REGISTRATION DEMOGRAPHICS"), highchartOutput("demo_plot", height = "200px")),
                 div(class = "plot-section", h5("DISTRICT VS. LOUISIANA STATEWIDE"), highchartOutput("comparison_plot", height = "250px"))
             )
    )
  })
  
  # ----------------------- PLOTS: STATEWIDE SIDEBAR -------------------
  
  # Mini precinct-level map colored by % Democrat
  output$precinct_map <- renderLeaflet({
    d <- clicked_district()
    req(d)
    
    selected_precincts <- precincts_data
    for (dist_type in input$districts) {
      district_value <- d[[dist_type]]
      selected_precincts <- selected_precincts %>% filter(!!sym(dist_type) == district_value)
    }
    
    selected_precincts <- selected_precincts %>%
      mutate(
        pct_dem = (reg_dem_total_25_12 / reg_total_25_12) * 100,
        pct_dem = ifelse(is.na(pct_dem) | !is.finite(pct_dem), 0, pct_dem),
        precinct_label = paste0(unit_name, ", ", countyname)
      ) %>% st_transform(4326)
    
    bounds <- st_bbox(selected_precincts)
    pal <- colorNumeric(palette = c(PARTISAN_GRADIENT["low"], PARTISAN_GRADIENT["mid"], PARTISAN_GRADIENT["high"]), domain = c(0, 100))
    
    labels <- sprintf(
      "<div style='font-family: Century-Schoolbook, serif;'>
      <b style='font-family: GT-America-Compressed-Bold, sans-serif; color: #0d47a1; font-size: 13px;'>%s</b><br/>
      <span style='font-size: 12px; color: #333;'>%% Democrat: <b style='font-family: GT-America-Compressed-Bold, sans-serif;'>%s%%</b></span><br/>
      <span style='font-size: 11px; color: #666;'>Registered: %s</span>
      </div>",
      selected_precincts$precinct_label, round(selected_precincts$pct_dem, 1), scales::comma(selected_precincts$reg_total_25_12)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(selected_precincts, options = leafletOptions(
      zoomControl = FALSE, attributionControl = FALSE, dragging = TRUE, scrollWheelZoom = TRUE, doubleClickZoom = FALSE, touchZoom = TRUE
    )) %>%
      addPolygons(
        fillColor = ~pal(pct_dem), fillOpacity = 0.8, color = "transparent", weight = 2.5, opacity = 0,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", "padding" = "6px 10px", "background" = "rgba(255, 255, 255, 0.97)",
                       "border" = "1px solid #ddd", "border-radius" = "4px", "box-shadow" = "0 2px 4px rgba(0,0,0,0.1)"),
          textsize = "13px", direction = "auto"
        ),
        highlightOptions = highlightOptions(weight = 2, color = "#0d47a1", fillOpacity = 0.95, bringToFront = TRUE)
      ) %>%
      htmlwidgets::onRender(sprintf("
        function(el, x) {
          var map = this;
          el.style.backgroundColor = 'white';
          var southWest = L.latLng(%f, %f);
          var northEast = L.latLng(%f, %f);
          var bounds = L.latLngBounds(southWest, northEast);
          map.setMaxBounds(bounds);
          setTimeout(function() { map.fitBounds(bounds, {padding: [10, 10]}); }, 100);
          map.options.minZoom = map.getZoom() - 1;
          map.options.maxZoom = 18;
        }
      ", bounds["ymin"], bounds["xmin"], bounds["ymax"], bounds["xmax"]))
  })
  
  # Party registration column chart
  output$party_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    party_data <- data.frame(
      Party = c("Democrat", "Republican", "Other"),
      Total = c(d$reg_dem_total_25_12, d$reg_rep_total_25_12, d$reg_oth_total_25_12),
      stringsAsFactors = FALSE
    ) %>% filter(Total > 0) %>% arrange(desc(Total))
    
    party_data$Color <- sapply(party_data$Party, function(party) {
      switch(party, "Democrat" = PARTY_COLORS["Democrat"], "Republican" = PARTY_COLORS["Republican"], "Other" = PARTY_COLORS["Other"], "#888888")
    })
    
    highchart() %>%
      hc_chart(type = "column", backgroundColor = "transparent") %>%
      hc_xAxis(categories = party_data$Party, labels = list(style = list(fontSize = "11px", fontFamily = "Century-Schoolbook, serif", color = "#333"))) %>%
      hc_yAxis(visible = FALSE, title = list(text = NULL)) %>%
      hc_add_series(
        name = "Registered", data = party_data$Total, colorByPoint = TRUE, colors = party_data$Color,
        dataLabels = list(enabled = TRUE, format = "{point.y:,.0f}", style = list(fontSize = "14px", fontWeight = "bold", fontFamily = "GT-America-Compressed-Bold, sans-serif", color = "#000000", textOutline = "none"), y = -10)
      ) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "<b>{point.category}</b><br/>Registered: <b>{point.y:,.0f}</b>") %>%
      hc_plotOptions(column = list(pointPadding = 0.1, groupPadding = 0.15, borderWidth = 0)) %>%
      hc_legend(enabled = FALSE) %>% hc_credits(enabled = FALSE) %>% hc_exporting(enabled = FALSE)
  })
  
  # Registration demographics column chart
  output$demo_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    demo_data <- data.frame(
      Race = c("White", "Black", "Asian", "Hispanic", "Native American"),
      Total = c(d$tot_white, d$tot_black, d$tot_asian, d$tot_hispanic, d$tot_amind),
      stringsAsFactors = FALSE
    ) %>% filter(Total > 0) %>% arrange(desc(Total))
    
    demo_data$Color <- sapply(demo_data$Race, function(race) {
      switch(race, "White" = DEMOGRAPHIC_COLORS["White"], "Black" = DEMOGRAPHIC_COLORS["Black"], "Asian" = DEMOGRAPHIC_COLORS["Asian"], "Hispanic" = DEMOGRAPHIC_COLORS["Hispanic"], "Native American" = DEMOGRAPHIC_COLORS["Native American"], "#888888")
    })
    
    highchart() %>%
      hc_chart(type = "column", backgroundColor = "transparent") %>%
      hc_xAxis(categories = demo_data$Race, labels = list(style = list(fontSize = "11px", fontFamily = "Century-Schoolbook, serif", color = "#333"))) %>%
      hc_yAxis(visible = FALSE, title = list(text = NULL)) %>%
      hc_add_series(
        name = "Population", data = demo_data$Total, colorByPoint = TRUE, colors = demo_data$Color,
        dataLabels = list(enabled = TRUE, format = "{point.y:,.0f}", style = list(fontSize = "14px", fontWeight = "bold", fontFamily = "GT-America-Compressed-Bold, sans-serif", color = "#000000", textOutline = "none"), y = -10)
      ) %>%
      hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "<b>{point.category}</b><br/>Population: <b>{point.y:,.0f}</b>") %>%
      hc_plotOptions(column = list(pointPadding = 0.1, groupPadding = 0.15, borderWidth = 0)) %>%
      hc_legend(enabled = FALSE) %>% hc_credits(enabled = FALSE) %>% hc_exporting(enabled = FALSE)
  })
  
  # District vs. statewide comparison horizontal bar chart
  output$comparison_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    state_totals <- precincts_data %>% st_drop_geometry() %>% summarize(
      tot_pop = sum(tot_pop, na.rm = TRUE), tot_white = sum(tot_white, na.rm = TRUE), tot_black = sum(tot_black, na.rm = TRUE),
      tot_asian = sum(tot_asian, na.rm = TRUE), tot_hispanic = sum(tot_hispanic, na.rm = TRUE), tot_amind = sum(tot_amind, na.rm = TRUE),
      reg_dem_total = sum(reg_dem_total_25_12, na.rm = TRUE), reg_rep_total = sum(reg_rep_total_25_12, na.rm = TRUE), reg_oth_total = sum(reg_oth_total_25_12, na.rm = TRUE)
    ) %>% mutate(
      pct_white = tot_white / tot_pop * 100, pct_black = tot_black / tot_pop * 100, pct_asian = tot_asian / tot_pop * 100,
      pct_hispanic = tot_hispanic / tot_pop * 100, pct_amind = tot_amind / tot_pop * 100,
      pct_dem = reg_dem_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100,
      pct_rep = reg_rep_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100,
      pct_oth = reg_oth_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100
    )
    
    comparison_data <- data.frame(
      Category = c("% White", "% Black", "% Asian", "% Hispanic", "% Democrat", "% Republican"),
      District = c(d$pct_white, d$pct_black, d$pct_asian, d$pct_hispanic, d$pct_reg_dem, d$pct_reg_rep),
      State = c(state_totals$pct_white, state_totals$pct_black, state_totals$pct_asian, state_totals$pct_hispanic, state_totals$pct_dem, state_totals$pct_rep)
    )
    
    highchart() %>%
      hc_chart(type = "bar", backgroundColor = "transparent") %>%
      hc_xAxis(categories = comparison_data$Category, labels = list(style = list(fontSize = "11px", fontFamily = "Century-Schoolbook, serif", color = "#333"))) %>%
      hc_yAxis(visible = FALSE, title = list(text = NULL)) %>%
      hc_add_series(
        name = "This District", data = comparison_data$District, color = "#0d47a1",
        dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%", style = list(fontSize = "11px", fontWeight = "bold", fontFamily = "GT-America-Compressed-Bold, sans-serif", color = "#000000", textOutline = "none"))
      ) %>%
      hc_add_series(
        name = "Louisiana Statewide", data = comparison_data$State, color = "#D93A3F",
        dataLabels = list(enabled = TRUE, format = "{point.y:.1f}%", style = list(fontSize = "11px", fontWeight = "bold", fontFamily = "GT-America-Compressed-Bold, sans-serif", color = "#000000", textOutline = "none"))
      ) %>%
      hc_tooltip(shared = TRUE, useHTML = TRUE, headerFormat = "<b>{point.key}</b><br/>", pointFormat = "{series.name}: <b>{point.y:.1f}%</b><br/>") %>%
      hc_legend(align = "center", verticalAlign = "bottom", layout = "horizontal", itemStyle = list(fontSize = "11px", fontFamily = "Century-Schoolbook, serif")) %>%
      hc_plotOptions(bar = list(groupPadding = 0.1)) %>%
      hc_credits(enabled = FALSE) %>% hc_exporting(enabled = FALSE)
  })
  
  # ----------------------- A VS B MODE --------------------------------
  
  # Dynamic district A dropdown
  output$district_a_ui <- renderUI({
    req(input$type_a)
    
    vals <- precincts_data[[input$type_a]]
    vals <- stringr::str_sort(unique(vals[!is.na(vals)]), numeric = TRUE)
    
    selectInput("district_a", "District", choices = c("Select..." = "", vals), width = "100%")
  })
  
  # Dynamic district B dropdown (filtered to districts overlapping A when possible)
  output$district_b_ui <- renderUI({
    req(input$type_b)
    
    if (!is.null(input$district_a) && nchar(input$district_a) > 0 && input$type_a != input$type_b) {
      vals <- precincts_data %>%
        st_drop_geometry() %>%
        filter(!!sym(input$type_a) == input$district_a) %>%
        pull(!!sym(input$type_b))
    } else {
      vals <- precincts_data[[input$type_b]]
    }
    
    vals <- stringr::str_sort(unique(vals[!is.na(vals)]), numeric = TRUE)
    
    selectInput("district_b", "District", choices = c("Select..." = "", vals), width = "100%")
  })
  
  # Compute intersection result for A vs B (triggered by Find Intersection button)
  ab_result <- eventReactive(input$find_ab, {
    req(input$district_a, nchar(input$district_a) > 0, input$district_b, nchar(input$district_b) > 0)
    
    if (input$type_a == input$type_b)
      return(list(error = "Please choose two different district types."))
    
    type_a <- input$type_a; dist_a <- input$district_a
    type_b <- input$type_b; dist_b <- input$district_b
    
    p_int    <- precincts_data %>% filter(!!sym(type_a) == dist_a, !!sym(type_b)  == dist_b)  %>% st_make_valid() %>% st_buffer(0)
    p_a_only <- precincts_data %>% filter(!!sym(type_a) == dist_a, !(!!sym(type_b) == dist_b)) %>% st_make_valid()%>% st_buffer(0)
    p_b_only <- precincts_data %>% filter(!!sym(type_b) == dist_b, !(!!sym(type_a) == dist_a)) %>% st_make_valid()%>% st_buffer(0)
    
    if (nrow(p_int) == 0)
      return(list(error = paste0(DISTRICT_NAMES[type_a], " ", dist_a, " and ",
                                 DISTRICT_NAMES[type_b], " ", dist_b, " do not intersect.")))
    
    # Aggregate stats for a precinct subset (NA-safe, zero-denominator-safe)
    stats <- function(df) {
      if (nrow(df) == 0) return(NULL)
      df %>% st_drop_geometry() %>%
        summarize(
          tot_pop   = sum(tot_pop,             na.rm = TRUE),
          vap       = sum(vap_total,           na.rm = TRUE),
          reg       = sum(reg_total_25_12,     na.rm = TRUE),
          dem       = sum(reg_dem_total_25_12, na.rm = TRUE),
          rep       = sum(reg_rep_total_25_12, na.rm = TRUE),
          white     = sum(tot_white,           na.rm = TRUE),
          black     = sum(tot_black,           na.rm = TRUE),
          hispanic  = sum(tot_hispanic,        na.rm = TRUE)
        ) %>%
        mutate(
          pct_dem  = if_else(reg   > 0, dem   / reg   * 100, NA_real_),
          pct_rep  = if_else(reg   > 0, rep   / reg   * 100, NA_real_),
          pct_wht  = if_else(tot_pop > 0, white     / tot_pop * 100, NA_real_),
          pct_blk  = if_else(tot_pop > 0, black     / tot_pop * 100, NA_real_),
          pct_his  = if_else(tot_pop > 0, hispanic / tot_pop * 100, NA_real_)
        )
    }
    
    s_int    <- stats(p_int)
    s_a_only <- stats(p_a_only)
    s_b_only <- stats(p_b_only)
    
    # Union geometries for zone backgrounds
    union_geom <- function(df) {
      if (nrow(df) == 0) return(NULL)
      df %>% st_buffer(0) %>% summarize(geometry = st_union(geometry)) %>% st_buffer(0) %>% st_make_valid() %>% st_transform(4326)
    }
    
    g_int    <- union_geom(p_int)
    g_a_only <- union_geom(p_a_only)
    g_b_only <- union_geom(p_b_only)
    
    # Add hover labels for individual precincts
    prep <- function(df) {
      if (nrow(df) == 0) return(df)
      df %>%
        st_buffer(0) %>%
        mutate(
          pct_dem = if_else(reg_total_25_12 > 0, reg_dem_total_25_12 / reg_total_25_12 * 100, 0),
          hover = paste0(
            "<div style='font-family:Century-Schoolbook; min-width:160px; font-size: 13px;'>",
            "<b>Precinct: </b>", unit_name, ", ", countyname, "<br/>"
          )
        ) %>% st_buffer(0) %>% st_transform(4326) %>%
        st_make_valid()
    }
    
    p_int    <- prep(p_int)
    p_a_only <- prep(p_a_only)
    p_b_only <- prep(p_b_only)
    
    all_g <- Filter(Negate(is.null), list(g_int, g_a_only, g_b_only))
    bbox  <- st_bbox(Reduce(st_union, all_g))
    
    overlap <- case_when(
      is.null(s_a_only) && is.null(s_b_only) ~ "both_complete",
      is.null(s_a_only)                      ~ "a_inside_b",
      is.null(s_b_only)                      ~ "b_inside_a",
      TRUE                                   ~ "partial"
    )
    
    list(
      error = NULL, overlap = overlap,
      label_a = paste(dist_a),
      label_b = paste(dist_b),
      s_int = s_int, s_a_only = s_a_only, s_b_only = s_b_only,
      g_int = g_int, g_a_only = g_a_only, g_b_only = g_b_only,
      p_int = p_int, p_a_only = p_a_only, p_b_only = p_b_only,
      n_int = nrow(p_int), n_a = nrow(p_a_only), n_b = nrow(p_b_only),
      bbox = bbox
    )
  })
  
  # ---- A VS B SIDEBAR: UI HELPERS ----
  
  pct_fn <- function(x) if (is.null(x) || is.na(x)) tags$span(class="stat-na","—") else paste0(round(x,1),"%")
  num_fn <- function(x) if (is.null(x) || is.na(x)) tags$span(class="stat-na","—") else comma(round(x))
  
  # Builds a single zone stats card for the intersection results panel
  card <- function(s, hdr_class, title, subtitle) {
    div(class = "zone-card",
        div(class = paste("zone-card-header", hdr_class),
            title, tags$small(subtitle)),
        div(class = "zone-card-body",
            div(class="stat-row", span(class="stat-lbl","Population"),   span(class="stat-val", num_fn(s$tot_pop))),
            div(class="stat-row", span(class="stat-lbl","VAP"),          span(class="stat-val", num_fn(s$vap))),
            div(class="stat-row", span(class="stat-lbl","Registered"),   span(class="stat-val", num_fn(s$reg))),
            div(class="stat-row", span(class="stat-lbl","% Democrat"),   span(class="stat-val", pct_fn(s$pct_dem))),
            div(class="stat-row", span(class="stat-lbl","% Republican"), span(class="stat-val", pct_fn(s$pct_rep))),
            div(class="stat-row", span(class="stat-lbl","% White"),      span(class="stat-val", pct_fn(s$pct_wht))),
            div(class="stat-row", span(class="stat-lbl","% Black"),      span(class="stat-val", pct_fn(s$pct_blk))),
            div(class="stat-row", span(class="stat-lbl","% Hispanic"),   span(class="stat-val", pct_fn(s$pct_his)))
        )
    )
  }
  
  # ---- A VS B SIDEBAR: FLOATING RESULTS PANEL ----
  
  output$ab_floating_panel <- renderUI({
    req(input$app_mode == "ab_compare")
    res <- ab_result()
    if (is.null(res) || !is.null(res$error)) return()
    
    # Status note describing the type of overlap
    note <- switch(res$overlap,
                   both_complete = div(class="ab-status-msg ab-status-info", tags$i(class="fas fa-info-circle"),
                                       paste0(" Complete overlap — ", res$label_a, " and ", res$label_b, " share all the same precincts.")),
                   a_inside_b    = div(class="ab-status-msg ab-status-info", tags$i(class="fas fa-info-circle"),
                                       paste0(" ", res$label_a, " is fully contained within ", res$label_b, ".")),
                   b_inside_a    = div(class="ab-status-msg ab-status-info", tags$i(class="fas fa-info-circle"),
                                       paste0(" ", res$label_b, " is fully contained within ", res$label_a, ".")),
                   partial       = div(class="ab-status-msg ab-status-ok", tags$i(class="fas fa-check-circle"),
                                       paste0(" Partial intersection: ", res$label_a, " & ", res$label_b))
    )
    
    int_card <- card(res$s_int, "hdr-int",
                     paste0("Intersection"),
                     paste0(res$n_int, " precinct", if(res$n_int!=1)"s"))
    
    a_card <- if (!is.null(res$s_a_only))
      card(res$s_a_only, "hdr-a",
           paste0(res$label_a, " only"),
           paste0(res$n_a, " precinct", if(res$n_a!=1)"s", " not in ", res$label_b))
    
    b_card <- if (!is.null(res$s_b_only))
      card(res$s_b_only, "hdr-b",
           paste0(res$label_b, " only"),
           paste0(res$n_b, " precinct", if(res$n_b!=1)"s", " not in ", res$label_a))
    
    tags$div(class = "right-sidebar active", 
             div(class = "sidebar-header",
                 div(class = "sidebar-header-content",
                     h3("Intersection Results")
                 ),
                 tags$button(class = "close-sidebar", "×")
             ),
             div(class = "sidebar-content",
                 note, int_card, a_card, b_card
             )
    )
  })
  
  # ----------------------- DATA TABLE & DOWNLOAD ----------------------
  
  # Returns the appropriate dataset depending on the active mode
  table_data <- reactive({
    if (input$app_mode == "statewide") {
      req(intersection_data())
      
      intersection_data() %>%
        st_drop_geometry() %>%
        select(-poly_id, -color_group, -distinct_color) %>%
        mutate(across(where(is.numeric), ~round(., 1)))
      
    } else if (input$app_mode == "ab_compare") {
      res <- ab_result()
      req(res, is.null(res$error))
      
      # Helper to drop geometry and tag rows by overlap zone
      process_chunk <- function(df, status_label) {
        if (is.null(df) || nrow(df) == 0) return(NULL)
        df %>%
          st_drop_geometry() %>%
          mutate(`Overlap Status` = status_label)
      }
      
      df_int <- process_chunk(res$p_int, "Intersection")
      df_a   <- process_chunk(res$p_a_only, paste(res$label_a, "Only"))
      df_b   <- process_chunk(res$p_b_only, paste(res$label_b, "Only"))
      
      bind_rows(df_int, df_a, df_b) %>%
        select(-any_of(c("pct_dem", "hover"))) %>%
        mutate(across(where(is.numeric), ~round(., 1)))
    }
  })
  
  # Render data table with human-readable column names
  output$district_table <- renderDT({
    data <- table_data()
    req(data)
    
    if (input$app_mode == "ab_compare") {
      cols_to_keep <- c("unit_name", "countyname", "Overlap Status", input$type_a, input$type_b)
      
      display_data <- data %>%
        select(any_of(cols_to_keep), tot_pop:last_col()) %>%
        select(unit_name, countyname, `Overlap Status`, everything()) %>%
        rename(any_of(column_renames))
      
    } else {
      display_data <- data %>%
        rename(any_of(column_renames))
    }
    
    datatable(
      display_data, 
      options = list(
        scrollY = '40vh', 
        scrollX = TRUE,
        dom = 'rtip',
        autoWidth = TRUE,
        paging = FALSE
      ),
      rownames = FALSE, 
      class = 'cell-border stripe hover compact'
    )
  })
  
  # CSV download (filename and column names adapt to active mode)
  output$download_data <- downloadHandler(
    filename = function() {
      mode_prefix <- ifelse(input$app_mode == "statewide", "louisiana_districts_", "louisiana_selected_precincts_")
      paste0(mode_prefix, format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- table_data()
      req(data)
      
      if (input$app_mode == "ab_compare") {
        cols_to_keep <- c("unit_name", "countyname", "Overlap Status", input$type_a, input$type_b)
        
        final_csv_data <- data %>%
          select(any_of(cols_to_keep), tot_pop:last_col()) %>%
          select(unit_name, countyname, `Overlap Status`, everything()) %>%
          rename(any_of(column_renames))
        
      } else {
        final_csv_data <- data %>%
          rename(any_of(column_renames))
      }
      
      write.csv(final_csv_data, file, row.names = FALSE)
    }
  )
  
  # ----------------------- MODE SWITCHING -----------------------------
  
  # Clear map, reset all inputs, and hide UI when switching modes
  observeEvent(input$app_mode, {
    leafletProxy("map") %>%
      clearGroup("main_polygons") %>%
      clearGroup("district_outline") %>%
      clearGroup("A only") %>%
      clearGroup("B only") %>%
      clearGroup("Intersection") %>%
      clearGroup("search_highlight") %>%
      clearMarkers() %>%
      clearControls()
    
    clicked_district(NULL)
    
    # Reset search inputs
    updateTextInput(session, "address_input", value = "")
    updateSelectInput(session, "district_type_search", selected = "")
    
    # Reset statewide controls
    updateCheckboxGroupInput(session, "districts", selected = "congressional")
    updateRadioButtons(session, "color_mode", selected = "distinct")
    updateSelectInput(session, "data_variable", selected = "tot_pop")
    updateCheckboxInput(session, "show_outline", value = FALSE)
    updateSelectInput(session, "outline_district", selected = "congressional")
    
    # Reset A vs B controls
    updateSelectInput(session, "type_a", selected = "congressional")
    updateSelectInput(session, "type_b", selected = "senate")
    
    shinyjs::runjs("$('.right-sidebar').removeClass('active');")
    
    shinyjs::runjs("
      if ($('#bottom-drawer').hasClass('open')) {
        $('#bottom-drawer').removeClass('open');
        $('#table-backdrop').removeClass('active');
        $('#toggle-drawer').text('Show Data Table ▲');
        $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
      }
    ")
  }, ignoreInit = TRUE)
  
  # ----------------------- A VS B DISTRICT DROPDOWNS ------------------
  
  # Base district type choices (used for mutual exclusion logic)
  district_choices <- c(
    "Congressional" = "congressional", 
    "State Senate" = "senate",
    "State House" = "house",
    "Public Service Commission" = "public_service_commission",
    "Supreme Court" = "supreme_court"
  )
  
  # When type A changes, remove it from type B's options
  observeEvent(input$type_a, {
    req(input$type_a)
    curr_b <- input$type_b 
    valid_choices_b <- district_choices[district_choices != input$type_a]
    new_sel_b <- if (curr_b %in% valid_choices_b) curr_b else valid_choices_b[1]
    updateSelectInput(session, "type_b", choices = valid_choices_b, selected = new_sel_b)
  }, ignoreInit = TRUE)
  
  # When type B changes, remove it from type A's options
  observeEvent(input$type_b, {
    req(input$type_b)
    curr_a <- input$type_a
    valid_choices_a <- district_choices[district_choices != input$type_b]
    new_sel_a <- if (curr_a %in% valid_choices_a) curr_a else valid_choices_a[1]
    updateSelectInput(session, "type_a", choices = valid_choices_a, selected = new_sel_a)
  }, ignoreInit = TRUE)
  
  # Disable Find Intersection button until both districts are selected
  observe({
    a_valid <- !is.null(input$district_a) && nchar(input$district_a) > 0
    b_valid <- !is.null(input$district_b) && nchar(input$district_b) > 0
    
    if (a_valid && b_valid) {
      shinyjs::enable("find_ab")
    } else {
      shinyjs::disable("find_ab")
    }
  })
  
  # Auto-zoom to selected A and/or B districts as dropdowns are updated
  observe({
    req(input$app_mode == "ab_compare")
    
    a_valid <- !is.null(input$district_a) && nchar(input$district_a) > 0
    b_valid <- !is.null(input$district_b) && nchar(input$district_b) > 0
    
    if (!a_valid && !b_valid) return()
    
    subset_a <- NULL
    subset_b <- NULL
    
    if (a_valid && !is.null(input$type_a)) {
      subset_a <- precincts_data %>% filter(!!sym(input$type_a) == input$district_a)
    }
    
    if (b_valid && !is.null(input$type_b)) {
      subset_b <- precincts_data %>% filter(!!sym(input$type_b) == input$district_b)
    }
    
    combined_areas <- bind_rows(subset_a, subset_b)
    
    if (!is.null(combined_areas) && nrow(combined_areas) > 0) {
      bbox <- st_bbox(st_transform(combined_areas, 4326))
      
      leafletProxy("map") %>%
        fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    }
  })
}

# Running the server
shinyApp(ui = ui, server = server)

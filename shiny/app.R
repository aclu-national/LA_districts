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

# Font setup for plots
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
# Loading in data
load("clean_data/precinct_area_data_simple.RData")
precincts_data <- precincts_area_simple

# Variable label mappings
VAR_LABELS <- c(
  "tot_pop" = "Total Population",
  "pct_white" = "% White",
  "pct_black" = "% Black",
  "pct_asian" = "% Asian",
  "pct_amind" = "% American Indian",
  "pct_other" = "% Other Race",
  "pct_hispanic" = "% Hispanic",
  "tot_white" = "White Population",
  "tot_black" = "Black Population",
  "tot_asian" = "Asian Population",
  "tot_amind" = "American Indian Population",
  "tot_other" = "Other Race Population",
  "tot_hispanic" = "Hispanic Population",
  "vap_total" = "VAP Total",
  "vap_pct_white" = "VAP % White",
  "vap_pct_black" = "VAP % Black",
  "vap_pct_asian" = "VAP % Asian",
  "vap_pct_amind" = "VAP % American Indian",
  "vap_pct_other" = "VAP % Other",
  "vap_pct_hispanic" = "VAP % Hispanic",
  "vap_white" = "VAP White",
  "vap_black" = "VAP Black",
  "vap_asian" = "VAP Asian",
  "vap_amind" = "VAP American Indian",
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

# Color palettes
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
  "American Indian" = "#235564"
)

PARTISAN_GRADIENT <- c(
  low = "#D93A3F", 
  mid = "#9370DB",
  high = "#0055AA"
)

DATA_VARIABLE_PALETTE <- c("#FEE5D8", "#FCC2AA", "#F03F2E", "#CC191E", "#B91419", "#67000D")

# Map settings
MAP_INITIAL_VIEW <- list(lng = -92.5, lat = 30.5, zoom = 6.5)
MAP_ZOOM_LIMITS <- list(min = 6, max = 18)
# --------------------------- FUNCTIONS ----------------------------------------

# Function map assigner (assigns unique colors)
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

# Defining map colors (and reversing them)
map_colors <- c(
  "#fEE5D8", "#FCC2AA", "#F03F2E", "#CC191E", "#B91419", "#67000D"
) %>%
  rev()

# ----------------------------- Shiny UI ---------------------------------------

# Defining UI
ui <- fluidPage(
  
  # Adding JavaScript libraries
  useShinyjs(),
  
  # Adding external CSS and JS libraries
  tags$head(
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
    # Custom stylesheet
    includeCSS("www/style.css"),
    # tags$script(src = "www/script.js")
    tags$script(HTML("
    $(document).ready(function() {
        
        // Making right sidebar draggable by its header
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
        
        // Toggling left sidebar
        $('#toggleLeft').click(function() {
          $('.left-sidebar').toggleClass('collapsed');
          if ($('.left-sidebar').hasClass('collapsed')) {
            $(this).html('Show Controls ▶'); 
          } else {
            $(this).html('Hide Controls ◀'); 
          }
        });
        
        // Toggling search container
        $('#toggleSearch').click(function() {
          $('.search-container').toggleClass('collapsed');
          if ($('.search-container').hasClass('collapsed')) {
            $(this).html('Show Search ◀'); 
          } else {
            $(this).html('Hide Search ▶'); 
          }
        });
        
        // Closing sidebars when clicking map
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
            $('.right-sidebar').removeClass('active');
            
            if ($('#bottom-drawer').hasClass('open')) {
              $('#bottom-drawer').removeClass('open');
              $('#table-backdrop').removeClass('active');
              $('#toggle-drawer').text('Show Data Table ▲');
              $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
            }
          }
        });
        
        // Closing table when clicking backdrop
        $('#table-backdrop').on('click', function() {
          $('#bottom-drawer').removeClass('open');
          $(this).removeClass('active');
          $('#toggle-drawer').text('Show Data Table ▲');
          $('#btn-show-table').html('<i class=\"fas fa-table\"></i> Show Data Table');
        });
        
        // Closing right sidebar
        $(document).on('click', '.close-sidebar', function() {
          $('.right-sidebar').removeClass('active');
        });
        
        // Toggling data table drawer
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
        
        // Showing about modal
        $('#about_btn').click(function() {
          Shiny.setInputValue('show_about', Math.random());
        });

        $('#toggle-drawer').click(toggleDataTable);
        $('#btn-show-table').click(toggleDataTable);
        
        // Showing loading overlay when updating map
        $('#update_map').on('click', function() {
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
        
        // Collapsing sidebars when searching address
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
        
        // Collapsing sidebars when searching district
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
      });
      
      // Hiding loading overlay
      Shiny.addCustomMessageHandler('hide_loading', function(message) {
        $('#loading-overlay').removeClass('active');
      });
    ")
    )),
  
  # Creating header
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
  
  # Loading overlay
  div(id = "loading-overlay", class = "loading-overlay",
      div(class = "loading-content",
          div(class = "loading-spinner"),
          div(class = "loading-text", "Updating Map"),
          div(class = "loading-subtext", "Processing district intersections...")
      )
  ),
  
  # Map as full background layer
  div(class = "map-section",
      leafletOutput("map") %>% withSpinner(color = "#0d47a1")
  ),
  
  # Floating UI elements on top of map
  div(class = "app-container",
      
      # Left sidebar toggle button
      tags$button(id = "toggleLeft", class = "sidebar-toggle left-toggle", "Hide Controls ◀"),
      
      # Search toggle button
      tags$button(id = "toggleSearch", class = "search-toggle", "Hide Search ▶"),
      
      # Search container
      div(class = "search-container",
          div(class = "search-header",
              tags$i(class = "fas fa-search"),
              "Search"
          ),
          div(class = "search-content",
              # Address search section
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
              # District search section
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
              # District selection panel
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
              
              # Color mode panel
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
                        "% American Indian" = "pct_amind",
                        "% Other Race" = "pct_other",
                        "% Hispanic" = "pct_hispanic",
                        "White Population" = "tot_white",
                        "Black Population" = "tot_black",
                        "Asian Population" = "tot_asian",
                        "American Indian Population" = "tot_amind",
                        "Other Race Population" = "tot_other",
                        "Hispanic Population" = "tot_hispanic",
                        "VAP Total" = "vap_total",
                        "VAP % White" = "vap_pct_white",
                        "VAP % Black" = "vap_pct_black",
                        "VAP % Asian" = "vap_pct_asian",
                        "VAP % American Indian" = "vap_pct_amind",
                        "VAP % Other" = "vap_pct_other",
                        "VAP % Hispanic" = "vap_pct_hispanic",
                        "VAP White" = "vap_white",
                        "VAP Black" = "vap_black",
                        "VAP Asian" = "vap_asian",
                        "VAP American Indian" = "vap_amind",
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
                  p(class = "control-hint", 
                    "Colors update instantly")
              ),
              
              # District outline panel
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
                  p(class = "control-hint", 
                    "Outlines update instantly")
              ),
              
              # How to use info box
              div(class = "info-box",
                  h4("How TO USe"),
                  p("1. Select district types and click 'Update Map'"),
                  p("2. Instantly adjust colors and variables"),
                  p("3. Add district outlines for reference"),
                  p("4. Search addresses (top-right) or districts"),
                  p("5. Hover over areas for quick info"),
                  p("6. Click regions for detailed breakdowns"),
                  p("7. View and download full data table")
              )
          )
      ),
      
      # Right floating sidebar (rendered dynamically)
      uiOutput("info_panel")
  ),
  
  # Table backdrop
  div(class = "table-backdrop", id = "table-backdrop"),
  
  # Bottom drawer for data table
  div(id = "bottom-drawer", class = "bottom-drawer",
      div(id = "toggle-drawer", class = "drawer-handle", "Show Data Table ▲"),
      div(class = "drawer-content",
          div(class = "table-controls",
              h3(class = "table-header", "District Data"),
              downloadButton("download_data", "Download CSV", class = "download-btn")
          ),
          DTOutput("district_table")
      )
  ),
  
  # ACLU logo footer
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

# --------------------------- HELPER FUNCTIONS --------------------------------

# Create hover labels for map polygons
create_hover_labels <- function(data, selected_districts, color_mode, data_variable) {
  lapply(1:nrow(data), function(i) {
    row <- data[i, ]
    
    # Build district names
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
    
    # Add variable value if in variable mode
    variable_display <- ""
    if (color_mode == "variable" && !is.null(data_variable)) {
      var_value <- row[[data_variable]]
      var_label <- VAR_LABELS[data_variable]
      
      # Format based on variable type
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

# --------------------------- Server --------------------------------


# Defining the server
server <- function(input, output, session) {
  
  # Clicked district
  clicked_district <- reactiveVal(NULL)
  
  #----------------------------MODALS --------------------------------
  
  # Welcome modal
  # Welcome modal
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
          tags$li("Click the map to close all sidebars and get a clear view"),
        ),
        
        h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
           "Questions"),
        p(HTML("For questions or feedback, contact: <strong>eappelson@laaclu.org</strong><br>Click the <strong>About</strong> button in the header for data sources and methodology."))
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
  
  # -----------------------------------------------------------------------
  
  # About modal
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
            tags$li(
              HTML(
                "<b>Precinct Shapefiles</b> —
       <a href='https://redist.legis.la.gov/2025%201RS/Shapefiles/2026%20Precinct%20Shapefiles%20(01-27-2026).zip' target='_blank'>
         Download ZIP
       </a> (01/27/2026)"
              )
            ),
            tags$li(
              HTML(
                "<b>Precinct Voting Data</b> —
       <a href='https://redist.legis.la.gov/2025%201RS/BlockEqu/LA_2026_01_VTD_DATA.zip' target='_blank'>
         Download ZIP
       </a> (01/27/2026)"
              )
            ),
            tags$li(
              HTML(
                "<b>Congressional Shapefiles</b> —
       <a href='https://www2.census.gov/geo/tiger/TIGER2025/CD' target='_blank'>
         Census TIGER Files
       </a> (09/22/2025)"
              )
            ),
            tags$li(
              HTML(
                "<b>State Senate Shapefiles</b> —
       <a href='https://www2.census.gov/geo/tiger/TIGER2025/SLDL/' target='_blank'>
         Census TIGER Files
       </a> (09/22/2025)"
              )
            ),
            tags$li(
              HTML(
                "<b>State House Shapefiles</b> —
       <a href='https://www2.census.gov/geo/tiger/TIGER2025/SLDU/' target='_blank'>
         Census TIGER Files
       </a> (09/22/2025)"
              )
            ),
            tags$li(
              HTML(
                "<b>Public Service Commission Shapefiles</b> —
       <a href='https://redist.legis.la.gov/2023_07/Adopted%20Plans%20From%20the%202022%201st%20Extraordinary%20Session/Public%20Service%20Commssion/Shapefiles%20and%20KML%20Files/HB2_PSC_221ES.zip' target='_blank'>
         Download ZIP
       </a> (01/01/2023)"
              )
            ),
            tags$li(
              HTML(
                "<b>Supreme Court Shapefiles</b> —
       <a href='https://redist.legis.la.gov/2024_Files/2024_RS/2024LASSCAct7/Shapefile/Act_7_-__RS_(2024)_-_LASC.zip' target='_blank'>
         Download ZIP
       </a> (05/01/2024)"
              )
            )
          )
          ,
          
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "Methodology"),
          p(strong("Data Analysis Level:"), " This analysis was conducted at both the census block level and precinct level. After comparing results from both geographic units, precinct-level data was selected as optimal for this tool."),
          p(strong("Geometric Processing:"), " All spatial data is transformed to Louisiana State Plane South (EPSG:3452) for accurate area calculations. Geometries are validated using st_make_valid() to ensure clean intersections and proper spatial operations."),
          p(strong("District Assignment Method Selection:"), " Two assignment methods were tested: centroid-based (assigning precincts based on where their geometric center falls) and area-weighted (assigning based on largest geographic overlap). Both methods were compared across all precincts and produced identical results except for a single precinct. The area-weighted method was selected as the optimal approach because it better handles precincts that span district boundaries."),
          p(strong("Area-Weighted Assignment:"), " For each precinct, spatial intersections with all district types are calculated. The intersection area between each precinct and overlapping districts is computed, and the precinct is assigned to the district with which it shares the largest geographic overlap. Another approach is to distribute precinct populations across the districts in intersects, however, we chose not to use this approach."),
          p(strong("Data Aggregation:"), " When multiple district types are selected, the tool identifies all unique intersections between those districts by grouping precincts. Demographic data and voter registration statistics are aggregated from precinct-level data to these intersection areas. Percentages are recalculated based on the aggregated totals."),
          p(strong("Visualization:"), " Final geometries are created by performing spatial unions of precinct boundaries grouped by selected district combinations, then transformed to WGS84 (EPSG:4326) for web mapping. The map offers two coloring modes: 'Distinct Colors' assigns different colors to adjacent regions for visual clarity, while 'Data Variable' uses a white-to-red gradient to show demographic or registration patterns."),
          h4(style = "font-family: 'GT-America-Compressed-Bold', sans-serif; color: #0d47a1; margin-top: 20px;", 
             "Questions"),
          p("Should you have any questions or concerns, please contact: eappelson@laaclu.org.")
      ),
      easyClose = TRUE,
      footer = tagList(
        actionButton("welcome_close", "Close", class = "modal-btn")
      )
    ))
    
    shinyjs::runjs("$('.modal-dialog').css('margin-top', '100px');")
  })
  
  #--------------------INTERSECTION CALCULATIONS------------------------
  
  # Main intersection data (non-geometric aggregation)
  intersection_data <- eventReactive(input$update_map, {
    req(length(input$districts) > 0)
    
    # Aggregate demographics across precincts
    aggregated <- precincts_data %>%
      st_drop_geometry() %>%
      group_by(across(all_of(input$districts))) %>%
      summarize(
        # Total population
        tot_pop = sum(tot_pop, na.rm = TRUE),
        tot_white = sum(tot_white, na.rm = TRUE),
        tot_black = sum(tot_black, na.rm = TRUE),
        tot_asian = sum(tot_asian, na.rm = TRUE),
        tot_amind = sum(tot_amind, na.rm = TRUE),
        tot_other = sum(tot_other, na.rm = TRUE),
        tot_hispanic = sum(tot_hispanic, na.rm = TRUE),
        
        # VAP
        vap_total = sum(vap_total, na.rm = TRUE),
        vap_white = sum(vap_white, na.rm = TRUE),
        vap_black = sum(vap_black, na.rm = TRUE),
        vap_asian = sum(vap_asian, na.rm = TRUE),
        vap_amind = sum(vap_amind, na.rm = TRUE),
        vap_other = sum(vap_other, na.rm = TRUE),
        vap_hispanic = sum(vap_hispanic, na.rm = TRUE),
        
        # Registration
        reg_total_25_12 = sum(reg_total_25_12, na.rm = TRUE),
        reg_white_25_12 = sum(reg_white_25_12, na.rm = TRUE),
        reg_black_25_12 = sum(reg_black_25_12, na.rm = TRUE),
        reg_other_25_12 = sum(reg_other_25_12, na.rm = TRUE),
        
        # Democrat registration
        reg_dem_total_25_12 = sum(reg_dem_total_25_12, na.rm = TRUE),
        reg_dem_white_25_12 = sum(reg_dem_white_25_12, na.rm = TRUE),
        reg_dem_black_25_12 = sum(reg_dem_black_25_12, na.rm = TRUE),
        reg_dem_other_25_12 = sum(reg_dem_other_25_12, na.rm = TRUE),
        
        # Republican registration
        reg_rep_total_25_12 = sum(reg_rep_total_25_12, na.rm = TRUE),
        reg_rep_white_25_12 = sum(reg_rep_white_25_12, na.rm = TRUE),
        reg_rep_black_25_12 = sum(reg_rep_black_25_12, na.rm = TRUE),
        reg_rep_other_25_12 = sum(reg_rep_other_25_12, na.rm = TRUE),
        
        # Other party registration
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
    
    # Create geometric unions
    result <- precincts_data %>%
      group_by(across(all_of(input$districts))) %>%
      summarize(geometry = st_union(geometry), .groups = "drop") %>%
      st_make_valid()
    
    # Ensure CRS
    if(is.na(st_crs(result))) {
      result <- st_set_crs(result, st_crs(precincts_data))
    }
    
    # Join with aggregated data
    result <- result %>%
      left_join(aggregated, by = input$districts)
    
    # Add color assignments
    result$color_group <- assign_map_colors(result)
    result$distinct_color <- map_colors[result$color_group]
    result$poly_id <- 1:nrow(result)
    
    # Transform to WGS84
    st_transform(result, 4326)
  }, ignoreNULL = FALSE)
  
  #-----------------------------------------------------------------------
  
  # District outline overlay data
  outline_data <- reactive({
    if (!input$show_outline) return(NULL)
    
    req(input$outline_district)
    
    outline <- precincts_data %>%
      group_by(!!sym(input$outline_district)) %>%
      summarize(geometry = st_union(geometry), .groups = "drop") %>%
      st_make_valid() %>%
      st_transform(4326)
    
    return(outline)
  })
  
  #--------------------DISTRICT SELECTION ------------------------------
  
  # Clear clicked district when map updates
  observeEvent(input$update_map, {
    clicked_district(NULL)
  })
  
  # Clear clicked district when selections change
  observeEvent(input$districts, {
    clicked_district(NULL)
  }, ignoreInit = TRUE)
  
  # Handle polygon clicks
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    data <- intersection_data()
    if (!is.null(click$id)) {
      clicked_district(data[data$poly_id == click$id, ])
    }
  })
  
  # ------------------ SEARCH FUNCTIONALITY ------------------------------
  
  # Address search
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
  
  #-----------------------------------------------------------------------
  
  # District search - UI for district number dropdown
  output$district_number_ui <- renderUI({
    req(input$district_type_search, input$district_type_search != "")
    
    district_values <- unique(precincts_data[[input$district_type_search]])
    district_values <- sort(district_values[!is.na(district_values)])
    
    selectInput("district_number_search", 
                NULL,
                choices = c("Select district..." = "", district_values),
                width = "100%")
  })
  
  # District search - handler
  observeEvent(input$search_district, {
    req(input$district_type_search, input$district_type_search != "")
    req(input$district_number_search, input$district_number_search != "")
    
    district_col <- input$district_type_search
    district_num <- input$district_number_search
    
    # Get district geometry
    district_geom <- precincts_data %>%
      filter(!!sym(district_col) == district_num) %>%
      st_union() %>%
      st_make_valid() %>%
      st_transform(4326)
    
    if (length(district_geom) > 0) {
      bbox <- st_bbox(district_geom)
      
      # Add highlight and zoom
      leafletProxy("map") %>%
        clearGroup("search_highlight") %>%
        clearMarkers() %>%
        addPolygons(
          data = district_geom,
          fillColor = "#FFD700",
          fillOpacity = 0.4,
          color = "#0d47a1",
          weight = 2,
          opacity = 1,
          group = "search_highlight",
          options = pathOptions(pane = "highlights")
        ) %>%
        fitBounds(
          lng1 = bbox$xmin, lat1 = bbox$ymin,
          lng2 = bbox$xmax, lat2 = bbox$ymax,
          options = list(padding = c(50, 50))
        )
      
      showNotification(
        paste("Found", DISTRICT_NAMES[district_col], "District", district_num), 
        type = "message",
        duration = 3
      )
      
      # Remove highlight after 6 seconds
      shinyjs::delay(6000, {
        leafletProxy("map") %>%
          clearGroup("search_highlight")
      })
    } else {
      showNotification("District not found.", type = "warning", duration = 5)
    }
  })
  
  # ------------------MAP RENDERING ----------------------------------
  
  # Color palette for data variables
  color_pal <- reactive({
    req(input$color_mode == "variable", input$data_variable)
    data <- intersection_data()
    if (input$data_variable %in% names(data)) {
      values <- data[[input$data_variable]]
      values <- values[!is.na(values) & is.finite(values)]
      if (length(values) > 0) {
        colorNumeric(
          palette = DATA_VARIABLE_PALETTE,
          domain = range(values),
          na.color = "#ccc"
        )
      } else NULL
    } else NULL
  })
  
  #-----------------------------------------------------------------------
  
  # Base map initialization
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      zoomControl = TRUE,
      minZoom = MAP_ZOOM_LIMITS$min,
      maxZoom = MAP_ZOOM_LIMITS$max
    )) %>%
      addTiles() %>%
      setView(
        lng = MAP_INITIAL_VIEW$lng,
        lat = MAP_INITIAL_VIEW$lat,
        zoom = MAP_INITIAL_VIEW$zoom
      ) %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        // Set base tiles to lowest z-index
        map.getPane('tilePane').style.zIndex = 200;
        
        // Create panes with proper z-index
        map.createPane('polygons');
        map.getPane('polygons').style.zIndex = 400;
        map.createPane('outlines');
        map.getPane('outlines').style.zIndex = 450;
        map.createPane('highlights');
        map.getPane('highlights').style.zIndex = 500;
        
        // Overlay pane for map features (CartoDB labels)
        map.createPane('mapOverlay');
        map.getPane('mapOverlay').style.zIndex = 600;
        map.getPane('mapOverlay').style.pointerEvents = 'none';
        
        // Tooltip pane above everything
        map.getPane('tooltipPane').style.zIndex = 700;
        
        // Add CartoDB labels
        L.tileLayer('https://{s}.basemaps.cartocdn.com/rastertiles/voyager_only_labels/{z}/{x}/{y}.png', {
          pane: 'mapOverlay',
          attribution: '&copy; OpenStreetMap, &copy; CartoDB'
        }).addTo(map);
      }
    ")
  })
  
  #-----------------------------------------------------------------------
  
  # Update map polygons when update button clicked
  observeEvent(input$update_map, {
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
        data = data,
        fillColor = fill_colors,
        fillOpacity = 0.7,
        color = "white",
        weight = 2.5,
        layerId = ~poly_id,
        group = "main_polygons",
        options = pathOptions(pane = "polygons"),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3, 
          color = "#0d47a1", 
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
    
    # Hide loading overlay
    session$sendCustomMessage(type = 'hide_loading', message = list())
  })
  
  #-----------------------------------------------------------------------
  
  # Update colors instantly when color mode or variable changes
  observe({
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
        data = data,
        fillColor = fill_colors,
        fillOpacity = 0.7,
        color = "white",
        weight = 2.5,
        layerId = ~poly_id,
        group = "main_polygons",
        options = pathOptions(pane = "polygons"),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 3, 
          color = "#0d47a1", 
          fillOpacity = 0.85,
          bringToFront = TRUE
        )
      )
  }) %>% bindEvent(input$color_mode, input$data_variable, ignoreNULL = FALSE)
  
  #-----------------------------------------------------------------------
  
  # Update district outlines independently
  observe({
    leafletProxy("map") %>%
      clearGroup("district_outline")
    
    if (input$show_outline) {
      outline <- outline_data()
      if (!is.null(outline)) {
        leafletProxy("map") %>%
          addPolygons(
            data = outline,
            fillColor = "transparent",
            fillOpacity = 0,
            color = "#000000",
            weight = 3,
            opacity = 1,
            group = "district_outline",
            options = pathOptions(interactive = FALSE, pane = "outlines")
          )
      }
    }
  })
  
  # ------------ SIDEBAR - INFO PANEL -----------------------

  output$info_panel <- renderUI({
    d <- clicked_district()
    if (is.null(d)) return(NULL)
    
    # Validate clicked district still exists in current intersection
    current_data <- intersection_data()
    if (is.null(current_data)) return(NULL)
    
    missing_districts <- setdiff(input$districts, names(d))
    if (length(missing_districts) > 0) {
      clicked_district(NULL)
      showNotification(
        "District selections changed. Please click 'Update Map' to see new intersections.",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }
    
    # Check if district still exists
    district_match <- current_data %>% st_drop_geometry()
    for (dist_type in input$districts) {
      district_match <- district_match %>%
        filter(!!sym(dist_type) == d[[dist_type]])
    }
    
    if (nrow(district_match) == 0) {
      clicked_district(NULL)
      showNotification(
        "District selections changed. Please click 'Update Map' to see new intersections.",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }
    
    # Build district names for display
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
                 # Key stats
                 div(class = "stat-grid",
                     div(class = "stat-card",
                         div(class = "label", "Population"),
                         div(class = "value", scales::comma(d$tot_pop))
                     ),
                     div(class = "stat-card",
                         div(class = "label", "Voting Age Population"),
                         div(class = "value", scales::comma(d$vap_total))
                     ),
                     div(class = "stat-card",
                         div(class = "label", "Registered"),
                         div(class = "value", scales::comma(d$reg_total_25_12))
                     ),
                     div(class = "stat-card",
                         div(class = "label", "% Democrat"),
                         div(class = "value", paste0(round(d$pct_reg_dem, 1), "%"))
                     )
                 ),
                 
                 # Precinct-level map
                 div(class = "plot-section",
                     h5("PRECINCT BREAKDOWN (% DEMOCRACT)"),
                     leafletOutput("precinct_map", height = "200px")
                 ),
                 
                 # Party registration bar chart
                 div(class = "plot-section",
                     h5("PARTY REGISTRATION"),
                     highchartOutput("party_plot", height = "200px")
                 ),
                 
                 # Demographics bar chart
                 div(class = "plot-section",
                     h5("REGISTRATION DEMOGRAPHICS"),
                     highchartOutput("demo_plot", height = "200px")
                 ),
                 
                 div(class = "plot-section",
                     h5("DISTRICT VS. LOUISIANA STATEWIDE"),
                     highchartOutput("comparison_plot", height = "250px")
                 )
             )
    )
  })
  
  #-----------------------------------------------------------------------
  
  # Build district names for display
  build_district_names <- function(district_data, selected_districts) {
    district_parts <- character(0)
    if ("congressional" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("Congressional: ", district_data$congressional))
    if ("senate" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("Senate: ", district_data$senate))
    if ("house" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("House: ", district_data$house))
    if ("public_service_commission" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("PSC: ", district_data$public_service_commission))
    if ("supreme_court" %in% selected_districts) 
      district_parts <- c(district_parts, paste0("Supreme Court: ", district_data$supreme_court))
    
    return(district_parts)
  }
  
  #------------------PLOTS-----------------------------------------

  output$precinct_map <- renderLeaflet({
    d <- clicked_district()
    req(d)
    
    # Filter precincts within this district intersection
    selected_precincts <- precincts_data
    
    for (dist_type in input$districts) {
      district_value <- d[[dist_type]]
      selected_precincts <- selected_precincts %>%
        filter(!!sym(dist_type) == district_value)
    }
    
    # Calculate % Democrat for each precinct
    selected_precincts <- selected_precincts %>%
      mutate(
        pct_dem = (reg_dem_total_25_12 / reg_total_25_12) * 100,
        pct_dem = ifelse(is.na(pct_dem) | !is.finite(pct_dem), 0, pct_dem),
        # Create precinct label from unit_name and county
        precinct_label = paste0(unit_name, ", ", countyname)
      ) %>%
      st_transform(4326)
    
    # Get bounds for restricting scroll
    bounds <- st_bbox(selected_precincts)
    
    # Create color palette
    pal <- colorNumeric(
      palette = c(PARTISAN_GRADIENT["low"], PARTISAN_GRADIENT["mid"], PARTISAN_GRADIENT["high"]),
      domain = c(0, 100)
    )
    
    # Create labels with precinct name
    labels <- sprintf(
      "<div style='font-family: Century-Schoolbook, serif;'>
    <b style='font-family: GT-America-Compressed-Bold, sans-serif; color: #0d47a1; font-size: 13px;'>%s</b><br/>
    <span style='font-size: 12px; color: #333;'>%% Democrat: <b style='font-family: GT-America-Compressed-Bold, sans-serif;'>%s%%</b></span><br/>
    <span style='font-size: 11px; color: #666;'>Registered: %s</span>
    </div>",
      selected_precincts$precinct_label,
      round(selected_precincts$pct_dem, 1),
      scales::comma(selected_precincts$reg_total_25_12)
    ) %>% lapply(htmltools::HTML)
    
    # Create the leaflet map
    leaflet(selected_precincts, options = leafletOptions(
      zoomControl = FALSE, 
      attributionControl = FALSE,
      dragging = TRUE,
      scrollWheelZoom = TRUE,
      doubleClickZoom = FALSE,
      touchZoom = TRUE
    )) %>%
      addPolygons(
        fillColor = ~pal(pct_dem),
        fillOpacity = 0.8,
        color = "transparent",  # No border between precincts
        weight = 2.5,
        opacity = 0,
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", 
            "padding" = "6px 10px",
            "background" = "rgba(255, 255, 255, 0.97)",
            "border" = "1px solid #ddd",
            "border-radius" = "4px",
            "box-shadow" = "0 2px 4px rgba(0,0,0,0.1)"
          ),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#0d47a1",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) %>%
      htmlwidgets::onRender(sprintf("
      function(el, x) {
        var map = this;
        
        // Set white background
        el.style.backgroundColor = 'white';
        
        // Define max bounds (expanded slightly from data bounds)
        var southWest = L.latLng(%f, %f);
        var northEast = L.latLng(%f, %f);
        var bounds = L.latLngBounds(southWest, northEast);
        
        // Set max bounds to restrict panning
        map.setMaxBounds(bounds);
        
        // Fit to show all precincts
        setTimeout(function() {
          map.fitBounds(bounds, {padding: [10, 10]});
        }, 100);
        
        // Prevent zooming out too far
        map.options.minZoom = map.getZoom() - 1;
        map.options.maxZoom = 18;
      }
    ", bounds["ymin"], bounds["xmin"], 
                                    bounds["ymax"], bounds["xmax"]))
  })
  
  #-----------------------------------------------------------------------
  output$party_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    # Create the data frame
    party_data <- data.frame(
      Party = c("Democrat", "Republican", "Other"),
      Total = c(d$reg_dem_total_25_12, d$reg_rep_total_25_12, d$reg_oth_total_25_12),
      stringsAsFactors = FALSE
    )
    
    # Filter out zero values and sort
    party_data <- party_data %>% 
      filter(Total > 0) %>%
      arrange(desc(Total))
    
    # Assign colors
    party_data$Color <- sapply(party_data$Party, function(party) {
      switch(party,
             "Democrat" = PARTY_COLORS["Democrat"],
             "Republican" = PARTY_COLORS["Republican"],
             "Other" = PARTY_COLORS["Other"],
             "#888888")
    })
    
    # Create the chart
    highchart() %>%
      hc_chart(
        type = "column",
        backgroundColor = "transparent"
      ) %>%
      hc_xAxis(
        categories = party_data$Party,
        labels = list(
          style = list(
            fontSize = "11px",
            fontFamily = "Century-Schoolbook, serif",
            color = "#333"
          )
        )
      ) %>%
      hc_yAxis(
        visible = FALSE,
        title = list(text = NULL)
      ) %>%
      hc_add_series(
        name = "Registered",
        data = party_data$Total,
        colorByPoint = TRUE,
        colors = party_data$Color,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:,.0f}",
          style = list(
            fontSize = "14px",
            fontWeight = "bold",
            fontFamily = "GT-America-Compressed-Bold, sans-serif",
            color = "#000000",
            textOutline = "none"
          ),
          y = -10
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br/>Registered: <b>{point.y:,.0f}</b>"
      ) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0.1,
          groupPadding = 0.15,
          borderWidth = 0
        )
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE)
  })
  
  output$demo_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    # Create the data frame
    demo_data <- data.frame(
      Race = c("White", "Black", "Asian", "Hispanic", "American Indian"),
      Total = c(d$tot_white, d$tot_black, d$tot_asian, d$tot_hispanic, d$tot_amind),
      stringsAsFactors = FALSE
    )
    
    # Filter out zero values and sort
    demo_data <- demo_data %>% 
      filter(Total > 0) %>%
      arrange(desc(Total))
    
    # Assign colors
    demo_data$Color <- sapply(demo_data$Race, function(race) {
      switch(race,
             "White" = DEMOGRAPHIC_COLORS["White"],
             "Black" = DEMOGRAPHIC_COLORS["Black"],
             "Asian" = DEMOGRAPHIC_COLORS["Asian"],
             "Hispanic" = DEMOGRAPHIC_COLORS["Hispanic"],
             "American Indian" = DEMOGRAPHIC_COLORS["American Indian"],
             "#888888")
    })
    
    # Create the chart
    highchart() %>%
      hc_chart(
        type = "column",
        backgroundColor = "transparent"
      ) %>%
      hc_xAxis(
        categories = demo_data$Race,
        labels = list(
          style = list(
            fontSize = "11px",
            fontFamily = "Century-Schoolbook, serif",
            color = "#333"
          )
        )
      ) %>%
      hc_yAxis(
        visible = FALSE,
        title = list(text = NULL)
      ) %>%
      hc_add_series(
        name = "Population",
        data = demo_data$Total,
        colorByPoint = TRUE,
        colors = demo_data$Color,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:,.0f}",
          style = list(
            fontSize = "14px",
            fontWeight = "bold",
            fontFamily = "GT-America-Compressed-Bold, sans-serif",
            color = "#000000",
            textOutline = "none"
          ),
          y = -10
        )
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        headerFormat = "",
        pointFormat = "<b>{point.category}</b><br/>Population: <b>{point.y:,.0f}</b>"
      ) %>%
      hc_plotOptions(
        column = list(
          pointPadding = 0.1,
          groupPadding = 0.15,
          borderWidth = 0
        )
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE)
  })
  
  output$comparison_plot <- renderHighchart({
    d <- clicked_district()
    req(d)
    
    # Calculate state totals from all precincts
    state_totals <- precincts_data %>%
      st_drop_geometry() %>%
      summarize(
        tot_pop = sum(tot_pop, na.rm = TRUE),
        tot_white = sum(tot_white, na.rm = TRUE),
        tot_black = sum(tot_black, na.rm = TRUE),
        tot_asian = sum(tot_asian, na.rm = TRUE),
        tot_hispanic = sum(tot_hispanic, na.rm = TRUE),
        tot_amind = sum(tot_amind, na.rm = TRUE),
        reg_dem_total = sum(reg_dem_total_25_12, na.rm = TRUE),
        reg_rep_total = sum(reg_rep_total_25_12, na.rm = TRUE),
        reg_oth_total = sum(reg_oth_total_25_12, na.rm = TRUE)
      ) %>%
      mutate(
        pct_white = tot_white / tot_pop * 100,
        pct_black = tot_black / tot_pop * 100,
        pct_asian = tot_asian / tot_pop * 100,
        pct_hispanic = tot_hispanic / tot_pop * 100,
        pct_amind = tot_amind / tot_pop * 100,
        pct_dem = reg_dem_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100,
        pct_rep = reg_rep_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100,
        pct_oth = reg_oth_total / (reg_dem_total + reg_rep_total + reg_oth_total) * 100
      )
    
    # Prepare comparison data
    comparison_data <- data.frame(
      Category = c("% White", "% Black", "% Asian", "% Hispanic", "% Democrat", "% Republican"),
      District = c(d$pct_white, d$pct_black, d$pct_asian, d$pct_hispanic, d$pct_reg_dem, d$pct_reg_rep),
      State = c(state_totals$pct_white, state_totals$pct_black, state_totals$pct_asian, 
                state_totals$pct_hispanic, state_totals$pct_dem, state_totals$pct_rep)
    )
    
    highchart() %>%
      hc_chart(type = "bar", backgroundColor = "transparent") %>%
      hc_xAxis(
        categories = comparison_data$Category,
        labels = list(
          style = list(
            fontSize = "11px",
            fontFamily = "Century-Schoolbook, serif",
            color = "#333"
          )
        )
      ) %>%
      hc_yAxis(
        visible = FALSE,
        title = list(text = NULL)
      ) %>%
      hc_add_series(
        name = "This District",
        data = comparison_data$District,
        color = "#0d47a1",
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%",
          style = list(
            fontSize = "11px",
            fontWeight = "bold",
            fontFamily = "GT-America-Compressed-Bold, sans-serif",
            color = "#000000",
            textOutline = "none"
          )
        )
      ) %>%
      hc_add_series(
        name = "Louisiana Statewide",
        data = comparison_data$State,
        color = "#D93A3F",
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%",
          style = list(
            fontSize = "11px",
            fontWeight = "bold",
            fontFamily = "GT-America-Compressed-Bold, sans-serif",
            color = "#000000",
            textOutline = "none"
          )
        )
      ) %>%
      hc_tooltip(
        shared = TRUE,
        useHTML = TRUE,
        headerFormat = "<b>{point.key}</b><br/>",
        pointFormat = "{series.name}: <b>{point.y:.1f}%</b><br/>"
      ) %>%
      hc_legend(
        align = "center",
        verticalAlign = "bottom",
        layout = "horizontal",
        itemStyle = list(
          fontSize = "11px",
          fontFamily = "Century-Schoolbook, serif"
        )
      ) %>%
      hc_plotOptions(
        bar = list(groupPadding = 0.1)
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE)
  })
  
  
  #----------------------DATA TABLE ---------------------------------
  
  output$district_table <- renderDT({
    data <- intersection_data() %>%
      st_drop_geometry() %>%
      select(-poly_id, -color_group, -distinct_color) %>%
      mutate(across(where(is.numeric), ~round(., 1))) %>%
      rename(
        any_of(c(
          "State Senate District" = "senate",
          "Congressional District" = "congressional",
          "House District" = "house",
          "Public Service Commission District" = "public_service_commission",
          "Supreme Court District" = "supreme_court"
        )),
        "Total Population" = "tot_pop",
        "% White" = "pct_white",
        "% Black" = "pct_black",
        "% Asian" = "pct_asian",
        "% American Indian" = "pct_amind",
        "% Other Race" = "pct_other",
        "% Hispanic" = "pct_hispanic",
        "White Population" = "tot_white",
        "Black Population" = "tot_black",
        "Asian Population" = "tot_asian",
        "American Indian Population" = "tot_amind",
        "Other Race Population" = "tot_other",
        "Hispanic Population" = "tot_hispanic",
        "VAP Total" = "vap_total",
        "VAP % White" = "vap_pct_white",
        "VAP % Black" = "vap_pct_black",
        "VAP % Asian" = "vap_pct_asian",
        "VAP % American Indian" = "vap_pct_amind",
        "VAP % Other" = "vap_pct_other",
        "VAP % Hispanic" = "vap_pct_hispanic",
        "VAP White" = "vap_white",
        "VAP Black" = "vap_black",
        "VAP Asian" = "vap_asian",
        "VAP American Indian" = "vap_amind",
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
    
    datatable(
      data, 
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
  
  #-------------------DOWNLOAD HANDLER ------------------------------

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("louisiana_districts_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      data <- intersection_data() %>%
        st_drop_geometry() %>%
        select(-poly_id, -color_group, -distinct_color) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
      
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
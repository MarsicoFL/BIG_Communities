################################################################################
#  BIG Communities Explorer – v2 (fixed header + valid skin)
#  Author: (your name)          Last update: 2025-08-04
################################################################################
suppressPackageStartupMessages({
  library(shiny);  library(shinydashboard)
  library(leaflet); library(leaflet.extras)
  library(sf);      library(geojsonio)
  library(dplyr);   library(tidyr);   library(readr)
  library(DT);      library(ggplot2); library(viridisLite)
})

# ------------------------------------------------------------------ #
# 0.  Helper to read all data -------------------------------------- #
# ------------------------------------------------------------------ #
read_data <- function(path_data){
  geo_all <- list.files(path_data, pattern = "_zip_codes_geo", full.names = TRUE) |>
    lapply(geojson_read, what = "sp") |>
    (\(x) do.call(rbind, x))() |>
    st_as_sf() |>
    mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
  
  zip_df  <- read_csv(file.path(path_data, "ag_zip.csv"),  show_col_types = FALSE) |>
    mutate(ZCTA5CE10 = sprintf("%05d", ZIP_CODE))
  
  comm_df <- read_csv(file.path(path_data, "ag_communities.csv"), show_col_types = FALSE)
  anc_df  <- read_csv(file.path(path_data, "ag_ancestries.csv"),  show_col_types = FALSE)
  pheno_df<- read_csv(file.path(path_data, "ag_pheno.csv"),      show_col_types = FALSE)
  
  list(geo = geo_all, zip = zip_df, comm = comm_df, anc = anc_df, pheno = pheno_df)
}

d <- read_data("data")

# variable helpers
env_vars   <- grep("^A[0-9]", names(d$comm), value = TRUE)
phen_vars  <- grep("prop$|Count", names(d$comm), value = TRUE)
all_numeric<- function(df) names(df)[sapply(df, is.numeric)]

# ------------------------------------------------------------------ #
# 1. UI ------------------------------------------------------------- #
# ------------------------------------------------------------------ #
header  <- dashboardHeader(title = "BIG Explorer")

sidebar <- dashboardSidebar(width = 220,
                            sidebarMenu(
                              menuItem("Map Compare", tabName = "map",  icon = icon("globe")),
                              menuItem("Phenotypes",  tabName = "pheno", icon = icon("heartbeat")),
                              menuItem("Environment", tabName = "env",   icon = icon("sun"))
                            )
)

body <- dashboardBody(
  tags$head(tags$style(HTML("
      .leaflet-label {font-weight:bold;}
      .main-sidebar {font-size:15px;}
  "))),
  tabItems(
    # 1️⃣  MAP COMPARE --------------------------------------------------
    tabItem(tabName = "map",
            fluidRow(
              box(width = 6, title = "Map A", status = "primary", solidHeader = TRUE,
                  selectInput("mapA_var", NULL, choices = all_numeric(d$zip),
                              selected = "A27_PM25_mean"),
                  checkboxInput("mapA_heat","Heat-map", FALSE),
                  sliderInput("mapA_op","Opacity",0.2,1,0.7,0.1),
                  leafletOutput("mapA", height = 400)
              ),
              box(width = 6, title = "Map B", status = "primary", solidHeader = TRUE,
                  selectInput("mapB_var", NULL, choices = all_numeric(d$zip),
                              selected = "C2_0"),
                  checkboxInput("mapB_heat","Heat-map", FALSE),
                  sliderInput("mapB_op","Opacity",0.2,1,0.7,0.1),
                  leafletOutput("mapB", height = 400)
              )
            )
    ),
    # 2️⃣  PHENOTYPES ---------------------------------------------------
    tabItem(tabName = "pheno",
            fluidRow(
              box(width = 3,
                  selectInput("cond", "Condition",
                              choices = sort(unique(d$pheno$condition)))
              ),
              box(width = 9, plotOutput("pheno_plot", height = 450))
            ),
            fluidRow(box(width = 12, DTOutput("pheno_tbl")))
    ),
    # 3️⃣  ENVIRONMENT --------------------------------------------------
    tabItem(tabName = "env",
            fluidRow(
              box(width = 3,
                  radioButtons("grp", "Aggregate by",
                               choices = c("Sub-community" = "comm",
                                           "Ancestry"       = "anc")),
                  selectInput("x_var", "Environmental variable", choices = env_vars),
                  selectInput("y_var", "Outcome / Demographic",
                              choices = phen_vars, selected = "Respiratory_prop")
              ),
              box(width = 9, plotOutput("env_plot", height = 450))
            ),
            fluidRow(box(width = 12, DTOutput("env_tbl")))
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")   # skin fixed

# ------------------------------------------------------------------ #
# 2. Server logic --------------------------------------------------- #
# ------------------------------------------------------------------ #
server <- function(input, output, session){
  
  # join polygons + zip data once
  base_sf <- inner_join(d$geo, select(d$zip, -ZIP_CODE), by = "ZCTA5CE10")
  
  # reusable map maker
  make_map <- function(map_id, var_react, op_react, heat_react){
    observe({
      var  <- var_react()
      sf_m <- mutate(base_sf, sel = .data[[var]])
      
      pal  <- colorNumeric(viridis(256), domain = sf_m$sel, na.color = "#f0f0f0")
      
      leafletProxy(map_id, data = sf_m) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor   = ~pal(sel), fillOpacity = op_react(),
          color = "grey20", weight = 0.5, opacity = 1,
          label = ~lapply(sprintf("ZIP:%s<br/>%s: %s",
                                  ZCTA5CE10, var, round(sel,4)), htmltools::HTML),
          highlightOptions = highlightOptions(weight = 1.5, bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = pal, values = sf_m$sel,
                  title = var, opacity = 0.7)
      
      # optional heat-map
      proxy <- leafletProxy(map_id); proxy |> clearHeatmap()
      if (heat_react()){
        cent <- st_centroid(sf_m)
        coords <- st_coordinates(cent)
        proxy |> addHeatmap(lng = coords[,1], lat = coords[,2],
                            intensity = sf_m$sel, blur = 15,
                            max = max(sf_m$sel, na.rm=TRUE), radius = 20)
      }
    })
  }
  
  # initialise blank maps
  output$mapA <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  output$mapB <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  
  # connect controls to maps
  make_map("mapA", reactive(input$mapA_var), reactive(input$mapA_op),
           reactive(input$mapA_heat))
  make_map("mapB", reactive(input$mapB_var), reactive(input$mapB_op),
           reactive(input$mapB_heat))
  
  # --------  Phenotypes -------------------------------------------
  output$pheno_plot <- renderPlot({
    df <- filter(d$pheno, condition == input$cond)
    ggplot(df, aes(subcommunity, prop)) +
      geom_col(fill = "#4db8ff") +
      geom_errorbar(aes(ymin=min_prop, ymax=max_prop), width=.2) +
      labs(y = "Proportion", x = NULL,
           title = paste("Prevalence of", input$cond)) +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  }, res = 96)
  
  output$pheno_tbl <- renderDT({
    filter(d$pheno, condition == input$cond) |>
      datatable(options = list(pageLength = 15), rownames = FALSE)
  })
  
  # --------  Environment ------------------------------------------
  env_data <- reactive({
    if (input$grp == "comm") d$comm else d$anc
  })
  
  output$env_plot <- renderPlot({
    df <- env_data()
    ggplot(df, aes(.data[[input$x_var]], .data[[input$y_var]])) +
      geom_point(size=3, colour="#FF7F0E") +
      geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
      labs(x = input$x_var, y = input$y_var,
           title = paste("Spearman ρ =",
                         round(cor(df[[input$x_var]], df[[input$y_var]],
                                   use="pairwise", method="spearman"),2))) +
      theme_classic(base_size = 15)
  }, res = 96)
  
  output$env_tbl <- renderDT({
    env_data() |>
      select(any_of(c(if (input$grp=="comm") "Community" else "Ancestry",
                      input$x_var, input$y_var))) |>
      datatable(options = list(pageLength = 20), rownames = FALSE)
  })
}

shinyApp(ui, server)

################################################################################
#  BIG Communities Explorer – vFINAL (2025‑08‑05)
#  Author: FM
################################################################################
suppressPackageStartupMessages({
  library(shiny);           library(shinyWidgets)
  library(shinydashboard);  library(shinydashboardPlus)
  library(leaflet);         library(leaflet.extras)
  library(sf);              library(geojsonio)
  library(dplyr);           library(tidyr);         library(readr)
  library(DT);              library(ggplot2);       library(viridisLite)
  library(plotly);          library(infotheo)
})

# ------------------------------------------------------------------ #
# 0.  Leer datos ---------------------------------------------------- #
# ------------------------------------------------------------------ #
read_data <- function(path_data){
  geo_all <- list.files(path_data, pattern = "_zip_codes_geo", full.names = TRUE) |>
    lapply(geojson_read, what = "sp") |>
    (\(x) do.call(rbind, x))() |>
    st_as_sf() |>
    mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
  
  zip_df <- read_csv(file.path(path_data, "ag_zip.csv"), show_col_types = FALSE) |>
    mutate(ZCTA5CE10 = sprintf("%05d", ZIP_CODE)) |>
    mutate(across(ends_with("_mean"), ~ .x/100))          # *_mean → proporción
  
  list(
    geo   = geo_all,
    zip   = zip_df,
    comm  = read_csv(file.path(path_data, "ag_communities.csv"), show_col_types = FALSE),
    anc   = read_csv(file.path(path_data, "ag_ancestries.csv"),  show_col_types = FALSE),
    pheno = read_csv(file.path(path_data, "ag_pheno.csv"),      show_col_types = FALSE)
  )
}
d <- read_data("data")

# ------------------------------------------------------------------ #
# Helpers ----------------------------------------------------------- #
is_num   <- function(df) names(df)[sapply(df, is.numeric)]
pretty_v <- function(v) gsub("_mean$|^A[0-9]+_|^CRD_", "", v)        # etiquetas limpias
extract_community <- function(sc) sub("(^C[0-9]+).*", "\\1", sc)

vars_zip <- setdiff(is_num(d$zip),
                    c("ZIP_CODE", grep("\\.\\.1$", names(d$zip), value = TRUE)))
env_vars_mean <- setdiff(grep("_mean$", names(d$comm), value = TRUE),
                         "Distinct_P_FIN_Count_mean")
conds_all     <- setdiff(sort(unique(d$pheno$condition)), "Symptoms")

custom_css <- "
.leaflet-label{font-weight:600;}
.main-sidebar{font-size:15px;}
.content-wrapper{background-color:#fcfcfc;}
h3,h4{font-weight:600;}
"

# ------------------------------------------------------------------ #
# 1. UI ------------------------------------------------------------- #
# ------------------------------------------------------------------ #
header <- dashboardHeader(title = span("BIG Explorer", style="font-weight:700"))

sidebar <- dashboardSidebar(
  width = 240,
  sidebarMenu(
    menuItem("BIG Map",    tabName = "bigmap", icon = icon("globe")),
    menuItem("Phenotypes", tabName = "pheno",  icon = icon("heartbeat")),
    menuItem("Environment",tabName = "env",    icon = icon("sun")),
    menuItem("More info",  tabName = "about",  icon = icon("info-circle"))
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML(custom_css))),
  tabItems(
    # ----------------------------------------------------------------
    # BIG MAP ---------------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "bigmap",
      fluidRow(
        box(width = 6, title = "Map A", status = "primary", solidHeader = TRUE,
            selectInput("mapA_var", "Variable",
                        choices = setNames(vars_zip, pretty_v(vars_zip)),
                        selected = "A27_PM25_mean"),
            sliderInput("mapA_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapA", height = 420)
        ),
        box(width = 6, title = "Map B", status = "primary", solidHeader = TRUE,
            selectInput("mapB_var", "Variable",
                        choices = setNames(vars_zip, pretty_v(vars_zip)),
                        selected = "C2_0"),
            sliderInput("mapB_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapB", height = 420)
        )
      ),
      fluidRow(
        box(width = 12, title = "ZIP‑level correlation", status = "info",
            solidHeader = TRUE,
            plotOutput("map_scatter", height = 350))
      )
    ),
    
    # ----------------------------------------------------------------
    # PHENOTYPES ------------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "pheno",
      fluidRow(
        box(width = 3,
            selectizeInput("conds", "Condition(s) (max 3)",
                           choices = conds_all, selected = conds_all[1],
                           multiple = TRUE,
                           options  = list(maxItems = 3))
        ),
        box(width = 9, plotOutput("pheno_plot", height = 480))
      ),
      fluidRow(box(width = 12, DTOutput("pheno_tbl")))
    ),
    
    # ----------------------------------------------------------------
    # ENVIRONMENT -----------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "env",
      fluidRow(
        box(width = 3,
            radioButtons("grp", "Aggregate by",
                         choices = c("Sub‑community" = "comm",
                                     "Ancestry"      = "anc")),
            conditionalPanel(
              condition = "input.grp == 'comm'",
              pickerInput("comm_sel", "Communities",
                          choices = paste0("C",1:4),
                          selected = paste0("C",1:4),
                          multiple = TRUE,
                          options = list(`actions-box`=TRUE,
                                         `style`="btn-primary"))
            ),
            selectInput("x_var", "Environmental variable (mean %)",
                        choices = setNames(env_vars_mean, pretty_v(env_vars_mean))),
            selectInput("y_cond", "Phenotype (outcome %)",
                        choices = conds_all, selected = "Respiratory")
        ),
        box(width = 9, plotlyOutput("env_plot", height = 500))
      ),
      fluidRow(box(width = 12, DTOutput("env_tbl")))
    ),
    
    # ----------------------------------------------------------------
    # ABOUT -----------------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "about",
      fluidRow(
        box(width = 10, offset = 1, status = "primary", solidHeader = TRUE,
            title = div(icon("microscope"), "More information"),
            HTML("
              <h4>Overview</h4>
              <p><em>BIG Communities Explorer</em> is an interactive dashboard that
                 integrates genomic, environmental and electronic‑health‑record data
                 at the ZIP‑code level, enabling exploration of spatial health
                 disparities and social determinants of health.</p>

              <h4>Population structure</h4>
              <ul>
                <li><strong>Communities (C1–C4)</strong> — clusters derived from shared
                    genomic‑ancestry profiles identified by hierarchical clustering.</li>
                <li><strong>Ancestries</strong> — self‑identified origins (e.g.&nbsp;African‑American,
                    European, Hispanic) harmonised across cohorts.</li>
              </ul>

              <h4>Environmental variables</h4>
              <p>Indicators are expressed as county‑level means or proportions (%).
                 Examples:</p>
              <ul>
                <li><strong>PM25</strong> — fine‑particulate matter (&micro;g/m³).</li>
                <li><strong>UNEMPLOY</strong> — unemployment rate.</li>
              </ul>

              <h4>Data sources</h4>
              <ul>
                <li>BIG Initiative, release2025&nbsp;Q2.</li>
              </ul>

              <h4>Suggested citation</h4>
              <p> Insights from the Biorepository and Integrative Genomics pediatric resource
                 <em>Nature Communications</em>. 2025;.
                 <a href='https://doi.org/10.1038/s41467-025-59375-0' target='_blank'>
                 https://doi.org/10.1038/s41467-025-59375-0</a></p>")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

# ------------------------------------------------------------------ #
# 2. Server --------------------------------------------------------- #
# ------------------------------------------------------------------ #
server <- function(input, output, session){
  
  # ---- Map helpers -------------------------------------------------
  base_sf <- inner_join(d$geo, select(d$zip, -ZIP_CODE), by="ZCTA5CE10")
  
  make_map <- function(map_id, var_react, op_react){
    observe({
      var  <- var_react()
      sf_m <- mutate(base_sf, sel = .data[[var]])
      pal  <- colorNumeric(viridis(256), domain = sf_m$sel, na.color="#f0f0f0")
      
      leafletProxy(map_id, data=sf_m) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor = ~pal(sel), fillOpacity = op_react(),
          color="grey20", weight=.5, opacity=1,
          label = ~lapply(sprintf("ZIP:%s<br/>%s: %.3f",
                                  ZCTA5CE10, pretty_v(var), sel),
                          htmltools::HTML),
          highlightOptions = highlightOptions(weight=1.5, bringToFront=TRUE)
        ) |>
        addLegend("bottomright", pal=pal, values=sf_m$sel,
                  title = pretty_v(var), opacity=.7)
    })
  }
  
  # ---- Render leaflets --------------------------------------------
  output$mapA <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng=-89.97, lat=35.08, zoom=7)
  })
  output$mapB <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng=-89.97, lat=35.08, zoom=7)
  })
  make_map("mapA", reactive(input$mapA_var), reactive(input$mapA_op))
  make_map("mapB", reactive(input$mapB_var), reactive(input$mapB_op))
  
  # ---- Scatter A vs B ---------------------------------------------
  output$map_scatter <- renderPlot({
    v1 <- d$zip[[input$mapA_var]]; v2 <- d$zip[[input$mapB_var]]
    ok <- complete.cases(v1, v2)
    rho <- round(cor(v1[ok], v2[ok], method="spearman"),3)
    
    ggplot(data.frame(A=v1[ok], B=v2[ok]), aes(A,B)) +
      geom_point(size=2.5, alpha=.8, colour="#1f77b4") +
      geom_smooth(method="lm", se=FALSE, colour="grey40") +
      labs(x=pretty_v(input$mapA_var), y=pretty_v(input$mapB_var),
           title=paste("Spearman ρ =", rho)) +
      theme_classic(base_size=15)
  }, res=96)
  
  # ---- Phenotypes --------------------------------------------------
  output$pheno_plot <- renderPlot({
    req(input$conds)
    df <- filter(d$pheno, condition %in% input$conds)
    gg <- ggplot(df, aes(subcommunity, prop, fill=condition)) +
      (if(length(input$conds)==1)
        geom_col(fill="#4db8ff")
       else
         geom_col(position=position_dodge(.8))) +
      geom_errorbar(aes(ymin=min_prop, ymax=max_prop),
                    width=.2,
                    position=if(length(input$conds)==1) "identity"
                    else position_dodge(.8)) +
      scale_fill_viridis_d(option="C", end=.8, name=NULL) +
      labs(y="Prevalence (%)", x=NULL,
           title=paste("Prevalence:", paste(input$conds, collapse=" / "))) +
      theme_minimal(base_size=15) +
      theme(axis.text.x=element_text(angle=45, hjust=1),
            legend.position=if(length(input$conds)==1) "none" else "top")
    gg
  }, res=96)
  
  output$pheno_tbl <- renderDT({
    filter(d$pheno, condition %in% input$conds) |>
      mutate(across(c(prop, min_prop, max_prop), round,2)) |>
      datatable(options=list(pageLength=17), rownames=FALSE) |>
      formatRound(columns=c("prop","min_prop","max_prop"), digits=2)
  })
  
  # ---- Environment helpers ----------------------------------------
  env_data <- reactive({
    base <- if(input$grp=="comm"){
      d$comm |> rename(subcommunity=Community) |>
        mutate(comm_group=extract_community(subcommunity)) |>
        filter(comm_group %in% input$comm_sel)
    } else {
      d$anc |> rename(subcommunity=Ancestry) |> mutate(comm_group="All")
    }
    
    left_join(base,
              d$pheno |> filter(condition==input$y_cond) |>
                select(subcommunity, pheno_prop=prop),
              by="subcommunity")
  })
  
  # ---- Interactive plotly -----------------------------------------
  output$env_plot <- renderPlotly({
    df   <- env_data(); xvar <- input$x_var
    rho  <- round(cor(df[[xvar]], df$pheno_prop,
                      use="pairwise", method="spearman"), 3)
    
    p <- ggplot(df, aes_string(x=xvar, y="pheno_prop",
                               colour=if(input$grp=="comm") "comm_group" else NULL)) +
      geom_point(size=3, alpha=.9) +
      geom_smooth(method="lm", se=FALSE, colour="grey40") +
      scale_colour_viridis_d(option="D", end=.85, name="Community") +
      labs(x=paste0(pretty_v(xvar)," (%)"),
           y=paste0(input$y_cond," (%)"),
           title=paste("Spearman ρ =", rho)) +
      theme_classic(base_size=15)
    
    ggplotly(p, tooltip=c("subcommunity", xvar, "pheno_prop")) |>
      layout(legend=list(orientation="h", x=0.1, y=-0.2))
  })
  
  output$env_tbl <- renderDT({
    env_data() |>
      select(subcommunity, any_of(c(input$x_var, "pheno_prop"))) |>
      mutate(across(where(is.numeric), round,2)) |>
      rename(`Phenotype (%)` = pheno_prop,
             !!pretty_v(input$x_var):=all_of(input$x_var)) |>
      datatable(options=list(pageLength=20), rownames=FALSE)
  })
}

shinyApp(ui, server)

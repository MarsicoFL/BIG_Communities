################################################################################
#  BIG Communities – vFINAL (2025-08-07)  – 
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
  library(tibble)
})

# ------------------------------------------------------------------ #
# 0. Read data ------------------------------------------------------ #
# ------------------------------------------------------------------ #
read_data <- function(path_data){
  geo_all <- list.files(path_data, pattern = "_zip_codes_geo", full.names = TRUE) |>
    lapply(geojson_read, what = "sp") |>
    (\(x) do.call(rbind, x))() |>
    st_as_sf() |>
    mutate(ZCTA5CE10 = as.character(ZCTA5CE10))
  
  zip_df <- read_csv(file.path(path_data, "ag_zip.csv"), show_col_types = FALSE) |>
    mutate(ZCTA5CE10 = sprintf("%05d", ZIP_CODE)) |>
    mutate(across(ends_with("_mean"), ~ .x/100))
  
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
pretty_v <- function(v) gsub("_mean$|^A[0-9]+_|^CRD_", "", v)
extract_community <- function(sc) sub("(^C[0-9]+).*", "\\1", sc)

vars_zip      <- setdiff(is_num(d$zip),
                         c("ZIP_CODE", grep("\\.\\.1$", names(d$zip), value = TRUE)))
env_vars_mean <- setdiff(grep("_mean$", names(d$comm), value = TRUE),
                         "Distinct_P_FIN_Count_mean")
conds_all     <- setdiff(sort(unique(d$pheno$condition)), "Symptoms")

# ------------------------------------------------------------------ #
# CSS --------------------------------------------------------------- #
custom_css <- "
body            {font-size:18px;}
.leaflet-label  {font-weight:600; font-size:16px;}
.main-sidebar   {font-size:22px;}
.content-wrapper{background-color:#fcfcfc;}
h3,h4           {font-weight:700; font-size:26px;}

.value-box-icon {font-size:60px!important;}
.value-box      {border-radius:1.1rem!important;}

.about-card{
  border-radius:1.2rem;
  background:linear-gradient(135deg,#f8fbff 0%,#eef5ff 100%);
  box-shadow:0 4px 12px rgba(0,0,0,.05);
}
.about-card p, .about-card ul, .about-card li, .table{font-size:18px;}

.sidebar-menu li a {            
  font-size:20px;              
}
"


# ------------------------------------------------------------------ #
# 1. UI ------------------------------------------------------------- #
# ------------------------------------------------------------------ #
header <- dashboardHeader(title = span("BIG Explorer", style = "font-weight:900"))

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("About BIG",  tabName = "about",  icon = icon("info-circle")),
    menuItem("BIG Map",    tabName = "bigmap", icon = icon("globe")),
    menuItem("Health conditions", tabName = "pheno",  icon = icon("heartbeat")),
    menuItem("Environment",tabName = "env",    icon = icon("sun"))
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML(custom_css))),
  tabItems(
    # ----------------------------------------------------------------
    # ABOUT BIG -------------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "about",
      fluidRow(
        valueBoxOutput("about_part",  width = 4),
        valueBoxOutput("about_biosp", width = 4),
        valueBoxOutput("about_seq",   width = 4)
      ),
      fluidRow(
        box(
          title   = tagList(icon("book-open"), "BIG in context"),
          status  = "info", solidHeader = TRUE, width = 12, class = "about-card",
          HTML(
            "<h4>What&nbsp;is&nbsp;BIG? 🔬</h4>
             <p>The <strong>Biorepository&nbsp;&amp;&nbsp;Integrative Genomics&nbsp;(BIG) Initiative</strong>
                links <b>&gt;42&nbsp;000</b> participants to genome data, ZIP-code environment,
                and longitudinal EHRs being of the largest paediatric genomic resources </p>

             <h4>How&nbsp;are&nbsp;communities defined? 🧬</h4>
             <p>Identity-by-descent (IBD) networks cluster individuals by long shared haplotypes
                rather than preset continental labels. Communities can be viewed as very large families of distant and close relatives.</p>
             <ul>
               <li>4 macro-communities <code>C1–C4</code>.</li>
               <li>17 sub-communities (<code>C1_1…C4_4</code>) capturing 99.7 % of sequences.</li>
               <li>Integrate ancestry <em>and</em> neighbourhood context.</li>
             </ul>

             <h4>Why&nbsp;does&nbsp;it&nbsp;matter? 🌍</h4>
             <ul>
               <li>Communities reveal uneven exposures (PM<sub>2.5</sub>, ETS, SVI).</li>
               <li>Inform public health decision making.</li>
               <li>Enable identification of shared environments and health conditions by groups of related individuals</li>
             </ul>

<h4>Key references 📑</h4>
   <ul>
     <li>
       <a href='https://www.nature.com/articles/s41467-025-59375-0'
          target='_blank' rel='noopener'>
         Buonaiuto <em>et&nbsp;al.</em> (2025) – Insights from the Biorepository and Integrative Genomics pediatric resource
       </a>
       <em>Nature Communications</em>
     </li>
     <li>
       <a href='https://www.biorxiv.org/content/10.1101/2025.05.03.652048v1'
          target='_blank' rel='noopener'>
         Marsico <em>et&nbsp;al.</em> (2025) – Identity-by-descent captures Shared Environmental Factors at Biobank Scale
       </a>
       <em>bioRxiv</em>
     </li>
   </ul>

             <p>Data access amp:
                <a href='https://uthsc.edu/cbmi/big/' target='_blank'>uthsc.edu/cbmi/big</a></p>"
          )
        )
      ),
      ## ── PLOTS + NEW VARIABLE CARD (4-4-4) ───────────────────────
      fluidRow(
        box(
          title   = tagList(icon("dna"), "Ancestry composition"),
          status  = "primary", solidHeader = TRUE, width = 4,
          plotlyOutput("about_ancestry_plot", height = 350)
        ),
        box(
          title   = tagList(icon("users"), "Community sizes"),
          status  = "primary", solidHeader = TRUE, width = 4,
          plotlyOutput("about_size_plot", height = 350)
        ),
        box(
          title   = tagList(icon("leaf"), "Key environment variables"),
          status  = "success", solidHeader = TRUE, width = 4,
          HTML(
            "<table class='table table-sm'>
               <thead><tr>
                 <th>Code</th><th>Description</th><th>Source</th><th>Year</th>
               </tr></thead>
               <tbody>
                 <tr><td><code>A27_PM25</code></td>
                     <td>PM<sub>2.5</sub> EJ Index (percentile)</td>
                     <td>EPA</td><td>2024</td></tr>
                 <tr><td><code>A12_CRD_CSMOKING</code></td>
                     <td>Current adult smokers (%)</td>
                     <td>CDC PLACES</td><td>2021</td></tr>
                 <tr><td><code>A17_UNEMPLOY</code></td>
                     <td>Unemployment rate</td>
                     <td>CDFI / Treasury</td><td>2016-20</td></tr>
                 <tr><td><code>A1_PPOV</code></td>
                     <td>People in poverty (%)</td>
                     <td>Census</td><td>2018-22</td></tr>
                 <tr><td><code>A10_CRD_OBESITY</code></td>
                     <td>Adult obesity (%)</td>
                     <td>CDC PLACES</td><td>2021</td></tr>
               </tbody>
             </table>"
          )
        )
      )
    ),
    
    # ----------------------------------------------------------------
    # BIG MAP ---------------------------------------------------------
    # ----------------------------------------------------------------
    tabItem(
      tabName = "bigmap",
      fluidRow(
        box(width = 6, title = "Map A", status = "primary", solidHeader = TRUE,
            selectInput(
              "mapA_var", "Variable",
              choices  = c("– select variable –" = "",
                           setNames(vars_zip, pretty_v(vars_zip))),
              selected = ""
            ),
            sliderInput("mapA_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapA", height = 420)
        ),
        box(width = 6, title = "Map B", status = "primary", solidHeader = TRUE,
            selectInput(
              "mapB_var", "Variable",
              choices  = c("– select variable –" = "",
                           setNames(vars_zip, pretty_v(vars_zip))),
              selected = ""
            ),
            sliderInput("mapB_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapB", height = 420)
        )
      ),
      fluidRow(
        box(width = 12, title = "ZIP-level correlation", status = "info",
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
            selectizeInput("conds", "Condition(s) (max 3)",
                           choices = conds_all, selected = conds_all[1],
                           multiple = TRUE,
                           options = list(maxItems = 3))
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
                         choices = c("Sub-community" = "comm",
                                     "Ancestry"       = "anc")),
            conditionalPanel(
              condition = "input.grp == 'comm'",
              pickerInput("comm_sel", "Communities",
                          choices = paste0("C", 1:4),
                          selected = paste0("C", 1:4),
                          multiple = TRUE,
                          options  = list(`actions-box` = TRUE,
                                          `style` = "btn-primary"))
            ),
            selectInput("x_var", "Environmental variable (mean %)",
                        choices = setNames(env_vars_mean, pretty_v(env_vars_mean))),
            selectInput("y_cond", "Phenotype (outcome %)",
                        choices = conds_all, selected = "Respiratory")
        ),
        box(width = 9, plotlyOutput("env_plot", height = 500))
      ),
      fluidRow(box(width = 12, DTOutput("env_tbl")))
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

# ------------------------------------------------------------------ #
# 2. Server (logic intact; fuentes mayores) -------------------------#
# ------------------------------------------------------------------ #
server <- function(input, output, session){
  
  # KPI tiles --------------------------------------------------------
  output$about_part <- renderValueBox(
    valueBox(HTML("42 000 +"), "Consented participants",
             icon = icon("user-friends"), color = "teal")
  )
  output$about_biosp <- renderValueBox(
    valueBox(HTML("15 000 +"), "Banked biospecimens",
             icon = icon("vial"), color = "olive")
  )
  output$about_seq <- renderValueBox(
    valueBox(HTML("13 143"), "Genome sequences",
             icon = icon("dna"), color = "purple")
  )
  
  # About plots ------------------------------------------------------
  ancestry_df <- tribble(
    ~Community, ~EUR, ~AFR, ~`EUR-AFR`, ~`EUR-AMR`, ~Multiway, ~EAS, ~Other,
    "C1", 91, 0, 0, 0, 0, 0, 9,
    "C2", 0, 49, 39, 0, 0, 0, 12,
    "C3", 0, 0, 0, 51, 43, 0, 6,
    "C4", 0, 0, 0, 0, 0, 81, 19
  ) |>
    pivot_longer(-Community, names_to = "Ancestry", values_to = "Prop")
  
  size_df <- tibble(
    Community = c("C1","C2","C3","C4"),
    Size      = c(3580, 4022, 2451, 3090)
  )
  
  output$about_ancestry_plot <- renderPlotly({
    plot_ly(ancestry_df,
            x = ~Community, y = ~Prop,
            color = ~Ancestry, colors = viridisLite::viridis(7),
            type = "bar",
            text = ~Ancestry,
            hovertemplate = "%{y}% %{text}<extra></extra>") |>
      layout(
        barmode = "stack",
        margin  = list(l = 40, r = 10, b = 40, t = 30),
        yaxis   = list(title = "Proportion (%)",
                       tickfont = list(size = 16),
                       titlefont = list(size = 18)),
        xaxis   = list(tickfont = list(size = 16)),
        legend  = list(orientation = "h", x = 0.02, y = -0.25,
                       font = list(size = 15))
      )
  })
  
  output$about_size_plot <- renderPlotly({
    plot_ly(size_df,
            x = ~Community, y = ~Size,
            type = "bar",
            hovertemplate = "%{y} individuals<extra></extra>") |>
      layout(
        margin = list(l = 40, r = 10, b = 40, t = 30),
        yaxis  = list(title = "Individuals",
                      tickfont = list(size = 16),
                      titlefont = list(size = 18)),
        xaxis  = list(tickfont = list(size = 16)),
        showlegend = FALSE
      )
  })
  
  # Map helpers ------------------------------------------------------
  base_sf <- inner_join(d$geo, select(d$zip, -ZIP_CODE), by = "ZCTA5CE10")
  
  make_map <- function(map_id, var_react, op_react){
    observe({
      var <- var_react()
      if (is.null(var) || var == ""){
        leafletProxy(map_id) |> clearShapes() |> clearControls()
        return()
      }
      sf_m <- mutate(base_sf, sel = .data[[var]])
      pal  <- colorNumeric(viridis(256), domain = sf_m$sel, na.color = "#f0f0f0")
      
      leafletProxy(map_id, data = sf_m) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor = ~pal(sel), fillOpacity = op_react(),
          color     = "grey20", weight = .5, opacity = 1,
          label     = ~lapply(
            sprintf("ZIP: %s<br/>%s: %.3f",
                    ZCTA5CE10, pretty_v(var), sel),
            htmltools::HTML),
          highlightOptions = highlightOptions(weight = 1.5, bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = pal, values = sf_m$sel,
                  title = pretty_v(var), opacity = .7)
    })
  }
  
  output$mapA <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  output$mapB <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  make_map("mapA", reactive(input$mapA_var), reactive(input$mapA_op))
  make_map("mapB", reactive(input$mapB_var), reactive(input$mapB_op))
  
  # Scatter A vs B ---------------------------------------------------
  output$map_scatter <- renderPlot({
    if (input$mapA_var == "" || input$mapB_var == "") return()
    
    v1  <- d$zip[[input$mapA_var]]
    v2  <- d$zip[[input$mapB_var]]
    ok  <- complete.cases(v1, v2)
    rho <- round(cor(v1[ok], v2[ok], method = "spearman"), 3)
    
    ggplot(data.frame(A = v1[ok], B = v2[ok]), aes(A, B)) +
      geom_point(size = 3, alpha = .8, colour = "#1f77b4") +
      geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
      labs(x = pretty_v(input$mapA_var), y = pretty_v(input$mapB_var),
           title = paste("Spearman ρ =", rho)) +
      theme_classic(base_size = 20)
  }, res = 96)
  
  # Phenotypes -------------------------------------------------------
  output$pheno_plot <- renderPlot({
    req(input$conds)
    df <- filter(d$pheno, condition %in% input$conds)
    
    gg <- ggplot(df, aes(subcommunity, prop, fill = condition)) +
      (if (length(input$conds) == 1)
        geom_col(fill = "#4db8ff")
       else
         geom_col(position = position_dodge(.8))) +
      geom_errorbar(aes(ymin = min_prop, ymax = max_prop),
                    width = .2,
                    position = if (length(input$conds) == 1)
                      "identity"
                    else
                      position_dodge(.8)) +
      scale_fill_viridis_d(option = "C", end = .8, name = NULL) +
      labs(y = "Prevalence (%)", x = NULL,
           title = paste("Prevalence:", paste(input$conds, collapse = " / "))) +
      theme_minimal(base_size = 20) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = if (length(input$conds) == 1) "none" else "top")
    gg
  }, res = 96)
  
  output$pheno_tbl <- renderDT({
    filter(d$pheno, condition %in% input$conds) |>
      mutate(across(c(prop, min_prop, max_prop), round, 2)) |>
      datatable(options = list(pageLength = 17), rownames = FALSE) |>
      formatRound(columns = c("prop", "min_prop", "max_prop"), digits = 2)
  })
  
  # Environment ------------------------------------------------------
  env_data <- reactive({
    base <- if (input$grp == "comm"){
      d$comm |> rename(subcommunity = Community) |>
        mutate(comm_group = extract_community(subcommunity)) |>
        filter(comm_group %in% input$comm_sel)
    } else {
      d$anc |> rename(subcommunity = Ancestry) |> mutate(comm_group = "All")
    }
    
    left_join(
      base,
      d$pheno |> filter(condition == input$y_cond) |>
        select(subcommunity, pheno_prop = prop),
      by = "subcommunity"
    )
  })
  
  output$env_plot <- renderPlotly({
    df   <- env_data()
    xvar <- input$x_var
    rho  <- round(cor(df[[xvar]], df$pheno_prop,
                      use = "pairwise", method = "spearman"), 3)
    
    p <- ggplot(
      df,
      aes_string(x = xvar, y = "pheno_prop",
                 colour = if (input$grp == "comm") "comm_group" else NULL)
    ) +
      geom_point(size = 4, alpha = .9) +
      geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
      scale_colour_viridis_d(option = "D", end = .85, name = "Community") +
      labs(x = paste0(pretty_v(xvar), " (%)"),
           y = paste0(input$y_cond, " (%)"),
           title = paste("Spearman ρ =", rho)) +
      theme_classic(base_size = 20)
    
    ggplotly(p, tooltip = "subcommunity") |>
      layout(
        legend = list(orientation = "h", x = 0.1, y = -0.2,
                      font = list(size = 15))
      )
  })
  
  output$env_tbl <- renderDT({
    env_data() |>
      select(subcommunity, any_of(c(input$x_var, "pheno_prop"))) |>
      mutate(across(where(is.numeric), round, 2)) |>
      rename(`Phenotype (%)` = pheno_prop,
             !!pretty_v(input$x_var) := all_of(input$x_var)) |>
      datatable(options = list(pageLength = 20), rownames = FALSE)
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
#shinyApp(ui, server)

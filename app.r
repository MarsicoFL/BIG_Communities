
suppressPackageStartupMessages({
  library(shiny);           library(shinyWidgets)
  library(shinydashboard);  library(shinydashboardPlus)
  library(leaflet);         library(leaflet.extras)
  library(sf);              library(geojsonio)
  library(dplyr);           library(tidyr);         library(readr)
  library(DT);              library(ggplot2);       library(viridisLite)
  library(plotly);          library(tibble);        library(forcats); library(stringr); library(RColorBrewer) 
})

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

is_num   <- function(df) names(df)[sapply(df, is.numeric)]
pretty_v <- function(v) {v <- gsub("_mean$|^A[0-9]+_|^CRD_", "", v); v <- gsub("_", " ", v); tools::toTitleCase(v)}
extract_community <- function(sc) sub("(^C[0-9]+).*", "\\1", sc)

vars_zip      <- setdiff(is_num(d$zip), c("ZIP_CODE", grep("\\.\\.1$", names(d$zip), value = TRUE)))
env_vars_mean <- setdiff(grep("_mean$", names(d$comm), value = TRUE), "Distinct_P_FIN_Count_mean")
conds_all     <- setdiff(sort(unique(d$pheno$condition)), "Symptoms")

custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;800&display=swap');
:root{--ink:#0e1726;--muted:#5b6b82;--bg:#fbfcfe;--card:#ffffff;--accent:#315efb;--accent-2:#2bb673;--stroke:#e8edf5;}
html,body{font-family:'Inter',system-ui,-apple-system,Segoe UI,Roboto,Arial,sans-serif;}
body{font-size:18px;color:var(--ink);background:var(--bg);}
.content-wrapper{background-color:var(--bg)!important;}
.main-header .logo,.main-header .navbar{background:#0b0f19!important;}
.skin-black .main-header .logo,.skin-black .main-header .logo:hover{color:#f2f6ff!important;background:#0b0f19!important;}
.skin-black .main-header .navbar .sidebar-toggle{color:#f2f6ff!important;}
.sidebar-mini.sidebar-collapse .main-header .logo{width:220px;}
.main-sidebar{background:#101524;}
.sidebar-menu>li>a{font-size:18px;}
.skin-black .sidebar a{color:#dbe6ff;}
.skin-black .sidebar-menu>li.active>a,.skin-black .sidebar-menu>li:hover>a{background:#16203a;color:#fff;}
h3,h4{font-weight:800;letter-spacing:.2px;}
.small-note{color:var(--muted);font-size:14px;}
.hr-faint{border:0;height:1px;background:var(--stroke);margin:10px 0 20px;}
.value-box{border-radius:1.1rem!important;box-shadow:0 8px 24px rgba(0,0,0,.08);}
.value-box .value{font-size:34px;font-weight:800;}
.value-box .caption{font-size:15px;color:var(--muted);}
.value-box-icon{font-size:60px!important;background:rgba(0,0,0,0.05)!important;}
.box{border-radius:1.2rem!important;border:1px solid var(--stroke)!important;}
.box.box-solid>.box-header{border-radius:1.2rem 1.2rem 0 0;}
.box-primary{border-color:#dae3ff!important;}
.box-success{border-color:#d9f2e5!important;}
.box-info{border-color:#e9eef6!important;}
.about-card{border-radius:1.2rem;background:linear-gradient(135deg,#f3f7ff 0%,#eef5ff 100%);box-shadow:0 10px 24px rgba(21,34,72,.08);}
.about-card p,.about-card ul,.about-card li,.table{font-size:18px;}
.leaflet-container{border-radius:1.0rem;border:1px solid var(--stroke);}
.leaflet-label{font-weight:600;font-size:16px;}
.table>thead>tr>th{border-bottom:1px solid var(--stroke);}
.dataTables_wrapper .dataTables_length,.dataTables_wrapper .dataTables_filter{color:var(--muted);}
.badge-soft{display:inline-block;padding:.3rem .6rem;border-radius:999px;font-weight:600;border:1px solid var(--stroke);background:#fff;color:var(--muted);}
.kpi-note{font-size:14px;color:var(--muted);margin-top:6px;}
"

header <- dashboardHeader(title = span("BIG Communities Explorer", style = "font-weight:900"))
sidebar <- dashboardSidebar(
  width = 260,
  sidebarMenu(
    menuItem("About BIG",         tabName = "about",  icon = icon("info-circle")),
    menuItem("BIG Map",           tabName = "bigmap", icon = icon("globe")),
    menuItem("Health Conditions", tabName = "pheno",  icon = icon("heartbeat")),
    menuItem("Environment",       tabName = "env",    icon = icon("sun"))
  )
)
body <- dashboardBody(
  tags$head(tags$style(HTML(custom_css))),
  tabItems(
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
            "<h4>What is BIG? </h4>
             <p>The <strong>Biorepository &amp; Integrative Genomics (BIG) Initiative</strong>
                links <b>&gt;42,000</b> participants to longitudinal EHRs, genome data, environmental measures by ZIP-code,
                , creating one of the largest pediatric genomic resources.</p>
             <h4>How are communities defined? 🧬</h4>
             <p>Identity-by-descent (IBD) networks cluster individuals by long shared haplotypes
                instead of preset continental labels. Communities act like very large families of distant
                and close relatives.</p>
             <ul>
               <li>4 macro-communities <code>C1–C4</code>.</li>
               <li>17 sub-communities (<code>C1a…C4b</code>).</li>
               <li>Integration of ancestry <em>and</em> neighborhood context.</li>
             </ul>
             <h4>Why does it matter? </h4>
             <ul>
               <li>Communities reveal shared environmental exposures linked to health conditions (PM<sub>2.5</sub>, SVI).</li>
               <li>Inform public health decision making.</li>
               <li>Enable identification of shared environments and health conditions across related groups.</li>
             </ul>
             <h4>Key references 📑</h4>
             <ul>
               <li><a href='https://www.nature.com/articles/s41467-025-59375-0' target='_blank' rel='noopener'>Nature Communications (2025) – Insights from the BIG pediatric resource</a>.</li>
               <li><a href='https://www.biorxiv.org/content/10.1101/2025.05.03.652048v1' target='_blank' rel='noopener'>bioRxiv (2025) – IBD-based communities capture shared environmental factors at biobank scale</a>.</li>
             </ul>
             <p>Learn more:
                <a href='https://uthsc.edu/cbmi/big/' target='_blank' rel='noopener'>uthsc.edu/cbmi/big</a></p>"
          )
        )
      ),
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
                     <td>CDFI / Treasury</td><td>2016–20</td></tr>
                 <tr><td><code>A1_PPOV</code></td>
                     <td>People in poverty (%)</td>
                     <td>Census</td><td>2018–22</td></tr>
                 <tr><td><code>A10_CRD_OBESITY</code></td>
                     <td>Adult obesity (%)</td>
                     <td>CDC PLACES</td><td>2021</td></tr>
               </tbody>
             </table>"
          )
        )
      )
    ),
    tabItem(
      tabName = "bigmap",
      fluidRow(
        box(width = 6, title = "Map A", status = "primary", solidHeader = TRUE,
            selectInput("mapA_var", "Variable",
                        choices  = c("– select variable –" = "", setNames(vars_zip, pretty_v(vars_zip))), selected = ""),
            sliderInput("mapA_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapA", height = 420),
            div(class="small-note", "Colors represent (i) ancestry or subcommunity proportion at the Zip code level, (ii) PM2.5 percentile, and (iii) prevalence or proportion of other environmental variables. Only Zip codes with more than 100 individuals are shown.")
        ),
        box(width = 6, title = "Map B", status = "primary", solidHeader = TRUE,
            selectInput("mapB_var", "Variable",
                        choices  = c("– select variable –" = "", setNames(vars_zip, pretty_v(vars_zip))), selected = ""),
            sliderInput("mapB_op", "Opacity", .2, 1, .7, .05),
            leafletOutput("mapB", height = 420))
      ),
      fluidRow(
        box(width = 12, title = "ZIP-level correlation", status = "info",
            solidHeader = TRUE,
            plotOutput("map_scatter", height = 350),
            div(class="small-note", "Each dot correspond to a Zip-code. Zip-code correlation is Spearman’s ρ with linear trend for orientation.")
        )
      )
    ),
    tabItem(
      tabName = "pheno",
      fluidRow(
        box(width = 3,
            selectizeInput("conds", "Condition(s) (max 4)",
                           choices = conds_all, selected = conds_all[1],
                           multiple = TRUE, options = list(maxItems = 4)),
            radioButtons("pheno_level", "Aggregate by",
                         choices = c("Sub-community" = "sub", "Community (C1–C4)" = "com"),
                         selected = "sub")
        ),
        box(width = 9, 
            title = tagList(icon("chart-bar"), "Health condition prevalence (proportion of individuals who reported the condition at least in one visit)"),
            status = "primary", solidHeader = TRUE,
            plotOutput("pheno_plot", height = 480),
            div(class="small-note", "Bars show prevalence (%); whiskers show uncertainty range reported in the dataset.")
        )
      ),
      fluidRow(
        box(width = 6, title = tagList(icon("ranking-star"), "Top sub-communities"),
            status = "success", solidHeader = TRUE,
            DTOutput("pheno_top_tbl"),
            div(class="small-note", "Top 10 by prevalence for the selected condition(s).")),
        box(width = 6, title = tagList(icon("braille"), "Co-variation heatmap"),
            status = "success", solidHeader = TRUE,
            plotOutput("pheno_heat", height = 420),
            div(class="small-note", "Spearman’s ρ."))
      )
    ),
    tabItem(
      tabName = "env",
      fluidRow(
        box(width = 3,
            div(class="badge-soft", "Communities only"),
            pickerInput("comm_sel", "Communities to show",
                        choices = paste0("C", 1:4),
                        selected = paste0("C", 1:4),
                        multiple = TRUE,
                        options  = list(`actions-box` = TRUE, `style` = "btn-primary")),
            selectInput("x_var", "Environmental variable (%)",
                        choices = setNames(env_vars_mean, pretty_v(env_vars_mean))),
            selectInput("y_cond", "Health condition (%)",
                        choices = conds_all, selected = "Respiratory")
        ),
        box(width = 9, 
            title = tagList(icon("line-chart"), "Environment vs phenotype"),
            status = "primary", solidHeader = TRUE,
            plotlyOutput("env_plot", height = 520),
            div(class="small-note", "Points are sub-communities. Line is linear fit for visual guidance. Title shows Spearman’s ρ.")
        )
      ),
      fluidRow(box(width = 12, title = "Data", status = "info", solidHeader = TRUE, DTOutput("env_tbl")))
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "black")

server <- function(input, output, session){
  kpi_card <- function(value, caption, icon_name, color){
    valueBox(HTML(value), caption, icon = icon(icon_name), color = color, width = 4)
  }
  output$about_part <- renderValueBox(kpi_card("42,000+", "Consented participants", "user-friends", "teal"))
  output$about_biosp <- renderValueBox(kpi_card("15,000+", "Banked biospecimens", "vial", "olive"))
  output$about_seq  <- renderValueBox(kpi_card("13,143",  "Genome sequences",      "dna",  "purple"))
  
  ancestry_df <- tribble(
    ~Community, ~EUR, ~AFR, ~`EUR-AFR`, ~`EUR-AMR`, ~Multiway, ~EAS,
    "C1", 93, 0, 4, 2, 1, 0,
    "C2", 0, 56, 38, 0, 6, 0,
    "C3", 1, 1, 2, 51, 44, 1,
    "C4", 5, 0, 0, 0, 0, 95
  ) |>
    pivot_longer(-Community, names_to = "Ancestry", values_to = "Prop")
  size_df <- tibble(Community = c("C1","C2","C3","C4"), Size = c(7561, 6405, 1695, 1092))
  
  ghibli_cols <- c("#1F77B4FF" , "#17BECFFF", "#FF7F0EFF", "#D62728FF", "#009E73",  "#8C564BFF")
  

  output$about_ancestry_plot <- renderPlotly({
    plot_ly(ancestry_df,
            x = ~Community, y = ~Prop,
            color = ~Ancestry, colors = ghibli_cols,
            type = "bar") |>
      layout(barmode = "stack",
             margin  = list(l = 40, r = 10, b = 40, t = 30),
             yaxis   = list(title = "Proportion (%)", tickfont = list(size = 16), titlefont = list(size = 18)),
             xaxis   = list(tickfont = list(size = 16)),
             legend  = list(orientation = "h", x = 0.02, y = -0.25, font = list(size = 15)))
  })
  
  output$about_size_plot <- renderPlotly({
    plot_ly(size_df, x = ~Community, y = ~Size, type = "bar",
            hovertemplate = "%{y} individuals<extra></extra>") |>
      layout(margin = list(l = 40, r = 10, b = 40, t = 30),
             yaxis  = list(title = "Individuals", tickfont = list(size = 16), titlefont = list(size = 18)),
             xaxis  = list(tickfont = list(size = 16)), showlegend = FALSE)
  })
  
  base_sf <- inner_join(d$geo, select(d$zip, -ZIP_CODE), by = "ZCTA5CE10")
  make_map <- function(map_id, var_react, op_react, pal_name){
    observe({
      var <- var_react()
      if (is.null(var) || var == ""){
        leafletProxy(map_id) |> clearShapes() |> clearControls()
        return()
      }
      sf_m <- mutate(base_sf, sel = .data[[var]])
      pal_fun <- colorNumeric(RColorBrewer::brewer.pal(9, pal_name),
                              domain = sf_m$sel, na.color = "#f0f0f0")
      
      leafletProxy(map_id, data = sf_m) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor = ~pal_fun(sel), fillOpacity = op_react(),
          color = "grey20", weight = .5, opacity = 1,
          label = ~lapply(sprintf("ZIP: %s<br/>%s: %.3f", ZCTA5CE10, pretty_v(var), sel), htmltools::HTML),
          highlightOptions = highlightOptions(weight = 1.5, bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = pal_fun, values = sf_m$sel,
                  title = pretty_v(var), opacity = .7)
    })
  }
  output$mapA <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |> addFullscreenControl() |> addResetMapButton() |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  output$mapB <- renderLeaflet({
    leaflet() |> addProviderTiles("CartoDB.Positron") |> addFullscreenControl() |> addResetMapButton() |>
      setView(lng = -89.97, lat = 35.08, zoom = 7)
  })
  make_map("mapA", reactive(input$mapA_var), reactive(input$mapA_op), "Reds")
  make_map("mapB", reactive(input$mapB_var), reactive(input$mapB_op), "Blues")
  output$map_scatter <- renderPlot({
    if (input$mapA_var == "" || input$mapB_var == "") return()
    v1  <- d$zip[[input$mapA_var]]
    v2  <- d$zip[[input$mapB_var]]
    ok  <- complete.cases(v1, v2)
    rho <- suppressWarnings(cor(v1[ok], v2[ok], method = "spearman"))
    rho <- round(rho, 3)
    ggplot(data.frame(A = v1[ok], B = v2[ok]), aes(A, B)) +
      geom_point(size = 3, alpha = .8, colour = "#315efb") +
      geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
      labs(x = pretty_v(input$mapA_var), y = pretty_v(input$mapB_var), title = paste("Spearman ρ =", rho)) +
      theme_classic(base_size = 20)
  }, res = 96)
  
  pheno_base <- reactive({req(input$conds); d$pheno |> filter(condition %in% input$conds)})
  pheno_agg <- reactive({
    df <- pheno_base()
    if (input$pheno_level == "com"){
      df |>
        mutate(Community = extract_community(subcommunity)) |>
        group_by(Community, condition) |>
        summarise(
          prop = mean(prop, na.rm=TRUE),
          min_prop = mean(min_prop, na.rm=TRUE),
          max_prop = mean(max_prop, na.rm=TRUE),
          .groups = "drop"
        ) |>
        rename(group = Community)
    } else {
      df |> rename(group = subcommunity)
    }
  })
  
  output$pheno_plot <- renderPlot({
    df <- pheno_agg()
    order_by <- df |> filter(condition == first(input$conds)) |> arrange(desc(prop)) |> pull(group)
    df$group <- factor(df$group, levels = order_by)
    ggplot(df, aes(group, prop, fill = condition)) +
      (if (length(input$conds) == 1) geom_col(fill = "#2bb673", width = .75) else geom_col(position = position_dodge(.8), width = .75)) +
      geom_errorbar(aes(ymin = min_prop, ymax = max_prop),
                    width = .2,
                    position = if (length(input$conds) == 1) position_identity() else position_dodge(.8)) +
      scale_fill_viridis_d(option = "C", end = .85, name = NULL) +
      labs(y = "Prevalence (%)", x = NULL, title = paste0("Prevalence: ", paste(input$conds, collapse = " / "))) +
      theme_minimal(base_size = 19) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = if (length(input$conds) == 1) "none" else "top")
  }, res = 96)
  
  output$pheno_top_tbl <- renderDT({
    pheno_agg() |>
      group_by(condition) |>
      arrange(desc(prop), .by_group = TRUE) |>
      slice_head(n = 10) |>
      ungroup() |>
      mutate(across(c(prop, min_prop, max_prop), ~round(.x, 2))) |>
      rename(Group = group, `Phenotype (%)` = prop, `Min` = min_prop, `Max` = max_prop) |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$pheno_heat <- renderPlot({
    req(length(input$conds) >= 2)
    df <- pheno_agg() |> select(group, condition, prop) |> pivot_wider(names_from = condition, values_from = prop)
    mat <- suppressWarnings(cor(df |> select(-group), use = "pairwise", method = "spearman"))
    ord <- colnames(mat)
    mat_df <- as.data.frame(as.table(mat))
    mat_df$Var1 <- factor(mat_df$Var1, levels = ord)
    mat_df$Var2 <- factor(mat_df$Var2, levels = ord)
    mat_df <- mat_df |> filter(as.numeric(Var1) <= as.numeric(Var2))
    ggplot(mat_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "#E9F2FF", high = "#1F5FBF") +
      geom_text(aes(label = sprintf("%.2f", Freq)), color = "black", size = 6) +
      labs(x = NULL, y = NULL, fill = "ρ", title = "Co-variation") +
      theme_minimal(base_size = 18) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }, res = 96)
  
  env_data <- reactive({
    d$comm |>
      rename(subcommunity = Community) |>
      mutate(comm_group = extract_community(subcommunity)) |>
      filter(comm_group %in% input$comm_sel) |>
      left_join(d$pheno |> filter(condition == input$y_cond) |> select(subcommunity, pheno_prop = prop), by = "subcommunity")
  })
  output$env_plot <- renderPlotly({
    df   <- env_data()
    xvar <- input$x_var
    rho  <- suppressWarnings(cor(df[[xvar]], df$pheno_prop, use = "pairwise", method = "spearman"))
    rho  <- round(rho, 3)
    p <- ggplot(df, aes_string(x = xvar, y = "pheno_prop", colour = "comm_group")) +
      geom_point(size = 4, alpha = .9) +
      geom_smooth(method = "lm", se = FALSE, colour = "grey40") +
      scale_colour_viridis_d(option = "D", end = .85, name = "Community") +
      labs(x = paste0(pretty_v(xvar), " (%)"), y = paste0(input$y_cond, " (%)"), title = paste("Spearman ρ =", rho)) +
      theme_classic(base_size = 20)
    ggplotly(p, tooltip = "subcommunity") |>
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2, font = list(size = 15)))
  })
  output$env_tbl <- renderDT({
    env_data() |>
      select(subcommunity, any_of(c(input$x_var, "pheno_prop"))) |>
      mutate(across(where(is.numeric), round, 2)) |>
      rename(`Phenotype (%)` = pheno_prop, !!pretty_v(input$x_var) := all_of(input$x_var)) |>
      datatable(options = list(pageLength = 20), rownames = FALSE)
  })
}
#shinyApp(ui, server, options = list(launch.browser = TRUE))
shinyApp(ui, server)

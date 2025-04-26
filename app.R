# Header ----------------------------------------------------------------
# Project: app
# File name: app.R
# Last updated: 2025-04-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/app

# Load libraries --------------------------------------------------------
library(shiny)
library(ggplot2)
library(ggiraph)

# Load functions --------------------------------------------------------
source("utils.R")

# Load data -------------------------------------------------------------
df <- readRDS("PBDB.RDS")
bins <- readRDS("stages.RDS")

# Input options ---------------------------------------------------------
families <- sort(unique(df$family))
regions <- sort(unique(df$region))
groups <- c(None = ".", Family = "family", Genus = "genus", Country = "cc")

# Plot defaults ---------------------------------------------------------
# Labs
xlab <- c("Time (Ma)")
# Theme
custom_theme <- theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(),
        axis.line.x = element_line(),
        panel.grid = element_blank())

# UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$style(HTML("
    #plot {
      height: 100vh !important; /* vh = viewport height */
    }
  ")),
  tags$head(tags$style(HTML('* {font-family: "Arial"};'))),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,

      # Input: Analyses type ----
      radioButtons("type", "Type",
                   c(Occurrences = "occurrences", Collections = "collections", 
                     Range = "range", 
                     Richness = "richness"),
                   selected = "range"),
      # Input: Taxonomic rank ----
      radioButtons("rank", "Taxonomic rank",
                   c(Species = "species", Genus = "genus", Family = "family"), 
                   selected = "genus"),
      # Input: Group by ----
      selectInput("group", "Group by",
                  c(groups)),
      # Input: Select region ----
      selectInput("region", "Geographic region",
                  c(regions),
                  selected = "Caribbean"),
      # Input: Select family ----
      selectInput("family", "Family",
                  c(All = ".", families),
                  selected = "All"),
      # Input: Plot parameters ----
      sliderInput("point", "Point size",
                  min = 0.1, max = 5,
                  value = 1),
      sliderInput("line", "Line size",
                  min = 0.1, max = 1.5,
                  value = 0.5),
      sliderInput("label", "Label size",
                  min = 0.1, max = 5,
                  value = 1),
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 10,

      # Output: Plot ----
      girafeOutput("plot")

    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output) {
  # Reactive: Data filtering and analyses ----
  data <- reactive({
    tmp <- df |>
      filter_region(region = input$region) |>
      filter_rank(rank = input$rank) |>
      filter_family(fam = input$family) |>
      group_data(group = input$group)
    # Analyses
    tmp <- lapply(names(tmp), function(x) {
      out <- tmp[[x]]
      if (input$type == "range") {
        out <- get_temporal_ranges(df = out, rank = input$rank)
      } else if (input$type == "occurrences") {
        out <- get_occurrence_counts(df = out)
      } else if (input$type == "collections") {
        out <- get_collection_counts(df = out)
      } else if (input$type == "richness") {
        out <- get_richness_counts(df = out, rank = input$rank)
      }
      out$group_id <- x
      out
    })
    do.call(rbind.data.frame, tmp)
  })
  # Reactive: Plot rendering ----  
  output$plot <- renderGirafe({
    out <- data()
    if (input$type == "range") {
      out <- out[order(out$max_ma, decreasing = FALSE), ]
      out$taxon_id <- 1:nrow(out)
      out$taxon <- factor(x = out$taxon, levels = out$taxon)
      p <- ggplot(data = out, aes(y = taxon, xmin = min_ma, xmax = max_ma,
                                  data_id = taxon, tooltip = taxon)) +
        geom_linerange_interactive(size = input$line) +
        geom_point_interactive(aes(y = taxon, x = max_ma), size = input$point,
                               pch = 20) +
        geom_point_interactive(aes(y = taxon, x = min_ma), size = input$point,
                               pch = 20) +
        geom_text_interactive(aes(y = taxon, x = max_ma + 1, label = taxon),
                              size = input$label, hjust = 1, check_overlap = FALSE) +
        scale_x_reverse(name = xlab, limits = c(70, 0)) +
        scale_y_discrete() +
        facet_wrap(~group_id, scales = "free_y") +
        theme_bw(base_size = 6) +
        custom_theme + 
        theme(strip.text = element_text(size = 5))
    } else {
      p <- ggplot(data = data(), aes(x = mid_ma, y = value)) +
        geom_point() +
        facet_wrap(~group_id) +
        custom_theme
    }
    girafe(ggobj = p, options = list(opts_sizing(rescale = TRUE),
                                     opts_zoom(max = 7),
                                     opts_toolbar(saveaspng = FALSE,
                                                  pngname = "plot")))
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)

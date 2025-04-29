# Header ----------------------------------------------------------------
# Project: app
# File name: app.R
# Last updated: 2025-04-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/app

# Load libraries --------------------------------------------------------
library(shiny)
library(vegan)

# Load functions --------------------------------------------------------
source("R/utils.R")

# Load data -------------------------------------------------------------
df <- readRDS("data/PBDB.RDS")
bins <- readRDS("data/stages.RDS")

# Input options ---------------------------------------------------------
families <- sort(unique(df$family))
regions <- sort(unique(df$region))
groups <- c(Ungrouped = ".", Family = "family", Genus = "genus", Country = "cc")

# Plot defaults ---------------------------------------------------------
# Labs
xlab <- c("Time (Ma)")

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
    sidebarPanel(width = 3,

      # Input: Analyses type ----
      h3("Analyses"),
      radioButtons("type", "Select a type of analyses",
                   c("Temporal ranges" = "range",
                     "Number of occurrences" = "occurrences", 
                     "Number of collections" = "collections", 
                     "Number of taxa" = "taxa",
                     "Accummulation curve" = "accum"),
                   selected = "range"),
      # Input: Taxonomic rank ----
      selectInput("rank", "Select a taxonomic rank",
                   c(Species = "species", Genus = "genus", Family = "family"), 
                   selected = "genus"),
      # Input: Select region ----
      selectInput("region", "Select a geographic region",
                  c(regions),
                  selected = "Caribbean"),
      # Input: Select family ----
      selectInput("family", "Select a taxonomic family",
                  c(All = ".", families),
                  selected = "All"),
      # Input: Group by ----
      selectInput("group", "Select a grouping variable",
                  c(groups)),
      h3("Plotting"),
      # Input: Plot parameters ----
      sliderInput("line", "Select a line size",
                  min = 0.1, max = 5,
                  value = 1.25),
      sliderInput("label", "Select a label size",
                  min = 0.1, max = 2,
                  value = 1)
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 9,
              plotOutput("plot")

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
        out <- out
      } else if (input$type == "occurrences") {
        out <- get_occurrence_counts(df = out)
      } else if (input$type == "collections") {
        out <- get_collection_counts(df = out)
      } else if (input$type == "taxa") {
        out <- get_richness_counts(df = out, rank = input$rank)
      } else if (input$type == "accum") {
        out <- out
      }
      out$group_id <- x
      out
    })
    do.call(rbind.data.frame, tmp)
  })
  output$plot <- renderPlot({
    # Get data
    out <- data()
    # Default plot paramters
    point_size <- input$point
    line_size <- input$line
    label_size <- input$label
    xlim <- c(68, 0)
    # Set facets
    groups <- unique(out[, "group_id"])
    n <- ceiling(sqrt(length(groups)))
    par(mfrow = c(n, n), mar=c(5, 4, 4, 2))
    
    for (i in groups) {
      out_df <- subset(out, group_id == i)
      
      if (input$type == "accum") {
        get_specaccum(df = out_df, rank = input$rank, 
                      label_size = label_size, line_size = line_size)
      }
      
      if (input$type == "range") {
        get_temporal_ranges(df = out_df, rank = input$rank, 
                            label_size = label_size, line_size = line_size)
      }
        
      if (any(input$type == c("occurrences", "collections", "taxa"))) {
        ylim <- c(0, max(out_df$value))
        plot(x = out_df$mid_ma, y = out_df$value, main = unique(out_df$group_id),
             xlab = "Time (Ma)", ylab = paste0("Number of ", input$type),
             xlim = xlim,
             ylim = ylim,
             type = "n", lwd = line_size,
             cex.axis = label_size, cex.lab = label_size)
        rect(xleft = out_df$max_ma, xright = out_df$min_ma, 
             ybottom = 0, ytop = out_df$value,
             col = out_df$colour, lwd = line_size)
        rect(xleft = max(bins$max_ma) * 2, xright = max(bins$max_ma) * -2, 
             ybottom = max(ylim) * -0.04, ytop = 0,
             col = "grey80")
        rect(xleft = bins$max_ma, xright = bins$min_ma, 
             ybottom = max(ylim) * -0.04, ytop = 0,
             col = bins$colour)
        geo_size <- bins$duration_myr / max(bins$duration_myr) * (1.5 - strwidth(out_df$interva_name))
        if (length(groups) == 1) {
          text(x = bins$mid_ma, y = max(ylim) * -0.02, 
               labels = bins$interval_name,
               adj = c(0.5, 0.5), cex = geo_size)
        }
      }
    }
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)
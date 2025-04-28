# Header ----------------------------------------------------------------
# Project: app
# File name: app.R
# Last updated: 2025-04-24
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/app

# Load libraries --------------------------------------------------------
library(shiny)

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
      selectInput("type", "Select a type of analyses",
                   c("Number of occurrences" = "occurrences", 
                     "Number of collections" = "collections", 
                     "Number of taxa" = "taxa", "Temporal ranges" = "range"),
                   selected = "occurrences"),
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
      sliderInput("point", "Select a point size",
                  min = 0, max = 5,
                  value = 1.25),
      sliderInput("line", "Select a line size",
                  min = 0, max = 5,
                  value = 1.25),
      sliderInput("label", "Select a label size",
                  min = 0, max = 1.5,
                  value = 0.75)
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
        out <- get_temporal_ranges(df = out, rank = input$rank)
      } else if (input$type == "occurrences") {
        out <- get_occurrence_counts(df = out)
      } else if (input$type == "collections") {
        out <- get_collection_counts(df = out)
      } else if (input$type == "taxa") {
        out <- get_richness_counts(df = out, rank = input$rank)
      }
      out$group_id <- x
      out
    })
    do.call(rbind.data.frame, tmp)
  })
  output$plot <- renderPlot({
    out <- data()
    groups <- unique(out[, "group_id"])
    n <- ceiling(sqrt(length(groups)))
    # Collect usr par for resetting
    usrpar <- par(no.readonly = TRUE)
    par(mfrow = c(n, n), mar=c(5, 4, 3, 3))
    point_size <- input$point
    line_size <- input$line
    label_size <- input$label

    
    for (i in groups) {
      df <- subset(out, group_id == i)
      if (input$type == "range") {
        df <- df[order(df$taxon, decreasing = TRUE), ]
        df <- df[order(df$max_ma, decreasing = FALSE), ]
        # Estimate max label width
        max_label_width <- max(strwidth(df$taxon, units = "inches"))
        # Convert inches to lines (approximate conversion factor: 0.2)
        extra_margin <- max_label_width / 0.2
        # Update left margin (add extra space, default is 4)
        par(mar = usrpar$mar + c(0, extra_margin, 0, 0))
        xlim <- c(max(df$max_ma), min(df$min_ma))
        ylim <- c(0, nrow(df) + 1)
        df$taxon_id <- 1:nrow(df)
        plot(x = NA, y = NA, xlim = xlim, ylim = ylim, 
             yaxt = "n", axes = TRUE,
             main = unique(df$group_id), xlab = "Time (Ma)", ylab = NA, 
             cex.axis = label_size, cex.lab = label_size)
        axis(2, at = df$taxon_id, labels = df$taxon, las = 2, cex.axis = label_size)
        segments(x0 = df$max_ma, x1 = df$min_ma,
                 y0 = df$taxon_id,
                 col = 1, lty = 1, lwd = line_size)
        points(x = df$max_ma, y = df$taxon_id,
               pch = 20, col = "black", cex = point_size)
        points(x = df$min_ma, y = df$taxon_id,
               pch = 20, col = "black", cex = point_size)
      } else {
        plot(x = df$mid_ma, y = df$value, main = unique(df$group_id),
             xlab = "Time (Ma)", ylab = paste0("Number of ", input$type),
             xlim = c(max(df$max_ma), min(df$min_ma)),
             type = "l", lwd = line_size, 
             cex.axis = label_size, cex.lab = label_size)
        points(x = df$mid_ma, y = df$value, pch = 20, cex = point_size)
      }
    }
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)
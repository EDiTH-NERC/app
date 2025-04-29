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
      radioButtons("rank", "Select a taxonomic rank",
                   c(Species = "species", Genus = "genus", Family = "family"), 
                   selected = "genus"),
      # Input: Select region ----
      radioButtons("region", "Select a geographic region",
                  c(regions),
                  selected = "Caribbean"),
      # Input: Select family ----
      selectInput("family", "Select a taxonomic family",
                  c(All = ".", families),
                  selected = "All"),
      # Input: Group by ----
      selectInput("group", "Select a grouping variable",
                  c(groups))
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
        out <- get_temporal_ranges(df = out, bins = bins, rank = input$rank)
      } else if (input$type == "occurrences") {
        out <- get_counts(df = out, bins = bins, what = "occurrence_no")
      } else if (input$type == "collections") {
        out <- get_counts(df = out, bins = bins, what = "collection_no")
      } else if (input$type == "taxa") {
        out <- get_counts(df = out, bins = bins, what = input$rank)
      } else if (input$type == "accum") {
        out <- get_specaccum(df = out, rank = input$rank)
      }
      out$group_id <- x
      out
    })
    do.call(rbind.data.frame, tmp)
  })
  output$plot <- renderPlot({
    # Get data
    out <- data()
    xlim <- c(68, 0)
    # Set facets
    groups <- unique(out[, "group_id"])
    n <- ceiling(sqrt(length(groups)))
    par(mfrow = c(n, n), mar=c(5, 4, 4, 2))
    
    for (i in groups) {
      out_df <- subset(out, group_id == i)
      
      if (input$type == "accum") {
        plot(x = out_df$collection, y = out_df$richness, 
             main = i, ylab = "Number of taxa", xlab = "Number of collections",
             type = "n", cex.axis = 1.25, cex.lab = 1.25)
        polygon(x = c(rev(out_df$collection), out_df$collection), 
                y = c(rev(out_df$richness + out_df$sd), 
                      out_df$richness - out_df$sd), 
                col = 'lightblue', border = NA)
        lines(x = out_df$collection, y = out_df$richness, lwd = 1.25)
      }
      
      if (input$type == "range") {
        # Plot parameters
        ## ylim
        ylim_lower <- 0 - (nrow(out_df) * 0.08)
        ylim_upper <- nrow(out_df) * 1.04
        ylim <- c(ylim_lower, ylim_upper)
        # Relative sizing
        h <- par("pin")[2]
        ## Taxon labels
        cex_taxon_scale <- (h / (10 + sqrt(nrow(out_df))))
        ## geoscale
        ylim_lower_upper <- 0 - (nrow(out_df) * 0.04)
        cex_geo_scale <- (h / 4)
        cex_geo_scale <- cex_geo_scale * ((bins$duration_myr / max(bins$duration_myr))*0.5)
      
        # Plotting
        plot(x = NA, y = NA, xlim = xlim, ylim = ylim,
             yaxt = "n", yaxs = "i", axes = TRUE, main = i, xlab = "Time (Ma)",
             ylab = NA, cex.axis = 1.25, cex.lab = 1.25)
        segments(x0 = out_df$max_ma, x1 = out_df$min_ma, y0 = out_df$taxon_id,
                 col = 1, lty = 1, lwd = 1.25)
        text(x = (out_df$max_ma) + 0.5, y = out_df$taxon_id,
             labels = out_df$taxon,
             adj = c(1, 0.5), cex = cex_taxon_scale)
        # Geological timescales
        # Geological timescales
        rect(xleft = max(bins$max_ma) * 2, xright = max(bins$max_ma) * -2,
             ybottom = ylim_lower, ytop = ylim_lower_upper,
             col = "grey80")
        rect(xleft = bins$max_ma, xright = bins$min_ma,
             ybottom = ylim_lower, ytop = ylim_lower_upper,
             col = bins$colour)
        ## Interval labels
        ### Convert rect width to inches
        user_width <- bins$min_ma - bins$max_ma
        plot_width_in <- par("pin")[1]
        xrange_user <- diff(par("usr")[1:2])
        rect_width_in <- user_width * plot_width_in / xrange_user
        
        # Measure text width in inches at base cex
        label <- bins$interval_name
        cex_base <- 1
        label_width_in <- strwidth(label, units = "inches", cex = cex_base)
        
        # Compute cex so that label fits the rectangle
        cex_fit <- (cex_base * (rect_width_in / label_width_in)) * 0.7 
        # Add labels
        text(x = bins$mid_ma, y = (ylim_lower + ylim_lower_upper) / 2,
             labels = label,
             adj = c(0.5, 0.5), cex = cex_fit)
      }
        
      if (any(input$type == c("occurrences", "collections", "taxa"))) {
        # Plot parameters
        ## ylim
        ylim_lower <- 0 - (max(out_df$value) * 0.04)
        ylim_upper <- max(out_df$value) * 1.04
        ylim <- c(ylim_lower, ylim_upper)
        # Relative sizing
        h <- par("pin")[2]
        ## Taxon labels
        cex_taxon_scale <- (h / (10 + sqrt(nrow(out_df))))
        ## geoscale
        cex_geo_scale <- (h / 4)
        cex_geo_scale <- cex_geo_scale * ((bins$duration_myr / max(bins$duration_myr))*0.5)
      
        # Plot
        plot(x = out_df$mid_ma, y = out_df$value, main = unique(out_df$group_id),
             xlab = "Time (Ma)", ylab = paste0("Number of ", input$type),
             xlim = xlim,
             ylim = ylim,
             type = "n", lwd = 1,
             yaxs = "i",
             cex.axis = 1.25, cex.lab = 1.25)
        rect(xleft = out_df$max_ma, xright = out_df$min_ma, 
             ybottom = 0, ytop = out_df$value,
             col = out_df$colour, lwd = 1)
        # Geological timescales
        rect(xleft = max(bins$max_ma) * 2, xright = max(bins$max_ma) * -2,
             ybottom = ylim_lower, ytop = 0,
             col = "grey80")
        rect(xleft = bins$max_ma, xright = bins$min_ma,
             ybottom = ylim_lower, ytop = 0,
             col = bins$colour)
        ## Interval labels
        ### Convert rect width to inches
        user_width <- bins$min_ma - bins$max_ma
        plot_width_in <- par("pin")[1]
        xrange_user <- diff(par("usr")[1:2])
        rect_width_in <- user_width * plot_width_in / xrange_user
        
        # Measure text width in inches at base cex
        label <- bins$interval_name
        cex_base <- 1
        label_width_in <- strwidth(label, units = "inches", cex = cex_base)
        
        # Compute cex so that label fits the rectangle
        cex_fit <- (cex_base * (rect_width_in / label_width_in)) * 0.7 
        # Add labels
        text(x = bins$mid_ma, y = ylim_lower / 2,
             labels = label,
             adj = c(0.5, 0.5), cex = cex_fit)
      }
    }
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)
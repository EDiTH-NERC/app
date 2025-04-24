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
custom_theme <- theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$style(HTML("
    #plot {
      height: 100vh !important; /* vh = viewport height */
    }
  ")),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width = 2,

      # Input: Analyses type ----
      radioButtons("type", "Type",
                   c(Occurrences = "occurrences", Collections = "collections", 
                     Range = "range", 
                     Richness = "richness", Diversification = "rates"),
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
                  selected = "All")
    ),

    # Main panel for displaying outputs ----
    mainPanel(width = 10,

      # Output: Plot ----
      plotOutput(outputId = "plot")

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
  output$plot <- renderPlot({
    out <- data()
    n <- nrow(out)
    point_size <- 8 / sqrt(n) + 1
    line_size <- 5 / sqrt(n)
    text_size <- (10 / sqrt(n)) + 1
    strip_size <- text_size * 5
    
    if (input$type == "range") {
      out <- out[order(out$max_ma, decreasing = FALSE), ]
      out$taxon_id <- 1:nrow(out)
      out$taxon <- factor(x = out$taxon, levels = out$taxon)
      p <- ggplot(data = out, aes(y = taxon, xmin = min_ma, xmax = max_ma)) +
        geom_linerange(size = line_size) +
        geom_point(aes(y = taxon, x = max_ma), size = point_size) +
        geom_point(aes(y = taxon, x = min_ma), size = point_size) +
        geom_text(aes(y = taxon, x = max_ma, label = taxon), 
                  size = text_size, hjust = 1, nudge_x = -0.25,
                  colour = "black", fontface = "bold") +
        scale_x_reverse(name = xlab) +
        facet_wrap(~group_id, scales = "free_y") +
        custom_theme +
        theme(strip.text = element_text(size = strip_size, margin = margin()))
    } else {
      p <- ggplot(data = data(), aes(x = mid_ma, y = value)) +
        geom_point() +
        facet_wrap(~group_id) +
        custom_theme
    }
    p
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)

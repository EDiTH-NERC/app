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

# UI --------------------------------------------------------------------
ui <- fluidPage(

  # App title ----
  titlePanel("Hello Shiny!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Analyses type ----
      radioButtons("type", "Type",
                   c(Occurrences = "occurrences", Collections = "collections", 
                     Range = "range", 
                     Richness = "richness", Diversification = "rates")),
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
    mainPanel(

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
      }
      if (input$type == "occurrences") {
        out <- get_occurrence_counts(df = out)
      }
      if (input$type == "collections") {
        out <- get_collection_counts(df = out)
      }
      if (input$type == "richness") {
        out <- get_richness_counts(df = out, rank = input$rank)
      }
      out$group_id <- x
      out
    })
    do.call(rbind.data.frame, tmp)
  })
  # Reactive: Plot rendering ----  
  output$plot <- renderPlot({
    
    if (input$type == "range") {
      out <- data()
      out <- out[order(out$max_ma, decreasing = FALSE), ]
      out$taxon_id <- 1:nrow(out)
      p <- ggplot(data = out, aes(y = taxon_id, xmin = min_ma, xmax = max_ma)) +
        geom_linerange() +
        geom_point(aes(y = taxon_id, x = max_ma), size = 2) +
        geom_point(aes(y = taxon_id, x = min_ma), size = 2) +
        geom_label(aes(y = taxon_id, x = (max_ma + min_ma) / 2, label = taxon), 
                   size = 2, hjust = 0.5, colour = "black", fontface = "bold",
                   fill = alpha('white', 0.5)) +
        scale_x_reverse() +
        theme_bw(base_size = 20) +
        theme(legend.position = "none",
              legend.title = element_blank(),
              axis.text = element_text(colour = "black"),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              panel.grid = element_blank())
    } else {
      p <- ggplot(data = data(), aes(x = mid_ma, y = value)) +
        geom_point() +
        theme(legend.position = "none")
    }
    # Facet?
    if (input$group != ".") {
      p <- p + facet_wrap(paste0("~group_id"), scales = "free_y")
    }
    p
    
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)

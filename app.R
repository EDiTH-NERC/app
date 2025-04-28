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
groups <- c(None = ".", Family = "family", Genus = "genus", Country = "cc")

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
      selectInput("type", "Type",
                   c(Occurrences = "occurrences", Collections = "collections", 
                     Taxa = "taxa", Range = "range"),
                   selected = "occurrences"),
      # Input: Taxonomic rank ----
      selectInput("rank", "Taxonomic rank",
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
    plot(x = out$max_ma, y = out$min_ma)
  })
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)
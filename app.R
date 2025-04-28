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
# Theme
custom_theme <- theme(legend.position = "none",
                      legend.title = element_blank(),
                      axis.text = element_text(colour = "black"),
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
    sidebarPanel(width = 3,
                 h3("Analyses"),
                 
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
                 h3("Filter"),
                 # Input: Select region ----
                 selectInput("region", "Geographic region",
                             c(regions),
                             selected = "Caribbean"),
                 # Input: Select family ----
                 selectInput("family", "Family",
                             c(All = ".", families),
                             selected = "All"),
                 # Input: Plot parameters ----
                 h3("Plot"),
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
    mainPanel(width = 9
              
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output) {
  # Reactive: Data filtering and analyses ----
  
}
# Create app ------------------------------------------------------------
shinyApp(ui = ui, server = server)
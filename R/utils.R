# Filtering -------------------------------------------------------------
filter_rank <- function(df, rank = "genus") {
  if (rank == "species") {
    df <- subset(df, accepted_rank == "species")
    df <- subset(df, !is.na(species))
  } else if (rank == "genus") {
    df <- subset(df, accepted_rank %in% c("species", "genus"))
    df <- subset(df, !is.na(genus))
  } else if (rank == "family") {
    df <- subset(df, accepted_rank %in% c("species", "genus", "family"))
    df <- subset(df, !is.na(family))
  }
  df
}

filter_region <- function(df, region = "Caribbean") {
  if (region == "Caribbean") {
    df <- subset(df, region == "Caribbean")
  } else if (region == "Mediterranean") {
    df <- subset(df, region == "Mediterranean")
  } else if (region == "Arabia") {
    df <- subset(df, region == "Arabia")
  } else if (region == "Indo-Australian Archipelago") {
    df <- subset(df, region == "Indo-Australian Archipelago")
  }
  df
}

filter_family <- function(df, fam = ".") {
  if (fam != ".") {
    df <- subset(df, family == fam)
  }
  df
}

# Group data ------------------------------------------------------------
group_data <- function(df, group = ".") {
  if (group == ".") {
    df$group_id <- "."
  } else {
    df$group_id <- df[, group]
  }
  split(x = df, f = df$group_id)
}

# Analyses --------------------------------------------------------------
get_occurrence_counts <- function(df) {
  # Get occurrence counts
  bins <- readRDS("data/stages.RDS")
  bins$value <- NA
  for (i in 1:nrow(bins)) {
    bins$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), 
                                        "occurrence_no"]))
  }
  bins
}
get_collection_counts <- function(df) {
  # Get collection counts
  bins <- readRDS("data/stages.RDS")
  bins$value <- NA
  for (i in 1:nrow(bins)) {
    bins$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), 
                                      "collection_no"]))
  }
  bins
}
get_richness_counts <- function(df, rank) {
  # Get taxonomic counts
  bins <- readRDS("data/stages.RDS")
  bins$value <- NA
  for (i in 1:nrow(bins)) {
    bins$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), 
                                      rank]))
  }
  bins
}
get_temporal_ranges <- function (df, rank, label_size = 1, line_size = 1) {
  # Get bins
  bins <- readRDS("data/stages.RDS")
  # Get group name
  group <- unique(df$group_id)
  # Get ranges counts
  out <- data.frame(taxon = unique(df[, rank]), max_ma = NA, min_ma = NA)
  for (i in 1:nrow(out)) {
    vec <- which(df[, rank] == out$taxon[i])
    out$max_ma[i] <- max(df[vec, "max_ma"])
    out$min_ma[i] <- min(df[vec, "min_ma"])
  }
  out$mid_ma <- (out$max_ma + out$min_ma) / 2
  # Plotting
  out <- out[order(out$taxon, decreasing = TRUE), ]
  out <- out[order(out$max_ma, decreasing = FALSE), ]
  out$taxon_id <- 1:nrow(out)
  
  # Plot parameters
  ylim1 <- 0 - (nrow(out) * 0.08)
  ylim1u <- 0 - (nrow(out) * 0.04)
  ylim2 <- nrow(out) + (nrow(out) * 0.04)
  ylim <- c(ylim1, ylim2)
  xlim <- c(68, 0)
  plot(x = NA, y = NA, xlim = xlim, ylim = ylim, 
       yaxt = "n", yaxs = "i", axes = TRUE, main = group, xlab = "Time (Ma)", 
       ylab = NA, cex.axis = label_size, cex.lab = label_size)
  segments(x0 = out$max_ma, x1 = out$min_ma, y0 = out$taxon_id,
           col = 1, lty = 1, lwd = line_size)
  text(x = (out$max_ma) + 0.5, y = out$taxon_id, 
       labels = out$taxon,
       adj = c(1, 0.5), cex = label_size * 0.8)
  rect(xleft = max(bins$max_ma) * 2, xright = max(bins$max_ma) * -2, 
       ybottom = ylim1, ytop = ylim1u,
       col = "grey80")
  rect(xleft = bins$max_ma, xright = bins$min_ma, 
       ybottom = ylim1, ytop = ylim1u,
       col = bins$colour)
  geo_size <- (bins$duration_myr / max(bins$duration_myr)) * (0.75 + label_size)
  if (length(group) == 1) {
    text(x = bins$mid_ma, y = (ylim1 + ylim1u) / 2, 
         labels = bins$interval_name,
         adj = c(0.5, 0.5), cex = geo_size)
  }
}

get_specaccum <- function (df, rank, line_size = 1, label_size = 1) {
  # Get group name
  group <- unique(df$group_id)
  # Create community matrix
  comm_matrix <- table(df$collection_no, df[, rank])
  # Get species dataframe
  sp <- as.data.frame.matrix(comm_matrix)
  # Get accummualtion curve
  sp <- specaccum(comm_matrix)
  # Plot accummulation curve
  plot(sp, main = group, xlab = "Number of collections", ylab = "Number of taxa",
       ci.type = "poly", col = "blue", lwd = line_size, 
       ci.lty = 0, ci.col = "lightblue", 
       cex.axis = label_size, cex.lab = label_size)
}

get_diversification_rates <- function () {}






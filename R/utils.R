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
get_counts <- function(df, bins, what) {
  bins$value <- NA
  for (i in 1:nrow(bins)) {
    vec <- which(df[, "mid_ma"] == bins[i, "mid_ma"])
    bins$value[i] <- length(unique(df[vec, what]))
  }
  bins
}

get_temporal_ranges <- function (df, bins = bins, rank) {
  # Get ranges counts
  out <- data.frame(taxon = unique(df[, rank]), max_ma = NA, min_ma = NA)
  for (i in 1:nrow(out)) {
    vec <- which(df[, rank] == out$taxon[i])
    out$max_ma[i] <- max(df[vec, "max_ma"])
    out$min_ma[i] <- min(df[vec, "min_ma"])
  }
  out$mid_ma <- (out$max_ma + out$min_ma) / 2
  out <- out[order(out$taxon, decreasing = TRUE), ]
  out <- out[order(out$max_ma, decreasing = FALSE), ]
  out$taxon_id <- 1:nrow(out)
  out
}

get_specaccum <- function(df, rank) {
  # Get group name
  group <- unique(df$group_id)
  # Create community matrix
  comm_matrix <- table(df$collection_no, df[, rank])
  # Get species dataframe
  sp <- as.data.frame.matrix(comm_matrix)
  # Get accumulation curve
  sp <- specaccum(comm_matrix)
  # Output
  data.frame(collections = sp$site, richness = sp$richness, sd = sp$sd)
}

# Plotting --------------------------------------------------------------

# get_specaccum <- function (df, rank, line_size = 1, label_size = 1) {
#   # Get group name
#   group <- unique(df$group_id)
#   # Create community matrix
#   comm_matrix <- table(df$collection_no, df[, rank])
#   # Get species dataframe
#   sp <- as.data.frame.matrix(comm_matrix)
#   # Get accummualtion curve
#   sp <- specaccum(comm_matrix)
#   # Plot accummulation curve
#   plot(sp, main = group, xlab = "Number of collections", ylab = "Number of taxa",
#        ci.type = "poly", col = "blue", lwd = line_size, 
#        ci.lty = 0, ci.col = "lightblue", 
#        cex.axis = label_size, cex.lab = label_size)
# }

# get_diversification_rates <- function () {}






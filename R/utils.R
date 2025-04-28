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
get_temporal_ranges <- function (df, rank) {
  # Get ranges counts
  out <- data.frame(taxon = unique(df[, rank]), max_ma = NA, min_ma = NA)
  for (i in 1:nrow(out)) {
    vec <- which(df[, rank] == out$taxon[i])
    out$max_ma[i] <- max(df[vec, "max_ma"])
    out$min_ma[i] <- min(df[vec, "min_ma"])
  }
  out$mid_ma <- (out$max_ma + out$min_ma) / 2
  out
}

get_diversification_rates <- function () {}






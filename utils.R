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
    df <- list(all = df)
  } else {
    df <- split(x = df, f = df[, group])
  }
  df
}

# Analyses --------------------------------------------------------------
get_sampling_counts <- function(df, bins) {
  # Get occurrence counts
  n_occ <- bins
  n_occ$value <- NA
  n_occ$type <- "Occurrences"
  for (i in 1:nrow(bins)) {
    n_occ$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), 
                                       "occurrence_no"]))
  }
  # Get collection counts
  n_coll <- bins
  n_coll$value <- NA
  n_coll$type <- "Collections"
  for (i in 1:nrow(bins)) {
    n_coll$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), 
                                        "collection_no"]))
  }
  # Bind data
  df <- rbind.data.frame(n_occ, n_coll)
  df$analyses <- "sampling"
  df
}

get_temporal_ranges <- function (df, name = "genus", group = ".") {
  # Get plotting data
  if (group == ".") {
    df <- tax_range_time(occdf = df, name = name)
  } else {
    df <- group_apply(occdf = df, group = group, 
                      fun = tax_range_time, name = name)
  }
  df$taxon_id <- 1:nrow(df)
  df$taxon_id <- factor(x = df$taxon_id, levels = df$taxon_id)
  # Plot parameters
  n <- nrow(df)
  point_size <- 8 / sqrt(n) + 1
  line_size <- 5 / sqrt(n)
  text_size <- (10 / sqrt(n)) + 1
  if (name == "family") {ff <- "bold"} else {ff <- "bold.italic"}
  # Plot
  p <- ggplot(data = df, aes(y = taxon_id, xmin = min_ma, xmax = max_ma,
                             colour = taxon_id)) +
    geom_linerange(size = line_size) +
    geom_point(data = df, aes(y = taxon_id, x = max_ma), size = point_size) +
    geom_point(data = df, aes(y = taxon_id, x = min_ma), size = point_size) +
    geom_label(data = df, aes(y = taxon_id, x = (max_ma + min_ma) / 2, label = taxon), 
               size = text_size, hjust = 0.5, colour = "black", fontface = ff,
               fill = alpha('white', 0.5)) +
    scale_x_reverse(expand = expansion(0, 0)) +
    scale_y_discrete(expand = expansion(add = 1)) +
    xlab("Time (Ma)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          axis.text = element_text(colour = "black"),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank()) +
    coord_geo(pos = list("bottom", "bottom"),
              dat = list("stages", "periods"), 
              height = list(unit(1.5, "line"), unit(1.5, "line")), 
              size = "auto", abbrv = TRUE, expand = TRUE)
  if (group != ".") {
    p <- p + facet_wrap(paste0("~", group), scales = "free_y")
  }
  return(p)
}

get_raw_counts <- function (df, bins, rank) {
  # Get occurrence counts
  bins$value <- NA
  bins$type <- "Counts"
  for (i in 1:nrow(bins)) {
    bins$value[i] <- length(unique(df[which(df[, "mid_ma"] == bins[i, "mid_ma"]), rank]))
  }
  bins$analyses <- "counts"
  bins
}
get_diversification_rates <- function () {}






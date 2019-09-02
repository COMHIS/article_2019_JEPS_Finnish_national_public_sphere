# R version 3.4.3
# author: Ville Vaara

source("code/lib/functions_metadata.R")
source("code/lib/functions_location.R")
source("code/lib/functions_provinciality.R")
source("code/lib/functions_helpers.R")
source("code/lib/functions_output.R")

library(reshape2)
library(ggplot2)


get_newspaper_provincility_plot <- function(newspapers_provinciality_plot_data) {
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_provinciality_plot_data,
             aes(x = year, y = value, fill = variable),
             width = 1,
             alpha = 0.9) +
    scale_fill_brewer(palette = "Set3")
  return(newspapers_provinciality_plot)
}


get_newspaper_provincility_plot_proportional <- function(newspapers_provinciality_plot_data) {
  cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#D27979", "#ffccb3", "#D2D179", "#999999")
  #                orange     green      dark blue  red         pink      light blue yellow  
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_provinciality_plot_data,
             aes(x = year, y = value, fill = variable),
             colour="black",
             position = "fill", 
             width = 1) +
    theme_gray() +
    # scale_fill_brewer(palette = "Set3") +
    scale_fill_manual(values = cbPalette) +
    scale_x_continuous(breaks = seq(1800, 1920, by=10)) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    labs(x = "Year", y = "Proportion", fill = "Location:")
  return(newspapers_provinciality_plot)
}


get_number_of_locations_with_min_x_papers_plot <- function(newspapers_provinciality_plot_data, x_papers) {
  plot_data <- get_number_of_locations_with_more_than_x_papers_per_y_years(newspapers_provinciality_plot_data, x_papers)
  plot <- ggplot() +
    geom_col(data = plot_data,
             aes(x = time_segment, y = locations),
             width = 1,
             alpha = 0.9) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(breaks = seq(0,
                                    max(plot_data$locations),
                                    2)) +
    scale_x_continuous(breaks = seq(min(plot_data$time_segment),
                                    max(plot_data$time_segment),
                                    10)) +
    theme(axis.title.x=element_blank())
  return(plot)
}


get_newspaper_sum_plot_data <- function(newspapers_provinciality_plot_data) {
  return_data <- data.frame(year = integer(), papers = integer())
  for (query_year in unique(newspapers_provinciality_plot_data$year)) {
    year_subset <- subset(newspapers_provinciality_plot_data, year == query_year)
    year_sum <- sum(year_subset$value, na.rm = TRUE)
    new_row <- data.frame(year = query_year, papers = year_sum)
    return_data <- rbind(return_data, new_row)
  }
  return(return_data)
}


get_newspaper_sum_plot <- function(newspapers_sum_plot_data) {
  newspapers_provinciality_plot <- ggplot() +
    geom_line(data = newspapers_sum_plot_data,
              aes(x = year, y = papers),
              size = 1.5, colour = "blue") +
    scale_y_continuous(breaks = seq(0,
                                    max(newspapers_sum_plot_data$papers),
                                    10)) +
    scale_x_continuous(breaks = seq(min(newspapers_sum_plot_data$year),
                                    max(newspapers_sum_plot_data$year),
                                    10)) +
    theme(axis.title.x=element_blank())
  return(newspapers_provinciality_plot)
}


get_newspaper_finswe_plot <- function(newspapers_language_share_sum) {
  newspapers_finswe_plot <- ggplot() +
    geom_col(data = newspapers_language_share_sum,
             aes(x = year, y = papers, fill = lang),
             width = 1,
             alpha = 0.7) +
    scale_fill_manual(values = c("blue", "black", "#006633", "yellow")) +
    scale_x_continuous(breaks = seq(min(newspapers_language_share_sum$year),
                                    max(newspapers_language_share_sum$year),
                                    20))
  return(newspapers_finswe_plot)
}


get_newspaper_finswe_plot_proportional <- function(newspapers_language_share_sum) {
  newspapers_provinciality_plot <- ggplot() +
    geom_col(data = newspapers_language_share_sum,
             aes(x = year, y = papers, fill = lang),
             position = "fill", 
             width = 1,
             alpha = 0.7) +
    scale_fill_manual(values = c("blue", "black", "#006633", "yellow")) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "share") +
    scale_x_continuous(breaks = seq(min(newspapers_language_share_sum$year),
                                    max(newspapers_language_share_sum$year),
                                    20))
  return(newspapers_provinciality_plot)
}

# Load data
newspaper_base_data <- read.csv("input/raw/newspapers-utf8.csv")
locations_data <- read.csv("input/raw/publication_locations-utf8.csv")

# Preprocess data
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
finnish_newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" & JULKAISUMAA == "FI")
enriched_newspapers_locations_data <- enrich_locations_data(locations_data, finnish_newspapers_only_subset)

# Create plot
year_range <- 1800:1920
newspapers_provinciality_plot_data <- get_provinciality_plot_data(finnish_newspapers_only_subset, enriched_newspapers_locations_data, 1800:1920)
newspapers_provinciality_plot_data$variable <- as.character(newspapers_provinciality_plot_data$variable)
newspapers_provinciality_plot_data[newspapers_provinciality_plot_data$variable == "Viipuri", ]$variable <- "Vyborg"
top10_newspapers_provinciality_plot_data <- get_topx_newspapers_data(newspapers_provinciality_plot_data, top_x = 10)
top10_newspapers_provinciality_plot_proportional <-
  get_newspaper_provincility_plot_proportional(top10_newspapers_provinciality_plot_data)

# Check plot in RStudio
# top10_newspapers_provinciality_plot

# Save plot
save_plot_png(plot = top10_newspapers_provinciality_plot_proportional, plotname = "fig11_newspapers_by_location", size_preset = "large")

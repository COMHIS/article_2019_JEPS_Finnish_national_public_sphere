library(ggplot2)


get_provinciality_plot_data <- function(newspapers_only_subset, enriched_newspapers_locations_data, year_range) {
  results_data <- data.frame(year = integer())
  results_data[1, ] <- NA
  all_locations <- subset(enriched_newspapers_locations_data,
                          start_year <= year_range[length(year_range)] &
                            end_year >= year_range[1])
  all_locations <- unique(all_locations$KAUPUNKI_NORM)
  
  for (location in all_locations) {
    results_data[, as.character(location)] <- NA
  }
  results_data <- results_data[-c(1), ]
  
  yearly_row_empty <- results_data
  
  for (year in year_range) {
    yearly_row <- yearly_row_empty
    yearly_row[1, "year"] <- as.integer(year)
    
    year_subset <- subset(newspapers_only_subset, start_year <= year & end_year >= year)
    if (nrow(year_subset) > 0) {
      year_subset_locations <- add_locations_data_to_metadata_year_subset(year_subset, enriched_newspapers_locations_data, year)
      year_subset_locations <- subset(year_subset_locations, !(is.na(location) | location == ""))
      
      locations_in_year <- unique(year_subset_locations$location)
      for (location in locations_in_year) {
        location_amount <- sum(year_subset_locations$location == location)
        yearly_row[1, location] <- as.integer(location_amount)
      }
      
      total_papers_number <- nrow(year_subset_locations)
      results_data <- rbind(results_data, yearly_row)
    }
  }
  results_data <- melt(results_data, id = "year")
  return(results_data)
}


get_hits_for_location <- function(fennicadata, location) { 
  hits_for_location <- sum(fennicadata$publication_place == location, na.rm = TRUE)
  return(hits_for_location)
}


get_fennica_location_hitdata_for_year_range <- function(fennicadata, year_range) {
  return_data <- data.frame(year = integer(), location = character(), hits = integer(), proportion = numeric())
  for (year in year_range) {
    print(paste0("processing year ", year))
    year_subset <- subset(fennicadata, publication_year == year)
    unique_locations <- as.character(unique(year_subset$publication_place))
    unique_locations <- unique_locations[!is.na(unique_locations)]
    for (location in unique_locations) {
      hits_for_location <- get_hits_for_location(year_subset, location)
      new_row <- data.frame(year = year,
                            location = location, 
                            hits = hits_for_location,
                            proportion = (hits_for_location / nrow(year_subset)))
      return_data <- rbind(return_data, new_row)
    }
  }
  return_data$location <- as.character(return_data$location)
  return(return_data)
}


get_provinciality_plot_data_greater_than_x <- function(newspapers_provinciality_plot_data, greater_than) {
  greater_than_x_subset <- subset(newspapers_provinciality_plot_data,
                                  value >= greater_than) # | is.na(value) | value == 0
  return(greater_than_x_subset)
}


get_number_of_locations_with_more_than_x_papers_per_y_years <- function(newspapers_provinciality_plot_data, greater_than) {
  
  results_data <- data.frame(time_segment = integer(), locations = integer(), min_locations = integer())
  
  provinciality_plot_data_greater_than_x <-
    get_provinciality_plot_data_greater_than_x(newspapers_provinciality_plot_data, greater_than)
  
  data_year_range <- min(newspapers_provinciality_plot_data$year):max(newspapers_provinciality_plot_data$year)
  
  for (i_year in data_year_range) {
    
    provinciality_plot_data_greater_than_x_time_subset <-
      subset(provinciality_plot_data_greater_than_x, year == i_year)
    number_of_locations <- nrow(provinciality_plot_data_greater_than_x_time_subset)
    
    new_row <- data.frame(time_segment = as.integer(i_year), locations = as.integer(number_of_locations),
                          min_locations = as.integer(greater_than))
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_fennica_provinciality_plot <- function(fennicadata, year_range) {
  fennica_hitdata <- get_fennica_location_hitdata_for_year_range(fennicadata, 1800:1920)
  # dump other locations than Helsinki or Turku
  fennica_hitdata[!(fennica_hitdata$location == "Helsinki" | fennica_hitdata$location == "Turku"), "location"] <- "other"
  fennica_hitdata$location <- factor(fennica_hitdata$location, levels = c("Helsinki", "Turku", "other"))
  plot <- ggplot() +
    geom_col(data = fennica_hitdata,
             aes(x = year, y = proportion, fill = location),
             width = 1,
             alpha = 0.7) +
    scale_fill_brewer(palette = "Paired")
  return(plot)
}


get_topx_newspapers_data <- function(newspapers_provinciality_plot_data,
                                     top_x,
                                     include_others = TRUE) {
  
  location_sum_results <- data.frame(location = character(), total = integer())
  
  for (location in unique(newspapers_provinciality_plot_data$variable)) {
    location_subset <- subset(newspapers_provinciality_plot_data, as.character(variable) == as.character(location))
    location_sum <- sum(location_subset$value, na.rm = TRUE)
    print(paste0(as.character(location_subset[1, "variable"]), "---", as.character(location_sum)))
    new_row <- data.frame(location = as.character(location_subset[1, "variable"]), total = as.integer(location_sum))
    location_sum_results <- rbind(location_sum_results, new_row)
  }
  
  location_sum_results <- location_sum_results[with(location_sum_results, order(-total, location)), ]
  topx_locs <- as.character(location_sum_results[1:top_x, "location"])
  
  topx_newspapers_provinciality_plot_data <- newspapers_provinciality_plot_data
  topx_newspapers_provinciality_plot_data$variable <- as.character(topx_newspapers_provinciality_plot_data$variable)
  
  for (i in (1:nrow(topx_newspapers_provinciality_plot_data))) {
    loc_to_check <- topx_newspapers_provinciality_plot_data[i, "variable"]
    if (!(as.character(loc_to_check) %in% topx_locs)) {
      loc_to_check <- "others"
      topx_newspapers_provinciality_plot_data[i, "variable"] <- loc_to_check
    }
  }
  
  topx_newspapers_provinciality_plot_data$variable <- as.factor(topx_newspapers_provinciality_plot_data$variable)
  
  new_levels <- levels(topx_newspapers_provinciality_plot_data$variable)
  new_levels <- new_levels[-(which(new_levels == "others"))]
  new_levels[length(new_levels) + 1] <- "others"
  
  topx_newspapers_provinciality_plot_data$variable <-
    factor(topx_newspapers_provinciality_plot_data$variable, levels = new_levels)
  topx_newspapers_provinciality_plot_data <-
    topx_newspapers_provinciality_plot_data[with(topx_newspapers_provinciality_plot_data, order(variable, value)), ]
  
  if (!include_others) {
    topx_newspapers_provinciality_plot_data <-
      subset(topx_newspapers_provinciality_plot_data, "location" != "others")
  }
  
  return(topx_newspapers_provinciality_plot_data)
}


enrich_newspaper_metadata <- function(newspaper_base_data) {
  enriched_data <- newspaper_base_data
  enriched_data$start_date <- as.Date(newspaper_base_data$ILM_ALPVM, "%d.%m.%Y")
  enriched_data$end_date <- as.Date(newspaper_base_data$ILM_LOPVM, "%d.%m.%Y")
  enriched_data[is.na(enriched_data[, "end_date"]), "end_date"] <- as.Date("2020-01-01")
  enriched_data$start_year <- as.integer(format(enriched_data$start_date, "%Y"))
  enriched_data$end_year <- as.integer(format(enriched_data$end_date, "%Y"))
  return(enriched_data)
}


get_metadata_issn_for_year <- function(newspaper_metadata, year) {
  year_subset <- subset(newspaper_metadata, start_year <= year & end_year >= year)
  year_issn <- as.character(year_subset$ISSN)
  return(year_issn)
}


get_metadata_issn_amount_for_year <- function(newspaper_metadata, year) {
  year_meta_issn_numbers <- length(get_metadata_issn_for_year(newspaper_metadata, year))
  return(year_meta_issn_numbers)
}


get_metadata_issn_amount_for_year_range <- function(newspaper_metadata, year_range, identifier = "") {
  return_data <- data.frame(year = integer(), issn_numbers = integer(), identifier = character())
  for (year in year_range) {
    # print(paste0("Processing year ", year))
    year_meta_issn_numbers <- length(get_metadata_issn_for_year(newspaper_metadata, year))
    new_row <- data.frame(year = as.integer(year),
                          issn_numbers = as.integer(year_meta_issn_numbers),
                          identifier = identifier)
    return_data <- rbind(return_data, new_row)
  }
  return(return_data)
}


get_start_year_numbers_for_year <- function(enriched_newspaper_metadata, year) {
  start_years <- sum(enriched_newspaper_metadata$start_year == year)
  if (is.na(start_years)) {
    start_years <- 0
  }
  return(start_years)
}


get_end_year_numbers_for_year <- function(enriched_newspaper_metadata, year) {
  end_years <- sum(enriched_newspaper_metadata$end_year == year)
  if (is.na(end_years)) {
    end_years <- 0
  }
  return(end_years)
}


get_start_and_end_years_for_year_range <- function(enriched_newspaper_metadata, year_range) {
  results_data <- data.frame(year = integer(), new_papers = integer(), closed_papers = integer(), total_papers = integer())
  for (year in year_range) {
    starts <- get_start_year_numbers_for_year(enriched_newspaper_metadata, year)
    ends <- get_end_year_numbers_for_year(enriched_newspaper_metadata, year)
    total <- get_metadata_issn_amount_for_year(enriched_newspaper_metadata, year)
    new_row <- data.frame(year = year, new_papers = starts, closed_papers = ends, total_papers = total)
    print(new_row)
    results_data <- rbind(results_data, new_row)
  }
  return(results_data)
}


get_changedata_by_location_for_year <- function(enriched_newspaper_metadata, enriched_locations_data, year, change_type) {
  
  return_data <- data.frame(location = character(), value = integer())
  
  if (change_type == "start" ) {
    year_subset <- subset(enriched_newspaper_metadata, start_year == year)
  } else {
    year_subset <- subset(enriched_newspaper_metadata, end_year == year)
  }
  year_subset_locations <- add_locations_data_to_metadata_year_subset(year_subset, enriched_locations_data, year)
  locations <- year_subset_locations$location
  
  for (loc in unique(locations)) {
    new_row <- data.frame(location = loc, value = sum(locations == loc))
    return_data <- rbind(return_data, new_row)
  }
  
  colnames(return_data) <- c("location", change_type)
  return(return_data)
}


get_names_by_issn <- function(issn_list, names_metadata) {
  issn_list <- as.character(issn_list)
  names_list <- c()
  for (issn in issn_list) {
    if (issn %in% names_metadata$ISSN) {
      name <- as.character(names_metadata[names_metadata$ISSN == issn, "PAANIMEKE"])
    } else {
      name <- "ISSN not found in metadata"
    }
    names_list <- c(names_list, name)
  }
  return(names_list)
}
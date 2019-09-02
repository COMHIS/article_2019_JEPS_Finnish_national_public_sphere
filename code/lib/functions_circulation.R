

enrich_circulation_data <- function(circulation_data) {
  circulation_data$start_year <-  as.integer(substr(as.character(circulation_data$ALPVM), 7, 10))
  circulation_data$end_year <-  as.integer(substr(as.character(circulation_data$LOPVM), 7, 10))
  circulation_data$start_date <- as.Date(circulation_data$ALPVM, "%d.%m.%Y")
  circulation_data$end_date <- as.Date(circulation_data$LOPVM, "%d.%m.%Y")
  return(circulation_data)
}


clean_uncomplete_circulation_data <- function(enriched_circulation_data) {
  clean_circulation_data <- subset(enriched_circulation_data, start_year >= 1600 & start_year <= 2017 & !is.na(start_year))
  clean_circulation_data <- subset(clean_circulation_data, !is.na(end_year) & end_year >= 1600 & end_year <= 2017)
  clean_circulation_data <- subset(clean_circulation_data, start_year <= end_year)
  clean_circulation_data <- subset(clean_circulation_data, !is.na(LEVIKKI))
  start_rows <- nrow(enriched_circulation_data)
  end_rows <- nrow(clean_circulation_data)
  difference_rows <- start_rows - end_rows
  print(paste0(difference_rows, " entries removed in cleaning. ", end_rows, " rows left."))
  return(clean_circulation_data)
}


get_issn_circulation_for_year <- function(issn_number, year, enriched_circulation_data) {
  issn_subset <- subset(enriched_circulation_data, ISSN == issn_number)
  issn_subset <- subset(issn_subset, start_year <= year & end_year >= year)
  if (nrow(issn_subset) == 0) {
    return(0)
  } else if (nrow(issn_subset) == 1) {
    return(as.numeric(issn_subset$LEVIKKI[1]))
  } else {
    print(paste0("Multiple hits for ISSN ", issn_number, " year ", year, "!"))
    return(as.numeric(issn_subset$LEVIKKI[1]))
  }
}


get_unique_circulation_issn_for_year <- function(circulation_data, year) {
  year_subset <- subset(circulation_data, start_year <= year & end_year >= year)
  unique_issn <- unique(as.character(year_subset$ISSN))
  return(unique_issn)
}



get_unique_issn <- function(circulation_data) {
  issn <- as.character(circulation_data$ISSN)
  unique_issn <- unique(issn)
  return(unique_issn)
}


get_circulation_per_issn_for_year_range <- function(year_range, circulation_data) {
  return_data <- data.frame(issn = character(), year = integer(), circulation = integer())
  
  for (year in year_range) {
    rows_to_consider <- subset(circulation_data, start_year <= year & end_year >= year)
    unique_issn <- get_unique_issn(rows_to_consider)
    
    for (issn_number in unique_issn) {
      issn_circ_for_year <- get_issn_circulation_for_year(issn_number, year, rows_to_consider)
      new_row <- data.frame(issn = as.character(issn_number),
                            year = as.integer(year),
                            circulation = as.integer(issn_circ_for_year))
      return_data <- rbind(return_data, new_row)
    }
  }
  return(return_data)
}


get_metadata_subset_issn_numbers <- function(newspaper_base_data, subset_field, subset_field_values) {
  metadata_subset_issn <- newspaper_base_data[ which(newspaper_base_data[, subset_field] %in% subset_field_values), ]
  metadata_subset_issn <- as.character(metadata_subset_issn$ISSN)
  return(metadata_subset_issn)
}


get_circulation_data_subset_by_issn <- function(circulation_data, subset_issn_numbers) {
  circulation_data_subset <- subset(circulation_data, ISSN %in% subset_issn_numbers)
  return(circulation_data_subset)
}


get_total_circulation_per_year <- function(circulation_data, year_range) {
  return_data <- data.frame(year = integer(), circulation = integer())
  for (year in year_range) {
    year_total <- 0
    rows_to_consider <- subset(circulation_data, start_year <= year & end_year >= year)
    unique_issn <- get_unique_issn(rows_to_consider)
    
    for (issn_number in unique_issn) {
      issn_circ_for_year <- get_issn_circulation_for_year(issn_number, year, rows_to_consider)
      year_total <- year_total + issn_circ_for_year
    }
    print(paste0("year-", year, " circulation-", year_total))
    new_row <- data.frame(year = as.integer(year), circulation = as.integer(year_total))
    return_data <- rbind(return_data, new_row)
  }
  
  return(return_data)
}


export_circulation_data <- function(circulation_data, enriched_newspaper_metadata, filename, first_year, last_year) {
  exportable_circ_data <- circulation_data
  exportable_circ_data <- enrich_circulation_data(exportable_circ_data)
  exportable_circ_data <- clean_uncomplete_circulation_data(exportable_circ_data)
  exportable_circ_data$type <-
    enriched_newspaper_metadata[match(exportable_circ_data$ISSN,
                                      enriched_newspaper_metadata$ISSN), "AINYLEISMAARE"]
  exportable_circ_data$title <-
    enriched_newspaper_metadata[match(exportable_circ_data$ISSN,
                                      enriched_newspaper_metadata$ISSN), "PAANIMEKE"]
  exportable_circ_data <- subset(exportable_circ_data, start_year <= last_year & end_year >= first_year)
  exportable_circ_data <- exportable_circ_data[, c("ISSN", "title", "type", "LEVIKKI", "start_date", "end_date")]
  write.csv(x = exportable_circ_data, file = filename, row.names = FALSE)
}

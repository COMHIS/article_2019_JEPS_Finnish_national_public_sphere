
get_alpvm_for_issn_from_metadata <- function(issn, enriched_newspaper_metadata) {
  alpvm <- as.character(enriched_newspaper_metadata[enriched_newspaper_metadata$ISSN == issn, "ILM_ALPVM"])
  return(alpvm)
}


get_lopvm_for_issn_from_metadata <- function(issn, enriched_newspaper_metadata) {
  lopvm <- as.character(enriched_newspaper_metadata[enriched_newspaper_metadata$ISSN == issn, "ILM_LOPVM"])
  return(lopvm)
}


enrich_locations_data_blank_dates <- function(locations_data, enriched_newspaper_metadata) {
  blank_alpvm_issn <- which(locations_data$ALPVM == "01.01.0001")
  blank_alpvm_issn2 <- which(is.na(locations_data$ALPVM))
  blank_alpvm_issn <- c(blank_alpvm_issn, blank_alpvm_issn2)
  blank_lopvm_issn <- which(locations_data$LOPVM == "")
  blank_lopvm_issn2 <- which(is.na(locations_data$LOPVM))
  blank_lopvm_issn <- c(blank_lopvm_issn, blank_lopvm_issn2)
  
  new_locations_data <- locations_data
  new_locations_data$ALPVM <- as.character(new_locations_data$ALPVM)
  new_locations_data$LOPVM <- as.character(new_locations_data$LOPVM)
  
  for (rownumber in blank_alpvm_issn) {
    issn <- as.character(new_locations_data[rownumber, "ISSN"])
    new_locations_data[rownumber, "ALPVM"] <-
      get_alpvm_for_issn_from_metadata(issn, enriched_newspaper_metadata)
  }
  
  for (rownumber in blank_lopvm_issn) {
    issn <- as.character(new_locations_data[rownumber, "ISSN"])
    new_locations_data[rownumber, "LOPVM"] <-
      get_lopvm_for_issn_from_metadata(issn, enriched_newspaper_metadata)
  }
  
  return(new_locations_data)
}


enrich_locations_data <- function(locations_data, enriched_newspaper_metadata) {
  locations_data <- subset(locations_data, KAUPUNKI_NORM != "")
  enriched_data <- locations_data[which(locations_data$ISSN %in% enriched_newspaper_metadata$ISSN), ]
  enriched_data <- enrich_locations_data_blank_dates(enriched_data, enriched_newspaper_metadata)
  enriched_data$start_date <- as.Date(enriched_data$ALPVM, "%d.%m.%Y")
  enriched_data$end_date <- as.Date(enriched_data$LOPVM, "%d.%m.%Y")
  enriched_data[is.na(enriched_data[, "end_date"]), "end_date"] <- as.Date("2020-01-01")
  enriched_data$start_year <- as.integer(format(enriched_data$start_date, "%Y"))
  enriched_data$end_year <- as.integer(format(enriched_data$end_date, "%Y"))
  enriched_data$KAUPUNKI_NORM <- as.character(enriched_data$KAUPUNKI_NORM)
  return(enriched_data)
}


get_publications_per_year_per_location <- function(enriched_locations_data, year_range) {
  return_data <- data.frame(year = integer(), location = character(), newspapers = integer())
  # year_range_subset <- subset(enriched_locations_data,
  #                             end_year >= year_range[1],
  #                             start_year <= year_range[length(year_range)])
  for (year in year_range) {
    year_subset <- subset(enriched_locations_data, start_year <= year & end_year >= year)
    municipalities <- unique(year_subset$KAUPUNKI_NORM)
    for (municipality in municipalities) {
      municipality_total_papers <- sum(year_subset$KAUPUNKI_NORM == municipality)
      new_row <- data.frame(year = as.integer(year),
                            location = as.character(municipality),
                            newspapers = as.integer(municipality_total_papers))
      return_data <- rbind(return_data, new_row)
    }
  }
  return(return_data)
}


enrich_locations_data_with_coordinates <- function(locations_data, field_name, coordinates_data) {
  coordinates_data$Kunta <- as.character(coordinates_data$Kunta)
  locations_data[, "lat"] <- as.numeric(NA)
  locations_data[, "lon"] <- as.numeric(NA)
  
  for (row in rownames(locations_data)) {
    municipality <- locations_data[row, field_name]
    if (municipality %in% coordinates_data$Kunta) {
      locations_data[row, "lat"] <- coordinates_data[coordinates_data$Kunta == municipality, "Latitude"]
      locations_data[row, "lon"] <- coordinates_data[coordinates_data$Kunta == municipality, "Longitude"]
    }
  }
  return(locations_data)
}


add_locations_data_to_metadata_year_subset <- function(metadata_subset, enriched_locations_data, year) {
  enriched_locations_data_subset <- subset(enriched_locations_data, start_year <= year & end_year >= year)
  enriched_locations_data_subset$KAUPUNKI_NORM <- as.character(enriched_locations_data_subset$KAUPUNKI_NORM)
  enriched_locations_data_subset$ISSN <- as.character(enriched_locations_data_subset$ISSN)
  metadata_subset[, "location"] <- as.character(NA)
  for (row in rownames(metadata_subset)) {
    issn <- as.character(metadata_subset[row, "ISSN"])
    if (issn %in% enriched_locations_data_subset$ISSN) {
      location <- enriched_locations_data_subset[(enriched_locations_data_subset$ISSN == issn), "KAUPUNKI_NORM"]
      if (length(location) > 1) {
        print(paste0("got multiple hits for ISSN ", issn
                     , " location should prob handle this somehow. Just taking the first one for now."))
        # maybe check if end dates match or something
        location <- location[1]
      }
      metadata_subset[row, "location"] <- location
    }
  }
  return(metadata_subset)
}

# R version 3.4.3
# author: Ville Vaara

source("code/lib/functions_metadata.R")
source("code/lib/functions_location.R")
source("code/lib/functions_provinciality.R")
source("code/lib/functions_helpers.R")
source("code/lib/functions_output.R")

library(reshape2)
library(ggplot2)


get_books_per_year_range <- function(fennicadata, start_year, end_year) {
  results_data <- data.frame(year = integer(), books = integer())
  
  for (year in start_year:end_year) {
    year_subset <- subset(fennicadata, publication_year == year)
    year_subset_count <- nrow(year_subset)
    new_row <- data.frame(year = year, books = year_subset_count)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_papers_per_year_range <- function(newspapers_provinciality_plot_data, start_year, end_year) {
  results_data <- data.frame(year = integer(), papers = integer())
  
  for (query_year in start_year:end_year) {
    year_subset <- subset(newspapers_provinciality_plot_data, year == query_year)
    year_subset_count <- sum(year_subset$value, na.rm = TRUE)
    new_row <- data.frame(year = query_year, papers = year_subset_count)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_units_per_decade <- function(units_per_year) {
  colnames(units_per_year) <- c("year", "units")
  first_decade <- ceiling(min(units_per_year$year) / 10) * 10
  last_decade <- floor(max(units_per_year$year - 1) / 10) * 10
  
  decades <- seq(first_decade, last_decade, 10)
  
  results_data <- data.frame(decade = integer(), units_avg = double())
  
  for (decade in decades) {
    decade_subset <- subset(units_per_year, year >= decade & year < decade + 10)
    decade_average <- sum(decade_subset$units) / 10
    new_row <- data.frame(decade = decade, units_avg = decade_average)
    results_data <- rbind(results_data, new_row)
  }
  
  return(results_data)
}


get_books_per_paper_decade <- function(books_data, papers_provinciality_plotdata, start_year, end_year) {
  books <- get_books_per_year_range(books_data, start_year, end_year)
  papers <- get_papers_per_year_range(papers_provinciality_plotdata, start_year, end_year)
  
  books_decade <- get_units_per_decade(books)
  papers_decade <- get_units_per_decade(papers)
  
  books_per_paper_decade <- books_decade
  books_per_paper_decade$papers <- papers_decade$units_avg
  books_per_paper_decade$ratio <- books_per_paper_decade$units_avg / books_per_paper_decade$papers
  
  # replace Inf in ratio with NA
  books_per_paper_decade[books_per_paper_decade$ratio == Inf | is.na(books_per_paper_decade$ratio),
                         "ratio"] <- NA 
  
  return(books_per_paper_decade)
}


get_books_per_paper_decade_plotdata <- function(newspapers_provinciality_plot_data,
                                                fennica_data,
                                                start_year = 1820,
                                                end_year = 1910,
                                                location = NA) {
  
  if (!is.na(location)) {
    newspapers_location <- subset(newspapers_provinciality_plot_data_all_fin, variable == location)
    if (location == "Viipuri") {
      fennicadata_location <- subset(fennica_data, publication_place == "Vyborg" | publication_place == "Viipuri")
    } else {
      fennicadata_location <- subset(fennica_data, publication_place == location)
    }
  } else {
    newspapers_location <- newspapers_provinciality_plot_data
    fennicadata_location <- fennica_data
  }
  
  books_per_paper_decade <-
    get_books_per_paper_decade(books = fennicadata_location,
                               papers = newspapers_location,
                               start_year = 1820,
                               end_year = 1910)
  
  books_per_paper_decade$location <- location
  
  names(books_per_paper_decade) <- c("time", "books", "papers", "ratio", "location")
  return(books_per_paper_decade)
}

# Load data
newspaper_base_data <- read.csv("input/raw/newspapers-utf8.csv")
locations_data <- read.csv("input/raw/publication_locations-utf8.csv")

# Preprocess data
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN" & JULKAISUMAA == "FI" & KIELI == "fin")
enriched_newspapers_locations_data <- enrich_locations_data(locations_data, newspapers_subset)
fennicadata <- read.csv("input/raw/fennica_loc_and_time.csv", stringsAsFactors = FALSE)

# prepare plotdata
start_year <- 1820
end_year <- 1910
newspapers_provinciality_plot_data_all_fin <-
  get_provinciality_plot_data(newspapers_subset,
                              enriched_newspapers_locations_data,
                              start_year:end_year)
newspapers_provinciality_plot_data_all_fin$variable <- as.character(newspapers_provinciality_plot_data_all_fin$variable)
newspapers_provinciality_plot_data_all_fin[newspapers_provinciality_plot_data_all_fin$variable == "Viipuri", ]$variable <- "Vyborg"
fennicadata_all_fin <- subset(fennicadata, language.Finnish == TRUE)


# fix countries in fennica data
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Vyborg" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Sortavala" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin[fennicadata_all_fin$publication_place == "Jääski" & !is.na(fennicadata_all_fin$publication_place), "country"] <- "Finland"
fennicadata_all_fin_finland <- subset(fennicadata_all_fin, country == "Finland")

# prepare plotdata
books_per_paper_fin_1820_1910_decade <-
  get_books_per_paper_decade(books = fennicadata_all_fin_finland,
                             papers = newspapers_provinciality_plot_data_all_fin,
                             start_year = start_year,
                             end_year = end_year)

books_per_paper_decade_plotdata_top5 <- data.frame(time=numeric(), books=numeric(), papers=numeric(), ratio=numeric(), location=character())

for (this_loc in c("Vyborg", "Helsinki", "Turku", "Oulu", "Vaasa")) {
  books_per_paper_decade_plotdata_this_loc <-
    get_books_per_paper_decade_plotdata(newspapers_provinciality_plot_data_all_fin,
                                        fennicadata_all_fin,
                                        start_year = start_year,
                                        end_year = end_year,
                                        location = this_loc)
  books_per_paper_decade_plotdata_top5 <- rbind(books_per_paper_decade_plotdata_top5,
    books_per_paper_decade_plotdata_this_loc)
}

# Plot visuals
cbPalette <- c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442")

# Create plot
books_per_papers_locations_top5_combined_plot <- ggplot() +
  geom_col(data = books_per_paper_fin_1820_1910_decade,
           aes(x = decade, y = ratio), alpha = 0.3, colour="black") +
  geom_line(data = books_per_paper_decade_plotdata_top5,
            aes(x = time, y = ratio, colour = location),
            size = 2) +
  theme_gray() +
  scale_x_continuous(breaks = seq(1800, 1920, by=10)) +
  scale_y_continuous(breaks=seq(0,1000,by=10)) +
  scale_colour_manual(values = cbPalette) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Year", y = "Books per newspaper", fill = "Event:", colour = "Location:")

# Check plot in RStudio
books_per_papers_locations_top5_combined_plot

# Save plot
save_plot_png(plot = books_per_papers_locations_top5_combined_plot, plotname = "fig12_fin_books_per_papers", size_preset = "large")

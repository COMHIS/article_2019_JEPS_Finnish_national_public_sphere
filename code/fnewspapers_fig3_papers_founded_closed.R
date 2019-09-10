# R version 3.4.3
# author: Ville Vaara

library(ggplot2)
library(reshape2)

source("code/lib/functions_metadata.R")
source("code/lib/functions_location.R")
source("code/lib/functions_circulation.R")
source("code/lib/functions_helpers.R")
source("code/lib/functions_output.R")

# Load data
circulation_data <- read.csv("input/raw/finnish-newspapers-data/processed/circulation-utf8.csv", stringsAsFactors = FALSE)
newspaper_base_data <- read.csv("input/raw/finnish-newspapers-data/processed/newspapers-utf8.csv", stringsAsFactors = FALSE)
locations_data <- read.csv("input/raw/finnish-newspapers-data/processed/publication_locations-utf8.csv", stringsAsFactors = FALSE)

# Preprocess data
enriched_circulation_data <- enrich_circulation_data(circulation_data)
cleaned_circulation_data <- clean_uncomplete_circulation_data(enriched_circulation_data)
enriched_newspaper_metadata <- enrich_newspaper_metadata(newspaper_base_data)
newspapers_only_subset <- subset(enriched_newspaper_metadata, AINYLEISMAARE == "SAN")
enriched_locations_data <- enrich_locations_data(locations_data, enriched_newspaper_metadata)

# Setup data for plotting.
old_and_new <- get_start_and_end_years_for_year_range(newspapers_only_subset, 1810:1920)
old_and_new_plotdata <- old_and_new[, c("year", "new_papers", "closed_papers")]
colnames(old_and_new_plotdata) <- c("Year", "Founded", "Closed")
old_and_new_plotdata$Closed <- old_and_new_plotdata$Closed * (-1)
old_and_new_plotdata$Difference <- old_and_new_plotdata$Founded + old_and_new_plotdata$Closed
old_and_new_plotdata <- melt(old_and_new_plotdata, id = "Year")

diffdata <- old_and_new_plotdata
diffdata <- diffdata[which(diffdata$variable == "Difference"),]

# Set visuals variables for plotting.
background_events <- read.csv("input/raw/finnish-newspapers-data/processed/censorship_events.csv", stringsAsFactors = FALSE)
# background_events$event <- factor(background_events$event,
# levels = as.character(background_events$event))
cbPalette <- c("#999999", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442")
#               grey       orange     green      dark blue  red         pink      light blue yellow
fill_pal <- c("#009E73", "#E69F00", "#999999")

# Create plot
start_vs_end_plot <- ggplot() +
  geom_rect(data = background_events,
            mapping = aes(xmin = start_year, xmax = end_year, ymin = -Inf, ymax = Inf),
            alpha = 0.2, size = 0) +
  geom_col(data = old_and_new_plotdata, position = "identity",
           aes(x = Year, y = value, fill = variable), colour="black") +
  geom_line(data = diffdata,
            aes(x = Year, y = value), # , colour = "Diffence"
            size = 1) +
  scale_fill_manual(values = fill_pal) +
  theme_gray() +
  scale_x_continuous(breaks = seq(1800, 1920, by=10)) +
  scale_y_continuous(breaks=seq(-1000,1000,by=10)) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Year", y = "Founded and closed papers", fill = "Event:")

# Check plot in R-Studio ...
start_vs_end_plot

# Save plot
save_plot_png(plot = start_vs_end_plot, plotname = "fig3_newspapers_founded_closed", size_preset = "large")


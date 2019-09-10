library(ggplot2)
library(scales)
source("code/lib/functions_output.R")
source("code/lib/functions_helpers.R")


get_circulation_plotdata <- function(circulation_data) {
  circulation_plot_data <- data.frame(issn = character(),
                                      title = character(),
                                      lang = character(),
                                      location = character(),
                                      year = integer(),
                                      circulation = integer(),
                                      estimate = character())
  i <- 1
  while (i <= nrow(circulation_data)) {
    for (year in 1800:1860) {
      circulation1 <- as.character(circulation_data[i, paste0("levikki_", year)]) 
      
      if (!is.na(circulation1)) {
        if (circulation1 != "") {
          issn = circulation_data[i, "ISSN"]
          title = circulation_data[i, "PAANIMEKE"]
          lang = circulation_data[i, "KIELI"]
          location = circulation_data[i, "KAUPUNKI_NORM"]
          if (location == "") {
            location <- "unknown"
          }
          circulation = as.integer(gsub("[^0-9]", "", circulation1))
          estimate = gsub("[0-9]", "", circulation1)
          
          new_row <- data.frame(issn = issn, title = title, lang = lang, year = year,
                                location = location,
                                circulation = circulation, estimate = estimate)
          circulation_plot_data <- rbind(circulation_plot_data, new_row)
        }
      }
    }
    i <- i + 1
  }
  return(circulation_plot_data)
}  


get_langdata_summary <- function(plot_lang_data) {
  years = 1800:1860
  langdata_sum <- data.frame(year = integer(), circulation = integer(), lang = character())
  for (year in years) {
    year_subset <- plot_lang_data[which(plot_lang_data$year == year),]
    year_swe <- sum(year_subset[which(year_subset$lang == "Swedish"), "circulation"], na.rm = T)
    langdata_sum_sweline <- data.frame(year = year, circulation = year_swe, lang = "Swedish", stringsAsFactors = F)
    year_fin <- sum(year_subset[which(year_subset$lang == "Finnish"), "circulation"], na.rm = T)
    langdata_sum_finline <- data.frame(year = year, circulation = year_fin, lang = "Finnish", stringsAsFactors = F)
    langdata_sum <- rbind(langdata_sum, langdata_sum_sweline)
    langdata_sum <- rbind(langdata_sum, langdata_sum_finline)
  }
  return(langdata_sum)
}

# options(scipen = 1000000)

circulation_data <- read.csv("input/raw/finnish-newspapers-data/processed/circulation_1800-1860_nontidy.csv", stringsAsFactors = F)
circulation_plot_data <- get_circulation_plotdata(circulation_data)
circulation_plot_data <- df_factors_to_string(circulation_plot_data)
circulation_plot_data$location[which(circulation_plot_data$location == "Viipuri")] <- "Vyborg"

cbPalette <- c("#999999", "#E69F00", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442")
#               grey       orange     green      dark blue  red         pink      light blue yellow

plot_lang_data <- circulation_plot_data[which(circulation_plot_data$lang != "ger"),]
plot_lang_data$lang[which(plot_lang_data$lang == "fin")] <- "Finnish"
plot_lang_data$lang[which(plot_lang_data$lang == "swe")] <- "Swedish"
plot_lang_data_sum <- get_langdata_summary(plot_lang_data)

plot_languages <-
  ggplot(plot_lang_data_sum,
         aes(x = year, y = circulation)) + 
  geom_bar(stat = "identity", aes(fill = lang), width = 1, colour="black") +
  theme_gray() +
  scale_x_continuous(breaks = seq(1800, 1920, by=10)) +
  scale_fill_manual(values = c("#56B4E9", "#F0E442")) +
  scale_y_continuous(breaks=seq(0,100000,by=1000)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = "Year", y = "Circulation", fill = "Language:") +
  theme(legend.position="bottom") 

# check plot in RStudio
plot_languages

# save plot png
save_plot_png(plot = plot_languages, plotname = "fig4_circulation_fin_swe", size_preset = "large")

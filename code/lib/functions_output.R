library(ggplot2)

save_plots_png <- function(plots, prefix, outputdir = "output/figures/", size_preset = "small") {
  plot_filename_prefix <- paste0(prefix, "_", Sys.Date(), "_")
  
  if (size_preset == "small") {
    preset_width <- 6
    preset_height <- 4
    preset_dpi <- 300
  } else {
    preset_width <- 7
    preset_height <- 4
    preset_dpi <- 300
  }
  
  
  for (item in 1:length(plots)) {
    filename <- paste0(outputdir,
                       plot_filename_prefix,
                       names(plots)[item],
                       ".png")
    ggsave(filename, plots[[item]],
           width = preset_width,
           height = preset_height,
           dpi = preset_dpi)  
  }
}


save_plot_png <- function(plot, plotname, outputdir = "output/figures/", size_preset = "small") {
  if (size_preset == "small") {
    preset_width <- 6
    preset_height <- 4
    preset_dpi <- 300
  } else {
    preset_width <- 7
    preset_height <- 4
    preset_dpi <- 300
  }
  filename <- paste0(outputdir,
                     plotname,
                     ".png")
  ggsave(filename, plot,
         width = preset_width,
         height = preset_height,
         dpi = preset_dpi)  
}

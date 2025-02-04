# Setup -------------------------------------------------------------------
# Loading libraries
library(tidyverse)
library(lubridate)
library(plyr)
library(ggpubr)
library(doParallel)
library(colorspace)
registerDoParallel(cores = 8)

# Setting a directory 
base_URL <- "East_Coast_extreme_SST/OISST"

# Loading the event files (These are the events produced after using the R package)
hot_files <- list.files(path = base_URL, pattern = "_MHW_protoevents.csv", recursive = TRUE, full.names = TRUE)
cold_files <- list.files(path = base_URL, pattern = "_MCS_protoevents.csv", recursive = TRUE, full.names = TRUE)

load_fun <- function(file) {
  events <- read_csv(file)
  return(events)
}

# Loading in the MHW files for all the dates >= January 2021 (This is when we started seeing the various biological effects)
MHW_events <- tibble(filename = hot_files) %>%
  mutate(file_contents = map(filename, ~ load_fun(.)),
         filename = basename(filename)) %>% 
  unnest(cols = file_contents) %>% 
  filter(t >= "2021-01-01",
         event == TRUE)

# Loading in the MHW files for all the dates >= January 2021 (This is when we started seeing the various biological effects)
MCS_events <- tibble(filename = cold_files) %>%
  mutate(file_contents = map(filename, ~ load_fun(.)),
         filename = basename(filename)) %>% 
  unnest(cols = file_contents) %>% 
  filter(t >= "2021-01-01",
         event == TRUE)

MCS_events$class <- rep("cold", length = nrow(MCS_events))
MHW_events$class <- rep("hot", length = nrow(MHW_events))
plt.dat <- rbind(MCS_events, MHW_events)
plt.dat$exceedance <- plt.dat$temp - plt.dat$thresh
rm(list = c("MHW_events", "MCS_events"))


# Make the plots ----------------------------------------------------------

source("R_scripts/plot_layers.R")
# source("R_scripts/plot_theme.R")

detail_xlim <- c(15, 35)
detail_ylim <- c(-36.25, -27.5)

dates <- c("2021-01-02", "2021-01-02")

# Creating the animation plot
anim_plot <- function(data) {
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = lon, y = lat, fill = temp - thresh)) + # Fill (colours- temprature - thresh)
    get("OISST_AC_layers_zoomed") + # This is the region where the data is collected from
    scale_fill_continuous_diverging(palette = "Blue-Red 3", mid = 0, # Blue represent cool temperatures with red representing high water temperatures
                                    limits = c(-1.9, 4.2), breaks = c(-2, -1, 0, 1, 2, 3, 4)) + # Setting the limits on the y axis
    coord_sf(xlim = detail_xlim, ylim = detail_ylim, expand = FALSE) +
    guides(alpha = "none", # Theme guidelines
           fill = guide_colourbar(title = "[°C]",
                                  frame.colour = "black",
                                  frame.linewidth = 0.4,
                                  ticks.colour = "black",
                                  barheight = unit(50, units = "mm"),
                                  barwidth = unit(4, units = "mm"),
                                  draw.ulim = F,
                                  title.position = 'top',
                                  title.hjust = 0.5,
                                  label.hjust = 0.5)) +
    labs(title = paste0("OISST Extreme SST"), # Plot labels
         subtitle = unique(data$t)) +
    theme_map()
  
  ggsave(filename = paste0("animation/OISST_animation_sequence_",
                                  as.character(unique(data$t)), ".jpg"),
         plot = plot, width = 2.6, height = 1.3125, scale = 2.6) # Plot and changing with the date factor
}

plt.dat %>%
  # dplyr::filter(t < "2021-01-03") %>%
  dplyr::group_split(t) %>%
  purrr::map(.f = anim_plot)


# Animate... --------------------------------------------------------------

# animate in the terminal using ffmpeg:
# ffmpeg creates animations from a series of pictures
ffmpeg \
-framerate 5 \
-pattern_type glob -i 'OISST_*.jpg' \
-vf scale=1280:-2 \
OISST_Extreme_events_animate.mp4 \
;

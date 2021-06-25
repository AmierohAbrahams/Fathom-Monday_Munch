# MUR_event_plot.R

# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(plyr)
library(ggpubr)
library(doParallel)
library(colorspace)
registerDoParallel(cores = 8)

base_URL <- "East_Coast_extreme_SST/OISST"

hot_files <- list.files(path = base_URL, pattern = "_MHW_protoevents.csv", recursive = TRUE, full.names = TRUE)
cold_files <- list.files(path = base_URL, pattern = "_MCS_protoevents.csv", recursive = TRUE, full.names = TRUE)

load_fun <- function(file) {
  events <- read_csv(file)
  return(events)
}

MHW_events <- tibble(filename = hot_files) %>%
  mutate(file_contents = map(filename, ~ load_fun(.)),
         filename = basename(filename)) %>% 
  unnest(cols = file_contents) %>% 
  filter(t >= "2020-12-01",
         event == TRUE)

MCS_events <- tibble(filename = cold_files) %>%
  mutate(file_contents = map(filename, ~ load_fun(.)),
         filename = basename(filename)) %>% 
  unnest(cols = file_contents) %>% 
  filter(t >= "2020-12-01",
         event == TRUE)

MCS_events$class <- rep("cold", length = nrow(MCS_events))
MHW_events$class <- rep("hot", length = nrow(MHW_events))
plt.dat <- rbind(MCS_events, MHW_events)
plt.dat$exceedance <- plt.dat$temp - plt.dat$thresh
rm(list = c("MHW_events", "MCS_events"))


# Make the plots ----------------------------------------------------------

source("R_scripts/plot_layers.R")

detail_xlim <- range(SWIO_bathy$lon) + c(-0.01, 0.01)
detail_ylim <- range(SWIO_bathy$lat)

anim_plot <- function(data) {
  
  plot <- ggplot() +
    geom_tile(data = data, aes(x = lon, y = lat, fill = temp - thresh)) +
    get("SWIO_layers_large") +
    scale_fill_continuous_diverging(palette = "Blue-Red 3", mid = 0,
                                    limits = c(-4.6, 7.05), breaks = c(-4.5, -2.25, 0, 3.5, 7.)) +
    coord_sf(xlim = detail_xlim, ylim = detail_ylim, expand = FALSE) +
    guides(alpha = "none",
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
    labs(title = paste0("OISST Extreme SST"),
         subtitle = unique(data$t)) +
    theme_map()
  
  ggsave(filename = paste0("animation/OISST_animation_sequence_",
                           as.character(unique(data$t)), ".jpg"),
         plot = plot, width = 3, height = 2.7, scale = 2.3)
}

plt.dat %>%
  # dplyr::filter(t == "2020-12-01") %>%
  dplyr::group_split(t) %>%
  purrr::map(.f = anim_plot)


# Animate... --------------------------------------------------------------

# animate in the terminal using ffmpeg:
ffmpeg \
-framerate 2 \
-pattern_type glob -i 'OISST_*.jpg' \
-vf scale=1280:-2 \
OISST_Extreme_events_animate.mp4 \
;

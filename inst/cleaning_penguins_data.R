# Code for cleaning the penguin data
# Data downloaded from https://dasl.datadescription.com/datafile/penguins/?_sfm_methods=Regression&_sfm_cases=4+59943&sf_paged=9

# Information from webpage where the data is from:
# Source: Jessica Meir personal communication
# Number of Cases: 125
# Story:
#   Emperor penguins are the most accomplished divers among birds, making routine
# dives of 5–12 minutes, with the longest recorded dive over 27 minutes. These
# birds can also dive to depths of over 500 meters! Since air-breathing animals like
# penguins must hold their breath while submerged, the duration of any given dive
# depends on how much oxygen is in the bird’s body at the beginning of the dive, how
# quickly that oxygen gets used, and the lowest level of oxygen the bird can tolerate.
# The rate of oxygen depletion is primarily determined by the penguin’s heart rate.
# Consequently, studies of heart rates during dives can help us understand how these
# animals regulate their oxygen consumption in order to make such impressive dives.The researchers equipped emperor penguins with devices that record their heart rates during
# dives. The dataset reports Dive Heart Rate (beats per minute), the Duration
# (minutes) of dives, and other related variables.

# Load libraries
library(dplyr)

# Read in the penguin data
penguins_raw <- read.delim("./data-raw/penguins.txt")

# Clean the penguin data
penguins <- penguins_raw %>%
  rename(heartrate = Dive.Heart.Rate,
         depth = Depth.m.,
         duration = Duration.min.,
         bird = Bird.) %>%
  mutate(bird = as.factor(as.numeric(bird))) %>%
  arrange(bird)

# Add the cleaned penguins data to the data folder
devtools::use_data(penguins, overwrite = TRUE)


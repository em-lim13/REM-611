# September 18, 2020
# Code to analyze the Aquatic Conservation Group Project
# Written by Em Lim

# Load packages and data -----

library(ggplot2)
library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)

species_data <- read.csv("2020_Sept_species_x_site.csv")
site_data <- read.csv("2020_Sept_site_x_environment.csv")

# Species Richness -----
site_data$shannon <- (diversity(species_data, index = "shannon")) #makes a new column in site data with the shannon values
site_data$simpson <- (diversity(species_data, index = "simpson"))


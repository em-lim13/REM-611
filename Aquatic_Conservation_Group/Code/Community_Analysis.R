# September 18, 2020
# Code to analyze the Aquatic Conservation Group Project
# Written by Em Lim

# Load packages and data -----

library(ggplot2)
library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
install.packages("googlesheets4")
library(googlesheets4)

species_data <- read.csv("2020_Sept_species_x_site.csv")
site_data <- read.csv("2020_Sept_site_x_environment.csv")

# Read sheets from google drive
site <- googledrive::drive_get("2020_Sept_site_x_environment") %>% 
  read_sheet()
site_data <- as.data.frame(site)

species <- googledrive::drive_get("2020_Sept_species_x_site") %>%
  read_sheet()
species_data <- as.data.frame(species)

# Species Richness -----
site_data$shannon <- (diversity(species_data, index = "shannon")) #makes a new column in site data with the shannon values
site_data$simpson <- (diversity(species_data, index = "simpson"))


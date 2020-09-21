# September 18, 2020
# Code to analyze the Aquatic Conservation Group Project
# Written by Em Lim

# Load packages and data -----

library(ggplot2)
library(tidyverse)
library(vegan)
library(lme4)
library(lmerTest)
library(googlesheets4)
library(ggfortify)


# Remove this from final analysis, this is data for practice
species_data<-read.csv("species_x_site.csv")
site_data<-read.csv("site_x_environment.csv")
site_data$Location<-as.factor(site_data$Location)

# Read sheets from google drive
site <- googledrive::drive_get("2020_Sept_site_x_environment") %>% 
  read_sheet()
site_data <- as.data.frame(site)

species <- googledrive::drive_get("2020_Sept_species_x_site") %>%
  read_sheet()
species1 <- as.data.frame(species)

#species_data <- species1 %>%
#  mutate_if(is.numeric, as.integer)

species_data <- subset(species1, select = -quadrat )

# Species Richness -----
site_data$shannon <- (diversity(species_data, index = "shannon")) #makes a new column in site data with the shannon values
site_data$simpson <- (diversity(species_data, index = "simpson"))


model_lm <-lm(shannon ~ beach, data = site_data)
anova(model_lm)


# PERMANOVA -----

# adonis
dissim.mat <- vegdist(species_data, method="horn")
adonis(dissim.mat ~ Side*Total_distance_m, data=site.data, strata = site.data$Location, permutations=9999)

# PCA -----

# this creates the PCA values
pca_values <- 
  prcomp(species_data,
         center = TRUE, 
         scale = TRUE)

summary(pca_values)
str(pca_values)

# quick and dirty plot
autoplot(pca_values, 
         loadings = TRUE,
         loadings.label = TRUE) 

# let's add these PCA values to our original dataframe
pca_points <- 
  data.frame(species_data, pca_values$x)

pca_points_site <- cbind(pca_points, site_data)

# now let's pull out the eigenvectors (the arrows showing the loadings)
pca_load <- 
  data.frame(variables = rownames(pca_values$rotation), pca_values$rotation)

pca_load

# we can create a convex hull - the smallest polygon that includes all the points
# of a given level
pca_hull <- 
  pca_points_site %>% 
  group_by(Side) %>% 
  slice(chull(PC1, PC2))


# now let's make the plot
pca_plot <- 
  ggplot(pca_points_site, aes(x = PC1, y = PC2)) +
  # this plots the actual PCA points
  geom_point(aes(colour = Side),
             size = 2, alpha = 0.7) +
  # plot the convex hull 
  geom_polygon(data = pca_hull,
               aes(fill = Side),
               alpha = 0.3,
               show.legend = FALSE) +
  # now let's plot the loadings (arrows denoting eigenvectors)
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   # multiplying by 5 helps fill-out a lot of the empty space
                   # and by multiplying by a constant, you don't lose the
                   # relative magnitude of the loadings to one another
                   xend = PC1*5,
                   yend = PC2*5),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  # you can use the ggrepel package for this as well
  # personally I prefer this method because it's easier to manipulate the label position
  annotate('text', x = (pca_load$PC1*5.7), y = (pca_load$PC2*5.2),
           label = pca_load$variables,
           size = 2.5) +
  theme_classic() 

pca_plot

# Ordination: nMDS -----
myNMDS<-metaMDS(species_data,k=2)
myNMDS #most important: is the stress low? Here it is >0.2 whihc is a bit on the high side
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s ...but it might be nice to label these, and connect samples in the same treatment

# link for graphing help
# https://rpubs.com/CPEL/NMDS

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site_data$beach,draw="polygon",col="grey99",label=T)
orditorp(myNMDS,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site_data$Location,draw="polygon",col="grey99",label=T)
orditorp(myNMDS,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)


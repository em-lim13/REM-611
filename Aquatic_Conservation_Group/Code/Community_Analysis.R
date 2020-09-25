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
library(ggimage)
library(ggrepel)
library(rsvg)


# Remove this from final analysis, this is data for practice
#species_data<-read.csv("species_x_site.csv")
#site_data<-read.csv("site_x_environment.csv")
#site_data$Location<-as.factor(site_data$Location)

# Read sheets from google drive
site <- googledrive::drive_get("2020_Sept_site_x_environment") %>% 
  read_sheet()
site_data <- as.data.frame(site)

species <- googledrive::drive_get("2020_Sept_species_x_site") %>%
  read_sheet()
species1 <- as.data.frame(species)

# cut the quadrant ID from species data for analysis
species_data <- subset(species1, select = -quadrat )

# Species Richness -----
site_data$shannon <- (diversity(species_data, index = "shannon")) #makes a new column in site data with the shannon values
site_data$simpson <- (diversity(species_data, index = "simpson"))

anova_model <- aov(shannon ~ management * beach, data = site_data)
summary(anova_model)


# Graph Shannon diversity -----
theme_set(theme_classic(base_size = 16)) # set default settings

ggplot(data = site_data, aes(beach, shannon)) + 
  geom_boxplot(aes(fill = beach)) +
  labs(y = "Shannon Diversity", x = "Beach") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c('Bluestone Beach','Boulder Beach','Dunbar Beach', 'Whytecliff Park'))

ggsave("../Figures/shannon.png", device = "png",
      height = 9, width = 16, dpi = 400)

# PERMANOVA -----

# adonis
dissim_mat <- vegdist(species_data, method = "horn")
adonis(dissim_mat ~ management + beach, data = site_data, permutations = 9999)
# this looks at species distribution as function of environmental data


# Ordination: nMDS -----
myNMDS <- metaMDS(species_data, k = 2)
myNMDS #most important: is the stress low? Here it is >0.2 whihc is a bit on the high side
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s ...but it might be nice to label these, and connect samples in the same treatment

# link for graphing help
# https://rpubs.com/CPEL/NMDS
# https://www.rpubs.com/RGrieger/545184

ordiplot(myNMDS, type = "n") 
ordihull(myNMDS, groups = site_data$beach,draw = "polygon",col = "grey99",label = T)
orditorp(myNMDS, display = "species", col = "purple4",air = 0.01, cex = 0.9) 
orditorp(myNMDS, display = "sites", cex = 0.75, air = 0.01)

# ordination plot with ggplot
myNMDS <- metaMDS(species_data, k = 2)
my_envfit <- envfit(myNMDS, site_data, permutations = 999)
spp_fit <- envfit(myNMDS, species_data, permutations = 999)

#save NMDS results into dataframe
site_scrs <- as.data.frame(scores(myNMDS, display = "sites")) 

#add grouping variable "Management" to dataframe
site_scrs <- cbind(site_scrs, Beach = site_data$beach) 

#add grouping variable of cluster grouping to dataframe
site_scrs <- cbind(site_scrs, Management = site_data$management) 

head(site_scrs)

spp_scrs <- as.data.frame(scores(spp_fit, display = "vectors")) #save species intrinsic values into dataframe
spp_scrs <- cbind(spp_scrs, Species = rownames(spp_scrs)) #add species names to dataframe
spp_scrs <- cbind(spp_scrs, pval = spp_fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig_spp_scrs <- subset(spp_scrs, pval<=0.05) #subset data to show species significant at 0.05

head(spp_scrs)

env_scores <- as.data.frame(scores(my_envfit, display = "vectors")) #extracts relevant scores from envifit
env_scores <- cbind(env_scores, env.variables = rownames(env_scores)) #and then gives them their names

env_scores <- cbind(env_scores, pval = my_envfit$vectors$pvals) # add pvalues to dataframe
sig_env_scrs <- subset(env_scores, pval<=0.05) #subset data to show variables significant at 0.05

head(env_scores)

#plot
nmds_plot <- ggplot(site_scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site_scrs$Beach), shape = factor(site_scrs$Management)), size = 2)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Beach", shape = "Management")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

nmds_plot + labs(title = "Basic ordination plot") #displays plot

#add species

# order species
signif_spp_scrs1 <- spp_scrs[order(spp_scrs$pval),]

#cut ones we didn't see
signif_spp_scrs_cut <- signif_spp_scrs1[-c(20:23), ]


# add a new column for the file names
images <- c("ian-symbol-sponge-1.svg", "ian-symbol-gammarus-spp.svg", "ian-symbol-sea-anemone-1.svg", "ian-symbol-pachygraspus-marmoratus.svg", "ian-symbol-littoraria-spp.svg", "ian-symbol-littoraria-spp.svg", "ian-symbol-littoraria-spp.svg", "ian-symbol-littoraria-spp.svg", "ian-symbol-bryozoan-colony.svg", "ian-symbol-bryozoan-colony.svg", "ian-symbol-sea-anemone-1.svg", "ian-symbol-oyster.svg", "ian-symbol-hermit-crab.svg", "ian-symbol-seastar-3.svg", "ian-symbol-palolo-viridis.svg", "ian-symbol-barnacle-open.svg", "ian-symbol-palolo-viridis.svg", "ian-symbol-sea-anemone-1.svg", "ian-symbol-mussels-2.svg")


#add image names to dataframe
signif_spp_scrs <- cbind(signif_spp_scrs_cut, images)

nmds_plot + geom_image(data = signif_spp_scrs, by = "height", aes(x = NMDS1, y = NMDS2, image = images), size = 2)
  
#  ggrepel::geom_text_repel(data = spp_scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3, direction = "both", segment.size = 0.25) #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap


#add species images
nmds_plot + geom_image(aes(image = image_col), size = 2)



# EXTRA -------


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
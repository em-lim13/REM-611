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
# make a new data frame for richness metrics 
site_richness <- site_data %>% mutate(
  shannon = (diversity(species_data, index = "shannon")),
  simpson = (diversity(species_data, index = "simpson"))
)

anova_model <- aov(shannon ~ management + beach, data = site_richness)
summary(anova_model)
hist(resid(anova_model))

# Post hoc testing
TukeyHSD(anova_model, "beach")
TukeyHSD(anova_model, "management")


# Graph Shannon diversity -----
theme_set(theme_classic(base_size = 28)) # set default settings

ggplot(data = site_richness, aes(beach, shannon)) + 
  geom_boxplot(aes(fill = beach)) +
  labs(y = "Shannon Diversity", x = "Beach") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) +
  scale_x_discrete(labels = c('Bluestone Beach','Collishaw Point','Dunbar Beach', 'Whytecliff Park'))

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
stressplot(new_NMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

#ugly plot
ordiplot(myNMDS, type = "n") 
ordihull(myNMDS, groups = site_data$beach,draw = "polygon",col = "grey99",label = T)
orditorp(myNMDS, display = "species", col = "purple4",air = 0.01, cex = 0.9)
orditorp(myNMDS, display = "sites", cex = 0.75, air = 0.01)


# Ordination plot with ggplot -----
# link for graphing help
# https://rpubs.com/CPEL/NMDS
# https://www.rpubs.com/RGrieger/545184

set.seed(604671825)
myNMDS <- metaMDS(species_data, k = 2)
my_envfit <- envfit(myNMDS, site_data, permutations = 999)
spp_fit <- envfit(myNMDS, species_data, permutations = 999)


#save NMDS results into dataframe
site_scrs <- as.data.frame(scores(myNMDS, display = "sites")) 

#add grouping variable "Beach" to dataframe
site_scrs <- cbind(site_scrs, Beach = site_data$beach) 

#add grouping variable of cluster grouping to dataframe
site_scrs <- cbind(site_scrs, Management = site_data$management) 

spp_scrs <- as.data.frame(scores(spp_fit, display = "vectors")) #save species intrinsic values into dataframe
spp_scrs <- cbind(spp_scrs, Species = rownames(spp_scrs)) #add species names to dataframe
spp_scrs <- cbind(spp_scrs, pval = spp_fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names

env_scores <- as.data.frame(scores(my_envfit, display = "vectors")) #extracts relevant scores from envifit
env_scores <- cbind(env_scores, env.variables = rownames(env_scores)) #and then gives them their names

env_scores <- cbind(env_scores, pval = my_envfit$vectors$pvals) # add pvalues to dataframe
sig_env_scrs <- subset(env_scores, pval<=0.05) #subset data to show variables significant at 0.05
  # there are no significant variables bc all our data is categorical except distance, which we don't actually care about

# make polygons
ordi_hull <- 
  site_scrs %>% 
  group_by(Beach) %>% 
  slice(chull(NMDS1, NMDS2))



#add species images
# get images here https://ian.umces.edu/imagelibrary/
# convert sdv to png here https://svgtopng.com/

# order species
signif_spp_scrs1 <- spp_scrs[order(spp_scrs$pval),]

#cut ones we didn't see
signif_spp_scrs_cut <- signif_spp_scrs1[-c(20:23), ]


# add a new column for the file names
images <- c("ian-symbol-sponge-1.png", "ian-symbol-amphipod.png","ian-symbol-pachygraspus-marmoratus.png", "ian-symbol-sea-anemone-2.png", "ian-symbol-amphipod.png", "ian-symbol-urosalpinx-cinerea.png", "ian-symbol_cellana_exarata.png", "ian-symbol-littoraria-spp.png",  "ian-symbol-sea-anemone-1.png", "ian-symbol-bryozoan-colony.png", "ian-symbol-bryozoan-colony.png", "ian-symbol-oyster.png",  "ian-symbol-hermit-crab.png", "ian-symbol-seastar-3.png", "ian-symbol-palolo-viridis.png", "ian-symbol-barnacle-open.png", "ian-symbol-palolo-viridis.png", "ian-symbol-sea-anemone-1.png", "ian-symbol-mussels-2.png")


#add image names to dataframe
signif_spp_scrs <- cbind(signif_spp_scrs_cut, images)

#cut species that weren't significant
sig_spp_scrs <- subset(signif_spp_scrs, pval<=0.05) #subset data to show species significant at 0.05


# make the plot
theme_set(theme_classic(base_size = 28)) # set default settings

nmds_plot <- ggplot(site_scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(Beach), shape = factor(Management)), size = 2)+ #adds site points to plot, shape determined by management, colour determined by beach ID
  coord_fixed()+
  theme_classic()+ 
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Beach", shape = "Management")+ # add legend labels for Management and Beach
  theme(legend.position = "right", legend.text = element_text(size = 18, colour = "black"), legend.title = element_text(size = 18, colour = "black"), axis.text = element_text(size = 18, colour = "black")) + # add legend at right of plot
  theme(axis.title = element_text(size = 18))

#add polygons
nmds_plot1 <- nmds_plot + geom_polygon(data = ordi_hull,
                                        aes(fill = Beach),
                                        alpha = 0.2,
                                        show.legend = FALSE)

# add just sig species images
nmds_plot_sig <- nmds_plot1 + geom_image(data = sig_spp_scrs, by = "height", aes(x = NMDS1, y = NMDS2, image = images), size = 0.08)

# add all species images
nmds_plot_all <- nmds_plot1 + geom_image(data = signif_spp_scrs, by = "height", aes(x = NMDS1, y = NMDS2, image = images), size = 0.08)


print(nmds_plot_sig)
ggsave("../Figures/sig_ordination.png", device = "png",
       height = 9, width = 16, dpi = 400)

print(nmds_plot_all)

ggsave("../Figures/all_ordination.png", device = "png",
       height = 9, width = 16, dpi = 400)


# make a plot just for the significant species
#use file sig_spp_scrs

# add a new column for the file names
sig_images <- c("ian-symbol-littoraria-spp.png", "ian-symbol-urosalpinx-cinerea.png", "limpet-shell-illustration-vector-134049223.png", "ian-symbol-pachygraspus-marmoratus.png","ian-symbol-sea-anemone-2.png", "ian-symbol-sea-anemone-1.png", "ian-symbol-sponge-1.png", "ian-symbol-bryozoan-colony.png", "ian-symbol-bryozoan-colony.png",  "ian-symbol-amphipod.png", "ian-symbol-amphipod.png")


#add image names to dataframe
sig_spp_scrs_images <- cbind(sig_spp_scrs, sig_images)

# add species images
sig_nmds_plot <- nmds_plot1 + geom_image(data = sig_spp_scrs, by = "height", aes(x = NMDS1, y = NMDS2, image = sig_images), size = 0.08)

print(sig_nmds_plot)
# looks better with all the species!





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
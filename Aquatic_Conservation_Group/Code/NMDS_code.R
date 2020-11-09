# Load packages and data -----

library(ggplot2)
library(tidyverse)
library(googlesheets4)
library(ggfortify)
library(ggimage)
library(ggrepel)
library(rsvg)

#Load and tidy data -----

# Read sheets from google drive
site <- read_csv("2020_Sept_site_x_environment")

species <- read_csv("2020_Sept_species_x_site")

# cut the quadrant ID from species data for analysis
species_data <- subset(species1, select = -quadrat )


# dissim analysis -----

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
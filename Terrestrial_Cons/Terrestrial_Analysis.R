# September 18, 2020
# Code to analyze the Terrestrial Conservation Group Project
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
library(visreg)
library(PNWColors)
library(car)
library(emmeans)

# Read sheets from google drive
size_data <- googledrive::drive_get("2020_size_data") %>% 
  read_sheet()
size_data <- as.data.frame(size_data) %>% mutate(
  forest = as.factor(forest),
  species = as.factor(species)
)
str(size_data)

# species is a dataframe where each row is a plot
species <- googledrive::drive_get("2020_diversity_data") %>% 
  read_sheet()
species <- as.data.frame(species)
species <- subset(species, select = -plot )


# site is a dataframe where each row is a plot
site <- googledrive::drive_get("2020_site_data") %>% 
  read_sheet()
site <- as.data.frame(site)
site <- subset(site, select = -undergrowth_species )

# make a palette
wood <- pnw_palette(name="Mushroom", n = 6, type="discrete")

# Biodiversity metrics ------

# make a new data frame for richness metrics 
site_richness <- site %>% mutate(
  shannon = (diversity(species, index = "shannon")),
  simpson = (diversity(species, index = "simpson"))
)

# look at the output
shannon_model <- lm(shannon ~ forest, data = site_richness)
summary(shannon_model) # so old growth is the "control" here
# replant has a 0.5095 higher shannon
# second has a 0.2921 higher shannon
# neither difference is significant

# Let's visualize this
visreg(shannon_model)

#check model assumptions
par(mfrow = c(2,2))
plot(shannon_model)
par(mfrow = c(1,1))
hist(resid(shannon_model))
#ehhh it's not great but not horrific

# Graph shannon diversity ------
theme_set(theme_classic(base_size = 28)) # set default settings

ggplot(data = site_richness, aes(forest, shannon)) + 
  geom_boxplot(aes(fill = forest)) +
  labs(y = "Shannon Diversity", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth','Replanted','Second Growth')) +
  scale_fill_manual(values = wood)

ggsave("../Figures/shannon_box.png", device = "png",
       height = 9, width = 16, dpi = 400)

# Size diversity -----
diam_model <- lm(diam_m ~ forest + species, data = size_data)
summary(diam_model)
anova(diam_model)
# so trees in replant and second are statistically smaller in diameter than old by -2.56293 and -1.16436 
# and cedar (and nearly hemlock) are the only ones that differ from alders
  #this isn't really that interesting

height_model <- lm(height_m ~ forest + species, data = size_data)
summary(height_model)
# replant are shorter than old by -15.6052 and second are taller than old by 12.0626
# no difference between the trees, hemlock is almost diff from alder but not quite

# calculate total aboveground biomass????
# from Canadian national biomass equations: new parameter estimates that include British Columbia data - Ung 2008
# hard pass

# variance
leveneTest(diam_m ~ forest, data = size_data)
# p > 0.05 means that the variance IS different! DOPE 

leveneTest(height_m ~ forest, data = size_data)
# p > 0.05 means that the variance IS different! DOPE 

# Graph size -----

# DIAMETER
ggplot(data = size_data, aes(forest, diam_m)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Diameter (m)", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth','Replanted','Second Growth')) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/diam_box.png", device = "png",
       height = 9, width = 16, dpi = 400)

#HEIGHT
ggplot(data = size_data, aes(forest, height_m)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Height (m)", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth','Replanted','Second Growth')) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/height_box.png", device = "png",
       height = 9, width = 16, dpi = 400)


# adonis ------
dissim_mat <- vegdist(species, method = "horn")
adonis(dissim_mat ~ forest, data = site, permutations = 9999)
# this looks at species distribution as function of environmental data
# so yes, different species in different forest stands


# Ordination: nMDS -----
myNMDS <- metaMDS(species, k = 2)
myNMDS #most important: is the stress low? Here it is >0.2 whihc is a bit on the high side
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

#ugly plot
ordiplot(myNMDS, type = "n") 
ordihull(myNMDS, groups = site$forest, draw = "polygon",col = "grey99",label = T)
orditorp(myNMDS, display = "species", col = "purple4",air = 0.01, cex = 0.9)
orditorp(myNMDS, display = "sites", cex = 0.75, air = 0.01)

# September 18, 2020
# Code to analyze the Terrestrial Conservation Group Project
# Written by Em Lim

# Load packages and data -----
# If you haven't installed these packages, do that first
# eg.  install.packages("ggplot2")

library(ggplot2) # graphing
library(tidyverse) # data organization
library(vegan) # biodiversity
library(googlesheets4) # pull data from google drive
library(visreg) # visualize your models
library(PNWColors) # pretty colours for palettes
library(car) # for some statistical analyses (for variance test)


# Read sheets from google drive
size_data <- googledrive::drive_get("2020_size_data") %>% 
  read_sheet()
size_data <- as.data.frame(size_data) %>% mutate(
  forest = factor(forest, levels = c("Old", "Second", "Replant")),
  species = factor(species, levels = c("Cedar", "Hemlock", "Douglas_Fir", "Alder", "Balsam_Poplar", "unknown")),
  dummy = "a",
  log_diam = log(diam_m),
  log_height = log(height_m),
  ratio = height_m/diam_m
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
wood <- pnw_palette(name = "Mushroom", n = 6, type="discrete")
wood2 <- c("brown4", wood)

# Biodiversity metrics ------

# make a new data frame for richness metrics 
site_richness <- site %>% mutate(
  shannon = (diversity(species, index = "shannon")),
  simpson = (diversity(species, index = "simpson"))
)

# look at the output
shannon_model <- lm(shannon ~ forest, data = site_richness)
anova(shannon_model) # so old growth is the "control" here
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
theme_set(theme_classic(base_size = 20)) # set default settings

ggplot(data = site_richness, aes(forest, shannon)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8, fatten = 3) +
  labs(y = "Shannon Diversity", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c("Old Growth", "Second Growth", "Replanted")) +
  scale_fill_manual(values = wood)

ggsave("../Figures/shannon_box.png", device = "png",
       height = 6, width = 10, dpi = 400)


# Compare variance in diam and height between forests ----

# variance
leveneTest(diam_m ~ forest, data = size_data)
# p > 0.05 means that the variance IS different! DOPE 

leveneTest(height_m ~ forest, data = size_data)
# p > 0.05 means that the variance IS different! DOPE 


# Diameter diversity -----
diam_model <- lm(diam_m ~ forest + species, data = size_data)
summary(diam_model)
anova(diam_model)
# so trees in replant are larger than old by 0.58855 and second are  smaller in diameter than old by -0.85675 

# assumptions?
par(mfrow = c(2,2))
plot(diam_model)
par(mfrow = c(1,1))
hist(resid(diam_model))

# yeah that looks prettyyyy gnarly...
# let's look at a log model
log_diam_model <- lm(log_diam ~ forest + species, data = size_data)
summary(log_diam_model)
anova(log_diam_model)
# intercept is no longer signif
# rest is pretty much the same

# assumptions?
par(mfrow = c(2,2))
plot(log_diam_model)
par(mfrow = c(1,1))
hist(resid(log_diam_model))

# Which does AIC like better
AIC(diam_model, log_diam_model)
# oh woah the untransformed model is much better

# Graph DIAMETER -----
ggplot(data = size_data, aes(forest, diam_m)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Diameter (m)", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth', "Second Growth",'Replanted')) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/diam_box.png", device = "png",
       height = 6, width = 10, dpi = 400)


# Height diversity ------
height_model <- lm(height_m ~ forest + species, data = size_data)
summary(height_model)
anova(height_model)
# second are taller than old by 12.0626 and replants are shorter by -15.6052 
# no difference between the trees, hemlock is almost diff from alder but not quite

# assumptions?
par(mfrow = c(2,2))
plot(height_model)
par(mfrow = c(1,1))
hist(resid(height_model))

# yeah again not the best
# let's try log
log_height_model <- lm(log_height ~ forest + species, data = size_data)
summary(log_height_model)
anova(log_height_model)
# eh pretty much the same

# assumptions?
par(mfrow = c(2,2))
plot(log_height_model)
par(mfrow = c(1,1))
hist(resid(log_height_model))
# yeah it's better looking

# what does AIC prefer
AIC(height_model, log_height_model)
# log is way preferred


# Graph HEIGHT -----
ggplot(data = size_data, aes(forest, height_m)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Height (m)", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth', "Second Growth","Replanted")) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/height_box.png", device = "png",
       height = 6, width = 10, dpi = 400)


# Height:diameter ratios -----
ratio_model <- lm(ratio ~ forest + species, data = size_data)
summary(ratio_model)
anova(ratio_model)

ggplot(data = size_data, aes(forest, ratio)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Height:diamter ratio", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth', "Second Growth","Replanted")) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/ratio_box.png", device = "png",
       height = 6, width = 10, dpi = 400)

# GETTING CREATIVE!!!!! ------
tree_labs <- c("Cedar", "Hemlock", "Douglas Fir", "Alder", "Balsam Poplar", "unknown")

set.seed(66666)
ggplot(aes(x = dummy, y = species, size = diam_m, colour = species), data = size_data) + geom_jitter() + 
  scale_colour_manual(values = wood2, guide = "none") +
  labs(x = "Forest Stand", y = " ", size = "Diameter (m)") +
  facet_wrap(~forest) +
  scale_y_discrete(labels = tree_labs) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA))

ggsave("../Figures/forest_size_species.png", device = "png",
       height = 5, width = 10, dpi = 400)


# adonis ------
dissim_mat <- vegdist(species, method = "horn")
adonis(dissim_mat ~ undergrowth_abundance + soil_moisture  + canopy_cover, data = site, permutations = 9999)
# this looks at species distribution as function of environmental data
# so yes, different species in different forest stands

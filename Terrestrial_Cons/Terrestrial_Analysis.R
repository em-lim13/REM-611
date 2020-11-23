# September 18, 2020
# Code to analyze the Terrestrial Conservation Group Project
# Written by Em Lim

# I have "soft wrap" turned on, so R automatically starts a new line when my code hits the edge of my page
# If you don't have this on, you'll get some super long lines (sorry)
# To turn it on, go Tools > Global Options > Code > check "soft wrap R source files" > Apply !

# Load packages  -----
# If you haven't installed these packages, do that first
# eg.  install.packages("ggplot2")

library(ggplot2) # graphing
library(tidyverse) # data organization
library(vegan) # biodiversity calculations
library(googlesheets4) # pull data from google drive
library(visreg) # visualize your models
library(PNWColors) # pretty colours for palettes
library(car) # for some statistical analyses (for variance test)
library(fmsb) # for radar plots

# Load data ------
# Originally while working on this project I was pulling the data straight from our group's google drive, but now that the project is finished I just downloaded the sheets into the "Data" folder as csv's
# I'm going to hash out the code to retrieve sheets from google docs but keep it for prosterity

#size_data <- googledrive::drive_get("2020_size_data") %>% 
#  read_sheet()
size_data <- read_csv("2020_size_data.csv") # load the data
size_data <- as.data.frame(size_data) %>% mutate(
  forest = factor(forest, levels = c("Old", "Second", "Replant")),
  species = factor(species, levels = c("Cedar", "Hemlock", "Douglas_Fir", "Alder", "Balsam_Poplar", "unknown")),
  dummy = "a",
  log_diam = log(diam_m),
  log_height = log(height_m),
  ratio = height_m/diam_m
)
str(size_data) # check to make sure everything looks good


# species is a dataframe where each row is a plot
#species <- googledrive::drive_get("2020_diversity_data") %>% 
#  read_sheet()
# species <- as.data.frame(species)

species <- read_csv("2020_diversity_data.csv")
species <- subset(species, select = -plot ) # remove plot so it's JUST one species per column and one plot per row. this is the format the vegan package uses


# site is a dataframe where each row is a plot
#site <- googledrive::drive_get("2020_site_data") %>% 
#  read_sheet()
# site <- as.data.frame(site)
site <- read_csv("2020_site_data.csv")
site <- subset(site, select = -undergrowth_species )

# radar data is for making graphs
# numbers calculated: max forest set to 5
  # 5/the value for that forest's metric is the conversion factor
  # other forests = conversion* their metric
# ratio is height/diam, so bigger is worse
# so I flipped it to be diam/height, so larger is better
# then converted to 0-5 scale like the others

# radar <- googledrive::drive_get("radar_data") %>% 
#  read_sheet()
# radar<- as.data.frame(radar)
radar <- read.csv("radar_data.csv")
rownames(radar) <- c(1, 2, "old", "second", "replant")

# make a palette using the PNWColours package
wood <- pnw_palette(name = "Mushroom", n = 6, type="discrete")
wood2 <- c("brown4", wood)
wood3 <- alpha(wood,0.3) # make a fainter palette for the radar

# Biodiversity metrics ------

# make a new data frame for richness metrics 
site_richness <- site %>% mutate(
  shannon = (diversity(species, index = "shannon")),
  richness = apply(species>0,1,sum)
)


# Is shannon signif diff?
shannon_model <- lm(shannon ~ forest, data = site_richness)
anova(shannon_model) 
summary(shannon_model) # so old growth is the "control" here
# replant has a 0.5095 higher shannon
# second has a 0.2921 higher shannon
# neither difference is significant

#Is rich signif diff?
rich_model <- lm(richness ~ forest, data = site_richness)
anova(rich_model)# richness isn't signif either
summary(rich_model) 

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
       height = 6, width = 10, dpi = 400) # use ggsave to save this plot to my Figures folder


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

# yeah that looks prettyyyy gnarly... we also know already that equal variance assumption is violated
# let's look at a log model
log_diam_model <- lm(log_diam ~ forest + species, data = size_data)
summary(log_diam_model)
anova(log_diam_model)
# intercept is no longer signif
# rest is pretty much the same

# assumptions for log model?
par(mfrow = c(2,2))
plot(log_diam_model)
par(mfrow = c(1,1))
hist(resid(log_diam_model))

# Which does AIC like better
AIC(diam_model, log_diam_model)
# oh woah the untransformed model is much better
# stick with untransformed model, ANOVA handles violations pretty well

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

# assumptions for log?
par(mfrow = c(2,2))
plot(log_height_model)
par(mfrow = c(1,1))
hist(resid(log_height_model))
# yeah it's better looking

# what does AIC prefer
AIC(height_model, log_height_model)
# log is way preferred
# lets go with this, then!


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
visreg(ratio_model) #visualize

# Graph height-to-diameter
ggplot(data = size_data, aes(forest, ratio)) + 
  geom_point(aes(colour = forest)) + 
  geom_boxplot(aes(fill = forest), alpha = 0.8) +
  labs(y = "Height:diamter ratio", x = "Forest") +
  theme(legend.position = "none", axis.text = element_text(colour = "black")) + scale_x_discrete(labels = c('Old Growth', "Second Growth","Replanted")) +
  scale_fill_manual(values = wood) + scale_colour_manual(values = wood)

ggsave("../Figures/ratio_box.png", device = "png",
       height = 6, width = 10, dpi = 400)

# GETTING CREATIVE!!!!!  ------
tree_labs <- c("Cedar", "Hemlock", "Douglas Fir", "Alder", "Balsam Poplar", "unknown") # make labels for tree names

set.seed(66666) # set a seed so the jitter function (which uses randomization) makes the same plot each time
ggplot(aes(x = dummy, y = species, size = diam_m, colour = species), data = size_data) + geom_jitter() + 
  scale_colour_manual(values = wood2, guide = "none") +
  labs(x = "Forest Stand", y = " ", size = "Diameter (m)") +
  facet_wrap(~forest) +
  scale_y_discrete(labels = tree_labs) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA))

ggsave("../Figures/forest_size_species.png", device = "png",
       height = 5, width = 10, dpi = 400)

# Tree density stuff ------
density <- species %>%mutate(density = rowSums(.[1:5])) #how many trees per plot
site <- cbind(site, density$density)
site <- site %>% mutate(
  density = density$density
)
# ugh this is not the cleanest/fastest way to do this but whatever it works

#make the model
density_model <- lm(density ~ forest, data = site)
summary(density_model)
anova(density_model) # yeah density is different between sites

# radar plot ------
# Here's the website where I learned to make radar plots! Check it out!
# https://www.r-graph-gallery.com/143-spider-chart-with-saveral-individuals.html

radarchart(radar) # make a basic plot

# let's make a pretty one!
radarchart( radar, axistype=1 , 
            #custom polygon
            pcol=wood , pfcol=wood3 , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(1,5,1), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)

# add a legend
legend(x=0.5, y=1.3, legend = c("Old Growth", "Second Growth", "Replanted"), bty = "n", pch=20 , col=wood , text.col = "black", cex=1.2, pt.cex=3)

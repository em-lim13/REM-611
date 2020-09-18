# This script goes through Principal Component Analyses 
# Part 1: graphing (Helen's mini tutorial)
# Part 2: interpretation and analysis (Jordan's mini tutorial)

# **** NOTE: there are many types of ordination analysis. Make sure you use an appropriate one!***
# Great resource: Oksanen J. (n.d.). Multivariate Analysis of Ecological Communities in R: vegan tutorial:43.
# quick and dirty differences-
# Principal components analysis (PCA): Euclidean distance, linear mapping
# Non-metric multidimensional scaling (NMDS): any dissimilarity metric, nonlinear mapping
# Correspondence analysis (CA): chi-square metric, weighted linear mapping
# Canonical correspondence analysis (CCA): CA but variance is constrained by measured variables (check out Hollarsmith et al. 2020 to see it in action)

# prepping packages and dataframes
library(tidyverse)
library(ggfortify)
library(vegan)
library(lme4)

# you can create a PCA plot with just the tidyverse package
# ggfortify can give you a quick and dirty PCA plot (but it's a bit ugly and has limitations)
# vegan is a huge package for community ecology analyses

#**Now that we know the iris dataset has its origins in racist eugenics science, let's switch to penguins!**

#head(iris)

library(palmerpenguins)
head(penguins) #Adelies! So much fun! 

# remove NAs
penguins <- penguins[complete.cases(penguins), ]


# let's first check the correlation matrix of explanatory variables
# you don't necessarily need to do this before a PCA, but in this case it will
# help illustrate the use of PCAs
corr_mat <- as.matrix(round(cor(penguins[, c(3:6)]), 2))

# remove top half for ease of interpretation
corr_mat[upper.tri(corr_mat)] <- NA
corr_mat

#### PART 1: GRAPHING ####
# ***Now let's make the PCA plot ***
# this creates the PCA values (input is only numeric values)
pca_values <- 
  prcomp(penguins[, c(3:6)],
         center = TRUE, 
         scale = TRUE)

summary(pca_values)
str(pca_values)

# quick and dirty (and ugly) PCA plot
autoplot(pca_values, 
         loadings = TRUE,
         loadings.label = TRUE) 

# you could stop here, but if you want to manipulate your plot and incorporate more
# ggplot features, you can use the code below to do a number of things

# let's add these PCA values to our original dataframe
pca_points <- 
  data.frame(penguins, pca_values$x)

head(pca_points)

# now let's pull out the eigenvectors (the arrows showing the loadings)
pca_load <- 
  data.frame(variables = rownames(pca_values$rotation), pca_values$rotation)

pca_load

# we can create a convex hull - the smallest polygon that includes all the points
# of a given level
pca_hull <- 
  pca_points %>% 
  group_by(species) %>% 
  slice(chull(PC1, PC2))

# now let's make the plot
pca_plot <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  # this plots the actual PCA points
  geom_point(aes(colour = species, shape=sex),
             size = 2, alpha = 0.7) +
  # plot the convex hull 
  geom_polygon(data = pca_hull,
               aes(fill = species),
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

#### PART 2: INTERPRETING ####
# Basedon the plot, it looks like Gentoo penguins are very different 

# 1) What drives these differences? Check out the axis loadings

pca_load
# we used this to define the arrows, but we can also interpret the numbers directly
# PC1: bill length, flipper length, body mass all load positively; bill depth loads neg
# PC2: bill length and depth load strongly neg., flipper length and body mass closer to 0
# therefore, if we have a point in the upper right quarter quadrant: 
#       - it is positive on x-axis (PC1), so it is a penguin with long bill, long flipper, high body mass
#       - it is positive on y-axis (PC2), so it has long flipper and high body mass
# since the Gentoo's differ from the others along the PC1 axis, suggests they have longer bills and flippers, shallower bills, and larger mass
# in this case, they do not differ along PC2 axis, suggests bill length and depth not as strong a driver as flipper length and body mass
# looks like the PC2 axis may be dividing sex 
# **always check your results with your ecological/biological knowledge of the system!**

summary(pca_values)
# if we return to this dataframe, we can see the proportion of variance that each axis represents
# here, PC1 is a whopping 68% of variance, PC2 is only 19%
# this compliments our interpretation of the graph, that species are responsible for most of the variation in the data, and these differentiate along PC1 axis
# proportions of variance add up to 1, there can be many more than 4 axes (n dimensional hyperspace!)


# do PC values differ by species? is there sexual dimorphism in these traits?
# a standard linear model isn't a great choice because variables are so autocorrelated
# so we can use the envfit function in vegan to test the "fit" of "env"ironmental variables
metadata <- penguins[,c(1,2,7)] #the explanatory variables we want to test
test <- envfit(pca_values, metadata, permu= 999)
test

# centroids show us the center of the distributions for species, islands, adn sex
# male and female differentiate alonge both axes, suggesting suggesting they have larger body masses, longer bills and flippers
# Gentoo is the most different (expected) 

# goodness of fit looks at the r2, in this case the fraction of variation explained by each variable
# significance is calculated based on random permutations of the data ("permu=") - "if you get as good or better r2 w/ randomly permuted data, your values are insignificant" (Oksanen)  


# AXES AS EXPLANATORY VARIABLES IN LINEAR MODELS:

# let's say we also had data on the size of target prey species for each individual
# and we wanted to know if body characteristics could predict target prey size
# (unfortunately we don't have these data)
# we could use the PC values for each individual as predictor variables
# it would look something like this:
lmer(prey ~ PC1 + PC2 + (1|island) + (1|species), data=pca_points)

# check out Hollarsmith et al. (2019) to see more - 
# we used PC values to characterize water masses, and used those values to predict oyster growth

# import the oyster_z.csv from the git repo
# subset for just the upwelling season
upwell <- subset(oyster_z, Season=="Upwell")
upwell$Station <- as.factor(upwell$Station)

# quick plot - 
# looks like PC1 axis explains water column variation across stations more than PC2
# harder to discern depth pattern
ggplot(upwell, aes(x = PC1, y = PC2)) +
  geom_point(data=upwell, aes(fill=Station, colour=Station, shape=Depth)) +
  theme_bw()

# linear model - does water column variation explain differences in oyster growth?
# random effect of tile
upwelling.m1 <- lmer(Adj_rel_growth ~ PC1 + PC2 + (1|StationDepthTile),
                     na.action=na.omit,
                     data=upwell)
summary(upwelling.m1)
# higher PC1 strongly associated w/ higher growth
# PC2 marginally so
# positive PC1 associated w/ high DO sat, low nutrients, high pH/low pCO2, high temp
# or, lower growth associated w/ upwelled waters
library(vegan)

setwd("~/Documents/UBC/Year 4-1/BIOL 404/Group Project")

#importing data
species.data<-read.csv("species_x_site.csv")
site.data<-read.csv("site_x_environment.csv")
site.data$Location<-as.factor(site.data$Location)
str(species.data)
str(site.data)

#species richness stuff
diversity(species.data, index = "shannon")#this is the Shannon-Wiener index **** maybe better for us????
diversity(species.data, index = "simpson")#this is the Simpson index
fisher.alpha(species.data) #this is Fisher's alpha from the log-series distribution, fairly independent of sample size

site.data$shannon<-(diversity(species.data, index = "shannon")) #makes a new column in site data with the shannon values
site.data$simpson<-(diversity(species.data, index = "simpson"))

#anova for species diversity using shannon
model_lmer<-lmer(shannon~Side*Total_distance_m+(1|Location), data =site.data)
anova(model_lmer)

#just dock 1 and just dock 2
model_lm_1<-lm(shannon~Side*Total_distance_m+Boat, data =site.data[site.data$Location=="1",])
anova(model_lm_1)

model_lm_2<-lm(shannon~Side*Total_distance_m, data =site.data[site.data$Location=="2",])
anova(model_lm_2)

###########graph shannon
ggplot(data=site.data, aes(x=Location, y=shannon, colour=Side))+geom_boxplot()+
  theme_classic()+labs(x="Dock", y="Shannon Diversity Index")+
  theme(axis.text=element_text(size=23))+theme(text=element_text(size=23))
ggplot(data=site.data, aes(x=Boat, y=shannon, colour=Location))+geom_boxplot()+theme_classic()


mean(site.data$shannon[site.data$Side=="N"])
mean(site.data$shannon[site.data$Side=="S"])

ggplot(data=site.data, aes(x=Total_distance_m, y=shannon))+geom_point()+geom_smooth(method=lm)


#evenness **** MAYBE NOT USEFUL
evenness<-diversity(species.data, index = "shannon")/log(specnumber(species.data)) #this is how to calculate Pielou's J
model_evenness<-lm(evenness~Side*Location*Boat*Total_distance_m, data =site.data)
anova(model_evenness)

# Rarefaction *** probably doesn't matter for us since we had total 100% cover in most spots
  #doesn't really make sense for percent cover data

min(rowSums(species.data)) #this is two functions, nested to be concise (R starts with innermost parantheses): what is the sum of abundances in each site (a row), and what is lowest sum?
site.data$rarefiedS<-rarefy(species.data, 76) #if only 113 individuals had been sampled in each plot, what would the diversity have been?

#now lets see if rarefying the data has affected our results:
model2<-lm(rarefiedS~Side*Location*Boat*Total_distance_m, data =site.data) 
anova(model2)

plot(fisherfit(colSums(species.data))) #here is a plot of the number of species for each "bin" of abundances - note that we have summed abundances over all sites here
#log series is a fairly convincing a descriptor of this data.
#so we can use fishers alpha
site.data$fisher<-fisher.alpha(species.data)


#Rank- abundance plots, with log normal and other fits overlain {vegan}
#
#####################################

#all
plot(rad.preempt(colSums(species.data))) #I first summed abundances across all sites with colSums, then fit a log normal, then plotted it

#1
radlattice(radfit(colSums(species.data[c(1:9,19:27),])))#here are more potential functions for rank-abundance, the lowest AIC is the best if >2 units lower than any other
plot(rad.preempt(colSums(species.data[c(1:9,19:27),]))) #I first summed abundances across all sites with colSums, then fit a log normal, then plotted it

#2
radlattice(radfit(colSums(species.data[c(10:18,28:36),])))#here are more potential functions for rank-abundance, the lowest AIC is the best if >2 units lower than any other
plot(rad.lognormal(colSums(species.data[c(10:18,28:36),]))) #I first summed abundances across all sites with colSums, then fit a log normal, then plotted it

#north
radlattice(radfit(colSums(species.data[c(1:18),])))
plot(rad.preempt(colSums(species.data[c(1:18),]))) 

#south
radlattice(radfit(colSums(species.data[c(19:36),])))
plot(rad.preempt(colSums(species.data[c(16:36),]))) 
###### PREEMPTION IS BEST

##################################

#PERMANOVA

#adonis for docks together
dissim.mat<-vegdist(species.data, method="horn")
adonis(dissim.mat ~ Side*Total_distance_m, data=site.data, strata = site.data$Location, permutations=9999)


#adonis for each dock
species.data.1<-species.data[c(1:9,19:27),]
dissim.mat.1<-vegdist(species.data.1, method="horn")
adonis(dissim.mat.1 ~ Side*Total_distance_m+Boat, data=site.data[site.data$Location=="1",], permutations=9999)

species.data.2<-species.data[c(10:18,28:36),]
dissim.mat.2<-vegdist(species.data.2, method="horn")
adonis(dissim.mat.2 ~ Side*Total_distance_m, data=site.data[site.data$Location=="2",], permutations=9999)

##################################


# Cluster analysis (no special package required)
cluster.spp <- hclust(dissim.mat, method="average") #uses the average linkage method described in class
plot(cluster.spp)


# Ordination: nMDS (requires vegan package)
#
#####################################

myNMDS<-metaMDS(species.data,k=2)
myNMDS #most important: is the stress low? Here it is >0.2 whihc is a bit on the high side
stressplot(myNMDS) #low stress means that the observed dissimilarity between site pairs matches that on the 2-D plot fairly well (points hug the line)

plot(myNMDS)#sites are open circles and species are red +'s ...but it might be nice to label these, and connect samples in the same treatment


#the following commands create layers on a plot, and should be run sequentially

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$Side,draw="polygon",col="grey99",label=T)
orditorp(myNMDS,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

ordiplot(myNMDS,type="n") 
ordihull(myNMDS,groups=site.data$Location,draw="polygon",col="grey99",label=T)
orditorp(myNMDS,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS,display="sites",cex=0.75,air=0.01)

par(mfrow=c(1,1))

###### CCA?
myCCA<-cca(species.data~Side*Location, data =site.data)
plot(myCCA)
anova(myCCA, by="term")
#plot all species
ggplot(species.data)+
  geom_bar(stat="identity")+theme_classic()



#### ordination for dock 1
myNMDS_1<-metaMDS(species.data[c(1:9,19:27),],k=2)
myNMDS_1 
stressplot(myNMDS_1)

ordiplot(myNMDS_1,type="n") 
ordihull(myNMDS_1,groups=site.data$Side[site.data$Location=="1"],draw="polygon",col="grey99",label=T)
orditorp(myNMDS_1,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS_1,display="sites",cex=0.75,air=0.01)

#### ordination for dock 2
myNMDS_2<-metaMDS(species.data[c(10:18,28:36),],k=2)
myNMDS_2 
stressplot(myNMDS_2)

ordiplot(myNMDS_2,type="n") 
ordihull(myNMDS_2,groups=site.data$Side[site.data$Location=="2"],draw="polygon",col="grey99",label=T)
orditorp(myNMDS_2,display="species",col="purple4",air=0.01, cex=0.9) 
orditorp(myNMDS_2,display="sites",cex=0.75,air=0.01)

library(vegan)
library(tidyverse)
library(ggplot2)
        
data(BCI)
BCI$Plot=as.numeric(row.names(BCI))

head(BCI)

# convert the data from long to short format
BCI_short = BCI %>% 
              gather(key="Species",value="Abundance",-Plot)

head(BCI_short)

# or using the reshape2 package (load it first), equivalently...
# BCI_short = melt(BCI, id.vars="Plot", variable.name="Species",value.name="Abundance")

BCI_short_nonzero = BCI_short %>% filter(Abundance>0) %>% arrange(Plot, Species)

head(BCI_short_nonzero)

# load in trait data
traits = read.csv("BCI.trait.csv")
str(traits)

# create a Species column to match the Species column in BCI_short_nonzero

# this requires changing underscores to periods
# the fixed=TRUE argument means the substitution will interpret the 
# find/replace exactly, rather than interpreting them as 'regular expressions'
# which allow matching of more complex patterns (e.g. all numbers, all spaces)
traits$Species = gsub("_",".",traits$genus_species,fixed=TRUE)

# now join the trait data and the abundance data
# we use left join to identify cases where there are species that are unmatched by traits
BCI_merged = inner_join(BCI_short_nonzero, traits,by="Species")

# calculate community weighted means
BCI_summary_trait = BCI_merged %>% group_by(Plot) %>% summarize(CWM.seed.mass=sum(SEED_DRY*Abundance,na.rm=T)/sum(Abundance,na.rm=T))

# load environmental data as a tab-separated file
BCI.env = read.csv('BCI.env.txt',sep='\t')
BCI.env$Plot = as.numeric(row.names(BCI.env))
# also add in soil data
BCI.soil = read.csv('BCI.soil.txt', sep='\t')
BCI.soil$Plot = as.numeric(row.names(BCI.soil))
# make a combined environmental dataset
BCI_env_combined = inner_join(BCI.env,BCI.soil,by="Plot")
head(BCI_env_combined)

BCI_summary_trait_env = inner_join(BCI_summary_trait, BCI_env_combined, by="Plot")







# Question 1
traits[which.max(traits$SEED_DRY),]
# Pouteria fossicola is the largest-seeded species
# It has a 24g oblong seed in a much larger fruit
# of approximately 30cm length based on images






# Question 2
BCI_summary_trait = BCI_merged %>% group_by(Plot) %>% summarize(CWM.seed.mass=sum(SEED_DRY*Abundance,na.rm=T)/sum(Abundance,na.rm=T), 
                                                                CWM.shade.tough=sum(Shade_LaminaTough*Abundance,na.rm=T)/sum(Abundance,na.rm=T))

# load environmental data as a tab-separated file
BCI.env = read.csv('BCI.env.txt',sep='\t')
BCI.env$Plot = as.numeric(row.names(BCI.env))
# also add in soil data
BCI.soil = read.csv('BCI.soil.txt', sep='\t')
BCI.soil$Plot = as.numeric(row.names(BCI.soil))
# make a combined environmental dataset
BCI_env_combined = inner_join(BCI.env,BCI.soil,by="Plot")
head(BCI_env_combined)

BCI_summary_trait_env = inner_join(BCI_summary_trait, BCI_env_combined, by="Plot")

ggplot(BCI_summary_trait_env,aes(x=x,y=y,col=CWM.shade.tough)) +
  geom_point(size=5)+
  coord_equal()+
  scale_color_gradient(low = "red",high="blue") + 
  theme_bw()

# the highest shade leaf toughness occurs at approximately x=700, y=500.






# 3. check soil nutrients
m_toughness_B = lm(CWM.shade.tough ~ B,data=BCI_summary_trait_env)
summary(m_toughness_B)

m_toughness_P = lm(CWM.shade.tough ~ P,data=BCI_summary_trait_env)
summary(m_toughness_P)

m_toughness_Fe = lm(CWM.shade.tough ~ Fe,data=BCI_summary_trait_env)
summary(m_toughness_Fe)

# the strongest effect (using R2 as the metric) is from boron
# with R2=0.33, and p = 8.4 x 10^-6.
# plots with more moron have higher leaf toughness.





# 4. predict seed mass
m_seedmass_slope = lm(CWM.seed.mass ~ slope,data=BCI_summary_trait_env)
summary(m_seedmass_slope)

# using predict
predict(m_seedmass_slope, data.frame(slope=15))
# using the coefficient estimates (same answer)
coef(m_seedmass_slope)[1] + coef(m_seedmass_slope)[2] * 15

# the predicted CWM seed mass would decrease to 0.23 grams






# 5. predict seed mass on steeper slope
# using predict
predict(m_seedmass_slope, data.frame(slope=25))

# The prediction is -0.32 g, which is wrong because seed mass
# cannot be negative. We should have transformed the y-axis or
# fit a nonlinear model to yield predictions that are never negative.





# 6. species with low seed mass
n_low_mass = traits %>% filter(SEED_DRY < 0.23) %>% nrow
n_tot = traits %>% filter(!is.na(SEED_DRY)) %>% nrow
# number of species lost
1 - n_low_mass/n_tot

# 26 percent of species have a seed mass that is larger than the
# predicted new mean value.




# 7. interpret findings
# Environmental filtering is likely operating in this community
# and reducing the set of species that can maintain positive
# population growth rates, such that species with high 
# seed mass cannot establish on steep slopes (perhaps because seeds
# roll off these slopes??)

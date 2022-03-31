# Prep data
library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)

foodweb_metadata = read.csv("thoms_towns_metadata.csv")
str(foodweb_metadata)

files = dir(path='thoms_towns_excel', pattern='*xls', full.names=TRUE)
file_basename = gsub(".xls", "", sapply(files, basename), fixed=TRUE)
head(file_basename)

foodwebs_all = lapply(files, read_excel)

foodwebs_all_clean = lapply(foodwebs_all, function(x) {
  x = x[,-1]
  mindim = min(dim(x))
  x = x[1:mindim, 1:mindim]
  speciesnames = names(x)
  x_matrix = as.matrix(x)
  dimnames(x_matrix) = list(speciesnames, speciesnames)
  return(x_matrix)
})
names(foodwebs_all_clean) = file_basename


# 1. plot Canton web
myweb_canton = foodwebs_all_clean$Canton
myweb_canton_graph = graph.adjacency(myweb_canton,mode='directed')
tl_canton <- TrophInd(myweb_canton)
layout_visual_canton <- layout.random(myweb_canton_graph)
layout_visual_canton[,2] <- jitter(tl_canton$TL,amount=0.05)
plot(myweb_canton_graph, 
     layout=layout_visual_canton,
     vertex.size=0,
     edge.arrow.size=0.25,
     vertex.label.color='black',
     vertex.label.cex=0.5)

# 2. Top predator
tl_canton[which.max(tl_canton$TL),]
# Salmo trutta (trout) is the top predator.

# 3. Primary producers
length(which(tl$TL<2))
# 53 species have trophic level 1 and are primary producers
tl[which(tl$TL<2),]
# two examples are Calothrix (bacteria) and Chlorella (alga).
# these are microorganisms living in the water.

# 4. Loops
which(diag(myweb_canton)==1)
# Paranephrops zealandicus shows loops.
# This species is a freshwater crayfish (crustacean). 
# Loops indicate self-consumption, i.e. cannibalistic species.

# 5. Predators and prey
degreevals_out = degree(myweb_graph,mode='out')
degreevals_in = degree(myweb_graph,mode='in')

degreevals_out[which.max(degreevals_out)]
degreevals_in[which.max(degreevals_in)]
# Unidentified organic matter is eaten by the most species
# Hydropsychids (caddisflies) eat the most other species

# 6. Specialization and predators
plot(tl$TL~degreevals_in)
summary(lm(tl$TL~degreevals_in))
# there is no significant linear relationship between # of species
# predated and trophic position.

# 7. Connectance
connectance_all = sapply(foodwebs_all_clean, function(x) {
  myweb_graph_this = graph.adjacency(x,mode='directed')
  
  L = length(E(myweb_graph_this))
  S = length(V(myweb_graph_this))
  
  connectance = L/S^2
  return(connectance)
})
mean(connectance_all)
sd(connectance_all)
# The mean is 0.0489 and the s.d. is 0.0145


# 8. Connectance vs. habitat
mean_connectance_in_all_df = data.frame(
  WebName=names(connectance_all), 
  Connectance=connectance_all)
mean_connectance_joined = inner_join(foodweb_metadata, mean_connectance_in_all_df, by='WebName')

ggplot(mean_connectance_joined,aes(x=Habitat,y=Connectance)) +
  geom_boxplot()

mean_connectance_joined_forest = mean_connectance_joined[mean_connectance_joined$Habitat %in% c("Pine forest", "Broadleaf forest"),]
t.test(Connectance~Habitat,data=mean_connectance_joined_forest)
# There is no significant difference in connectance in broadleaf/pine forest
# (p=0.11) but the sample sizes are low
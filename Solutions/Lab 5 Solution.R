library(data.table) # install this package if needed
library(dplyr)
library(ggplot2)

data_gentry = read.csv("gentry_transects.csv")

data_gentry$scrubbed_species_binomial = as.character(data_gentry$scrubbed_species_binomial)

index_sp_no_name = which(is.na(data_gentry$scrubbed_species_binomial))
data_gentry$scrubbed_species_binomial[index_sp_no_name] = paste("Species",1:length(index_sp_no_name))

data_gentry$individual_count[which(is.na(data_gentry$individual_count))] = 1

gentry_counts_list = by(data_gentry, data_gentry$plot_name, function(x) {
  result_table = table(factor(rep(x$scrubbed_species_binomial, x$individual_count)))

  df_table = data.frame(plot_name=x$plot_name[1], # keep the plot_name as primary key
                        latitude=x$latitude[1],
                        longitude=x$longitude[1],
                        species=names(result_table), # pull table names
                        abundance=as.numeric(result_table)) # pull table counts
  return(df_table)
})
gentry_counts = rbindlist(gentry_counts_list)

gentry_metadata = na.omit(unique(data_gentry[,c("plot_name","latitude","longitude","elevation_m")]))


summary_richness_tidyr = gentry_counts %>% 
  group_by(plot_name) %>% 
  summarize(species_richness=n_distinct(species))

summary_abundance_tidyr = gentry_counts %>% 
  group_by(plot_name) %>% 
  summarize(total_abundance=sum(abundance))

shannons_H = function(A) 
{
  p = A / sum(A)
  
  H = -1*sum(p * log(p))
  
  return(H)
}

summary_H_tidyr = gentry_counts %>% 
  group_by(plot_name) %>% 
  summarize(H=shannons_H(abundance))



# 1. Min and max richness
summary_richness_with_metadata = inner_join(summary_richness_tidyr, gentry_metadata,by="plot_name")
summary_richess_min = summary_richness_with_metadata[which.min(summary_richness_with_metadata$species_richness),]
print(summary_richess_min)
summary_richess_max = summary_richness_with_metadata[which.max(summary_richness_with_metadata$species_richness),]
print(summary_richess_max)
# Minimum is in KITLOPE1 (9 sp / 0.1 ha), maximum in ARARACUA (346 sp / 0.1 ha)

# 2. Where are these located geographically
summary_richess_min[,c("longitude","latitude")]
summary_richess_max[,c("longitude","latitude")]
# KITLOPE1 is at 128°W, 53.1°N, in British Columbia: https://www.google.com/maps/place/53%C2%B006'00.0%22N+128%C2%B000'00.0%22W/@53.1017576,-129.1207333,274680m/data=!3m1!1e3!4m5!3m4!1s0x0:0x0!8m2!3d53.1!4d-128
# ARRACUA is at 72.3°W, 0.4°S, in Colombia: https://www.google.com/maps/place/0%C2%B024'00.0%22S+72%C2%B018'00.0%22W/@-0.399955,-74.8201162,1025077m/data=!3m1!1e3!4m5!3m4!1s0x0:0x0!8m2!3d-0.4!4d-72.3

# 3. plot Richness and H
summary_richess_H_joined = inner_join(summary_richness_tidyr, summary_H_tidyr, by="plot_name")
summary_richess_H_joined

ggplot(summary_richess_H_joined, aes(x=species_richness,y=H)) + geom_point()
# The H index is strongly correlated with richness and thus is not useful for differentiating plots.

# 4. Latitude plot
# we already joined the data in our answer to question 1.
ggplot(summary_richness_with_metadata, aes(x=abs(latitude), y=species_richness)) +
  geom_point()
# do regression model
summary(lm(species_richness~abs(latitude),data=summary_richness_with_metadata))
# the slope estimate is -4.23, indicating we lose 4 species in 0.1 ha plots per degree of latitude.
# the overall regression is significant (p<2.2e-16) and has good explanatory power (R2=0.42)
# this result is consistent with other studies exploring how biodiversity is highest in the tropics.








gentry_rank = gentry_counts %>% 
  group_by(plot_name) %>% 
  mutate(relative_abundance=abundance/sum(abundance), 
         rank=rank(abundance)) %>% # new columns
  mutate(rank=max(rank) - rank) %>% # reverse order ranks for easy plotting
  arrange(rank) # reorder by rank within each dataframe for easy plotting

# either
gentry_rank_ss = gentry_rank[gentry_rank$plot_name %in% c("YANAM2","POTOMAC"),]
# or
gentry_rank_ss %>% filter(plot_name %in% c("YANAM2","POTOMAC")) %>% filter(rank==min(rank))

# 5. most common species is the one with lowest rank
gentry_rank_ss %>% group_by(plot_name) %>% filter(rank==min(rank))
# YANAM2 has Otobo glycycarpa as most common (9) indivs
# POTOMAC has Asimina triloba as most common (51) indivs
# In general tropical sites have lower abundances of most common species due to the greater
# partitioning of individuals among species.

# 6. plot the rank-abundance diagrams
ggplot(gentry_rank_ss,aes(x=rank,y=abundance,col=plot_name)) + geom_line() + scale_y_log10()
# the x-intercept is the maximum richness of the site; the y-intercept is the relative
# abundance of the most common species.

# 7. Rare species invasion
# Many rare species would extend the x-intercept of the graph, leading to a longer 'tail',
# i.e. a long flat portion on the right part of the graph.
# These long tails cn be indicative of systems experiencing rapid change or invasion.






# Species-area relationships
data_brownepeck = read.csv('browne_peck_1996.csv')

# calculate log-axes
data_brownepeck$logS = log(data_brownepeck$S.num)
data_brownepeck$logA = log(data_brownepeck$A.km2)

# 8. Make a plot of the SAR
ggplot(data_brownepeck,aes(x=logA, y=logS)) + 
  geom_text(aes(label=Name)) + geom_smooth(method='lm')

# 9. Estimate the slope
m_sar = lm(logS~logA,data=data_brownepeck)
summary(m_sar)
confint(m_sar)
# the slope estimate is 0.253, very close to 0.25.
# the confidence interval is 0.09 to 0.41, which includes 0.25
# so 0.25 could be the true value of the parameter.

# 10. Outliers
# The Big Torch Key has many more species relative to its size than all other islands.

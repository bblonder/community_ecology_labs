library(primer)
library(deSolve)
library(reshape2)
library(ggplot2)
library(dplyr)

# set the random number generator state 
# so that our random numbers come out the same each time
set.seed(1)

# set the number of species
S = 3

# choose a random set of intrinsic growth rates 
# uniformly distributed from 0.8 to 1.2
r = rep(1, times=S)
print(r)

# choose competition coefficients normally distributed
# with a mean of 0.01 and a standard deviation of 0.001
A = matrix(rnorm(n=S*S,mean=0.05,sd=0.01),nrow=S,ncol=S)
print(A)

# set the initial abundances of all species to 100
N0 = rep(20, times=S)

# specify the times over which we do the simulation
# here from t=0 to t=50
time = seq(0, 1000, by=0.1)

# run the simulation (the actual math of the Lotka-Volterra
# equations is hidden iin the lvcompg variable, which is 
# loaded from the 'primer' package
lv_out <- ode(y=N0, times=time, func=lvcompg, parms=list(r,A))
lv_out_for_plotting = melt(as.data.frame(lv_out),id.vars="time")

# plot the time series, with labels 1-2-3 for each species
ggplot(lv_out_for_plotting,aes(x=time,y=value,col=variable)) + 
  geom_line() + theme_classic() + 
  xlab("Time") + ylab("Number of individuals")

# 1. Two species stably coexist with each other (sp. 1 and sp. 3)

# 2. The values of r are the same for all species, so the answer must depend on A. 
#  Species 2 experiences more competition than either species 1 or 3.

# 3. 
# set the random number generator state 
# so that our random numbers come out the same each time
set.seed(1)

# set the number of species
S = 3

# choose a random set of intrinsic growth rates 
# uniformly distributed from 0.8 to 1.2
r = rep(1, times=S)
print(r)

# choose competition coefficients normally distributed
# with a mean of 0.01 and a standard deviation of 0.001
A = matrix(rnorm(n=S*S,mean=0.05,sd=0.01),nrow=S,ncol=S)
print(A)

# set the initial abundances of all species to 100
N0 = rep(20, times=S)

# specify the times over which we do the simulation
# here from t=0 to t=50
time = seq(0, 1000, by=0.1)

# run the simulation (the actual math of the Lotka-Volterra
# equations is hidden iin the lvcompg variable, which is 
# loaded from the 'primer' package
lv_out <- ode(y=N0, times=time, func=lvcompg, parms=list(r,A))
lv_out_for_plotting = melt(as.data.frame(lv_out),id.vars="time")

# plot the time series, with labels 1-2-3 for each species
ggplot(lv_out_for_plotting,aes(x=time,y=value,col=variable)) + 
  geom_line() + theme_classic() + 
  xlab("Time") + ylab("Number of individuals")# set the random number generator state 
# so that our random numbers come out the same each time
set.seed(1)

# set the number of species
S = 5

# choose a random set of intrinsic growth rates 
# uniformly distributed from 0.8 to 1.2
r = rep(1, times=S)
print(r)

# choose competition coefficients normally distributed
# with a mean of 0.01 and a standard deviation of 0.001
A = matrix(rnorm(n=S*S,mean=0.05,sd=0.01),nrow=S,ncol=S)
print(A)

# set the initial abundances of all species to 100
N0 = rep(20, times=S)

# specify the times over which we do the simulation
# here from t=0 to t=50
time = seq(0, 1000, by=0.1)

# run the simulation (the actual math of the Lotka-Volterra
# equations is hidden iin the lvcompg variable, which is 
# loaded from the 'primer' package
lv_out <- ode(y=N0, times=time, func=lvcompg, parms=list(r,A))
lv_out_for_plotting = melt(as.data.frame(lv_out),id.vars="time")

# plot the time series, with labels 1-2-3 for each species
ggplot(lv_out_for_plotting,aes(x=time,y=value,col=variable)) + 
  geom_line() + theme_classic() + 
  xlab("Time") + ylab("Number of individuals")

# 3. There are extensive cycles of populations that are 
# transient over the first ~500 time steps.  Eventually 
# most species are outcompeted and a single species (#4)
# dominates the community.

# 4. Tripling intraspecific competition
A_new = A
diag(A_new) = 3*diag(A_new)
lv_out_3x <- ode(y=N0, times=time, func=lvcompg, parms=list(r,A_new))
lv_out_for_plotting_3x = melt(as.data.frame(lv_out_3x),id.vars="time")

# plot the time series, with labels 1-2-3 for each species
ggplot(lv_out_for_plotting_3x,aes(x=time,y=value,col=variable)) + 
  geom_line() + theme_classic() + 
  xlab("Time") + ylab("Number of individuals")
# In this case, four species (all except #5) are able to stably coexist.






# 5. Some species start at zero abundance and flicker in-out because
# they are coming from the broader metacommunity and sometimes fail to
# establish via random mortality. Smaller populations are more likely
# to go extinct than larger ones by chance (each death is proportionately
# more important).

# 6. These species are not competing with each other, except indirectly
# for sites. Competition cannot be inferred from time-series of co-occurrence.

# 7. Species 1 goes locally extinct at approximately t=120000.
# No species stably coexist in this model - they are all transient.
library(untb)
set.seed(1)
results_untb_sim = untb(start=rep(1,60),
                        prob=0.002,
                        gens=200000, # increase # of steps
                        D=1,
                        keep=TRUE)

untb_predictions = data.frame(species.table(results_untb_sim))
names(untb_predictions) = paste("Species",1:ncol(untb_predictions))
untb_predictions$time = 1:nrow(untb_predictions)
untb_predictions_for_plotting = melt(untb_predictions, id.vars='time')
ggplot(untb_predictions_for_plotting,aes(x=time,y=value,col=variable)) +
  geom_line() + 
  xlab("Time") + ylab("Abundance") +
  facet_wrap(~variable) +
  theme_classic()

# 8. There are 1.5 +/- 0.6 species in the community at any time.
richness_untb = data.frame(time=untb_predictions$time, 
                           species_richness=rowSums(untb_predictions[,1:(ncol(untb_predictions)-1)] > 0)
)
mean(richness_untb$species_richness)
sd(richness_untb$species_richness)

# 9. Very species-rich tropical forests may have neutral dynamics.
# Microbial assemblages could also have neutral dynamics.





# 10. The finch community is non-randomly structured. The observed
# C-score is 3.8 and the mean null C-score is 2.7. The observed value
# is significantly higher than the null, indicating that there is more
# co-occurrence than expected by chance.

# Other processes could include environmental filtering (some birds
# prefer some conditions), competition (some birds prevent others from
# occupying similar conditions), or dispersal (some birds are unable
# to reach some islands)
library(EcoSimR)
data(dataWiFinches)
m_finch = cooc_null_model(dataWiFinches, 
                          algo='sim9', 
                          metric='c_score', 
                          suppressProg=TRUE,
                          nReps=1000)
summary(m_finch)
load("comadre_v.2.0.1.RData")
index = 1749 # the spectacled caiman
comadre$metadata[index,]
comadre$mat[[index]]
# we can get the integrated 'A' matrix (incorporating all of growth, fecundity, etc.) as
A = comadre$mat[[index]]$matA
print(A)

# species with lambda < 1
# index=815 works again
lambda(comadre$mat[[815]]$matA) = 0.811
# species with lambda > 1
# index=15 works
lambda(comadre$mat[[15]]$matA) = 2.474

# projection for 100 individuals in smallest size class
matplot(project_population(comadre$mat[[815]]$matA,c(100,0,0,0,0),100))
# projection for 100 individuals in largest size class
matplot(project_population(comadre$mat[[815]]$matA,c(0,0,0,0,100),100))

# stable age distribution for growing population
# most individuals are juveniles, consistent with fish life history 
# (this is a bass species)
stable_age(comadre$mat[[15]]$matA)

# most sensitive life stages
elasticity(comadre$mat[[15]]$matA)
# adult reproduction and juvenile survival largest
# management implication is that we should either
# - promote juvenile survival 
# - increase adult reproduction


# why don't populations continue to increase?
# - matrix models don't incorporate negative density dependence & increasing competition at high abundance
# - transition probabilities are modeled as independent of environment





# Fox-rabbit model
# nullcline analysis

# by setting dNrabbit/dt=0:
# Nfox = k_rabbitgrowthrate / k_preycapturerate OR Nfox=0
# Thus we get more foxes if rabbits grow faster or prey capture rates are low

# by setting dNfox/dt=0:
# Nrabbit = k_foxmortalityrate / k_predatorconversionefficiency OR Nrabbit=0
 # Thus we get more rabbits if foxes die faster or 
# if foxes are inefficient at converting rabbits into new foxes

# note the alternative equilibrium state occurs when there are no foxes & no rabbits - if both populations crash.



# The rabbit population leads the fox population - 
# the prey increase without initial negative feedback from predators, 
# until the predators reach a high density and provide stronger 
# negative feedback to the prey


# we can cause a population crash if the predators become 'better'
# e.g. if the predator_conversion_efficiency increases by a factor of 10
# and all other parameters are left the same:
# predator_conversion_efficiency = 0.1
# The prey population increases, yielding a spike in the predator population
# but the increase is large enough they eat all the prey to extinction
# before themeselves going extinct.



library(ggplot2)
library(reshape2)

# number of time steps in the simulation
n_steps = 4000
delta_t = 0.005 # the smaller delta_t, the more accurate the simulation

# allocate vectors for the predicted abundances over time
N_rabbit = rep(NA, n_steps)
N_fox = rep(NA, n_steps)
time_intervals = (1:n_steps)*delta_t

# initial population sizes for each species
N_rabbit[1] = 100
N_fox[1] = 10

# key parameters for species interactions
reproduction_rate_rabbit = 2
prey_capture_rate = 0.1
predator_conversion_efficiency = 0.1
mortality_rate_fox = 0.2

for (i in 2:n_steps)
{
  delta_rabbit = reproduction_rate_rabbit*N_rabbit[i-1] - 
    prey_capture_rate * N_rabbit[i-1] * N_fox[i-1]
  
  delta_fox = predator_conversion_efficiency * N_rabbit[i-1] * N_fox[i-1] -
    mortality_rate_fox * N_fox[i-1]
  
  N_rabbit[i] = N_rabbit[i-1] + delta_rabbit * delta_t
  N_fox[i] = N_fox[i-1] + delta_fox * delta_t
}

# combine results in a data frame
predictions_predprey = data.frame(time=time_intervals,N_rabbit,N_fox)
# convert the data frame from 'wide' to 'long' format for easier grouped plotting
predictions_predprey_melted = melt(predictions_predprey, id.vars="time")

# plot predictions as time-series using the melted dataframe
ggplot(predictions_predprey_melted,aes(x=time,y=value,col=variable)) + 
  geom_line() + theme_minimal() +
  xlab("Time") + ylab("Abundance")
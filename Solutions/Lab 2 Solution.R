library(ggplot2)

data_venable = read.csv("Venable LTREB - SpeciesByYear.csv",na.strings=".")
data_venable = data_venable[,c("year","species","germ","survive","bx")]

# calculate years of peak survivorship for scba and plpa
data_scba = data_venable[data_venable$species=="scba",]
data_scba$year[which.max(data_scba$survive)]
data_plpa = data_venable[data_venable$species=="plpa",]
data_plpa$year[which.max(data_plpa$survive)]
# 2016 (scba) vs 1995 (plpa)

# possible hypotheses for correlated germination:
# shared environmental response, shared predator response, mutualisms between species

# survivorship
data_venable$lx = data_venable$survive / data_venable$germ
ggplot(data_venable[data_venable$species=="pere",],aes(x=year,y=lx)) + geom_line()

# reproductive value
data_venable$lxbx = data_venable$lx * data_venable$bx
ggplot(data_venable[data_venable$species=="pere",],aes(x=year,y=lxbx)) + geom_line()

# variation in reproductive value across all years
aggregate(lxbx~species,data=data_venable,mean)
# evmu and scba are highest
# evmu is native; scba is non-native (Mediterranean origin)

# Question 1
data_class <- data.frame(name=c("person a", "person b", "person c", "person d"),
                         gender=c("male","female","female","male"),
                         height.cm=c(1.7,1.6,1.9,2.2))
print(data_class)

heights_by_gender = tapply(data_class$height.cm,data_class$gender,mean)
print(heights_by_gender)

heights_total = sum(data_class$height.cm)
print(heights_total)



# Question 2
data_hallmann = read.csv("journal.pone.0185809.s004.csv")

data_hallmann$biomass.per.day = 
  data_hallmann$biomass / (data_hallmann$to.daynr - data_hallmann$from.daynr)

mean_trends <- tapply(data_hallmann$biomass.per.day, data_hallmann$year,mean, na.rm=TRUE)

fraction_biomass = mean_trends["2016"] / mean_trends["1989"]
print(fraction_biomass)

# Interpretation: biomass has decreased to approximately 25% of 1980s/1990s values. 
# High pesticide use and land use fragmentation are likely drivers.
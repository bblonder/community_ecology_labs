library(reshape2)

data_warblers = read.csv(file="dataMacWarb.csv")

# which species spends the most time at the base?
timePosition6 = data.frame(Time.6=data_warblers$X6)
data_warblers$SumPercentPosition6 = apply(X=timePosition6, MARGIN=1, FUN=sum)
# the Myrtle warbler (27%)

# which species has the most variable resource use?
data_warblers$ResourceUseVariability = apply(data_warblers[,2:17],1,sd)
data_warblers[,c("Species","ResourceUseVariability")]
# the Cape May warbler has the most variation in resource use (13%)

# which species spends the most time at the branch tips?
timeTips = data_warblers[,c("X1T","X2T","X3T","X4T","X5T")]
data_warblers$SumTips = apply(X=timeTips,1,sum)
data_warblers[,c("Species","SumTips")]
# the Cape May warbler spends the most time at the tips (74.5%)

# which species pair is most similar in resource use?
data_transposed = as.matrix(t(data_warblers[,2:17]))
str(data_transposed)
dimnames(data_transposed)[[2]] =  data_warblers$Species
warbler_similarity = cor(data_transposed)
warbler_similarity_melted = melt(warbler_similarity)
# remove main diagonal entries
warbler_similarity_melted = warbler_similarity_melted[warbler_similarity_melted$Var1!=warbler_similarity_melted$Var2,]
warbler_similarity_melted[which.max(warbler_similarity_melted$value),]
# the Blackburnian and Cape May warblers have 92% similar resource use according to the Pearson correlation analysis








# Niche modeling analyses
library(raster)

raster_mat = raster('wc2.0_bio_10m_01.tif')
raster_ap = raster('wc2.0_bio_10m_12.tif')

data_oaks = read.csv("oak_distribution.csv")

data_oaks$Mean.Annual.Temperature = extract(raster_mat, data_oaks[,c("Longitude","Latitude")])
data_oaks$Annual.Precipitation = extract(raster_ap, data_oaks[,c("Longitude","Latitude")])

# which species requires higher mean precip?
tapply(data_oaks$Annual.Precipitation, data_oaks$Species, mean)
# Quercus alba (1501 mm) but only just barely

# is the difference significant?
t.test(Annual.Precipitation~Species,data=data_oaks)
# the difference is not significant (p=0.82)

# which species has a broader temperature niche?
tapply(data_oaks$Mean.Annual.Temperature, data_oaks$Species, sd)
# Quercus alba has a slightly broader thermal niche (2.3 degC vs 2.2 degC)

# in a warmer and dried future climate, Q. alba will do better as it is more warm-tolerant.
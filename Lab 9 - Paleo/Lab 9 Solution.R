library(neotoma)
library(analogue)
library(ggplot2)

marion <- get_site(sitename = 'Marion Lake%')
louise <- get_site(sitename = 'Louise Pond%') 

marion.data <- get_dataset(marion)
louise.data <- get_dataset(louise)

louise.data[[1]]$site.data

# get underlying data for each site
marion.dl <- get_download(marion.data)
louise.dl <- get_download(louise.data)

# calculate taxon/count information for each site
# we have to match the counts against a known species list, here the 'P25' list
# see the help for this function for details if curious
marion.taxa <- compile_taxa(marion.dl[[1]], list.name ='P25')
louise.taxa <- compile_taxa(louise.dl[[1]], list.name ='P25')

marion.alnus <- tran(x = marion.taxa$counts, method ='percent')[,'Alnus']
louise.alnus <- tran(x = louise.taxa$counts, method ='percent')[,'Alnus']

# get age information
marion.age = marion.taxa$sample.meta$age
louise.age = louise.taxa$sample.meta$age

# assemble final dataframe of alnus relative abundance
df_marion.louise = rbind(
  data.frame(Site='Marion',Age=marion.age, Percent.Alnus=marion.alnus),
  data.frame(Site='Louise',Age=louise.age, Percent.Alnus=louise.alnus)
)

# make stratigraphic plot
marion.pct <- data.frame(tran(marion.taxa$counts, method = "percent"))
marion.pct$age <- marion.age
marion.pct.strati <- chooseTaxa(marion.pct)
Stratiplot(age ~ ., marion.pct.strati, sort = 'wa', type = 'poly',
           ylab = "Years Before Present")

# Question 1
# Louise Pond is on Louise Island near the Queen Charlotte Ranges in western Canada.
# Marion Lake is northeast of Vancouver in western Canada.

# Question 2
louise.pct <- data.frame(tran(louise.taxa$counts, method = "percent"))
louise.pct$age <- louise.age
louise.pct.strati <- chooseTaxa(louise.pct)
Stratiplot(age ~ ., louise.pct.strati, sort = 'wa', type = 'poly',
           ylab = "Years Before Present")
# Grasses (Poaceae) have been present since approximately 6 Kya according to the pollen diagram

# Question 3
# Pines persist at the Marion site until approximately 10 Kya and then continue to the
# present but in much lower abundances.

# Question 4
# Alnus may have increased in abundance to to primary succession
# after a series of large fires in British Columbia in the last 500 years
# as supported by charcoal evidence described in the publication.
# This increase and the fires are both consistent with high land disturbance
# from white settlers colonizing this landscape and engaging in agriculture/forestry
# activities in these regions.




# load some necessary packages
library("ggmap")
library("ggplot2")
library("reshape2")
library("Bchron")
library("gridExtra")
library("mapproj")

# find pollen datasets containing Pinus in British Columbia 
# note that loc is in (lonW, latS, lonE, latN) format
all.datasets <- get_dataset(loc = c(-140, 45, -110, 65), 
                            datasettype = 'pollen', 
                            taxonname = 'Pinus%')

# extract coordinates from the search results
all.coords <- get_site(all.datasets)

# download the data for all of these sites 
# (this may take a few minutes, ignore warnings, be patient)
all.downloads <- get_download(all.datasets, verbose = FALSE)

# match species names
compiled.cores <- compile_taxa(all.downloads, 'P25')

# filter the data to find the first occurrence of pinus in the dataset at above 5% relative abundance
# discarding also any cores that span less than 5 kya - present
top.pinus <- function(x) {
  # first convert the pollen counts to proportions
  x.pct <- tran(x$counts, method = "proportion")
  #  Cores must span at least the last 5000 years (and have no missing dates):
  old.enough <- max(x$sample.meta$age) > 5000 & !all(is.na(x$sample.meta$age))
  #  Find the highest row index associated with Pinus presence over 5%
  oldest.row <- ifelse(any(x.pct[, 'Pinus'] > .05 & old.enough),
                       max(which(x.pct[, 'Pinus'] > .05)),
                       0)
  #  return a data.frame with site name & location, and the age and date type
  #  (since some records have ages in radiocarbon years) for the oldest Pinus.
  out <- if (oldest.row > 0) {
    data.frame(site = x$dataset$site.data$site.name,
               lat = x$dataset$site.data$lat,
               long = x$dataset$site.data$long,
               age = x$sample.meta$age[oldest.row],
               date = x$sample.meta$age.type[oldest.row])
  } else {
    return(NULL)
  }
  return(out)
}

# apply the 'top.pinus' function to each site, then bind results into a dataframe
summary.pinus <- do.call("rbind", lapply(compiled.cores, top.pinus))

# keep only sites that have dates within the last 40 kyr
summary.pinus <- summary.pinus[summary.pinus$age < 40000 & summary.pinus$age > 1000,]

# for data that have uncalibrated radiocarbon ages, convert them to calibrated ages
radio.years <- summary.pinus$date %in% 'Radiocarbon years BP'
sryears <- sum(radio.years, na.rm = TRUE)
# use the 'intcal13' calibration
calibrated.dates <- BchronCalibrate(summary.pinus$age[radio.years],
                                    ageSds = rep(100, sryears),
                                    calCurves = rep('intcal13', sryears))

# define a weighted mean giving more evidence to points on the calibration curve
# with more data
wmean.date <- function(x) sum(x$ageGrid*x$densities / sum(x$densities))

# replace the uncalibrated ages with the calibrated ages
summary.pinus$age[radio.years] <- sapply(calibrated.dates, wmean.date)




# Question 5
map <- map_data('world')
ggplot(data = data.frame(map), aes(long, lat)) + 
  geom_polygon(aes(group=group), color = 'steelblue', alpha = 0.2) +
  geom_point(data = summary.pinus, aes(x = long, y = lat,col=age)) +
  xlab('Longitude West') + 
  ylab('Latitude North') +
  coord_map(projection = 'albers', lat0 = 40, lat1 = 65, 
            xlim = c(-140, -110), ylim = c(45, 70)) + 
  scale_color_gradient(low="yellow",high="blue")

# Question 6
m_lgm_migration = lm(lat~age,data=summary.pinus[summary.pinus$age < 21000,])
summary(m_lgm_migration)
# the slope estimate is -0.0008249 degrees per year
# or converting, 0.8 degrees per Kyr -- quite slow, but steady!

# Question 7
# The 5% cutoff is needed because Pinus pollen is wind-transported
# and sometimes occurs in sedimentary records even when not 
# locally present at the site. The cutoff lets us 'ignore' such chance
# events in the data. The actual threshold to be used is controversial
# and requires calibration in modern lake / vegetation datasets.

# Question 8
# Carbon dating assumes that objects pick up carbon from the atmosphere at the 
# time of their biological creation (e.g. via photosynthesis). The approach
# relies on the ratio of 14-C (radioactive) to 12-C (non-radioactive) in the object,
# which should decay over time and thus allowing inversion of the equation 
# to get age from the isotope ratio.

# However, the atmospheric 14-C to 12-C ratio is not constant over time - for example
# due to variation in cosmic radiation and/or the testing of hydrogen bombs
# in the mid-20th century (the so-called 'bomb pulse')

# A calibration between radiocarbon-inferred dates and real dates is thus necessary.

# Question 9
# There are appproximately 3470 sites in the Neotoma database that are pollen-related
# based on the Advanced Search on Neotoma Explorer.
# Ghis is a total of ~7000 person-years of work and 100 million dollars of costs.
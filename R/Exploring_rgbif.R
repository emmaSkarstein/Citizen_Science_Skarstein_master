# Exploring rgbif

install.packages("rgbif")
library(rgbif)
library(ggplot2)

occ_count(basisOfRecord = 'OBSERVATION')

occ_count(taxonKey = 2435099, georeferenced = TRUE)

occ_count(georeferenced = TRUE)

# Records from Norway
norway_code = isocodes[grep("Norway", isocodes$name), "code"]
occ_count(country = norway_code)

head(name_suggest(q = 'Puma concolor'))

# Making maps
key = name_backbone(name = 'Puma concolor')$speciesKey
dat = occ_search(taxonKey = key, return = 'data', limit = 300)
gbifmap(dat)

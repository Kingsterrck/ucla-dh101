# options(timeout = 600)
# install.packages("sf", repos = "https://cloud.r-project.org", type = "binary")

library(sf)
ca <- st_read("CA_Counties.shp")
names(ca)                                   # find the county-name field
setdiff(unique(long$county), ca$NAME)       # should be character(0)
setdiff(ca$NAME, unique(long$county))       # should be character(0)
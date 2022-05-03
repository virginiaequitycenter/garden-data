
library(tidyverse)
library(tmaptools)
library(sf)
library(leaflet)
library(DT)

## This chunk of code makes revisions to the data file "04-21-gardens_geo.RDS"
# The revisions are based on getting size information for the city schoolyard 
# gardens on 5/03 and from Bread and Roses on 5/02

gardens <- readRDS("04-21-gardens_geo.RDS")

# Manually adding size information for the following gardens:
# Buford = 4,560 ft2
# Johnson = 2,426 ft2
# Jackson-Via = 3,088 ft2
# Clark = 3,142 ft2
# Venable = 1,395 ft2
# Burnley-Moran = 6,822 ft2
# Greenbrier = 2,950 ft2
# Bread and Roses = 650 square feet

gardens$Total_size[which(gardens$Garden_property_name == "Buford Middle School")] <- "4,560 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Johnson Elementary School")] <- "2,426 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Jackson Via Elementary School")] <- "3,088 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Clark Elementary School")] <- "3,142 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Venable Elementary School")] <- "1,395 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Burnley-Moran Elementary School")] <- "6,822 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Burnley-Moran Elementary School")] <- "6,822 sq ft"
gardens$Total_size[which(gardens$Garden_property_name == "Greenbrier Elementary School")] <- "2,950 sq ft"
gardens$Total_size[which(gardens$Managed_by == "Bread and Roses")] <- "650 sq ft"

# Saving data with updated geolocations 
write.csv(gardens, '05-03-gardens_geo.csv', row.names = F)
write_rds(gardens, "05-03-gardens_geo.RDS")

##########################################################################################################################

## This chunk of code makes revisions to the data file "04-12-gardens_geo.RDS"
# which was created with code below. The revisions are based on feedback from 
# Richard Morris at Cultivate on 4/14

gardens <- readRDS("04-12-gardens_geo.RDS")

# Fixing the visible records garden to have the correct management 
gardens$Managed_by[which(gardens$Garden_property_name == "Visible records garden")] <- "Kendall King"

# Fixing the Trinity Episcopal church bread and roses garden to have the correct management
gardens$Managed_by[which(gardens$Location_description == "Trinity Episcopal Church")] <- "Bread and Roses"

# Editing the garden size information for one garden that Richard Morris had info for
gardens$Total_size[which(gardens$Garden_property_name == "6th Street")] <- "4400 sq ft"
gardens$Size[which(gardens$Garden_property_name == "6th Street")] <- "4400 sq ft"

gardens$Total_size[which(gardens$Managed_by == "Urban Agriculture Collective")] <- gardens$Size[which(gardens$Managed_by == "Urban Agriculture Collective")]

# The 6th street garden was lost, but it has been revived
gardens$Status[which(gardens$Garden_property_name == "6th Street")] <- "Existing"

## Manually fixing the geolocations 
# West St garden = 38.036166511857765, -78.48929141009091
gardens$geometry[which(gardens$Garden_property_name == "West St")] <- st_point(c(-78.48929141009091, 38.036166511857765))

# CATEC garden = 38.06224403490436, -78.46482075941262
gardens$geometry[which(gardens$Garden_property_name == "CATEC School")] <- st_point(c(-78.46482075941262, 38.06224403490436))

# South 1st St = 38.019160547205594, -78.48690403419546
gardens$geometry[which(gardens$Garden_property_name == "South 1st St")] <- st_point(c(-78.48690403419546, 38.019160547205594))

# 6th st = 38.02520028910517, -78.48115967430914
gardens$geometry[which(gardens$Garden_property_name == "6th Street")] <- st_point(c(-78.48115967430914, 38.02520028910517))

# Friendship court = 38.025798361893564, -78.48067282927684
gardens$geometry[which(gardens$Garden_property_name == "Friendship Court")] <- st_point(c(-78.48067282927684, 38.025798361893564))

# Azalea Park gardens = 38.01023749045567, -78.5138139458892
gardens$geometry[which(gardens$Garden_property_name == "Azalea Park Garden")] <- st_point(c(-78.5138139458892, 38.01023749045567))

# Meadow creek garden = 38.05673992416423, -78.49333775468119
gardens$geometry[which(gardens$Garden_property_name == "Meadow Creek Garden")] <- st_point(c(-78.49333775468119, 38.05673992416423))

# Michie Drive = 38.05948411617561, -78.48700270170862
gardens$geometry[which(gardens$Garden_property_name == "Michie Drive")] <- st_point(c(-78.48700270170862, 38.05948411617561))

# Adding additional category column based on whether the garden is managed by a group or specific person
gardens$cat <- ifelse(gardens$Managed_by == "IRC New Roots" |
                        gardens$Managed_by == "Urban Agriculture Collective" |
                        gardens$Managed_by == "Charlottesville Parks Department" |
                        gardens$Managed_by == "City Schoolyard Garden",
                      gardens$Managed_by, "Other")

# Making missing values NA's
gardens$Size <- ifelse(gardens$Size == "", NA, gardens$Size)

# Saving data with updated geolocations 
write.csv(gardens, '04-21-gardens_geo.csv', row.names = F)
write_rds(gardens, "04-21-gardens_geo.RDS")


## Initial work to geo-code gardens. This chunk of code created the data file 
# called "04-12-gardens_geo.csv". I used open street maps and approximate addresses
# for some of the gardens to retrieve their geolocations. 
# We received feedback from Richard Morris at Cultivate on 4/14 that some of the geolocations
# for specific gardens were not precise, so in the above code, I manually edited them. 

gardens <- read.csv("gardens_nogeo.csv")

## Need to remove the columns that don't have the address--we have their geo-location from the data that Taha shared
gardens <- gardens[-which(gardens$Address == ""),]

locations_df <- geocode_OSM(
  gardens[,"Address"],
  projection = 4326,
  return.first.only = TRUE,
  keep.unfound = TRUE,
  as.sf = TRUE,
  geometry = "point",
  server = "https://nominatim.openstreetmap.org"
)

# Manually adding lat and long for one garden that didn't work with 
# open street map (found the lat and lon on google maps)
# Bellair Farm = 37.89181455241969, -78.51879205984392
locations_df$x <- ifelse(locations_df$query == "5290 Bellair Farm, Charlottesville VA 22902", -78.51879205984392,
                         locations_df$x)
locations_df$y <- ifelse(locations_df$query == "5290 Bellair Farm, Charlottesville VA 22902", 37.89181455241969,
                         locations_df$y)

locations_df$point[(locations_df$query == "5290 Bellair Farm, Charlottesville VA 22902")] <- st_point(c(locations_df$x[5], locations_df$y[5]))

#re-naming column so I can join locations_df with gardens
colnames(locations_df)[colnames(locations_df) == "query"] <- "Address"

# Only need a few columns
locations_df <- locations_df[,c("Address", "x", "y", "point")]

# Changing the projection to the same as the data below 
locations_df <- st_transform(locations_df, 4326)

# combining geometry data with other meta data
gardens2 <- locations_df %>%
  left_join(gardens)

rename_geometry <- function(g, name) {
  current = attr(g, "sf_column")
  names(g)[names(g) == current] = name
  st_geometry(g) = name
  g
}

gardens2 <- rename_geometry(gardens2, "geometry")

# Reading in the other data file to combine everything 
files <- list.files("New Data", pattern = ".shp", full.names = TRUE)

# Cultivate gardens 
cult_gard <- st_read(files[5], quiet = T)

# Changing the projection to the same as the data frame above 
cult_gard <- st_transform(cult_gard, 4326)

# changing the column names to be consistent and adding same columns as above
colnames(cult_gard)[colnames(cult_gard) == "Location"] <- "Garden_property_name"

gardens2[(nrow(gardens2) + 1):((nrow(gardens2) + 1) + nrow(cult_gard) - 1), c('Garden_property_name', 'geometry')] <- cult_gard[, c('Garden_property_name', 'geometry')]

# The 6th st and Monticello garden was in the data twice, so I'm getting rid of the extra 
gardens2 <- gardens2[-26,]

# All of the gardens with missing managed by information are managed by City Schoolyard Garden
gardens2$Managed_by <- ifelse(is.na(gardens2$Managed_by == T), "City Schoolyard Garden", gardens2$Managed_by)

# Getting rid of the spare lat and lon columns because they aren't necessary with the
# geometry 
gardens3 <- gardens2[,c(1, 4:12)]

# Coding all non-lost gardens as "existing"
gardens3$Status <- ifelse(is.na(gardens3$Status == T), "Existing", gardens3$Status)

# Writing out data file
write.csv(gardens3, '04-12-gardens_geo.csv', row.names = F)
write_rds(gardens3, "04-12-gardens_geo.RDS")




library(tidyverse)
library(tmaptools)
library(sf)
library(leaflet)
library(DT)

gardens <- read.csv("gardendata.csv")

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
gardens <- locations_df %>%
  left_join(gardens)

rename_geometry <- function(g, name) {
  current = attr(g, "sf_column")
  names(g)[names(g) == current] = name
  st_geometry(g) = name
  g
}

gardens <- rename_geometry(gardens, "geometry")

# Reading in the other data file to combine everything 
files <- list.files("New Data", pattern = ".shp", full.names = TRUE)

# Cultivate gardens 
cult_gard <- st_read(files[5], quiet = T)

# Changing the projection to the same as the data frame above 
cult_gard <- st_transform(cult_gard, 4326)

# changing the column names to be consistent and adding same columns as above
colnames(cult_gard)[colnames(cult_gard) == "Location"] <- "Garden_property_name"

gardens[(nrow(gardens) + 1):((nrow(gardens) + 1) + nrow(cult_gard) - 1), c('Garden_property_name', 'geometry')] <- cult_gard[, c('Garden_property_name', 'geometry')]

gardens$Managed_by[13:22] <- cult_gard$Managed_by

# The 6th st and Monticello garden was in the data twice, so I'm getting rid of the extra 
gardens$Managed_by[12] <- "Urban Agriculture Collective"
gardens <- gardens[-22,]

# Also getting rid of the spare lat and lon columns because they aren't necessary with the
# geometry 
gardens <- gardens[,c(1, 4:12)] 

# Coding all non-lost gardens as "existing"
gardens$Status <- ifelse(gardens$Status == "Lost", gardens$Status, "Existing")
gardens$Status <- ifelse(is.na(gardens$Status == T), "Existing", gardens$Status)

## Coding who manages each garden based on feedback from Michael on 03-30
gardens$Managed_by[1:5] <- "IRC New Roots"
gardens$Managed_by[6:11] <- "Urban Agriculture Collective"

# Writing out data file
write.csv(gardens, 'gardens_geo.csv', row.names = F)
write_rds(gardens, "gardens_geo.RDS")



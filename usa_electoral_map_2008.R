# United States 2008 presidential election results ------------------------

# We're now going to make some graphs of the 2008 United States presidential  
# election. I have attempted to reproduce the York Times electoral maps. 
# The election data comes from a US National Atlas collection of county-level 
# election results, which will be combined with the US state and county maps from 
# the R maps package. The data manipulation was  somewhat complex so I won't 
# go over it.

## @knitr election-data
install.packages(c("maps", "mapproj", "rgdal", "ggplot2"))
library(maps); library(mapproj); library(rgdal); library(ggplot2); library(plyr); library(scales)
library(grid)

state <- map_data("state")
data(state.fips)
county <- map_data("county")
data(county.fips)

polyname <- function(data) {
  polyname <- strsplit(as.character(data), "[,:]")
  polyname.length.max <- max(ldply(polyname, length))
  split <- ldply(polyname, function(x) c(x, rep(NA, polyname.length.max - length(x))))
  return(split)
}

# state
state.fips[ , c("region", "subregion")] <- polyname(state.fips$polyname)
state.fips <- state.fips[tapply(1:nrow(state.fips), state.fips$abb, min) , c("fips", "region", "abb")]
# state.fips <- merge(state.fips, data.frame(state.abb, state.center), 
#                     by.x = "abb", by.y = "state.abb", all.x = TRUE)
state <- merge(state, state.fips[ , c("region", "abb")], by = "region", all.x = TRUE)
# order for use in geom_map()
state <- state[order(state$order), ]
# rename variables for use in geom_map()
names(state)[names(state) == "abb"] <- "id"
names(state)[names(state) == c("region")] <- "state"
state$subregion <- NULL

# county
county.fips[ , c("region", "subregion")] <- polyname(county.fips$polyname)[ , 1:2]
# the following line removes the row for 'yellowstone national'
county.fips <- county.fips[tapply(1:nrow(county.fips), county.fips$fips, min) , c("fips", "region", "subregion")]
county <- merge(county, county.fips, by = c("region", "subregion"), all.x = TRUE)
# fix missing fips
county$fips[county$region == "montana" & county$subregion == "yellowstone national"] <- as.integer(30111)
county$fips[county$region == "new mexico" & county$subregion == "cibola"] <- as.integer(35006)
# order for use in geom_map()
county <- county[order(county$order), ] 
# rename variables for use in geom_map()
names(county)[names(county) == "fips"] <- "id"
names(county)[names(county) %in% c("region", "subregion")] <- c("state", "county")

# atlas file
# for whatever reason, this no longer works like it did when originally written
# temp.dir <- tempdir()
# url <- "http://dds.cr.usgs.gov/pub/data/nationalatlas/elpo08p020.tar.gz"
# file <- basename(url)
# download.file(url, file)
# untar(file, compressed = 'gzip', exdir = temp.dir )
# list.files(temp.dir)

temp.dir <- "C:\\Users\\martin.cote\\Documents\\github-repo\\usa_electoral_map_2008"
#temp.dir <- "C:\\Users\\David\\Documents\\GitHub\\usa_electoral_map_2008"

# data frame
library(foreign)
election.db <- read.dbf(paste(temp.dir, "\\elpo08p020.dbf", sep = ""))
election.db$fips <- as.integer(as.character(election.db$FIPS))
election.db[ , c(9:11)] <- apply(election.db[ , c(9:11)], 2, function(x) as.numeric(as.character(x)))

# shape file
election.shp <- readOGR(dsn = temp.dir, layer = "elpo08p020")
election.db[ , c("long", "lat")] <- coordinates(election.shp)

unlink(temp.dir)

# fix missing and negative vote counts
election.db[ , c(9:11)][is.na(election.db[ , c(9:11)])] <- 0
election.db[ , c(9:11)][election.db[ , c(9:11)] < 0] <- 0

# reduce to one row per county
election.county <- ddply(election.db, .(fips), function(x) {
  data.frame(VOTE_DEM = max(as.numeric(as.character(x$VOTE_DEM))), 
             VOTE_REP = max(as.numeric(as.character(x$VOTE_REP))), 
             VOTE_OTH = max(as.numeric(as.character(x$VOTE_OTH))), 
             long = mean(x$long), 
             lat = mean(x$lat))
})

election.county$STATE <- election.db[tapply(1:nrow(election.db), election.db$fips, min), "STATE"]

# by county
election.county$leader <- factor(apply(election.county[ , 2:4], 1, 
                                       function(x) {y <- which(x == max(x)); ifelse(length(y) == 1, y, NA)}), 
                                 c(1, 2), c("D", "R"))
election.county$lead <- apply(election.county[ , 2:4], 1, function(x) max(x) - max(x[-which.max(x)]))
election.county$dem.pct <- with(election.county, 100*VOTE_DEM/(VOTE_DEM + VOTE_REP + VOTE_OTH))
election.county$rep.pct <- with(election.county, 100*VOTE_REP/(VOTE_DEM + VOTE_REP + VOTE_OTH))
election.county$diff <- with(election.county, dem.pct - rep.pct)
election.county <- election.county[order(-election.county$lead), ] # sort by descending lead to order bubbles

# fips id must be a factor to work in geom_map
county$id <- factor(county$id, levels = 1:99999, labels = as.character(1:99999))
election.county$fips <- factor(election.county$fips, levels = 1:99999, labels = as.character(1:99999))

# aggregate by state
election.state <- rowsum(election.county[ , 2:4], election.county$STATE)
election.state$state <- as.character(row.names(election.state))
election.state$leader <- factor(apply(election.state[ , 1:3], 1, 
                                      function(x) which.max(x[x > 0])), 
                                c(1, 2), c("D", "R"))

# The first map is the state-level results, color coded according to the winning  
# party. This plot uses geom_map(), which uses two datasets, one for the map  
# itself and one containing the fill levels, which in this case is the leading  
# party in leader. The state outlines are created separately though with  
# geom_polygon(). An interesting feature of ggplot2 used in this plot  
# is the coord_map() coordinate transformation function for maps, which  
# applies map projections to the resulting plot. In this case we've used the  
# Albers conic projection, which preserves areas.

## @knitr election-state
theme_set(theme_bw())
theme_update(panel.border = element_blank(), 
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(), 
             axis.line = element_blank(), 
             axis.ticks = element_blank(),
             axis.text.x = element_blank(), 
             axis.text.y = element_blank(), 
             axis.title.x = element_blank(), 
             axis.title.y = element_blank(),
             legend.title = element_blank(),
             legend.key = element_blank(),
             plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
theme_update(legend.position = "none")

ggplot() + 
  geom_map(aes(map_id = state, fill = leader), data = election.state, map = state) + 
  expand_limits(x = state$long, y = state$lat) +
  geom_polygon(aes(x = long, y = lat, group = group), data = state, 
               fill = NA, colour = "white", size = 0.1) +
  scale_fill_manual(values = c("dodgerblue4", "brown")) + 
  coord_map(project = "albers", lat0 = min(state$lat), lat1 = max(state$lat))

# The next map shows each county color coded according to the difference between  
# Democratic and Republican vote shares. It's constructed similarly to the  
# previous map using geom_map() but using the county-level datasets. The  
# fill levels are taken from the diff vote share difference variable in the  
# election.county dataset. The fill colors are defined in scale_fill_gradient2()  
# so that zero difference will appear white, and nonzero difference will appear as  
# increasingly dark blue or red respectively. This color coding is somewhat  
# different from the New York Times' map, which appears to be color coded in a  
# discrete scale and all differences over 20\% in either direction top coded, but  
# I elected to keep these distinctions on the map. There is a second layer using  
# geom_polygon() which adds the state outlines.

## @knitr election-county-choropleth
ggplot() + 
  geom_map(aes(map_id = fips, fill = diff), data = election.county, map = county) + 
  expand_limits(x = county$long, y = county$lat) +
  scale_fill_gradient2(low = "brown", mid = "white", high = "dodgerblue4") +
  geom_polygon(aes(x = long, y = lat, group = group), data = state, 
               fill = NA, colour = "white") +
  coord_map(project = "albers", lat0 = min(county$lat), lat1 = max(county$lat))

# This is a bubble plot showing county-level results, where color represents the  
# leading party in the county and the area of the bubble represents the lead in  
# votes of the leading party over the party with the next largest share of votes.  
# The gray states in the background are made in the first layer by geom_polygon()  
# using dataset state. The bubbles are made by the geom_point() layer  
# using the election.county county-level election dataset, from which we've  
# taken only the counties in the states that appear in the map. The color of each  
# bubble is controlled by the variable leader which has been mapped to the  
# fill parameter and the size of each bubble is controlled by the variable  
# lead which has been mapped to the size parameter. Ordinarily  
# size affects the radius of points, but we've used scale_area() to  
# make it relate directly to area instead. 

## @knitr election-county-bubbleplot
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = state, 
               colour = "white", fill = "gray85") + 
  geom_point(aes(x = long, y = lat, fill = diff, size = lead, order = -lead), 
             data = election.county[election.county$STATE %in% unique(state$id), ], 
             pch = 21, colour = alpha("white", 0.5), show_guide = FALSE) + 
  scale_fill_gradient2(high = "dodgerblue4", mid = "white", low = "brown") + 
  scale_size_area(max_size = 40) +
  coord_map(project = "albers", lat0 = min(county$lat), lat1 = max(county$lat))
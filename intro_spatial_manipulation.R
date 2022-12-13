#' # Introduction to spatial data manipulation in R

#' Starting with some easy simulations to understand data types 

# First load packages we need
library(terra)
library(ggplot2)
library(sf)

# The "simplest spatial data" is point data 
# Point data is x-y coordinates 

# Simulated rainfall data
stations <- data.frame(lon = sample(-116:110, size = 20), 
lat = sample(36:45, size = 20, replace = TRUE))

set.seed(20)
precip <- round((runif(nrow(stations)) * 10)^3)
psize <- 1 + precip / 500
df <- data.frame(stations, precip, psize)

ggplot(df) + geom_point(aes(x = lon, y = lat, 
color = precip, size = psize)) + theme_bw() + ylab("Latitude") + 
xlab("Longitude")

# We can use x-y coordinates to create simple geometries 

lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
ll <- data.frame(cbind(lon, lat))

ggplot(ll) + geom_point(aes(lon, lat)) + 
geom_polygon(aes(lon, lat), fill = "dodgerblue4") + 
geom_line(aes(lon, lat), col = "red") + theme_bw()

#' Numeric vectors representing locations can be used to draw simple geometries
#' A line and polygon can be represented by a number of points 
#' With the main difference being that a polygon needs to be closed i.e. the 
#' first and last points need to coincide with each other (geom_polygon()) took 
#' care of that for us here 

#' It is non-trivial to do some basic spatial operations. 
#' For example, the blue polygon drawn on the map above might represent a state,
#' and a next question might be which of the 10 stations fall within that
#' polygon. And how about any other operation on spatial data, including 
#' reading from and writing data to files? To facilitate such operation a 
#' number of R packages have been developed that define new spatial data types
#' that can be used for this type of specialized operations.

#' # Vector data 
#' The main package we're dealing with in this script is `terra`
#' `terra` introduces a number of classes that being with "Spat". The vector
#' class we're looking at right now is called "SpatVector"

# Create a SpatVector using variables defined above,

pts <- terra::vect(ll)
# Check the class 
class(pts)
# Look at what's inside of it 
pts
terra::geom(pts)

# We see our coordinates but also an object called extent  - this is computed 
# from the coordinates supplied. There is also a coordinate reference system 

#' # Coordinate Reference System 
#' ## Angular coordinate systems 
#' A coordinate reference system contextualizes our coordinates. For example, 
#' a coordinate (4, 10) is useful only if you know where (0, 0) is and the unit
#' of measurement (e.g. is 4 one meter from 5? 1 km? 500 ft?). There are a 
#' number of coordinate reference systems available. 
#' 
#' The natural coordinate reference system for geographic data is longitude and
#' latitude. This is an **angular** coordinate reference system. The most 
#' commonly used is called World Geodesic System 1984 (WGS84)
#'  and The North American Datum (NAD83). 
#' (Alot of information is available for further reading). 
#' 
#' The best way to record a location is by a coordinate pair and a reference 
#' datum. Note with angular CRS, the location is always recorded in latitude/
#' longitude. These aren't the best for geographic calculations. Better yet 
#' are projected CRS
#' 
#' ## Projected coordinate systems 
#' The projected systems tackle the question of how to take a three dimensional 
#' angular system and convert it to a two dimensional planar system (also 
#' called a Cartesian system). There are different types of planar coordinate 
#' reference systems - each has different implications. The most popular 
#' are  UTM or Equal Area. Note: There isn't one "best" projection. Often the 
#' choice of projected CRS depends on the question to answer, e.g. some 
#' projections can only be used in certain parts of the world, others can be 
#' used globally. One important tradeoff with planar systems is whether it is 
#' equal area (the scale of the map is constant) or whether it is conformal 
#' (the shapes of the geographic features are as they are seen on a globe). 

# Assigning a CRS 
pdef <- "+proj=longlat +datum=WGS84"
pts <- terra::vect(ll, crs = pdef)

# Now look at the object 
pts
# We have a CRS! 

# Let's add attributes to this SpatVector 

precipvalue <- runif(nrow(ll), min = 0, max = 100)
pts[["precipval"]] <- precipvalue
pts[["ID"]] <- seq_len(nrow(ll))

# Take a look 
pts

# Making SpatVectors with lines or polyogns is slightly more complicated 
# Here it is as a line 
lon <- c(-116.8, -114.2, -112.9, -111.9, -114.2, -115.4, -117.7)
lat <- c(41.3, 42.9, 42.4, 39.8, 37.6, 38.3, 37.6)
lonlat <- cbind(id=1, part=1, lon, lat)
lonlat

lns <- vect(lonlat, type = "lines", crs = pdef)
lns

# Looking at it visually 
terra::plot(lns)

# Here it is as a polygon 
pols <- vect(lonlat, type = "polygons", crs = pdef)
pols
terra::plot(pols)

# Notice the difference between lines and polygons! 

#' # Rasters
#' Rasters are a different class of spatial object. Predictably in terra, 
#' they're called SpatRaster. Rasters multi-layer (multi-variable) raster data
#' The parameters that describe a raster include number of columns, rows, the 
#' spatial extent, and the CRS. (Like with vectors and polygons, we can add more
#' attributes). 

# Create a raster from scratch 
raster <- terra::rast(ncol = 10, nrow = 10, 
xmin = -150, xmax = -80, ymin = 20, ymax = 60)
class(raster)
# Look at it's attributes
raster

#' At the moment, this raster only has information about the location and 
#' resolution - there are no values associated with it - let's add values!

terra::values(raster) <- runif(terra::ncell(raster))
raster

# Visualize 
terra::plot(raster)
# Overlay points and polygons (just to know we can do this)
terra::points(pts)
terra::lines(pols)

#' That's the basics of spatial data specifications! We've learnt how to create 
#' points, lines, rasters from scratch. These are the most commonly used 
#' spatial objects 

#' Spatial data manipulation 
#' Spatial data visualization 


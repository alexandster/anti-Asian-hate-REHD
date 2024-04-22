#libraries
library(sparr)
library(sf)
library(dplyr)
library(raster)
library(stringr)

# set workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read master data table
df <- read.csv("../data/master_table.csv")
df$GEOID <- str_pad(df$GEOID, 5, pad="0")

#read county geometries
geom <- st_read("../data/us_counties.shp") %>%
    # st_transform(., crs=2163) %>%
  left_join(., df, by = "GEOID") %>% 
  subset(., select = c("GEOID", "hate", "nohate", "geometry")) %>%
  mutate(centroids = st_centroid(st_geometry(.)))
geom <- geom %>% 
  mutate(x = st_coordinates(geom$centroids)[,1],
         y = st_coordinates(geom$centroids)[,2])

# total number of tweets
geom$tot_tweets <- geom$hate + geom$nohate

#ppp
tw <- ppp(geom$x, geom$y, window = owin(c(min(geom$x) - 0.5,max(geom$x) + 0.5),c(min(geom$y) - 0.5,max(geom$y) + 0.5)))

parallel::detectCores()

#kde adverse tweets
f_breve <- bivariate.density(pp=tw, h0=OS(tw)/4, adapt=FALSE, resolution=750, weights=geom$hate, verbose=TRUE, parallelise = 7)

#kde total tweets
g_tilde <- bivariate.density(pp=tw, h0=OS(tw)/4, adapt=FALSE, resolution=750, weights=geom$tot_tweets, verbose=TRUE, parallelise = 7)

#risk
f <- risk(f_breve, g_tilde, tolerate = TRUE)

#risk surface
r <- raster(f$rr)
crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(r, "../data/r", format = "GTiff", overwrite=TRUE)

#risk surface
p <- raster(f$P)
crs(p) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
writeRaster(p, "../data/p", format = "GTiff", overwrite=TRUE)


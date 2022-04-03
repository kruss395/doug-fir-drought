library(ggplot2)
library(rgdal)
# library(sf)
library(maps)

# map_dat = map_data('world')
# map_dat = subset(map_dat, region %in% c('Canada', 'USA', 'Mexico'))
# 
# ggplot() + 
#   geom_polygon(data=map_dat, aes(x=long, y=lat, group=subregion))

# read in data file
dat = read.csv("data/ITRDB_NA_PSME.csv")

#this changes back to regular lat long
#assigning a crs to the pollen coordinates
dat_sp <- SpatialPointsDataFrame(coords = dat[,c('long','lat')], 
                                 data = dat,
                                 proj4string = CRS("+init=epsg:4326"))

# #transforming to the albedo crs: epsg 102001
# pol_transform = spTransform(spdf, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


alb_proj = '+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0
+ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

regions <- readOGR("data/map-data/geographic/ne_50m_admin_1_states_provinces_lakes.shp", 
                   'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

# regions <- readOGR("data/map-data/geographic/ne_50m_admin_1_states_provinces_lakes.shp", 
#                    'ne_50m_admin_1_states_provinces_lakes', encoding='UTF-8')

regions <- subset(regions, geonunit %in% c("United States of America", "Canada"))
regions <- subset(regions, !(name %in% c("Hawaii"))) # region is defined in the first part of the code (see above)
# region is defined in the first part of the code (see above)
# regions = spTransform(regions, alb_proj)
regions = spTransform(regions, CRS("+init=epsg:4326"))

# 
pbs <- readOGR("data/map-data/geographic/PoliticalBoundaries/boundary_p_v2.shp",
                   'boundary_p_v2', encoding='UTF-8')
pbs = spTransform(pbs, CRS("+init=epsg:4326"))
pbs = subset(pbs, COUNTRY %in% c('CAN', 'USA', 'MEX'))
pbs = subset(pbs, !(STATEABB %in% NA))

# 
# pbs <- readOGR("data/map-data/geographic/ne_50m_admin_0_scaleranks/ne_50m_admin_0_scale_rank.shp", 
#                'ne_50m_admin_0_scale_rank', encoding='UTF-8')
# pbs = spTransform(pbs, CRS("+init=epsg:4326"))
# pbs = subset(pbs, GEOUNIT %in% c('Canada', 'United States of America', 'Mexico'))

# ecoregions for north america
eco = readOGR('data/map-data/ecoregions/', layer='NA_CEC_Eco_Level2')

# shapefile reprojection
# eco_reproj = spTransform(eco, alb_proj)
eco_reproj = spTransform(eco, CRS("+init=epsg:4326"))

ggplot() + 
  # geom_polygon(data=regions, aes(long,lat, group = group), fill = "white") + 
  geom_path(data=regions, aes(long,lat, group = group), color="black") +
  geom_point(data=dat, aes(x=long, y=lat),colour='dodgerblue') +
  # coord_equal(xlim = c(-123, -108),
  #             ylim = c(48, 60.5)) + 
  # coord_map(xlim = c(-123, -108),
  #           ylim = c(48, 60.5)) +
  # xlim(xlim = c(-140, -75)) +
  theme_bw() + 
  coord_equal() +
  guides(fill = FALSE)


ggplot() + 
  # geom_polygon(data=regions, aes(long,lat, group = group), fill = "white") + 
  geom_path(data=pbs, aes(long,lat, group = group), color="black") +
  geom_point(data=dat, aes(x=long, y=lat),colour='dodgerblue') +
  # coord_equal(xlim = c(-123, -108),
  #             ylim = c(48, 60.5)) + 
  # coord_map(xlim = c(-123, -108),
  #           ylim = c(48, 60.5)) +
  xlim(xlim = c(-150, -70)) +
  theme_bw() + 
  coord_equal() +
  guides(fill = FALSE)



# ggplot() + 
#   # geom_polygon(data=pbs, aes(long,lat, group = group, fill = group)) + 
#   geom_path(data=pbs, aes(long,lat, group = group), color="black") +
#   geom_point(data=dat, aes(x=long, y=lat)) +
#   # coord_equal(xlim = c(-123, -108),
#   #             ylim = c(48, 60.5)) + 
#   # coord_map(xlim = c(-123, -108),
#   #           ylim = c(48, 60.5)) +
#   xlim(xlim = c(-123, -75)) +
#   theme_bw() + 
#   coord_equal() +
#   guides(fill = FALSE)


# ggplot() +
#   geom_polygon(data=regions, aes(long,lat, group = group, fill = group)) +
#   geom_path(data=regions, aes(long,lat, group = group), color="white") +
#   geom_path(data = eco, aes(x=long, y=lat, group=group)) +
#   # geom_point(data=dat, aes(x=long, y=lat)) +
#   # coord_equal(xlim = c(-123, -108),
#   #             ylim = c(48, 60.5)) +
#   # coord_map(xlim = c(-123, -108),
#   #           ylim = c(48, 60.5)) +
#   # xlim(xlim = c(-123, -108)) +
#   theme_bw() +
#   coord_equal() +
#   guides(fill = FALSE)

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(mapboxapi)

setwd("Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/")

# AOI same size as original Warrington example

height = 6786.6
width = 8828.8

# Define AOI

define_AOI = function() {
  location = readline(prompt = "Enter the location (no spaces): ")
  east = readline(prompt = "Enter the centroid easting: ")
  north = readline(prompt = "Enter the centroid northing: ")
  
  return(c(location, east, north))
}

aoi_res = define_AOI()

location = as.name(aoi_res[1])
east = as.numeric(aoi_res[2])
north = as.numeric(aoi_res[3])

df_aoi = tibble(
  "Corner" = c("NW", "NE", "SW", "SE"),
  "x" = c(east-(width/2), east+(width/2), east-(width/2), east+(width/2)),
  "y" = c(north+(height/2), north+(height/2), north-(height/2), north-(height/2)))

aoi = vect(x = df_aoi,
           geom = c("x", "y"),
           crs = "epsg:27700") %>%  # British National Grid
  as.polygons(extent = T)


# Import Data

accp = vect("Y:/PersonalFolders/AliceH/Handovers/NCF_Public_Access/Data/Downloaded_data/OS_data/OS_Greenspace_AccessPoints.shp",
            extent = aoi)

prows = vect("Y:/ExternalProjects/NCF Access/NE_greenspace/Green_and_Blue_Infrastructure_NE_GeoPackage - working copy/Green_and_Blue_Infrastructure_NE_GeoPackage/Green_Infrastructure_Linear_Access/Linear_access_network.gpkg",
             layer = "Map5_England_PROW_Update2022",
             extent = aoi) %>% crop(aoi)

roads = vect("Z:/CESB/Land Use and Ecosystem Service/GIS_Data/_Data/OS_data/OS_Open_Data/oproad_gpkg_gb/Data/oproad_gb.gpkg",
             layer = "road_link",
             extent = aoi) %>% crop(aoi)

woodlands_ne = vect("Y:/PersonalFolders/Joe/NCF_Public_Access/NE_GreenInfrastructure/Green_Infrastructure_access_maps.gpkg",  # cannot access file in Projects folder for some reason
                    layer = "Map4_woodlands",
                    extent = aoi) %>% crop(aoi)

woodlands_ne_acc = woodlands_ne %>%
  filter(AccessLevel == "Accessible")

#woodlands_w4p = vect("Y:/ExternalProjects/NCF Access/woods_for_people_2023 1.geojson",
#                         extent = aoi) %>%
#  crop(aoi) %>%
#  rename("restricted_information" = "restricted information")

imd = vect("Y:/PersonalFolders/Joe/NCF_Public_Access/Data/Lower_Super_Output_Area_(LSOA)_IMD_2019__(OSGB1936)/Exported from ArcGIS proj corrected/Export_Output.shp")

popden = read_csv("Y:/PersonalFolders/Joe/NCF_Public_Access/Data/sape23dt11mid2020lsoapopulationdensity_popden.csv") %>% 
  rename("LSOA.Code" = "LSOA Code",
         "People_per_Sq_Km" = "People per Sq Km")














# Shapefiles

# Must be a better way of doing this!!

# Not complete

write_shp_accesspoints = function(SpatVector){
  loc = location
  fname = sprintf('Access Points/%s', deparse(substitute(SpatVector)))
  writeVector(x = SpatVector, filename = fname, filetype = "ESRI Shapefile")
}

write_shp_accesspoints(accp)



# Access Points
writeVector(x = accp,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Access Points/accp_all.shp",
            filetype = "ESRI Shapefile")

writeVector(x = accp_ne_acc,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Access Points/accp_NE_accessible.shp",
            filetype = "ESRI Shapefile")

writeVector(x = accp_ne_acc_2ha,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Access Points/accp_NE_accessible_2ha.shp",
            filetype = "ESRI Shapefile")

writeVector(x = accp_ne_acc_20ha,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Access Points/accp_NE_accessible_20ha.shp",
            filetype = "ESRI Shapefile")

# AOIs
writeVector(x = hwyc,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/AOIs/hwyc.shp",
            filetype = "ESRI Shapefile")

# Euclidean
writeVector(x = eucl_ne_acc_1260m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Euclidean/eucl_accp_NE_accessible_1260m.shp",
            filetype = "ESRI Shapefile")

writeVector(x = eucl_ne_acc_2ha_500m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Euclidean/eucl_accp_NE_accessible_2ha_500m.shp",
            filetype = "ESRI Shapefile")

writeVector(x = eucl_ne_acc_20ha_4km,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Euclidean/eucl_accp_NE_accessible_20ha_4km.shp",
            filetype = "ESRI Shapefile")

# IMD
writeVector(x = imd,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/IMD/imd.shp",
            filetype = "ESRI Shapefile")


# Isochrones
writeVector(x = isochrone_ne_acc_1260m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Isochrones/isochrone_ne_acc_1260m.shp",
            filetype = "ESRI Shapefile")

writeVector(x = isochrone_ne_acc_2ha_500m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Isochrones/isochrone_ne_acc_2ha_500m.shp",
            filetype = "ESRI Shapefile")

writeVector(x = isochrone_ne_acc_20ha_4km,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Isochrones/isochrone_ne_acc_20ha_4km.shp",
            filetype = "ESRI Shapefile")

# Networks
writeVector(x = roads,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Networks/roads.shp",
            filetype = "ESRI Shapefile")

writeVector(x = prows,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Networks/prows.shp",
            filetype = "ESRI Shapefile")

# Woodlands
writeVector(x = woodlands_ne,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_20m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE_20m_buffer.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE_acc.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc_20m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE_acc_20m_buffer.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc_2ha,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE_2ha.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc_2ha_20m,
            filename = "Y:/PersonalFolders/Joe/NCF_Public_Access/Case Studies/Woodlands/woodlands_NE_2ha_buff20m.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc_20ha,
            filename = "Woodlands/woodlands_NE_2ha.shp",
            filetype = "ESRI Shapefile")

writeVector(x = woodlands_ne_acc_20ha_20m,
            filename = "Woodlands/woodlands_NE_2ha_buff20m.shp",
            filetype = "ESRI Shapefile")

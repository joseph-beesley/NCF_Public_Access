library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(mapboxapi)

setwd("E:/NCF_Public_Access/Case_Studies/")

# AOI same size as original Warrington example

warr = vect("Z:/CESB/Land Use and Ecosystem Service/LUES_Sware/PersonalFolders/Joe/Data/NCF_Public_Access_Warrington/warrington_local_area.shp")
warr_ext = ext(warr)

height = as.numeric(warr_ext[4] - warr_ext[3])
width = as.numeric(warr_ext[2] - warr_ext[1])

##### Define AOI #####

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


##### Import Data #####

accp = vect("Z:/CESB/Land Use and Ecosystem Service/LUES_Sware/PersonalFolders/AliceH/Handovers/NCF_Public_Access/Data/Downloaded_data/OS_data/OS_Greenspace_AccessPoints.shp",
            extent = aoi)

woodlands_ne = vect("Z:/CESB/Land Use and Ecosystem Service/LUES_Sware/PersonalFolders/Joe/Data/NE_GreenInfrastructure/Green_Infrastructure_access_maps.gpkg",  # cannot access file in Projects folder for some reason
                    layer = "Map4_woodlands",
                    extent = aoi) %>% crop(aoi)

woodlands_ne_acc = woodlands_ne %>% filter(AccessLevel == "Accessible")

imd = vect("Z:/CESB/Land Use and Ecosystem Service/LUES_Sware/PersonalFolders/Joe/Data/Lower_Super_Output_Area_(LSOA)_IMD_2019__(OSGB1936)/Exported from ArcGIS proj corrected/Export_Output.shp")

popden = read_csv("Z:/CESB/Land Use and Ecosystem Service/LUES_Sware/PersonalFolders/Joe/Data/sape23dt11mid2020lsoapopulationdensity_popden.csv") %>% 
  rename("LSOA.Code" = "LSOA Code",
         "People_per_Sq_Km" = "People per Sq Km")


##### Woodland Trust Access Standard #####

### Data

woodlands_ne_acc_2ha = woodlands_ne_acc %>% filter(Area_ha >= 2)
woodlands_ne_acc_20ha = woodlands_ne_acc %>% filter(Area_ha >= 20)

if_woodlands_2ha = ifelse(length(woodlands_ne_acc_2ha) > 0, TRUE, FALSE)
if_woodlands_20ha = ifelse(length(woodlands_ne_acc_20ha) > 0, TRUE, FALSE)

### 2ha, 500m

if(if_woodlands_2ha){
  
  accp_ne_acc_2ha = crop(accp, buffer(woodlands_ne_acc_2ha, width = 20) %>% aggregate())
  
  eucl_ne_acc_2ha_500m = 
    buffer(accp_ne_acc_2ha, width = 500) %>%  
    aggregate()
  
  iso_ne_acc_2ha_500m = mb_isochrone(
    st_as_sf(accp_ne_acc_2ha),
    distance = 500,
    profile = "walking"
  ) %>% 
    st_transform(crs = 27700) %>% 
    vect() %>% 
    aggregate()
}

### 20ha, 4km

if(if_woodlands_20ha){
  
  accp_ne_acc_20ha = crop(accp, buffer(woodlands_ne_acc_20ha, width = 20) %>% aggregate())
  
  eucl_ne_acc_20ha_4km = 
    buffer(accp_ne_acc_20ha, width = 4000) %>%
    aggregate()
  
  iso_ne_acc_20ha_4km = mb_isochrone(
    st_as_sf(accp_ne_acc_20ha),
    distance = 4000,
    profile = "driving"
  ) %>% 
    st_transform(crs = 27700) %>% 
    vect() %>% 
    aggregate()
}

##### Natural England Access Standard #####

if_woodlands_1260m = ifelse(length(woodlands_ne_acc) > 0, TRUE, FALSE)

if(if_woodlands_1260m){
  
  accp_ne = crop(accp, buffer(woodlands_ne, width = 20) %>% aggregate())
  accp_ne_acc = crop(accp, buffer(woodlands_ne_acc, width = 20) %>% aggregate())
  
  eucl_ne_acc_1260m =
    buffer(accp_ne_acc, width = 1260) %>%  # 15 mins walking at 1.4 m/s
    aggregate()
  
  iso_ne_acc_1260m = mb_isochrone(
    st_as_sf(accp_ne_acc),
    distance = 1260,
    profile = "walking"
  ) %>% 
    st_transform(crs = 27700) %>%
    vect() %>% 
    aggregate()
}

###### IMD #####

calc_imd = function(polygon) {
  
  poly = crop(polygon, aoi)
  poly_imd = crop(imd, poly)
  poly_imd_decil =
    poly_imd %>% 
    crop(aoi) %>% 
    mutate("Area_sqkm" = expanse(poly_imd,
                                 unit = "km")) %>% 
    rename('LSOA.Code' = 'lsoa11cd') %>% 
    merge(popden, by = "LSOA.Code") %>%
    mutate("Area_pop" = Area_sqkm * People_per_Sq_Km) %>% 
    group_by(IMDDecil) %>% 
    mutate("Total_pop" = sum(Area_pop)) %>% 
    distinct(IMDDecil, .keep_all = T) %>% 
    as.data.frame() %>% 
    arrange(IMDDecil) %>% 
    select(IMDDecil, Total_pop)
  
  return(poly_imd_decil)
}


imd_df = tibble(
  "IMDDecil" = c(1:10)) %>%
  full_join(as_tibble(calc_imd(aoi)), by = "IMDDecil") %>% 
  rename("Decil_pop" = "Total_pop")
if(if_woodlands_2ha){
  imd_df = 
    imd_df %>%  
    full_join(as_tibble(calc_imd(eucl_ne_acc_2ha_500m)), by = "IMDDecil") %>% 
    rename("2ha_500m_eucl" = "Total_pop") %>% 
    full_join(as_tibble(calc_imd(iso_ne_acc_2ha_500m)), by = "IMDDecil") %>% 
    rename("2ha_500m_iso" = "Total_pop") 
}
if(if_woodlands_20ha){
  imd_df = 
    imd_df %>%    
    full_join(as_tibble(calc_imd(eucl_ne_acc_20ha_4km)), by = "IMDDecil") %>% 
    rename("20ha_4km_eucl" = "Total_pop") %>% 
    full_join(as_tibble(calc_imd(iso_ne_acc_20ha_4km)), by = "IMDDecil") %>% 
    rename("20ha_4km_iso" = "Total_pop") 
}
if(if_woodlands_1260m){
  imd_df = 
    imd_df %>%  
    full_join(as_tibble(calc_imd(eucl_ne_acc_1260m)), by = "IMDDecil") %>% 
    rename("1260m_eucl" = "Total_pop") %>% 
    full_join(as_tibble(calc_imd(iso_ne_acc_1260m)), by = "IMDDecil") %>% 
    rename("1260m_iso" = "Total_pop")
}

write_csv_named = function(df){
  loc = location
  fname = sprintf('%s_IMDdecils.csv', deparse(substitute(loc)))
  write_csv(x = df, file = fname)
}

write_csv_named(imd_df)

# Spatial data compile

library(sf)
library(ggplot2)

# download Leeds Ward boundaries
#download.file('https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2016_Boundaries/MapServer/0/query?where=lad16nm+%3D+%27Leeds%27&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/leeds_wards.geojson')

wards<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/leeds_wards.geojson')
# wards bbox: -1.800424,53.6989,-1.290355,53.94589

# download.file('https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Ancient_Woodland_England/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.800424%2C53.6989%2C-1.290355%2C53.94589&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/ancient_woodland.geojson')

oldwood<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/ancient_woodland.geojson')


#download.file('https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Special_Areas_of_Conservation_England/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.800424%2C53.6989%2C-1.290355%2C53.94589&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/special_areas_conservation.geojson')

sp_cons<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/special_areas_conservation.geojson')

#download.file('https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Special_Protection_Areas_England/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.800424%2C53.6989%2C-1.290355%2C53.94589&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/special_protection_areas.geojson')

sp_pro<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/special_protection_areas.geojson')

#download.file('https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Local_Nature_Reserves_England/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.800424%2C53.6989%2C-1.290355%2C53.94589&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/local_nature_reserves.geojson')

loc_nature_res<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/local_nature_reserves.geojson')

# SSIs downloaded from DEFRA spatial data platform
sssi<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/sites_special_scientific_interest.geojson')

#download.file('https://services2.arcgis.com/mHXjwgl3OARRqqD4/arcgis/rest/services/Priority_Places_for_England_2016/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.800424%2C53.6989%2C-1.290355%2C53.94589&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/priority_places.geojson')
# big download as ignores spatial query as all one not a multipolygon

priority<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/priority_places.geojson')

#download.file('https://services.arcgis.com/JJzESW51TqeY9uat/arcgis/rest/services/Provisional%20Agricultural%20Land%20Classification%20(ALC)%20(England)/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-3.855%2C53.641%2C0.306%2C54.207&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
#              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/agricultural_land_class.geojson')

ag_class<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/agricultural_land_class.geojson')

# Forest Inventory woodland 2018


download.file('https://services2.arcgis.com/mHXjwgl3OARRqqD4/arcgis/rest/services/National_Forest_Inventory_Woodland_GB_2018/FeatureServer/0/query?outFields=*&where=1%3D1&outFields=*&geometry=-3.855%2C53.641%2C0.306%2C54.207&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json',
              'C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/woodland.geojson')

wood<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/agricultural_land_class.geojson')

# Lower layer super output layers (LSOA) sub-ward boundaries for matching with deprevation index

https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_DEC_2001_EW_BFC/FeatureServer/0/query?where=1%3D1&outFields=*&geometry=-1.919%2C53.774%2C-1.410%2C53.845&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=4326&f=json


ggplot()+geom_sf(data=ag_class, aes(fill=ALC_GRADE), alpha=0.5)+geom_sf(data=wards, fill=NA, colour='black')+ geom_sf(data=sp_pro, fill='purple', alpha=0.5)+
  geom_sf(data=sp_cons, fill='orange', alpha=0.5)+geom_sf(data=oldwood, fill='green', alpha=0.5)+
  geom_sf(data=loc_nature_res, fill='blue', alpha=0.5)+geom_sf(data=sssi, fill='red', alpha=0.5)+
  geom_sf(data=priority, fill=NA, colour='blue')+
  coord_sf(xlim=c(-1.800424,-1.290355), ylim=c(53.6989,53.94589))


# data from digimap

# mastermap sites
st_layers('C:/trees/spatial_data/mastermap_sites/mastermap-sites_3498500/mastermap-sites_3498500.gdb')

mmap_sites<-st_read('C:/trees/spatial_data/mastermap_sites/mastermap-sites_3498500/mastermap-sites_3498500.gdb',
                    layer='Functional_Sites')
plot(mmap_sites['function.'])

# OS greenspace

g1<-st_read('C:/trees/spatial_data/greenspace/MasterMap Greenspace_3498497/se/SE0520.gml')
plot(g1['primaryFunction'])
# Massive!

# CEH landcover
st_layers('C:/trees/spatial_data/landcover/lcm-2015-vec_3498721/LCM2015_GB.gdb')

landcover<-st_read('C:/trees/spatial_data/landcover/lcm-2015-vec_3498721/LCM2015_GB.gdb')
# didn't load

# roads

road<-st_read('C:/trees/spatial_data/highways_roads/MasterMap Highways Network_roads_3498495/Highways_Roads_RoadLink_FULL_085.gml.gz')
plot(road['formOfWay'])

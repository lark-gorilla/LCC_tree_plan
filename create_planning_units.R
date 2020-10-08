# create project Planning Unit layer

library(dplyr)
library(sf)
library(ggplot2)

wards<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/leeds_wards.geojson')

ward_boundary<-st_union(wards)
# clip to ward extent
ward_boundary_t<-st_transform(ward_boundary, 27700) # trans to 

# mosiac greenspaces

gs_files<-list.files('C:/trees/spatial_data/greenspace/MasterMap Greenspace_3498497/se')[
  grep('gml',list.files('C:/trees/spatial_data/greenspace/MasterMap Greenspace_3498497/se'))]

gs_lyr<-NULL
for(i in 1:length(gs_files))
{
  if(i==41){next}
  g1<-st_read(paste0('C:/trees/spatial_ data/greenspace/MasterMap Greenspace_3498497/se/',gs_files[i]), quiet=T)
  # need to include secondary form and function
  # could drop private garden function + others?
  g1%>%select(gml_id, primaryFunction, primaryForm)->g1
  
  inty<-st_intersects(g1, ward_boundary_t)
  
  if(1%in%lengths(inty)==FALSE){print(paste(i, 'didnt intersect'));next}
  
  if(0%in%lengths(inty)==FALSE){g2<-g1; print(paste(i, 'was all inside'))}else{
    g2<-st_intersection(g1, ward_boundary_t); print(paste(i, 'intersected'))}
  
  if(is.null(gs_lyr)){gs_lyr<-g2}else{
    gs_lyr<-rbind(gs_lyr, g2)}
  print(i)
}

#st_write(gs_lyr, 'C:/trees/spatial_data/greenspace/greenspace_mosaic_ward.gml' )

# mosiac vectormap areas

va_files<-list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/')[
  grep('gml',list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/'))]

va_lyr<-NULL
for(i in 1:length(va_files))
{
  #if(i==41){next}
  g1<-st_read(paste0('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/',
                     va_files[i]), layer='Area', quiet=T)
  g1%>%select(gml_id, featureDescription)->g1
  
  inty<-st_intersects(g1, ward_boundary_t)
  
  if(1%in%lengths(inty)==FALSE){print(paste(i, 'didnt intersect'));next}
  
  if(0%in%lengths(inty)==FALSE){g2<-g1; print(paste(i, 'was all inside'))}else{
    g2<-st_intersection(g1, ward_boundary_t); print(paste(i, 'intersected'))}
  
  if(is.null(va_lyr)){va_lyr<-g2}else{
    va_lyr<-rbind(va_lyr, g2)}
  print(i)
}

#st_write(va_lyr, 'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward.gml')

# mosiac vectormap lines

va_files<-list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/')[
  grep('gml',list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/'))]

va_lyr<-NULL
for(i in 1:length(va_files))
{
  #if(i==41){next}
  g1<-st_read(paste0('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/',
                     va_files[i]), layer='Line', quiet=T)
  g1%>%dplyr::select(gml_id, featureDescription)->g1
  
  # filter out unwanted lines
  g1<-g1%>%filter(featureDescription %in% unique(g1$featureDescription)[c(1,2,3,4,5,6,12,13)])
  
  inty<-st_intersects(g1, ward_boundary_t)
  
  if(1%in%lengths(inty)==FALSE){print(paste(i, 'didnt intersect'));next}
  
  if(0%in%lengths(inty)==FALSE){g2<-g1; print(paste(i, 'was all inside'))}else{
    g2<-st_intersection(g1, ward_boundary_t); print(paste(i, 'intersected'))}
  
  if(is.null(va_lyr)){va_lyr<-g2}else{
    va_lyr<-rbind(va_lyr, g2)}
  print(i)
}

st_write(va_lyr, 'C:/trees/spatial_data/vectormap_local/vectormap_line_mosaic_ward.gml')

# clip out area of greenspace coverage inside vectormap_local mosaics

gs<-st_read('C:/trees/spatial_data/greenspace/greenspace_mosaic_ward.gml')
st_crs(gs)<-27700 
gs2<-st_union(gs)
#write_sf(gs2, 'C:/trees/spatial_data/greenspace/greenspace_mosaic_ward_union.gml')

# remove unwanted (plantable) polygons from vectormap area
vm_pol<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward.gml')
st_crs(vm_pol)<-27700 
vm_pol<-vm_pol[vm_pol$featureDescription!='Building Polygon',]
#write_sf(vm_pol, 'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_noBuilding.gml')
#remove other stuff we can't plant on
vm_pol<-vm_pol[-which(vm_pol$featureDescription%in%c('Urban Extent','Railway Bridge',
                                                     'Inland Water Polygon','Glasshouse Polygon',
                                                     'Custom Landform Polygon')),]
#write_sf(vm_pol, 'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_availPlant.gml')

#read in crop layer and clip out fields that overlay local area polys
crops<-st_read('C:/trees/spatial_data/crops2018/lccm-2018_3498720/lccm-2018_3498720.gdb')
st_crs(crops)<-27700 
crops<-crops[ward_boundary_t,]
diss1<-st_difference(crops, st_union(vm_pol))
diss2<-st_cast(diss1, "MULTIPOLYGON")%>%st_cast("POLYGON")
diss2$area<-st_area(diss2)
library(smoothr)
diss3<-drop_crumbs(diss2, 2000) 
#write_sf(diss3, 'C:/trees/spatial_data/crops2018/crops_vectormap_area_clipped3.shp')
# manually edit diss3 in QGIS

# read in crops (diss3) and vectormap polys (vectormap_area_mosaic_ward_availPlant)
# merge together 

#make sure to read in .shp version of file
#lcArea<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_availPlant.gml')
#lcArea<-lcArea%>%st_cast("POLYGON")
#write_sf(lcArea,'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_availPlant2.shp' )
#attr(lcArea, "sf_column") = 'geometry' not working linked to .gml format
lcArea<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_availPlant2.shp')
names(lcArea)[3]<-'featureDescription'
crops<-st_read('C:/trees/spatial_data/crops2018/crops_vectormap_area_clipped3.shp')
st_crs(lcArea)<-27700 
st_crs(crops)<-27700 
crops$featureDescription='field_crops'
crops[crops$crop_name=='Grass',]$featureDescription='field_grass'
cropsNlcA<-rbind(select(lcArea, featureDescription),
      select(crops, featureDescription))
write_sf(cropsNlcA,'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops.gml' )

# clip out greenspace layer
#processing done in qgis
crop_vm<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_cropsBACKUP.shp')
green<-st_read('C:/trees/spatial_data/greenspace/greenspace_mosaic_ward_PUNCHOUT.shp')
st_crs(crop_vm)<-27700 
st_crs(green)<-27700 
crop_vm2<-crop_vm%>%st_buffer(0) # makes valid
green2<-green%>%st_buffer(0)
st_write(green2,'C:/trees/spatial_data/greenspace/greenspace_mosaic_ward_PUNCHOUTvalid.shp')
st_write(crop_vm2,'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_cropsBACKUPvalid.shp')
# Difference() i qgis did the job, read in output
crop_vm_clipped<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops_GSclip.shp')
crop_vm_clipped2<-drop_crumbs(crop_vm_clipped, 2000) 
st_write(crop_vm_clipped2,'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops_GSclip.shp', delete_dsn = T)
#manually edit in qgis then read in and export as gml
cl1<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops_GSclip.shp')
st_write(cl1,'C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops_GSclip.gml')

# final bind of cleaned crops and vectormap areas with greenspace
gs<-st_read('C:/trees/spatial_data/greenspace/greenspace_mosaic_ward.gml')
st_crs(gs)<-27700
vm_crops<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward_crops_GSclip.gml')
st_crs(vm_crops)<-27700
names(vm_crops)[2]<-'primaryFunction'
vm_crops$primaryForm<-'nonGSother'
vm_crops[vm_crops$primaryFunction%in%c('Broad-leafed Woodland','Coniferous Woodland',
        'Mixed Woodland', 'Orchard','Broad-leafed Woodland And Shrub',
        'Coniferous Woodland And Shrub', 'Mixed Woodland And Shrub'),]$primaryForm<-'Woodland'

allPU<-rbind(select(gs, primaryFunction, primaryForm),
                 select(vm_crops, primaryFunction, primaryForm))

st_write(allPU,'C:/trees/modelling/full/greenspace_vectormap_crops_PU.shp')


# create 20 square metre fishnet within ward boundary
#https://rpubs.com/dieghernan/beautifulmaps_I

# intersect with rural lines 

# remove area under 

# calc area of each poly
#st_area
# subset out polys larger than 20 square metres

# apply fishnet to larger polys

# merge fishnet divided polys back into 
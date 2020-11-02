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

# mosiac major raods

va_files<-list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/')[
  grep('gml',list.files('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/'))]

va_lyr<-NULL
for(i in 1:length(va_files))
{
  #if(i==41){next}
  g1<-st_read(paste0('C:/trees/spatial_data/vectormap_local/Download_1492830/vml_3498503/se/',
                     va_files[i]), layer='RoadCline', quiet=T)
  g1%>%dplyr::select(gml_id, featureDescription)->g1
  
  # filter out unwanted lines ALSO run without filter to get all roads!
  g1<-g1%>%filter(featureDescription %in% c('A Road, Alignment', 'A Road, Primary, Alignment',
                  'Minor Road, Alignment', 'B Road, Alignment', 'Motorway, Alignment'))
  
  inty<-st_intersects(g1, ward_boundary_t)
  
  if(1%in%lengths(inty)==FALSE){print(paste(i, 'didnt intersect'));next}
  
  if(0%in%lengths(inty)==FALSE){g2<-g1; print(paste(i, 'was all inside'))}else{
    g2<-st_intersection(g1, ward_boundary_t); print(paste(i, 'intersected'))}
  
  if(is.null(va_lyr)){va_lyr<-g2}else{
    va_lyr<-rbind(va_lyr, g2)}
  print(i)
}

st_write(va_lyr, 'C:/trees/spatial_data/vectormap_local/vectormap_majorRoads_mosaic_ward.gml')
st_write(va_lyr, 'C:/trees/spatial_data/vectormap_local/vectormap_allRoads_mosaic_ward.gml')


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

## Section to update 'Open semi-natural' landcover in greenspace for PUs that are actually woodland
allPU<-read_sf('C:/trees/modelling/full/greenspace_vectormap_crops_PU.shp')
st_crs(allPU)<-27700 
vm_pol<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward.gml')
st_crs(vm_pol)<-27700 

PU_opensemi<-allPU%>%filter(prmryFr=='Open Semi-Natural')
PU_else<-allPU%>%filter(prmryFr!='Open Semi-Natural' | is.na(prmryFr))
vm_forest<-vm_pol%>%filter(featureDescription%in%c('Broad-leafed Woodland','Coniferous Woodland',
             'Mixed Woodland', 'Orchard','Broad-leafed Woodland And Shrub',
             'Coniferous Woodland And Shrub', 'Mixed Woodland And Shrub'))
#inty1<-st_intersects(PU_opensemi, vm_forest)
#PU_opensemi[which(lengths(inty1) > 0),]
# spatial join
PU_opensemi2<-st_join(PU_opensemi, vm_forest, largest=T)
# update
PU_opensemi2[!is.na(PU_opensemi2$fid),]$prmryFn<-as.character(PU_opensemi2[!is.na(PU_opensemi2$fid),]$featureDescription)
PU_opensemi2[!is.na(PU_opensemi2$fid),]$prmryFr<-'Woodland'
PU_opensemi2<-PU_opensemi2%>%select(prmryFn, prmryFr)

allPU2<-rbind(PU_else, PU_opensemi2)
#update prmryFn of woodland in greenspace area
forest_update<-allPU2%>%filter(prmryFr=='Woodland' &
         !prmryFn %in% c('Broad-leafed Woodland','Coniferous Woodland',
          'Mixed Woodland', 'Orchard','Broad-leafed Woodland And Shrub',
          'Coniferous Woodland And Shrub', 'Mixed Woodland And Shrub'))


forest_update<-st_join(forest_update, vm_forest, largest=T)
forest_update$featureDescription<-as.character(forest_update$featureDescription)
# set nas (only occur in greenspace)
forest_update[is.na(forest_update$featureDescription),]$featureDescription<-'Broad-leafed Woodland And Shrub'

allPU2[allPU2$prmryFr=='Woodland' & !is.na(allPU2$prmryFr) &
         !allPU2$prmryFn %in% c('Broad-leafed Woodland','Coniferous Woodland',
   'Mixed Woodland', 'Orchard','Broad-leafed Woodland And Shrub',
   'Coniferous Woodland And Shrub', 'Mixed Woodland And Shrub'),]$prmryFn<-forest_update$featureDescription

#st_write(allPU2,'C:/trees/temp/greenspace_vectormap_crops_PU_update.shp')
st_write(allPU2,'C:/trees/modelling/full/greenspace_vectormap_crops_PU.shp', delete_dsn=T)


#### Create neighbourhood matrix for PUs
# PUs within 10m of each other and not bisected by a major road are 
# said to be neighbours

# use attrib data as already removed some PUs
allPU<-st_read('C:/trees/modelling/demo/final_PU_attrib.shp')
pubuff<-st_buffer(allPU, dist=10) # 10 m buff
roadz<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_majorRoads_mosaic_ward.gml')
st_crs(roadz)<-st_crs(pubuff)
pubuff_road<-st_difference(pubuff, st_union(st_buffer(roadz, 3)))# buffer roads by 3 m
write_sf(pubuff_road, 'C:/trees/modelling/demo/PU_10mbuff_roads.shp', delete_dsn=T)
pubuff_road<-st_read('C:/trees/modelling/demo/PU_10mbuff_roads.shp')
#inty<-st_intersects(pubuff_road, sparse=F)
cm<-connected_matrix(as(pubuff_road, 'Spatial'))
#bm<-boundary_matrix(as(pubuff_road, 'Spatial'), TRUE)
# run with this cm reveals 661 Forested PUs that are not connected (road or > 10m)
pubuff_road$NOconn_tree<-0
# get the unconnected tree areas from matrix
c1<-cm[which(allPU$cost==0),]
pubuff_road[which(allPU$cost==0)[which(rowSums(c1)==0)],]$NOconn_tree<-1
pubuff_road1<-pubuff_road%>%st_buffer(dist=pubuff_road$NOconn_tree*35)

# repeat with 35km buffer to see how many we now have
pubuff_road1$NOconn_tree<-0
cm2<-connected_matrix(as(pubuff_road1, 'Spatial'))
c2<-cm2[which(allPU$cost==0),]
pubuff_road1[which(allPU$cost==0)[which(rowSums(c2)==0)],]$NOconn_tree<-1
# 19 wooded unconnected PUs left, crank bufdist up to 70m
pubuff_road2<-pubuff_road1%>%st_buffer(dist=pubuff_road1$NOconn_tree*70)
pubuff_road2$NOconn_tree<-0
cm3<-connected_matrix(as(pubuff_road2, 'Spatial'))
c3<-cm3[which(allPU$cost==0),]
pubuff_road2[which(allPU$cost==0)[which(rowSums(c3)==0)],]$NOconn_tree<-1
# 9 wooded unconnected PUs left, crank bufdist up to 100m
pubuff_road3<-pubuff_road2%>%st_buffer(dist=pubuff_road2$NOconn_tree*100)
pubuff_road3$NOconn_tree<-0
cm4<-connected_matrix(as(pubuff_road3, 'Spatial'))
c4<-cm4[which(allPU$cost==0),]
pubuff_road3[which(allPU$cost==0)[which(rowSums(c4)==0)],]$NOconn_tree<-1

write_sf(pubuff_road3, 'C:/trees/modelling/demo/PU_10mbuff_roads_updated.shp', delete_dsn=T)


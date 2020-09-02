# create project Planning Unit layer

library(sf)
library(dplyr)
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
  g1<-st_read(paste0('C:/trees/spatial_data/greenspace/MasterMap Greenspace_3498497/se/',gs_files[i]), quiet=T)
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


# create 20 square metre fishnet within ward boundary
#https://rpubs.com/dieghernan/beautifulmaps_I

# intersect with rural lines 

# remove area under 

# calc area of each poly
#st_area
# subset out polys larger than 20 square metres

# apply fishnet to larger polys

# merge fishnet divided polys back into 
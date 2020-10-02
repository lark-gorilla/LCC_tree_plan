# Modelling demo
library(dplyr)
library(sf)
library(raster)
library(prioritizr)
library(ggplot2)

#PU expl
data(sim_pu_polygons)
spplot(sim_pu_polygons, zcol = "cost")
data(sim_features)

# make our layers

#CARBON BENEFIT ~70:100 Mg of C able to be sequestered. NA = non greenspace
car_pot<-raster::subset(sim_features, 1)
car_pot<-car_pot*100
car_pot[values(car_pot)%in%sample(values(car_pot), 60)]<-NA #eliminate some as non greenspace

# SOCIALBENEFIT val: 1= LOW, 2= HIGH
soc_ben<-raster::subset(sim_features, 2)
soc_ben<-raster::cut(soc_ben, breaks=c(0.2, 0.3, 0.4)) # 1= low, 2=high

# EXISTING TREES val: 1= in PA, NA = not
trees<-raster::subset(sim_features, 3)
values(trees)<-1
trees[sample(1:10, 20, replace=T), sample(1:10, 20, replace=T)]<-NA

# HABITAT val: 1= in PA, NA = not
hab_a<-raster::subset(sim_features, 3)
values(hab_a)<-1
hab_a[sample(1:10, 20, replace=T), sample(1:10, 20, replace=T)]<-NA


p1<-problem(sim_pu_polygons, stack(car_pot, soc_ben, trees, hab_a),
            cost_column='cost')
#targets for each layer

targets<-c(500, 20, 5, 5)

p2<-p1%>%add_min_set_objective() %>%   
  add_absolute_targets(targets) %>%
  add_locked_in_constraints('locked_in') %>%
  add_locked_out_constraints('locked_out')

s1<-solve(p2)
plot(s1)

plot(s1, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))

# calculate the number of selected planning units
cellStats(s1, "sum") # if raster
# calculate the cost of the solution
cellStats(sim_pu_raster * s1, "sum")# if raster

# calculate how well features are represented in the solution
feature_representation(p2, s1)

# with real data
wards<-st_read('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/leeds_wards.geojson')
plot(wards['wd16nm'])

hp<-filter(wards, wd16nm=='Hyde Park and Woodhouse')

hp<-st_transform(hp, 27700) # trans to 
# now for all wars
wards<-st_transform(wards, 27700)

#read greenspace and clip to inside all wards
gs<-st_read('C:/trees/spatial_data/greenspace/greenspace_mosaic_ward.gml')
st_crs(gs)<-27700 # missing crs so set
#gs<-st_intersection(gs, hp) # keep as full wards size

# remove certain forms and functions
gs$primaryForm<-as.character(gs$primaryForm)
gs[is.na(gs$primaryForm),]$primaryForm<-'maybe grass'
gs<-gs%>%filter(!primaryForm %in% c('Manmade Surface', 'Inland Water'))
gs<-gs%>%filter(primaryFunction != 'Private Garden') # for moment we remove gardens

#plot(gs['primaryForm'])

#make cost surface probably want to do cost * PU area
table(gs$primaryFunction)
gs$cost<-NA
gs[gs$primaryFunction %in% c('Amenity - Residential Or Business', 'Bowling Green',
                             'Other Sports Facility'),]$cost<-3 # high cost for residential or business amenities
gs[gs$primaryFunction %in% c('Amenity - Transport','Institutional Grounds',
                             'Play Space', 'Playing Field',
                             'Religious Grounds', 'School Grounds', 'Cemetery'),]$cost<-2
# med cost, insitutions that may be interested or council owned or recreation
gs[gs$primaryFunction %in% c('Public Park Or Garden','Land Use Changing', 
                             'Allotments Or Community Growing Spaces', 'Camping Or Caravan Park',
                             'Golf Course','Natural'),]$cost<-1
# low cost 
gs[gs$primaryForm=='Woodland',]$cost<-0 # no cost for existing woods

#check what functions you ge perform
plot(gs['cost'])

# remove columns
gs<-dplyr::select(gs, gml_id, primaryForm, primaryFunction, cost)

#intersect with wards
gs<-st_join(gs, wards['wd16nm'], largest=T)
names(gs)[5]<-'ward'

#read vectormap local area and clip to greenspace -
# WILL NEED TO AMMEND for rural: merge (st_union?) datasets with gs top layer
# for overall planning unit creation thinking something like:
# greenspace-[top]->vectormap_polygons-[top]->polgonized(vectormap_lines)
# NA values inside polygonized(vectormap_lines) filled via extract from raster underlayer 
# and road layer
vm<-st_read('C:/trees/spatial_data/vectormap_local/vectormap_area_mosaic_ward.gml')
st_crs(vm)<-27700 # missing crs so set
# remove urban extent and buildings
vm<-vm%>%filter(!featureDescription %in% c('Urban Extent', 'Building Polygon'))
# clip to ward for this example
vm<-vm[hp,]
# vm<-st_intersection(vm, gs) seemed not to work well
# Think the use in this layer is making sure we don't plant over important habitat
# e.g.heathland, will probably go into cost layer

gs2<-st_join(gs, vm, largest=T) # largest=T maks sure only 1 value
# of x is attrib to y; the one with largest overlap
plot(gs2[c('primaryForm', 'featureDescription')])
gs2[gs2$primaryForm=='maybe grass',] # hmm
# 

# IMD do IMD * PU area, make sense?
imd<-st_read('C:/trees/spatial_data/IMD19/IMD_2019/IMD_2019.shp')
st_crs(imd)<-27700 # detected CRS bit different but IS the same
# clip to ward for this example
#imd<-imd[hp,]
imd<-dplyr::select(imd, IMDDec0) # IMD deciles
imd_mostD30<-filter(imd, IMDDec0<4)
names(imd_mostD30)[1]<-'most30depr'
imd_mostD30$most30depr<-1
#imd_leastD30<-filter(imd, IMDDec0>8)
#names(imd_leastD30)[1]<-'least30depr'
imd_D30to60<-filter(imd, IMDDec0%in%c(4,5,6))
names(imd_D30to60)[1]<-'med30to60depr'
imd_D30to60$med30to60depr<-1

#attribute gs layer
gs<-st_join(gs, imd_mostD30, largest=T)
gs<-st_join(gs, imd_D30to60, largest=T)
# each PU IND set to 1 or NA therefore tarets irrespective of area size

# priority flood mitigation planting
flood<-st_read('C:/trees/spatial_data/flood_mapping/PAF_CFMP456.shp')
st_crs(flood)<-27700 # detected CRS bit different but IS the same
#flood<-flood[hp,]
flood$flood<-1
flood<-dplyr::select(flood, flood)
gs<-st_join(gs, flood, largest=T)


 # Make carbon potential layer
# area of PU * land quality 
# then use forestry calc 
gs$car_pot<-st_area(gs)

# write gs layer
write_sf(gs, 'C:/trees/modelling/demo/leedswards_PU_attrib.shp')

# prioritizR run
pu_ward<-read_sf('C:/trees/modelling/demo/leedswards_PU_attrib.shp')


ggplot(pu_ward)+geom_sf(aes(fill=prmryFn))+theme_bw()

ggplot(pu_ward)+geom_sf(aes(fill=factor(cost)))+theme_bw()

ggplot(pu_ward)+geom_sf(aes(fill=mst30dp))+theme_bw()

ggplot(pu_ward)+geom_sf(aes(fill=md30t60))+theme_bw()

ggplot(pu_ward)+geom_sf(aes(fill=flood))+theme_bw()

ggplot(pu_ward)+geom_sf(aes(fill=car_pot))+theme_bw()

p1<-problem(as(pu_ward, 'Spatial'), features=c('car_pot', 'mst30dp', 
             "md30t60", 'flood'),cost_column='cost')
#targets for each layer

sum(pu_ward$mst30dp, na.rm=T) # 38906
sum(pu_ward$md30t60, na.rm=T) # 18997
sum(pu_ward$flood, na.rm=T) # 2430

sum(pu_ward$car_pot) #max possible 101597964 what m2 of tree cover do we want
sum(pu_ward[pu_ward$cost==0,]$car_pot, na.rm=T) # getting 8458774 m2 for free from exisiting cover
101597964-8458774 # ~93139190 m2,  
# set target anwhere aboe 8.6 ha
# 100000 = plant ~13.6 ha
                  
targets<-c(10000000, 10000,5000, 2000) 

p2<-p1%>%add_min_set_objective() %>%   
  add_absolute_targets(targets)
#%>%  add_boundary_penalties(10, 1) # selects bigger blocks

s1<-solve(p2)

pu_ward$sol<-s1$solution_1
plot(pu_ward['sol'])

write_sf(pu_ward, 'C:/trees/modelling/demo/leedswards_PU_attrib_solution1.shp')

ggplot(hyde_p)+geom_sf(aes(fill=factor(sol)))+theme_bw()


  #add_locked_in_constraints('locked_in') could lock in existing trees rather than cost=0
# less flexible?
#  add_locked_out_constraints('locked_out') removed all buildings/manmade prior to run

s1<-solve(p2)
plot(s1)

plot(s1, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))


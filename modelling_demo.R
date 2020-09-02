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

#read greenspace and clip to inside hp ward
gs<-st_read('C:/trees/spatial_data/greenspace/greenspace_mosaic_ward.gml')
st_crs(gs)<-27700 # missing crs so set
gs<-st_intersection(gs, hp)

# remove certain forms and functions
gs$primaryForm<-as.character(gs$primaryForm)
gs[is.na(gs$primaryForm),]$primaryForm<-'maybe grass'
gs<-gs%>%filter(!primaryForm %in% c('Manmade Surface', 'Inland Water'))
gs<-gs%>%filter(primaryFunction != 'Private Garden') # for moment we remove gardens

plot(gs['primaryForm'])

#make cost surface probably want to do cost * PU area
table(gs$primaryFunction)
gs$cost<-NA
gs[gs$primaryFunction=='Amenity - Residential Or Business',]$cost<-3 # high cost for residential or business amenities
gs[gs$primaryFunction %in% c('Amenity - Transport','Institutional Grounds',
                             'Play Space', 'Playing Field',
                             'Religious Grounds', 'School Grounds'),]$cost<-2
# med cost, insitutions that may be interested or council owned or recreation
gs[gs$primaryFunction %in% c('Public Park Or Garden','Land Use Changing'),]$cost<-1
# low cost 
gs[gs$primaryForm=='Woodland',]$cost<-0 # no cost for existing woods

#check what functions you ge perform
plot(gs['cost'])

# remove columns
gs<-dplyr::select(gs, gml_id, primaryForm, primaryFunction, cost)

#read vectormap local area and clip to greenspace -
# WILL NEED TO AMMEND for rural: merge (st_union?) datasets with gs top layer
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
imd<-imd[hp,]
imd<-dplyr::select(imd, IMDScore)
#attribute gs layer
gs<-st_join(gs, imd, largest=T)

# Make carbon potential layer
# area of PU * land quality 
# then use forestry calc 
gs$car_pot<-st_area(gs)

# write gs layer
write_sf(gs, 'C:/trees/modelling/demo/hyde_park_PU_attrib.shp')

# prioritizR run
hyde_p<-read_sf('C:/trees/modelling/demo/hyde_park_PU_attrib.shp')


ggplot(hyde_p)+geom_sf(aes(fill=prmryFn))+theme_bw()

ggplot(hyde_p)+geom_sf(aes(fill=factor(cost)))+theme_bw()

ggplot(hyde_p)+geom_sf(aes(fill=IMDScor))+theme_bw()

ggplot(hyde_p)+geom_sf(aes(fill=car_pot))+theme_bw()

p1<-problem(as(hyde_p, 'Spatial'), features=c('car_pot', 'IMDScor'),
            cost_column='cost')
#targets for each layer

sum(hyde_p$IMDScor) # 67249.68 # probably makes more sense to cut into bins then use area

sum(hyde_p$car_pot) #max possible 990929.5 what m2 of tree cover do we want
sum(hyde_p[hyde_p$cost==0,]$car_pot) # getting 86301.28 m2 for free from exisiting cover
990929.5-86301.28 # ~900000 m2, 90 ha 
# set target anwhere aboe 8.6 ha
# 100000 = plant ~13.6 ha

targets<-c(500000, 20000) # low ish IMD

p2<-p1%>%add_min_set_objective() %>%   
  add_absolute_targets(targets) %>% 
  add_boundary_penalties(10, 1) # selects biggar blocks

s1<-solve(p2)

hyde_p$sol<-s1$solution_1
plot(hyde_p['sol'])

ggplot(hyde_p)+geom_sf(aes(fill=factor(sol)))+theme_bw()


  #add_locked_in_constraints('locked_in') could lock in existing trees rather than cost=0
# less flexible?
#  add_locked_out_constraints('locked_out') removed all buildings/manmade prior to run

s1<-solve(p2)
plot(s1)

plot(s1, col = c("grey90", "darkgreen"), main = "Solution",
     xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))


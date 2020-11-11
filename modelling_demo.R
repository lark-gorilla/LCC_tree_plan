# Modelling demo
library(dplyr)
library(sf)
library(raster)
library(prioritizr)
library(ggplot2)

#### Real data ####

#read Planning units
gs<-st_read('C:/trees/modelling/full/greenspace_vectormap_crops_PU.shp')
st_crs(gs)<-27700 # missing crs so set

# remove certain forms and functions
gs$prmryFr<-as.character(gs$prmryFr)
gs[is.na(gs$prmryFr),]$prmryFr<-'maybe grass'
gs<-gs%>%filter(!prmryFr %in% c('Manmade Surface', 'Inland Water'))
gs<-gs%>%filter(prmryFn != 'Private Garden') # remove gardens

#make cost surface probably want to do cost * PU area
gs$area<-as.numeric(st_area(gs))
table(gs$prmryFn)
gs$cost<-NA
gs[gs$prmryFn %in% c('Amenity - Residential Or Business', 'Bowling Green',
                             'Other Sports Facility', 'Tennis Court'),]$cost<-3 # high cost for residential or business amenities
gs[gs$prmryFn %in% c('Amenity - Transport','Institutional Grounds',
                             'Play Space', 'Playing Field',
                             'Religious Grounds', 'School Grounds', 'Cemetery','field_crops',
                     'field_grass','Golf Course','Camping Or Caravan Park'),]$cost<-2
# med cost, insitutions that may be interested or council owned or recreation
gs[gs$prmryFn %in% c('Public Park Or Garden','Land Use Changing', 
                             'Allotments Or Community Growing Spaces',
                             'Natural', 'Heathland And Unimproved Grass',
                     'Heathland And Unimproved Grass And Boulders', 'Marsh',
                     'Marsh And Unimproved Grass','Reeds','Refuse Or Slag Heap',
                    'Shingle','Shingle And Sand','Shrub','Shrub And Marsh','Shrub And Marsh And Unimproved Grass',
                    'Shrub And Unimproved Grass','Unimproved Grass','Unimproved Grass And Boulders'),]$cost<-1
# low cost 
gs[gs$prmryFr=='Woodland',]$cost<-0 # no cost for existing woods

gs%>%st_set_geometry(NULL)%>%group_by(prmryFn)%>%
  summarise(cost=paste(unique(cost), collapse=','))%>%as.data.frame()


# Update costby multiplying LCC land cost by 0.1
LCC_land<-st_read('C:/trees/spatial_data/LCC_Own/LCC_Own_03_11_2020.shp')
LCC_land<-na.omit(LCC_land)
st_crs(LCC_land)<-27700 
# buffer fulfils 2 purposes, to reduce LCC_land polys by 5m and to fix invalid err
LCC_land<-st_buffer(LCC_land, -5)
inty1<-st_intersects(gs, LCC_land)
gs[which(lengths(inty1)>0),]$cost<-gs[which(lengths(inty1)>0),]$cost*0.1

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
flood1<-st_read('C:/trees/spatial_data/flood_mapping/Updated_version/WfW 2017 PNW RR YH_region.shp')
flood1$ID<-NULL
flood1$GRIDCODE<-NULL
flood2<-st_read('C:/trees/spatial_data/flood_mapping/Updated_version/WfW 2017 PNW YH_region.shp')
flood<-rbind(flood1, flood2)

st_crs(flood)<-27700 # detected CRS bit different but IS the same
flood$flood<-1
flood<-dplyr::select(flood, flood)
gs<-st_join(gs, flood, largest=T)
# edit flood to area of flood
gs$flood<-gs$flood*gs$area

# sssi extract last so overrides other costs
sssi<-read_sf('C:/Users/fbsmmi/OneDrive - University of Leeds/LCC_trees/spatial_data/sites_special_scientific_interest.geojson')
#Join in SSSI data and attribute, then modify cost column
# if SSSI=T & not woodland = cost=3
sssi_int<-st_intersects(gs, sssi)
gs[which(lengths(sssi_int)>0 & gs$prmryFr!='Woodland'),]$cost<-3

 # Make carbon potential layer (area)
gs$car_pot<-gs$area

# Make carbon potential layer yield
yield<-st_read('C:/trees/spatial_data/yield_class/Broadleaved Woodlands.gpkg')

yield<-dplyr::select(yield, X2050_mean_yc)
gs<-st_join(gs, yield, largest=T)

#correct max sizes to below 1000000 for gurobi
gs[gs$car_pot>1e+6,]$flood<-800000
gs[gs$car_pot>1e+6,]$car_pot<-800000
gs$area<-NULL
# write gs layer
write_sf(gs, 'C:/trees/modelling/demo/final_PU_attrib.shp', delete_dsn=T)


# prioritizR run
pu_all<-read_sf('C:/trees/modelling/demo/final_PU_attrib.shp')
names(pu_all)[names(pu_all)=='X2050__']<-'yield'
#neighbourhood constraint matrix (within 10m and not crossed by main road)
pubuff_road<-st_read('C:/trees/modelling/demo/PU_10mbuff_roads_updated.shp')# read manually edited file
cm<-connected_matrix(as(pubuff_road, 'Spatial'))
pu_all$cost2<-pu_all$car_pot*pu_all$cost
# make both deprovation indices areas based
pu_all$mst30dp<-pu_all$car_pot*pu_all$mst30dp
pu_all$md30t60<-pu_all$car_pot*pu_all$md30t60

p1<-problem(as(pu_all, 'Spatial'), 
            features=c('car_pot', 'yield','mst30dp', "md30t60", 'flood'),
            cost_column='cost2')
#targets for each layer
sum(pu_all[pu_all$cost==0,]$car_pot, na.rm=T)# new existing tree cover=68km2
# so crank target to 140km2

# All targets apart from yield now run on area.All at +/- 50%, apart from med deprivation @ 25% 
targets<-c(140000000, 379419, 30208665,23600052, 25609198)

p2<-p1%>%add_min_set_objective() %>%   
  add_absolute_targets(targets)%>%
  add_neighbor_constraints(k=1, data=cm) %>%
  add_locked_in_constraints(locked_in=pu_all$cost==0)
presolve_check(p2)

s1<-solve(p2) 

pu_all$sol<-s1$solution_1

write_sf(pu_all, 'C:/trees/modelling/demo/leedswards_PU_attrib_solution1.shp', delete_dsn=T)

# convert to wgs for leaflet
library(rgdal)
library(gdalUtils)
src_datasource_name <- 'C:/trees/modelling/demo/leedswards_PU_attrib_solution1.shp'
dst_datasource_name <- 'C:/trees/modelling/demo/leedswards_PU_attrib_solution1_wgs.shp'

ogr2ogr(src_datasource_name,dst_datasource_name,
        s_srs="+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs +nadgrids=C:/trees/spatial_data_downloads/OSTN02_NTv2.gsb",
        t_srs="EPSG:4326",verbose=TRUE)




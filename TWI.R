library(raster)
library(sf)
library(here)
library(tidyverse)
library(viridis)
library(whitebox)


# load DEM and field data  ------------------------------------------------


DEM<-raster(here('UMD_technical_assessment', 
                 'USGS_one_meter_x33y433_MD_VA_Sandy_NCR_2014_crop.tif'
)
)
fields<-st_read(here('UMD_technical_assessment', 
                     'North_farm_fields.shp'
)
)


# check projections of two layers -----------------------------------------

projection(DEM) #this is NAD83
crs(fields) #this is WGS84

#transform vector projection to match the DEM raster
fields<-st_transform(fields,
                     crs = st_crs(DEM))


#check projections of two layers
projection(DEM) #NAD83
crs(fields) #this is now NAD83

st_write(fields, "outputs/fields_NAD83.shp")



# plot layers ---------------------------------------------------------------

DEM_df<-as.data.frame(DEM, xy=T)

ggplot()+
  geom_raster(data=DEM_df,
              aes(x=x,y=y, fill=USGS_one_meter_x33y433_MD_VA_Sandy_NCR_2014_crop))+
  scale_fill_viridis(option = 'mako')+
  geom_sf(data=fields, fill = NA, color = "white")


# calculate slope and topographic wetness index using raster and whitebox packages----------------------------------

# slope = raster::terrain(DEM,opt='slope', unit='degrees')
# 
# raster::writeRaster(slope,'outputs/slope', format='GTiff')


wbt_fill_single_cell_pits(
  dem = 'UMD_technical_assessment/USGS_one_meter_x33y433_MD_VA_Sandy_NCR_2014_crop.tif',
  output = 'outputs/dem_filled.tif')

wbt_breach_depressions_least_cost(
  dem='outputs/dem_filled.tif',
  output = 'outputs/filled_breached.tif',
  dist = 5,
  fill = TRUE)


wbt_slope(dem = "outputs/filled_breached.tif",
          output = "outputs/slope.tif",
          units = "degrees")


wbt_d_inf_flow_accumulation(input = 'outputs/filled_breached.tif',
                            output = 'outputs/sca.tif',
                            out_type = 'Specific Contributing Area')

# fb<-raster('filled_breached.tif')
# fb_df<-as.data.frame(fb)
# 
# sca<-raster('sca.tif')
# sca_df<-as.data.frame(sca)

wbt_wetness_index(sca = 'outputs/sca.tif',
                  slope = 'outputs/slope.tif',
                  output = 'outputs/topgraphic_wetness.tif')


# map slope and TWI -------------------------------------------------------


slope<-raster('outputs/slope.tif')
crs(slope)
slope_df<-as.data.frame(slope, xy=T)


TWI<-raster('outputs/topgraphic_wetness.tif')
TWI_df<-as.data.frame(TWI, xy=T)

ggplot()+
  geom_raster(data=slope_df,
              aes(x=x,y=y, fill=slope))+
  # scale_fill_viridis(option = 'plasma', direction=-1)+
  geom_sf(data=fields, fill = NA, color = "black")


ggplot()+
  geom_raster(data=TWI_df,
              aes(x=x,y=y, fill=topgraphic_wetness))+
  #scale_fill_viridis(option = 'plasma', direction=-1)+
  geom_sf(data=fields, fill = NA, color = "black")

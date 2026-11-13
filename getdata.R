## Get Data

# Setup -------------------------------------------------------------------
## Specify Python venv
my_venv <- "C:/Users/jajce/Documents/Class/FWE_711/711_project/.venv/"
venv_python <- "C:/Users/jajce/Documents/Class/FWE_711/711_project/.venv/Scripts/python.exe"
Sys.setenv(RETICULATE_PYTHON = venv_python)

library(reticulate)
use_virtualenv(my_venv, required = TRUE)
use_python(venv_python, required = TRUE)

## Initialize Google Earth Engine
library(rgee)
ee_clean_user_credentials()
ee_Authenticate()
ee_Initialize()


# Locations ---------------------------------------------------------------
## Extract Wisconsin
states <- ee$FeatureCollection("TIGER/2018/States")
wisconsin <- states$filter(ee$Filter$eq('NAME', 'Wisconsin'))

## Randomly Choose nlocs=2000 locations across the state 
nlocs <- 2000
points <- ee$FeatureCollection$randomPoints(
  region = wisconsin$geometry(), 
  points = nlocs, 
  seed = 32)

## Extract Lat and Lon
lat_lon_bands <- ee$Image$pixelLonLat()$rename(c('Longitude', 'Latitude'))


# Get Data from GEE -------------------------------------------------------
### Climate Baselines -----------------------------------------------------
## TerraClimate (monthly data from [2011-2020])
## https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE#bands
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate('2011-01-01', '2020-12-31')$
  select(c('tmmx', ## Max Monthly Temperature
           'tmmn', ## Min Monthly Temperature
           'pr',   ## Precipitation
           'pdsi', ## Drought Severity Index
           'soil', ## Soil Moisture
           'swe',  ## Snow Water Equivalent
           'def',  ## Climate Water Deficit
           'srad', ## Downward Surface Shortwave Radiation
           'vap'   ## Vapor Pressure
  ))

## Perform Summary Operations on the monthly data (locationwise)
tc_mean <- terraclimate$mean()
tc_sd <- terraclimate$reduce(ee$Reducer$stdDev())
tc_max <- terraclimate$max()
tc_min <- terraclimate$min()

## Scale and Combine Data
tc_scaled <- (
  ### Variables Means
  tc_mean$select('tmmx')$multiply(0.1)$rename('MaxTemp_mean')$addBands(
  tc_mean$select('tmmn')$multiply(0.1)$rename('MinTemp_mean'))$addBands(
  tc_mean$select('pr')$rename('Precip_mean'))$addBands(
  tc_mean$select('pdsi')$multiply(0.01)$rename('PDSI_mean'))$addBands(
  tc_mean$select('soil')$multiply(0.1)$rename('SoilMoisture_mean'))$addBands(
  tc_mean$select('swe')$rename('SWE_mean'))$addBands(
  tc_mean$select('def')$multiply(0.1)$rename('WaterDef_mean'))$addBands(
  tc_mean$select('srad')$multiply(0.1)$rename('SurfaceRad_mean'))$addBands(
  tc_mean$select('vap')$multiply(0.001)$rename('VaporPressure_mean'))$addBands(
  ### Variable SDs
  tc_sd$select('tmmx_stdDev')$multiply(0.1)$rename('MaxTemp_sd'))$addBands(
  tc_sd$select('tmmn_stdDev')$multiply(0.1)$rename('MinTemp_sd'))$addBands(
  tc_sd$select('pr_stdDev')$rename('Precip_sd'))$addBands(
  tc_sd$select('pdsi_stdDev')$multiply(0.01)$rename('PDSI_sd'))$addBands(
  tc_sd$select('soil_stdDev')$multiply(0.1)$rename('SoilMoisture_sd'))$addBands(
  tc_sd$select('swe_stdDev')$rename('SWE_sd'))$addBands(
  tc_sd$select('def_stdDev')$multiply(0.1)$rename('WaterDef_sd'))$addBands(
  tc_sd$select('srad_stdDev')$multiply(0.1)$rename('SurfaceRad_sd'))$addBands(
  tc_sd$select('vap_stdDev')$multiply(0.001)$rename('VaporPressure_sd'))$addBands(
  ### Variable Maxs
  tc_max$select('tmmx')$multiply(0.1)$rename('MaxTemp_max'))$addBands(
  tc_max$select('tmmn')$multiply(0.1)$rename('MinTemp_max'))$addBands(
  tc_max$select('pr')$rename('Precip_max'))$addBands(
  tc_max$select('pdsi')$multiply(0.01)$rename('PDSI_max'))$addBands(
  tc_max$select('soil')$multiply(0.1)$rename('SoilMoisture_max'))$addBands(
  tc_max$select('swe')$rename('SWE_max'))$addBands(
  tc_max$select('def')$multiply(0.1)$rename('WaterDef_max'))$addBands(
  tc_max$select('srad')$multiply(0.1)$rename('SurfaceRad_max'))$addBands(
  tc_max$select('vap')$multiply(0.001)$rename('VaporPressure_max'))$addBands(
  ### Variable Mins
  tc_min$select('tmmx')$multiply(0.1)$rename('minTemp_min'))$addBands(
  tc_min$select('tmmn')$multiply(0.1)$rename('MinTemp_min'))$addBands(
  tc_min$select('pr')$rename('Precip_min'))$addBands(
  tc_min$select('pdsi')$multiply(0.01)$rename('PDSI_min'))$addBands(
  tc_min$select('soil')$multiply(0.1)$rename('SoilMoisture_min'))$addBands(
  tc_min$select('swe')$rename('SWE_min'))$addBands(
  tc_min$select('def')$multiply(0.1)$rename('WaterDef_min'))$addBands(
  tc_min$select('srad')$multiply(0.1)$rename('SurfaceRad_min'))$addBands(
  tc_min$select('vap')$multiply(0.001)$rename('VaporPressure_min')))
  
  

### Elevation and Topography ----------------------------------------------
## Topography
## https://developers.google.com/earth-engine/datasets/catalog/USGS_SRTMGL1_003#bands
elevation <- ee$Image('USGS/SRTMGL1_003')

## Add the Slope and Aspect to elevation
elevation_bands <- elevation$
  addBands(ee$Terrain$slope(elevation))$
  addBands(ee$Terrain$aspect(elevation))$
  rename(c('Elevation', 'Slope', 'Aspect'))



### Soil Properties -------------------------------------------------------
## Soil Clay from OpenLandMap
## https://developers.google.com/earth-engine/datasets/catalog/OpenLandMap_SOL_SOL_CLAY-WFRACTION_USDA-3A1A1A_M_v02
soil_clay <- ee$Image("OpenLandMap/SOL/SOL_CLAY-WFRACTION_USDA-3A1A1A_M/v02")$
  select(c('b0',   ## clay content at 0 cm depth
           'b10',  ## clay content at 10 cm depth
           'b30',  ## clay content at 30 cm depth 
           'b60',  ## clay content at 60 cm depth 
           'b100', ## clay content at 100 cm depth 
           'b200'  ## clay content at 200 cm depth 
  )) 

## Soil water capacity from OpenLandMap
## https://developers.google.com/earth-engine/datasets/catalog/OpenLandMap_SOL_SOL_WATERCONTENT-33KPA_USDA-4B1C_M_v01
soil_water <- ee$Image("OpenLandMap/SOL/SOL_WATERCONTENT-33KPA_USDA-4B1C_M/v01")$
  select(c('b0',   ## Soil water capacity at 0 cm depth
           'b10',  ## Soil water capacity at 10 cm depth
           'b30',  ## Soil water capacity at 30 cm depth 
           'b60',  ## Soil water capacity at 60 cm depth 
           'b100', ## Soil water capacity at 100 cm depth 
           'b200'  ## Soil water capacity at 200 cm depth 
  )) 

## Combine Soil Data
soil_bands <- soil_clay$addBands(soil_water)$rename(
  c(paste('Soil_Clay', c('b0', 'b10', 'b30', 'b60', 'b100', 'b200'), sep="_"),
    paste('Soil_Water_Cap', c('b0', 'b10', 'b30', 'b60', 'b100', 'b200'), sep="_")))



### Ecological Stress -----------------------------------------------------
## MODIS Vegetation (Every 16 days for Summer 22021 )
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13A1#bands
modis_veg <- ee$ImageCollection("MODIS/061/MOD13A1")$
  filterDate('2021-06-01', '2021-08-31')$
  select(c('NDVI',  ## Normalized Difference Vegetation Index
           'EVI'    ##  Enhanced Vegetation Index
  ))

## MODIS GPP (Every 8 days for Summer 22021 )
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A2H#bands
modis_gpp <- ee$ImageCollection("MODIS/061/MOD17A2H")$
  filterDate('2021-06-01', '2021-08-31')$
  select('Gpp')   ## Gross Primary Productivity

## MODIS LST (Every 8 days for Summer 2021)
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD11A2
modis_lst <- ee$ImageCollection("MODIS/061/MOD11A2")$
  filterDate('2021-06-01', '2021-08-31')$
  select('LST_Day_1km')   ## Land Surface Temperature

## MODIS ET (Every 8 days for Summer 2021)
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD16A2
modis_et <- ee$ImageCollection("MODIS/061/MOD16A2")$
  filterDate('2021-06-01', '2021-08-31')$
  select('ET',   ## Evapotranspiration 
         'PET')  ## Potential ET
## Ratio of EF/PET
summer_et <- modis_et$select("ET")$sum()
summer_pet <- modis_et$select("PET")$sum()
et_pet_ratio <- summer_et$divide(summer_pet)$rename("ET_PET_Ratio")

## MODIS Surface Reflectance (Every 8 days for Summer 2021)
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD09A1
modis_sr <- ee$ImageCollection("MODIS/061/MOD09A1")$
  filterDate('2021-06-01', '2021-08-31')$
  select('sur_refl_b02',   ## Near Infrared Reflectance
         'sur_refl_b06')   ## Short-wave Infrared Reflectance
## Function to Calculate NDWI from bands 2 and 6
add_ndwi <- function(image) {
  ndwi <- image$normalizedDifference(c('sur_refl_b02', 'sur_refl_b06'))$rename('NDWI')
  return(image$addBands(ndwi))
}
modis_ndwi <- modis_sr$map(add_ndwi)

## MODIS Tree Cover (Annual for 2021)
## https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD44B
tree_cover <- ee$ImageCollection("MODIS/061/MOD44B")$
  filterDate("2021-01-01", "2021-12-31")$
  first()$
  select("Percent_Tree_Cover",
         "Percent_NonTree_Vegetation",
         "Percent_NonVegetated")

## Perform Summary Operations on the MODIS data (locationwise)
modis_veg_mean <- modis_veg$mean()
modis_veg_sd <- modis_veg$reduce(ee$Reducer$stdDev())
modis_veg_max <- modis_veg$max()
modis_veg_min <- modis_veg$min()
modis_gpp_mean <- modis_gpp$mean()
modis_gpp_sd <- modis_gpp$reduce(ee$Reducer$stdDev())
modis_gpp_max <- modis_gpp$max()
modis_gpp_min <- modis_gpp$min()
modis_lst_mean <- modis_lst$mean()
modis_lst_sd <- modis_lst$reduce(ee$Reducer$stdDev())
modis_lst_max <- modis_lst$max()
modis_lst_min <- modis_lst$min()
modis_et_mean <- modis_et$mean()
modis_et_sd <- modis_et$reduce(ee$Reducer$stdDev())
modis_et_max <- modis_et$max()
modis_et_min <- modis_et$min()
modis_ndwi_mean <- modis_ndwi$mean()
modis_ndwi_sd <- modis_ndwi$reduce(ee$Reducer$stdDev())
modis_ndwi_max <- modis_ndwi$max()
modis_ndwi_min <- modis_ndwi$min()

## Scale and Combine Data
modis_scaled <- (
  ### Variable Means
  modis_veg_mean$select('NDVI')$multiply(0.0001)$rename('NDVI_mean')$addBands(
  modis_veg_mean$select('EVI')$multiply(0.0001)$rename('EVI_mean'))$addBands(
  modis_gpp_mean$select('Gpp')$multiply(0.0001)$rename('GPP_mean'))$addBands(
  modis_lst_mean$select('LST_Day_1km')$multiply(0.02)$rename('LST_mean'))$addBands(
  modis_et_mean$select('ET')$multiply(0.1)$rename('ET_mean'))$addBands(
  modis_et_mean$select('PET')$multiply(0.1)$rename('PET_mean'))$addBands(
  modis_ndwi_mean$select('NDWI')$rename('NDWI_mean'))$addBands(
  ### Variable SDs
  modis_veg_sd$select('NDVI_stdDev')$multiply(0.0001)$rename('NDVI_sd'))$addBands(
  modis_veg_sd$select('EVI_stdDev')$multiply(0.0001)$rename('EVI_sd'))$addBands(
  modis_gpp_sd$select('Gpp_stdDev')$multiply(0.0001)$rename('GPP_sd'))$addBands(
  modis_lst_sd$select('LST_Day_1km_stdDev')$multiply(0.02)$rename('LST_sd'))$addBands(
  modis_et_sd$select('ET_stdDev')$multiply(0.1)$rename('ET_sd'))$addBands(
  modis_et_sd$select('PET_stdDev')$multiply(0.1)$rename('PET_sd'))$addBands(
  modis_ndwi_sd$select('NDWI_stdDev')$rename('NDWI_sd'))$addBands(
  ### Variable Maxs
  modis_veg_max$select('NDVI')$multiply(0.0001)$rename('NDVI_max'))$addBands(
  modis_veg_max$select('EVI')$multiply(0.0001)$rename('EVI_max'))$addBands(
  modis_gpp_max$select('Gpp')$multiply(0.0001)$rename('GPP_max'))$addBands(
  modis_lst_max$select('LST_Day_1km')$multiply(0.02)$rename('LST_max'))$addBands(
  modis_et_max$select('ET')$multiply(0.1)$rename('ET_max'))$addBands(
  modis_et_max$select('PET')$multiply(0.1)$rename('PET_max'))$addBands(
  modis_ndwi_max$select('NDWI')$rename('NDWI_max'))$addBands(
  ### Variable Mins
  modis_veg_min$select('NDVI')$multiply(0.0001)$rename('NDVI_min'))$addBands(
  modis_veg_min$select('EVI')$multiply(0.0001)$rename('EVI_min'))$addBands(
  modis_gpp_min$select('Gpp')$multiply(0.0001)$rename('GPP_min'))$addBands(
  modis_lst_min$select('LST_Day_1km')$multiply(0.02)$rename('LST_min'))$addBands(
  modis_et_min$select('ET')$multiply(0.1)$rename('ET_min'))$addBands(
  modis_et_min$select('PET')$multiply(0.1)$rename('PET_min'))$addBands(
  modis_ndwi_min$select('NDWI')$rename('NDWI_min'))$addBands(
  tree_cover$select("Percent_Tree_Cover"))$addBands(
  tree_cover$select("Percent_NonTree_Vegetation"))$addBands(
  tree_cover$select("Percent_NonVegetated"))$addBands(
  et_pet_ratio$select("ET_PET_Ratio")))
  

# Combine and Sample Data -------------------------------------------------
## Combine all data sources 
final_image <- tc_scaled$
  addBands(elevation_bands)$
  addBands(soil_bands)$
  addBands(modis_scaled)$
  addBands(lat_lon_bands)

## Sample Combined Data on Google Earth Engine
print("Sampling data on Google servers...")
sampled_data <- final_image$sampleRegions(
  collection = points,
  scale = 4000,       ## Match the largest resolution of 4km grid cells 
  tileScale = 4,      ## Split up the map for processing
  geometries = FALSE  ## Don't include the geometry
)

## Download Data to my Computer
print("Downloading tabular data into R...")
data_list <- sampled_data$getInfo()$features
df <- do.call(rbind, lapply(data_list, function(x) as.data.frame(x$properties)))

## Write to CSV for later
write.csv(df, './wisconsin_climate_data.csv')

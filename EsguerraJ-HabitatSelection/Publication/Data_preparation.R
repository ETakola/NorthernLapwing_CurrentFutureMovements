# Research: Tracking the Hidden Niches: Movement-Based Insights into Northern Lapwing Individual variation and Conservation.

# All datasets must use the ETRS89 / LAEA Europe projection (EPSG:3035). 
# If a dataset uses a different CRS, it must be reprojected to ensure consistency between rasters and shapefiles.

library(sf)          # Manage maps and spatial data
library(dplyr)       # Data manipulation and transformation 
library(lubridate)   # Handle dates and times fields
library(amt)         # Animal movement analysis 
library(purrr)       # Functional programming for lists and data
library(raster)      # Manage raster files
library(terra)       # Tools to manage rasters and shapefiles
library(lunar)       # Calculate moon parameters
library(tidyr)       # Helps tidy up data
library(readr)       # Manage CSVs files
library(units)       # Keeps track of measurement units



###############################################################################
# 1. Load the shapefile containing the tracking data
###############################################################################

ssf_dat_shp <- st_read("/your_path/tracking_data_laea.shp")

###############################################################################
# 2. Convert the timestamp is in text format POSIXct
###############################################################################

ssf_dat_shp <- ssf_dat_shp %>%
  mutate(timestamp = parse_date_time(timestamp, orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM")))

###############################################################################
# 3. Retrieve the spatial coordinates (x_, y_) for further analysis.
###############################################################################

ssf_dat_shp <- ssf_dat_shp %>%
  mutate(x_ = st_coordinates(.)[, 1],
         y_ = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()


###############################################################################
# 4. Create a track object to represent animal movement paths.
###############################################################################

ssf_dat_trk <- make_track(ssf_dat_shp, x_, y_, timestamp, id = tag_ident, ind_ident, crs = 3035)

ssf_dat_trk %>%
  count(x_, y_) %>%       
  filter(n > 2) %>%     
  arrange(desc(n))      

# Save this intermediate output.
saveRDS(ssf_dat_trk, file = "/your_path/track.rds")


###############################################################################
# 5. Resampling tracking data object, filter bursts with less than 3 positions
#    and adjust duplicated points
###############################################################################

# Resampling rate of 180 min with 30 seconds of tolerance.
ssf_dat_trk180 <- ssf_dat_trk %>%
  track_resample(rate = minutes(180), tolerance = seconds(30), keep_cols = TRUE)

# Filter bursts with less than 3 positions, otherwise it will show an error in the generation of the random steps.
ssf_dat_trk180 <- ssf_dat_trk180 %>%
  filter_min_n_burst(min_n = 3)

# It avoids the exact overlapping of positions, for which a small distance of +0.001 m is added when there are duplicate points.
# The comparison of coordinates (x_, y_) must be done per individual and within each burst.

ssf_dat_trk180 <- ssf_dat_trk180 %>%
  group_by(x_, y_) %>%
  mutate(dup_n = row_number() - 1) %>%
  ungroup() %>%
  mutate(
    x_ = x_ + dup_n * 0.001,
    y_ = y_ + dup_n * 0.001
  ) %>%
  dplyr::select(-dup_n)

#To know the number of points by individual after resampling
ssf_dat_trk180 %>%
  count(id, name = "n_points_after_resample")

# Saving intermediate results throughout the code is suggested in case you need to secure your progress.
# Save this intermediate output.
saveRDS(ssf_dat_trk180, file = "/your_path/track_resampled180min.rds")

###############################################################################
# 6. From the track create the random steps: 10 random steps by 1 used step.
###############################################################################

# Delete individual with id = 5286, since it just have 4 used steps, no enough to be included in the analysis.

ssf_dat_trk180_filt <- ssf_dat_trk180 %>%
  filter(id != "5286")

# Generation of random steps from the used steps, including the calculation of the movement characteristics: step lenght and turning angle. 

ssf_dat_steps <- ssf_dat_trk180_filt %>%
  steps_by_burst(burst_ = burst_) %>%
  filter(
    !is.na(sl_),
    !is.na(ta_)
  ) %>%
  random_steps(n = 10) %>%
  group_by(step_id_) %>%
  mutate(
    log_sl_ = log(sl_),
    cos_ta_ = cos(ta_)
  ) %>% 
  ungroup()  %>% 
  filter(!is.na(x2_) & !is.na(y2_) & !is.na(log_sl_) & !is.na(cos_ta_)) 


# Save this intermediate output.
saveRDS(ssf_dat_steps, file = "/your_path/steps.rds")


###############################################################################
# 7. Recover of individual identification data.
###############################################################################

# When the random steps area created, automatically the other fields from the track are removed
# We recover important fields like id(tag_id) and ind_ident(name given to each individual).
trk_ind_data <- ssf_dat_trk180 %>%
  dplyr::select(x_1 = x_, y_1 = y_, t_1 = t_, id, ind_ident)

ssf_dat_steps <- ssf_dat_steps %>%
  left_join(trk_ind_data, by = c("x1_" = "x_1", "y1_" = "y_1", "t1_" = "t_1"))

# Save this intermediate output.
saveRDS(ssf_dat_steps, file = "/your_path/steps.rds")


###############################################################################
# 8. Extraction of environmental variables values 
###############################################################################

# Convert the data in track format to simple feature. This is necessary to perform spatial operations.

# Copy the fields x2_, y2_ and use them as a reference in the Simple Feature format, since the fields containing 
# the reference coordinates will disappear during conversion.

ssf_dat_steps <- ssf_dat_steps %>%
  mutate(x_coord = x2_, y_coord = y2_)
saveRDS(ssf_dat_steps, file = "/your_path/sf.rds")

# Convert the steps to a simple feature (sf) format to be able of extracting the environmental data. 
ssf_dat_sf <- st_as_sf(ssf_dat_steps, coords = c("x_coord", "y_coord"), crs = 3035)
saveRDS(ssf_dat_steps, file = "C:/your_path/sf.rds")


############# 8.1 Extraction earthworm factor abundance from Raster 

etf_abundance <- raster("/your_path/Et_Factor_Abundance_3035.tif")
# Extract raster values for each used/random step
cat("Extracting earthworm factor abundance values\n")
ssf_dat_sf$etf_abundance <- raster::extract(etf_abundance, ssf_dat_sf)


############# 8.2 Extraction earthworm factor abundance-richnessfrom Raster

etf_abundance_r <- raster("/your_path/Et_factor_abundance_Richness_3035.tif")
# Extract raster values for each used/random step
cat("Extracting earthworm factor abundance-richness values\n")
ssf_dat_sf$etf_abundance_r <- raster::extract(etf_abundance_r, ssf_dat_sf)


############# 8.3 Extraction of "Moon phase" and "Distance to the moon" from the R Package "Lunar".

## The "Moon phase" Return 4 category labels for lunar phases ("New"    "Waxing" "Full"   "Waning") 
## and "Distance to the moon" returns the distance of the moon from the earth on specified date in Km. 

ssf_dat_sf <- ssf_dat_sf %>%
  mutate(
    moon_phase = lunar.phase(t2_, name = TRUE),
    moon_dist  = lunar.distance(t2_)
  )


############# 8.4 Extraction of "Population density by NUTS 3 regions" in shapefile format

#To extract the population density values, a spatial intersection is made between points (steps) and polygons (NUTS 3).
# Load the polygon shapefile of population density type 
pop_density <- st_read("/your_path/nuts3_density.shp")

# Performs the spatial join to bring the "density" field from the shapefile.
ssf_dat_sf <- st_join(ssf_dat_sf, pop_density["density"])

# To enhance clarity, the field has been renamed.
ssf_dat_sf <- ssf_dat_sf %>%
  rename(pop_dens = density)



############# 8.5 Extraction of crops intensity values from Raster GCI30 

# Read the cropping intensity raster.
crop_intens <- raster("/your_path/GCI30_CroppingIntensity_3035.tif")
# Extract raster values for each used/random step
cat("Extracting crops intensity factor values\n")
ssf_dat_sf$crop_intens <- raster::extract(crop_intens, ssf_dat_sf)



############# 8.6 Extraction of Pesticides ingredients from Raster

# Read the ingredients raster
p_glyphosate <- raster("/your_path/APR_Glyphosate_H.tif")
p_propiconazole <- raster("/your_path/APR_Propiconazole_H.tif")

# Extract raster values for each used/random step
cat("Extracting application values of Pesticides ingredientes\n")
ssf_dat_sf$p_glyphosate <- raster::extract(p_glyphosate, ssf_dat_sf)
ssf_dat_sf$p_propiconazole <- raster::extract(p_propiconazole, ssf_dat_sf)



############# 8.7 Extraction of climate variables from CSV - ERA 5 hourly dataset 

### 8.7.1 Generate subset to get Climate ERA-5 data  
# If the positions of the steps are modified (e.g. in case of using a different rate resampling), 
# you will have to generate again the climate data, since these are obtained in a specific way for each coordinate.
# See the script "Generate_ERA5climate.js"
# If the steps are the same as those proposed by default, go directly to 8.7.2. otherwise, follow these steps:

# Get a subset of the simple feature to be used as input in the retrieve of the climate data in Google Earth Engine (GEE)
# Prepare a data frame with coordinates, id and right timestamp %Y-%m-%dT%H:%M:%SZ format.

# Add and identifier to each row (step)
ssf_dat_sf <- ssf_dat_sf %>%
  mutate(id_row = row_number())

# Reproject the step files to crs = 4326, as this is often used in the GEE code editor environment.
steps_coords <- st_transform(ssf_dat_sf, 4326)
#Get just the coordinates
coords <- st_coordinates(steps_coords)

# Create the dataframe with the variables needed to extract the data from ERA5
df_gee <- ssf_dat_sf %>%
  st_drop_geometry() %>%
  dplyr::select(id_row, t2_) %>%
  mutate(
    lon = coords[,1],
    lat = coords[,2],
    timestamp = format(t2_, "%Y-%m-%dT%H:%M:%SZ")
  )

# The original t2_ field is removed and the timestamp is left in the format required by GEE.
df_gee <- df_gee %>%
  dplyr::select(-t2_)

#Exportar en csv
write.csv(df_gee, "/your_path/climate_input_GEE.csv", row.names = FALSE)


### 8.7.2 Extract the climate data if it's already generated

#call the csv with the weather data in the form of a data frame that we extracted from GEE from the script “ERA_5”.
clim_data <- read.csv("/your_path/era5_hourly_climate.csv")

#Convert temperature from K° to C°.
clim_data <- clim_data %>%
  mutate(temp_2m = temperature_2m - 273.15)

#We calculate the wind speed considering the components u and v
clim_data <- clim_data %>%
  mutate(
    wind_speed_10m = sqrt(u_component_of_wind_10m^2 + v_component_of_wind_10m^2)
  )

#Rename fields 
clim_data <- clim_data %>%
  rename(
    uc_wind_10m = u_component_of_wind_10m,
    vc_wind_10m = v_component_of_wind_10m
  )

clim_data <- clim_data %>%
  rename(
    t_precipitation = total_precipitation
  )

# After get the climate data by point we joined it to the simple feature main dataset. 
# Perform the left_join, ensuring the correspondence between the input information and the steps using the id_row created earlier.
ssf_dat_sf <- ssf_dat_sf %>%
  left_join(
    clim_data %>%
      dplyr::select(id_row, temp_2m, t_precipitation, uc_wind_10m, vc_wind_10m, wind_speed_10m),
    by = c("id_row" = "id_row")
  )


############# 8.8 Extraction of Soil Water Index "SWI" from raster

# Read SWI raster
swi_t1 <- raster("/your_path/SWI_20210414_T002_3035.tif")
cat("Extracting the Soil Water Index values by step\n")
# Extract raster values for each used/random step
ssf_dat_sf$swi_t1 <- raster::extract(swi_t1, ssf_dat_sf)



############# 8.9 Extraction of Terrestrial Human Footprint "THP" from raster #######################

# Read HTP raster
thp <- raster("/your_path/hfp_100m_200Unified_3035.tif")
# Extract raster values for each used/random step
cat("Extracting the Human Footprint values by step\n")
ssf_dat_sf$thp <- raster::extract(thp, ssf_dat_sf)



############# 8.10 Extraction of SoilGrids properties from raster #######################

# Read soil properties data: organic carbon and nitrogen raster
org_carbon_st <- raster("/your_path/org_carb_st3035.tif")
nitrog <- raster("/your_path/nitrogen_3035.tif") 

# Extract raster values for each used/random step
cat("Extracting the SoilGrids properties values by step\n")
ssf_dat_sf$org_carbon_st <- raster::extract(org_carbon_st, ssf_dat_sf)
ssf_dat_sf$nitrog <- raster::extract(nitrog, ssf_dat_sf)



############# 8.11 Extraction of Crop Types from raster EU Crop Map 2022 y 2018 #######################

# Read EU crop type map of 2022
crop_type <- raster("/your_path/EU_CropMap_22.tif")
cat("Extracting the crop type 2022 by step\n")
ssf_dat_sf$crop_type <- raster::extract(crop_type, ssf_dat_sf)

# Read EU crop type map of 2018 to complement the 2022 data
crop_type18  <- raster("/your_path/EU_CropMap_18.tif")
# Create a secure copy of the original field
ssf_dat_sf$crop_type_filled <- ssf_dat_sf$crop_type
# Identify points with NA in crop_type
steps_na <- ssf_dat_sf %>% filter(is.na(crop_type))
# Convert to Spatial object (class required by raster::extract)
steps_na_sp <- as(steps_na, "Spatial")
# Extracting values from the EUCropMap 2018 raster
cat("Extracting the crop type 2018 by step\n")
values_2018 <- raster::extract(crop_type18, steps_na_sp)
# Assign these values to the fill column (in the correct order)
ssf_dat_sf$crop_type_filled[is.na(ssf_dat_sf$crop_type)] <- values_2018
# Clear temporary column
ssf_dat_sf$crop_type_filled <- NULL



############# 8.12 Extraction of Data of normalized difference vegetation index (NDVI) #######################

# The NDVI data was generated in a specific way for each coordinate, if the positions of the steps are modified 
# (e.g. in case of using a different rate resampling), you will have to generate again the NDVI
# You can use the same dataframe df_gee ("/your_path/climate_input_GEE.csv") as input in the script "Generate_NDVI.js"

# Call the csv with the ndvi data as dataframe that we extracted from GEE using the script “Generate_NDVI.js”.
ndvi <- read.csv("/your_path/NDVI_Extraction_MOD13Q1_180.csv")

# Perform the left_join, ensuring the correspondence between the input information and the steps using the id_row created earlier.
ssf_dat_sf <- ssf_dat_sf %>%
  left_join(ndvi %>% dplyr::select(id_row, NDVI),  by = c("id_row" = "id_row"))

# We transform the values of thousands to scale from 1 to 0 by dividing by 10000.
ssf_dat_sf <- ssf_dat_sf %>%
  mutate(NDVI = NDVI / 10000)



###############################################################################
# 9. Remove random steps >10km away from their used step. 
#    Eliminate steps with NAs in any of the environmental variables
###############################################################################

# Remove any points more than 10 kilometers away from the point of use.
# Calculate distances by separating into two subsets: one for the steps used and one for the random steps.
# Separate used and random steps
used_s <- ssf_dat_sf %>%
  filter(case_ == TRUE)
rand_s <- ssf_dat_sf %>%
  filter(case_ == FALSE)
# Extract geometry as an auxiliary column for used
used_geom <- st_geometry(used_s)
# Create auxiliary table with step_id_ and geometry only
used_aux <- used_s %>%
  st_drop_geometry() %>%
  dplyr::select(step_id_) %>%
  mutate(used_geom = used_geom)
# Match random with steps used by step_id_.
rand_used <- rand_s %>%
  left_join(used_aux, by = "step_id_") %>%
  mutate(dist = st_distance(geometry, used_geom, by_element = TRUE)) %>%
  filter(dist <= set_units(10000, "m")) %>%
  dplyr::select(-used_geom, -dist)
# Join filtered random with original used steps
ssf_dat_sf10k <- bind_rows(used_s, rand_used)


#Remove steps with NAs and generate a cleaned steps dataset.
ssf_dat_sfc10k <- na.omit(ssf_dat_sf10k)

# Save this intermediate outputs.
saveRDS(ssf_dat_sf10k, file = "/your_path/ssf_dat_sf10k.rds")
saveRDS(ssf_dat_sfc10k, file = "/your_path/ssf_dat_sfc10k.rds")



###############################################################################
# 10. Label categorical variables, convert them to factors, and scale the 
# continuous variables.
###############################################################################

# Visualize the categories of moon_phase
#unique(ssf_dat_sfc10k$moon_phase)

# Visualize the categories of cropping intensity
# unique(ssf_dat_sfc10k$crop_intens)

# Visualize the categories of cropping type
# unique(ssf_dat_sfc10k$crop_type)

# Convert in factor the field "moon_phase". This field is already categorized appropriately: "New", "Waxing", "Full" and "Waning. 
ssf_dat_sfc10k$moon_phase <- as.factor(ssf_dat_sfc10k$moon_phase)
# levels(ssf_dat_sfc10k$moon_phase)

# Convert in factor the field "crop_intens"
ssf_dat_sfc10k$crop_intens <- as.factor(ssf_dat_sfc10k$crop_intens)
# levels(ssf_dat_sfc10k$crop_intens)

# Convert in factor the field "crop_type"
ssf_dat_sfc10k$crop_type <- as.factor(ssf_dat_sfc10k$crop_type)
# levels(ssf_dat_sfc10k$crop_type)

# Convert in factor the field "date" to include it as a random effect in the fitting of the models
ssf_dat_sfc10k$date <- as.factor(ssf_dat_sfc10k$date)
# levels(ssf_dat_sfc10k$date)


#Label the categories of the cropping intensity map, following the classification used by the authors
ssf_dat_sfc10k$crop_intens <- factor(ssf_dat_sfc10k$crop_intens,
                                     levels = c("-1", "1", "2", "3"),
                                     labels = c("Background", "Single_season", "Double-season", " multi_season"))

#Label the categories of the crop type map, following the classification used by the authors
ssf_dat_sfc10k$crop_type <- factor(ssf_dat_sfc10k$crop_type,
                                   levels = c("100", "211", "212", "213", "214", "215", "216", "218", "219", 
                                              "221", "222", "223", "230", "231", "232", "233", "240", "250", 
                                              "290", "300", "500", "600", "700", "800"),
                                   labels = c("Artificial_land", "Common_wheat", "Durum_wheat", "Barley", "Rye", 
                                              "Oats", "Maize", "Triticale", "Other_cereals", "Potatoes", "Sugar_beet",
                                              "Other_roots_crops", "Other_nonperm_crops", "Sunflower", "Rape", "Soya", 
                                              "Puls_veg_flow", "Other_fod_crops", "Bare_arable", "Wood_shrubland", 
                                              "Grassland", "Bare_land", "water", "Wetlands"))


# Scale numerical variables

ssf_dat_sfc10k$etf_abundance_r <- scale(ssf_dat_sfc10k$etf_abundance_r) 
ssf_dat_sfc10k$etf_abundance <- scale(ssf_dat_sfc10k$etf_abundance)
ssf_dat_sfc10k$moon_dist <- scale(ssf_dat_sfc10k$moon_dist)
ssf_dat_sfc10k$swi_t1 <- scale(ssf_dat_sfc10k$swi_t1)
ssf_dat_sfc10k$t_precipitation <- scale(ssf_dat_sfc10k$t_precipitation)
ssf_dat_sfc10k$pop_dens <- scale(ssf_dat_sfc10k$pop_dens)
ssf_dat_sfc10k$org_carbon <- scale(ssf_dat_sfc10k$org_carbon)
ssf_dat_sfc10k$temp_2m <- scale(ssf_dat_sfc10k$temp_2m)
ssf_dat_sfc10k$wind_speed_10m <- scale(ssf_dat_sfc10k$wind_speed_10m)
ssf_dat_sfc10k$nitrog <- scale(ssf_dat_sfc10k$nitrog)
ssf_dat_sfc10k$NDVI <- scale(ssf_dat_sfc10k$NDVI)
ssf_dat_sfc10k$thp <- scale(ssf_dat_sfc10k$thp)
ssf_dat_sfc10k$p_propiconazole <- scale(ssf_dat_sfc10k$p_propiconazole)
ssf_dat_sfc10k$p_glyphosate <- scale(ssf_dat_sfc10k$p_glyphosate)

# Save the final output: the prepared dataset for performing resource use analysis.
saveRDS(ssf_dat_sfc10k, file = "/your_path/ssf_dat_sfc10k.rds")

#################################################################################
# END







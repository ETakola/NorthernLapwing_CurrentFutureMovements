// Research: Tracking the Hidden Niches: Movement-Based Insights into Northern Lapwing Individual variation and Conservation.

// ========================================================================================================
// Generate climate data: Temperature, precipitation, u and v components of wind.
// From the dataset ERA5-Land Hourly - ECMWF Climate Reanalysis
// ========================================================================================================

// Before running this code, make sure to upload the climate_input_GEE.csv file to your GEE Assets. 
// This file was created in Data_preparation.R (see 8.7) and includes the step coordinates and timestamps.


// 1. Load data
// These are the points where we want to extract NDVI values.
// Each point should have a geometry and a timestamp (date).
var points = ee.FeatureCollection("/your_path/assets/climate_input_GEE");


// 2. Extract climate variables for each point
// Use the ERA5-Land Hourly dataset from ECMWF.
var dataset = ee.ImageCollection('ECMWF/ERA5_LAND/HOURLY');

// For each point in the table:
// - Get its location and timestamp
// - Find the closest hourly image (±1 hour)
var withClimate = points.map(function(feature) {
  var point = feature.geometry();
  var timestamp = ee.Date(feature.get('timestamp'));
  
  var image = dataset
    .filterDate(timestamp.advance(-60, 'minute'), timestamp.advance(60, 'minute'))
    .sort('system:time_start')
    .first();
  // Select the variables of interest  
  var vars = image.select([
    'temperature_2m',           // air temperature at 2 meters
    'total_precipitation',      // total precipitation
    'u_component_of_wind_10m',  // wind in east-west direction
    'v_component_of_wind_10m'   // wind in north-south direction
  ]);
  
  // Get the values at the point location
  var climate = vars.reduceRegion({
    reducer: ee.Reducer.first(),
    geometry: point,
    scale: 11000,
    maxPixels: 1e13
  });
  
  return feature.set(climate);
});

// 3. Export results values extracted at each step coordinate.
// Export the point data (now with temperature, precipitation, u and v components of wind. values) as a CSV file.

Export.table.toDrive({
  collection: withClimate,
  description: 'era5_hourly_export_180',
  fileFormat: 'CSV'
});
// END
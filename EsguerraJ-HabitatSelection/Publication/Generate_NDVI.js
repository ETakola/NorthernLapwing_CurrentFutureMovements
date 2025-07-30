// Research: Tracking the Hidden Niches: Movement-Based Insights into Northern Lapwing Individual variation and Conservation.

// ========================================================================================================
// Generate NDVI values for each step using the MOD13Q1.061 Terra Vegetation Indices 16-Day Global dataset.
// ========================================================================================================


// Before running this code, make sure to upload the climate_input_GEE.csv file to your GEE Assets. 
// This file was created in Data_preparation.R (see 8.7) and includes the step coordinates and timestamps.

// 1. Load data
// These are the points where we want to extract NDVI values.
// Each point should have a geometry and a timestamp (date).
var points = ee.FeatureCollection("/your_path/assets/climate_input_GEE");


// 2. Load MODIS NDVI data
// We use MODIS Terra Vegetation Indices (MOD13Q1) version 061.
// This collection provides NDVI images every 16 days at 250m resolution.
// We select only the NDVI band.
var modis = ee.ImageCollection("MODIS/061/MOD13Q1").select("NDVI");

// 3. Extract NDVI for each point
// For each point:
// - Get its date (timestamp)
// - Find the closest MODIS image within ±16 days
// - Extract the NDVI value at the point location
var withNDVI = points.map(function(feature) {
  var date = ee.Date(feature.get("timestamp")); 
  var image = modis
    .filterDate(date.advance(-16, 'day'), date.advance(16, 'day'))  // +/- 16 days
    .sort("system:time_start")
    .first();

  var ndvi = ee.Algorithms.If(
    image,
    image.reduceRegion({
      reducer: ee.Reducer.first(),
      geometry: feature.geometry(),
      scale: 250
    }).get("NDVI"),
    null
  );
  // - Add that NDVI value as a new property to the point
  return feature.set("NDVI", ndvi);
});

// 4. Export NDVI values extracted at each step coordinate.
// Export the point data (now with NDVI values) as a CSV file.
Export.table.toDrive({
  collection: withNDVI,
  description: "NDVI_Extraction_MOD13Q1_180",
  fileFormat: "CSV"
});

// END
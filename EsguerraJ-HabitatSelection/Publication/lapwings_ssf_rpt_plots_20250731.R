library(glmmTMB)
library(dplyr)
library(ggplot2)
library(ggeffects)
library(broom.mixed)
library(lme4)
library(performance)
library(sf)
library(rptR)
library(purrr)


ssf_dat_sfc10k <- readRDS("ssf_dat_sfc10k.rds")
ssf_dat_sfc10k_orig <- ssf_dat_sfc10k
#ssf_dat_sfc10k <- ssf_dat_sfc10k_orig
ssf_dat_sfc10k_no_geom <- sf::st_drop_geometry(ssf_dat_sfc10k)

# Remove rare crop types
ssf_dat_sfc10k <- ssf_dat_sfc10k_no_geom %>%
  dplyr::filter(crop_type %in% names(table(crop_type))[table(crop_type) >= 31])

#######################################
# Night-time
#######################################
library(lubridate)
# Create a subset for all times 19:00-05:00
night <- ssf_dat_sfc10k_no_geom %>%
  dplyr::filter(hour(t1_) >= 19 | hour(t1_) < 5)
# Keep only the used steps because available is not useful for temporal dynamics
night_true <- night %>%
  filter(case_ == TRUE)

# Fit the model 
ssf_moon <- glmmTMB(log(sl_ + 1) ~ moon_phase + moon_dist + cos_ta_ + (1 | id),
                    data = night %>% filter(case_ == TRUE),
                    family = gaussian())
summary(ssf_moon)

# Plot moon phases
ggplot(moon_pred, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(x = "Moon Phase", y = "Step length (log + 1)",
       title = "Lapwing Step Length Across Moon Phases") +
  theme_minimal()
#######################################
# Food resources
#######################################
ssf_food <- glmmTMB(
  case_ ~  cos_ta_ + log_sl_ + etf_abundance_r*swi_t1 + (1 | id/step_id_),
  data = ssf_dat_sfc10k,
  family = binomial(link = "logit"),
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(ssf_food)
# Results show low individual variation and earthworms and swi do not affect step selection

# Extract model estimates
tidy_food <- broom.mixed::tidy(ssf_food, effects = "fixed") %>%
  filter(!term %in% c("(Intercept)", "cos_ta_", "log_sl_")) %>%
  mutate(term = recode(term, "etf_abundance_r" = "Earthworm Abundance",
                  "swi_t1" = "Soil Moisture", "etf_abundance_r:swi_t1" = "Earthworm x Soil Moisture"),
    model = "Food Model")
#######################################
# Weather
#######################################
ssf_weather <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                         temp_2m + t_precipitation + wind_speed_10m + 
                         (1 | id/step_id_),
                       data = ssf_dat_sfc10k, family = binomial(link = "logit"))
summary(ssf_weather)

tidy_weather <- broom.mixed::tidy(ssf_weather, effects = "fixed") %>%
  filter(!term %in% c("(Intercept)", "cos_ta_", "log_sl_")) %>%
  mutate(
    term = recode(term,
                  "temp_2m" = "Temperature",
                  "t_precipitation" = "Precipitation",
                  "wind_speed_10m" = "Wind Speed"),
    model = "Weather Model"
  )
#######################################
# Soil
#######################################
ssf_soil <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                      swi_t1 + nitrog + org_carbon + (1 | id/step_id_),
                    data = ssf_dat_sfc10k , family = binomial(link = "logit"))
summary(ssf_soil)

tidy_soil <- broom.mixed::tidy(ssf_soil, effects = "fixed") %>%
  filter(!term %in% c("(Intercept)", "cos_ta_", "log_sl_")) %>%
  mutate(
    term = recode(term,
                  "swi_t1" = "Soil Moisture",
                  "nitrog" = "Nitrogen",
                  "org_carbon" = "Organic Carbon"),
    model = "Soil Model"
  )
#######################################
# Human activity
#######################################
ssf_humans <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        pop_dens + thp + crop_intens + p_glyphosate + p_propiconazole + 
                        (1 | id/step_id_),
                      data = ssf_dat_sfc10k, family = binomial(link = "logit"), 
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(ssf_humans)

tidy_humans <- broom.mixed::tidy(ssf_humans, effects = "fixed") %>%
  filter(!term %in% c("(Intercept)", "cos_ta_", "log_sl_")) %>%
  mutate(
    term = recode(term,
                  "pop_dens" = "Population Density",
                  "thp" = "Human Footprint",
                  "crop_intensSingle_season" = "Crop Intensity: Single season",
                  "crop_intensDouble-season" = "Crop Intensity: Double season",
                  "crop_intens multi_season" = "Crop Intensity: Multi season",
                  "p_glyphosate" = "Glyphosate",
                  "p_propiconazole" = "Propiconazole"),
    model = "Human Activity Model"
  )
#######################################
# Vegetation
#######################################
ssf_veg <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        NDVI + crop_type + p_glyphosate + p_propiconazole + 
                     (1 | id/step_id_),
                      data = ssf_dat_sfc10k, family = binomial(link = "logit"),
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
summary(ssf_veg)

tidy_veg <- broom.mixed::tidy(ssf_veg, effects = "fixed") %>%
  filter(!term %in% c("(Intercept)", "cos_ta_", "log_sl_")) %>%
  mutate(
    term = recode(term,
                  "NDVI" = "NDVI",
                  "crop_typeCommon_wheat" = "Common wheat",
                  "crop_typeBarley" = "Barley",
                  "crop_typeMaize" = "Maize",
                  "crop_typePotatoes" = "Potatoes",
                  "crop_typeSugar_beet" = "Sugar beet",
                  "crop_typeSunflower" = "Sunflower",
                  "crop_typePuls_veg_flow" = "Pulses/veg/flowers",
                  "crop_typeOther_fod_crops" = "Other fodder crops",
                  "crop_typeWood_shrubland" = "Wood/shrubland",
                  "crop_typeGrassland" = "Grassland",
                  "crop_typewater" = "Water",
                  "crop_typeWetlands" = "Wetlands",
                  "p_glyphosate" = "Glyphosate",
                  "p_propiconazole" = "Propiconazole"),
    model = "Vegetation Model"
  )


#######################################
# Plot model estimates for all models
#######################################
tidy_models <- list(tidy_food, tidy_weather, tidy_soil, tidy_humans, tidy_veg)

# Loop to create plots
plots <- lapply(tidy_models, function(df) {
  ggplot(df, aes(x = term, y = estimate)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(title = NULL,
         x = "Predictor", y = "Model Estimate") +
    theme_minimal(base_size = 14)
})

# Number of plots
n_plots <- length(plots)

# Generate labels A, B, C, ... matching number of plots
labels <- LETTERS[1:n_plots]

combined_plot <- plot_grid(plotlist = plots, labels = labels, nrow = 1)

combined_plot
#######################################
# Plot individual intercepts for all models
#######################################
model_list <- list(food = ssf_food,
  weather = ssf_weather,
  soil = ssf_soil,
  humans = ssf_humans,
  veg = ssf_veg,
  moon = ssf_moon)
# Define helper function to extract random intercepts for id
extract_ranef_id <- function(model, model_name) {
  re <- ranef(model)$cond$id
  
  if ("(Intercept)" %in% colnames(re)) {
    re_df <- data.frame(id = rownames(re), random_intercept = re[, "(Intercept)"])
  } else {
    stop(paste("No (Intercept) column found in model:", model_name))
  }
  
  re_df$model <- model_name
  return(re_df)
}

ranef_df <- map2_dfr(model_list, names(model_list), extract_ranef_id)
ranef_df <- ranef_df %>%
  group_by(model) %>%
  mutate(id_ordered = factor(id, levels = id[order(random_intercept)])) %>%
  ungroup()
ggplot(ranef_df, aes(x = id_ordered, y = random_intercept, fill = model)) +
  geom_col() +
  facet_wrap(~ model, scales = "free_y") +
  labs(
    x = "Individual ID",
    y = "Random Intercept (Deviation from Grand Mean)",
    title = "Random Intercepts per Individual across Models"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

#######################################################
# Model metrics/diagnostics
#######################################################
results <- lapply(model_list, function(model) {
  # AIC
  aic_val <- AIC(model)
  
  # R²: fixed effects (marginal) and fixed + random (conditional)
  r2 <- performance::r2(model)
  r2_marginal <- r2$R2_marginal
  r2_conditional <- r2$R2_conditional
  
  # Variance explained by random effects = conditional - marginal
  r2_random <- r2_conditional - r2_marginal
  
  # Return as named list
  list(
    AIC = aic_val,
    R2_fixed = r2_marginal,
    R2_random = r2_random
  )
})

# View results as a data frame
model_summary <- do.call(rbind, lapply(names(results), function(name) {
  cbind(model = name, as.data.frame(results[[name]]))
}))
rownames(model_summary) <- NULL
model_summary



#######################################################
# Calculate repeatability for each crop type
#######################################################

# Fit the SSF model
ssf_model <- glmmTMB(case_ ~ (1 | id/step_id_) + cos_ta_ + log_sl_ + crop_type,
  data = ssf_dat_sfc10k, family = binomial(link = "logit"))
summary(ssf_model)
VarCorr(ssf_model)
# So important crops are: Common_wheat, Barley, Maize, Potatoes, Sugar_beet, Sunflower, Puls_veg_flow, Other_fod_crops, Grassland, water  


# Get all crop types as character vector
# Define important crop types
important_crops <- c("Common_wheat", "Barley", "Maize", "Potatoes", "Sugar_beet", 
                     "Sunflower", "Puls_veg_flow", "Other_fod_crops", 
                     "Grassland", "water")
#ssf_dat_sfc10k$case_ <- as.factor(ssf_dat_sfc10k$case_)

# Create subsets
for (crop in important_crops) {
  df_name <- gsub(" ", "_", crop)
  
  subset_df <- ssf_dat_sfc10k_no_geom %>% # Fit the dataset without geometry
    filter(crop_type == crop)
  
  assign(df_name, subset_df, envir = .GlobalEnv)
}


library(rptR)
# Wheat
Common_wheat$id <- as.factor(Common_wheat$id)
Common_wheat$case_ <- as.numeric(Common_wheat$case_)
unique(Common_wheat$case_)
rpt_wheat <- rpt(case_ ~ (1 | id), grname = "id",data = Common_wheat,
                 datatype = "Binary", nboot = 1000, npermut = 0)
unique(Common_wheat$case_)
summary(rpt_wheat)
plot(rpt_wheat)

# Barley
Barley$id <- as.factor(Barley$id)
Barley$case_ <- as.numeric(Barley$case_)
unique(Barley$case_)
rpt_barley <- rpt(case_ ~ (1 | id), grname = "id",data = Barley,
                    datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_barley)
plot(rpt_barley)

# Potatoes
Potatoes$id <- as.factor(Potatoes$id)
Potatoes$case_ <- as.numeric(Potatoes$case_)
unique(Potatoes$case_)
rpt_potatoes <- rpt(case_ ~ (1 | id), grname = "id",data = Potatoes,
                    datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_potatoes)
plot(rpt_potatoes)

# Maize
Maize$id <- as.factor(Maize$id)
Maize$case_ <- as.numeric(Maize$case_)
unique(Maize$case_)
rpt_maize <- rpt(case_ ~ (1 | id), grname = "id",data = Maize,
                    datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_maize)
plot(rpt_maize)

# Sugar_beet
Sugar_beet$id <- as.factor(Sugar_beet$id)
Sugar_beet$case_ <- as.numeric(Sugar_beet$case_)
unique(Sugar_beet$case_)
rpt_sugar_beet <- rpt(case_ ~ (1 | id), grname = "id",data = Sugar_beet,
                    datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_sugar_beet)
plot(rpt_sugar_beet)

# Sunflower
Sunflower$id <- as.factor(Sunflower$id)
Sunflower$case_ <- as.numeric(Sunflower$case_)
unique(Sunflower$case_)
rpt_sunflower <- rpt(case_ ~ (1 | id), grname = "id",data = Sunflower,
                      datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_sunflower)
plot(rpt_sunflower)

# Puls_veg_flow
Puls_veg_flow$id <- as.factor(Puls_veg_flow$id)
Puls_veg_flow$case_ <- as.numeric(Puls_veg_flow$case_)
unique(Puls_veg_flow$case_)
rpt_puls <- rpt(case_ ~ (1 | id), grname = "id",data = Puls_veg_flow,
                datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_puls)
plot(rpt_puls)

# Grassland
Grassland$id <- as.factor(Grassland$id)
Grassland$case_ <- as.numeric(Grassland$case_)
unique(Grassland$case_)
rpt_grass <- rpt(case_ ~ (1 | id), grname = "id",data = Grassland,
                datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_grass)
plot(rpt_grass)

# water
water$id <- as.factor(water$id)
water$case_ <- as.numeric(water$case_) 
unique(water$case_)
rpt_water <- rpt(case_ ~ (1 | id), grname = "id",data = water,
                datatype = "Binary", nboot = 1000, npermut = 0)
summary(rpt_water)
plot(rpt_water)


#######################################################
# Plot repeatability for each crop type
#######################################################
rpt_list <- list(
  Common_wheat = rpt_wheat,
  Barley = rpt_barley,
  Maize = rpt_maize,
  Potatoes = rpt_potatoes,
  Sugar_beet = rpt_sugar_beet,
  Puls_veg_flow = rpt_puls,
  Grassland = rpt_grass,
  Water = rpt_water
)



# Initialize empty list to store results
rpt_summaries <- list()

# Loop through each rpt model
for (crop_name in names(rpt_list)) {
  rpt_model <- rpt_list[[crop_name]]
  
  # Use tryCatch to handle errors if model failed
  rpt_summary <- tryCatch({
    rpt_sum <- summary(rpt_model)
    
    # Check if rpt_sum has expected elements and not empty
    if (is.null(rpt_sum$R) || nrow(rpt_sum$R) == 0) stop("No repeatability estimates")
    if (is.null(rpt_sum$CI_emp$CI_org) || nrow(rpt_sum$CI_emp$CI_org) == 0) stop("No confidence intervals")
    
    # Extract repeatability (original scale)
    R <- rpt_sum$R[1, 1]
    
    # Extract SE if available
    SE <- if (!is.null(rpt_sum$BootSE) && nrow(rpt_sum$BootSE) > 0) rpt_sum$BootSE[1, 1] else NA
    
    # Extract confidence intervals
    CI_low <- rpt_sum$CI_emp$CI_org[1, "2.5%"]
    CI_high <- rpt_sum$CI_emp$CI_org[1, "97.5%"]
    
    data.frame(Crop = crop_name, R = R, SE = SE, CI_low = CI_low, CI_high = CI_high)
    
  }, error = function(e) {
    # If error, return NA row and optionally print warning
    warning(paste("Failed to extract from model:", crop_name, "-", e$message))
    data.frame(Crop = crop_name, R = NA, SE = NA, CI_low = NA, CI_high = NA)
  })
  
  rpt_summaries[[crop_name]] <- rpt_summary
}

# Combine all into a single data frame
repeatability_summary <- do.call(rbind, rpt_summaries)

# View result
print(repeatability_summary)


# Crop should be a factor (order by R)
repeatability_summary$Crop <- factor(repeatability_summary$Crop,
                                     levels = repeatability_summary$Crop[order(repeatability_summary$R, decreasing = TRUE)])

# Plot
ggplot(repeatability_summary, aes(x = Crop, y = R)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "#0072B2") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Repeatability (ICC) per Crop Type",
       x = "Crop Type", y = "Repeatability (R)") +
  theme(panel.grid.minor = element_blank())





#######################################################
# Individual-level variation - data prep
#######################################################
library(ggridges)

ssf_true <- ssf_dat_sfc10k %>% filter(case_ == TRUE)

# Get unique IDs
id_levels <- unique(ssf_true$id)

# Create palette 
color_palette <- setNames(
  rainbow(length(id_levels)),  # or use `viridis::viridis(length(id_levels))`
  id_levels)


# Setup plot area with no axes
plot(1, type = "n", xlim = c(0, 2), ylim = c(0, length(color_palette)+1), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')

# Draw colored circles and text
y_positions <- seq(length(color_palette), 1)
for (i in seq_along(color_palette)) {
  symbols(1, y_positions[i], circles=0.04, inches=FALSE, add=TRUE, bg=color_palette[i])
  text(1.5, y_positions[i], names(color_palette)[i], adj=0, cex=1.2)
}

#######################################################
# Individual-level variation in earthworm abundance factor with SR
#######################################################
etfr_plot <- ggplot(ssf_true, aes(x = etf_abundance_r, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in earthworm abundance (scaled with SR)",
    x = "Earthworm Abundance Factor (scaled with SR)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in earthworm abundance factor without SR
#######################################################
etf_plot <- ggplot(ssf_true, aes(x = etf_abundance, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in earthworm abundance",
    x = "Earthworm Abundance Factor",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in human population density
#######################################################
popdens_plot <- ggplot(ssf_true, aes(x = pop_dens, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in human population density",
    x = "Human population density",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in cropping intensity
#######################################################
cropintens_plot <- ggplot(ssf_true, aes(x = crop_intens, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in cropping intensity",
    x = "Cropping intensity (categories)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 


# Or plot as a heatmap
# Calculate % for each individual
crop_intens_heat <- ssf_true %>%
  group_by(id, crop_intens) %>%
  summarise(n_steps = n(), .groups = "drop") %>%
  group_by(id) %>%
  mutate(percent = 100 * n_steps / sum(n_steps)) %>%
  ungroup()

ggplot(crop_intens_heat, aes(x = id, y = crop_intens, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "% of Steps", low = "white", high = "darkblue") +
  labs(
    x = "Individual ID",
    y = "Crop Intensity",
    title = "Step Distribution (% per Crop Intensity) by Individual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )



#######################################################
# Individual-level variation in glyphosate
#######################################################
glyph_plot <- ggplot(ssf_true, aes(x = p_glyphosate, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in glyphosate",
    x = "Glyphosate concentration",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in propiconazole
#######################################################
prop_plot <- ggplot(ssf_true, aes(x = p_propiconazole, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in propiconazole",
    x = "Propiconazole concentration",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in temperature
#######################################################
temp_plot <- ggplot(ssf_true, aes(x = temp_2m, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in temperature",
    x = "Temperature",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in precipitation
#######################################################
prec_plot <- ggplot(ssf_true, aes(x = t_precipitation, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in precipitation",
    x = "Precipitation",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in wind
#######################################################
wind_plot <- ggplot(ssf_true, aes(x = wind_speed_10m, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in wind",
    x = "Wind speed",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in soil water index
#######################################################
swi_plot <- ggplot(ssf_true, aes(x = swi_t1, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in soil water index",
    x = "Soil Water Index (T=1)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 

#######################################################
# Individual-level variation in Terrestrial Human Footprint
#######################################################
thp_plot <- ggplot(ssf_true, aes(x = thp, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in Terrestrial Human Footprint",
    x = "Terrestrial Human Footprint",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 
#######################################################
# Individual-level variation in organic carbon
#######################################################
orgcar_plot <- ggplot(ssf_true, aes(x = org_carbon, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in organic carbon",
    x = "Organic carbon",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 


#######################################################
# Individual-level variation in organic carbon stock
#######################################################
orgcarst_plot <- ggplot(ssf_true, aes(x = org_carbon_st, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in organic carbon stock",
    x = "Organic carbon stock",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 


#######################################################
# Individual-level variation in soil bulk density
#######################################################
bulkdens_plot <- ggplot(ssf_true, aes(x = bulk_dens, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in soil bulk density",
    x = "Soil bulk density",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 


#######################################################
# Individual-level variation in nitrogen
#######################################################
# Plot ridgelines stacked on top of each other (same y)
nitrog_plot <- ggplot(ssf_true, aes(x = nitrog, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Fix the fill mapping
  labs(
    title = "Individual-level variation in nitrogen use",
    x = "Nitrogen concentration",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )
#######################################################
# Individual-level variation in NDVI
#######################################################
ndvi_plot <- ggplot(ssf_true, aes(x = NDVI, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in NDVI",
    x = "Normalized Difference Vegetation Index (NDVI)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 

#######################################################
# Individual-level variation in crop types
#######################################################
cropt_plot <- ggplot(ssf_true, aes(x = crop_type, y = 0, group = id, fill = id)) +
geom_density_ridges(scale = 1, alpha = 0.15, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in crop types",
    x = "Crop types (categories)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.position = "none"
  )

# Or plot as a heatmap
# Calculate % for each individual
crop_heat <- ssf_true %>%
  group_by(id, crop_type) %>%
  summarise(n_steps = n(), .groups = "drop") %>%
  group_by(id) %>%
  mutate(percent = 100 * n_steps / sum(n_steps)) %>%
  ungroup()

ggplot(crop_heat, aes(x = id, y = crop_type, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "% of Steps", low = "white", high = "darkblue") +
  labs(
    x = "Individual ID",
    y = "Crop Type",
    title = "Step Distribution (% per Crop Type) by Individual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank())

# Plot heatmap without grassland, because it obstructs the within-individual trends
no_grass <- ssf_true[ssf_true$crop_type != "Grassland", ]

crop_heat2 <- no_grass %>%
  group_by(id, crop_type) %>%
  summarise(n_steps = n(), .groups = "drop") %>%
  group_by(id) %>%
  mutate(percent = 100 * n_steps / sum(n_steps)) %>%
  ungroup()

ggplot(crop_heat2, aes(x = id, y = crop_type, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "% of Steps", low = "white", high = "darkblue") +
  labs(
    x = "Individual ID",
    y = "Crop Type - No grassland",
    title = "Step Distribution (% per Crop Type) by Individual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank())

#######################################################
# Individual-level variation in moon distance
#######################################################
moondist_plot <- ggplot(ssf_true, aes(x = moon_dist, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in moon distance",
    x = "Moon distance",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 

#######################################################
# Individual-level variation in moon phase
#######################################################
moonphase_plot <- ggplot(ssf_true, aes(x = moon_phase, y = 0, group = id, fill = id)) +
  geom_density_ridges(scale = 1, alpha = 0.4, color = NA) +
  scale_fill_manual(values = color_palette) +  # Use same mapping here
  labs(
    title = "Individual-level variation in moon phase",
    x = "Moon phase",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) 

# Or plot as a heatmap
# Calculate % for each individual
heat_moon <- ssf_true %>%
  group_by(id, moon_phase) %>%
  summarise(n_steps = n(), .groups = "drop") %>%
  group_by(id) %>%
  mutate(percent = 100 * n_steps / sum(n_steps)) %>%
  ungroup()

ggplot(heat_moon, aes(x = id, y = moon_phase, fill = percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(name = "% of Steps", low = "white", high = "darkblue") +
  labs(
    x = "Individual ID",
    y = "Moon phase",
    title = "Step Distribution (% per Moon phase) by Individual"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#######################################################
# Save plots
#######################################################
ggsave("moondist_plot.png", plot = moondist_plot, width = 5, height = 5, dpi = 300)
ggsave("moonphase_plot.png", plot = moonphase_plot, width = 5, height = 5, dpi = 300)
ggsave("cropt_plot.png", plot = cropt_plot, width = 5, height = 5, dpi = 300)
ggsave("ndvi_plot.png", plot = ndvi_plot, width = 5, height = 5, dpi = 300)
ggsave("nitrog_plot.png", plot = nitrog_plot, width = 5, height = 5, dpi = 300)
ggsave("bulkdens_plot.png", plot = bulkdens_plot, width = 5, height = 5, dpi = 300)
ggsave("orgcar_plot.png", plot = orgcar_plot, width = 5, height = 5, dpi = 300)
ggsave("orgcarst_plot.png", plot = orgcarst_plot, width = 5, height = 5, dpi = 300)
ggsave("thp_plot.png", plot = thp_plot, width = 5, height = 5, dpi = 300)
ggsave("swi_plot.png", plot = swi_plot, width = 5, height = 5, dpi = 300)
ggsave("wind_plot.png", plot = wind_plot, width = 5, height = 5, dpi = 300)
ggsave("prec_plot.png", plot = prec_plot, width = 5, height = 5, dpi = 300)
ggsave("temp_plot.png", plot = temp_plot, width = 5, height = 5, dpi = 300)
ggsave("prop_plot.png", plot = prop_plot, width = 5, height = 5, dpi = 300)
ggsave("glyph_plot.png", plot = glyph_plot, width = 5, height = 5, dpi = 300)
ggsave("cropintens_plot.png", plot = cropintens_plot, width = 5, height = 5, dpi = 300)
ggsave("popdens_plot.png", plot = popdens_plot, width = 5, height = 5, dpi = 300)
ggsave("etf_plot.png", plot = etf_plot, width = 5, height = 5, dpi = 300)
ggsave("etfr_plot.png", plot = etfr_plot, width = 5, height = 5, dpi = 300)


#######################################################
# Seasonal variation
#######################################################
# summer: March 1 to May 31
summer <- ssf_dat_sfc10k_no_geom %>% filter(month(t1_) %in% 3:5)
# Summer: June 1 to August 31
summer <- ssf_dat_sfc10k_no_geom %>% filter(month(t1_) %in% 6:8)
# Autumn: September 1 to November 30
autumn <- ssf_dat_sfc10k_no_geom %>% filter(month(t1_) %in% 9:11)
# Winter: December 1 to February 28/29
winter <- ssf_dat_sfc10k_no_geom %>% filter(month(t1_) %in% c(12, 1, 2))

# Seasonal variation in night activity
ssf_moon <- glmmTMB(log(sl_ + 1) ~ moon_phase + moon_dist + cos_ta_ + (1 | id),
                    data = night %>% filter(case_ == TRUE),
                    family = gaussian())
summary(ssf_moon)

# Seasonal variation in food resources
ssf_foodspring <- glmmTMB(
  case_ ~  + cos_ta_ + log_sl_ + etf_abundance_r*swi_t1 + (1 | id/step_id_),
  data = spring, family = binomial(link = "logit"),
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_foodsummer <- glmmTMB(
  case_ ~  + cos_ta_ + log_sl_ + etf_abundance_r*swi_t1 + (1 | id/step_id_),
  data = summer, family = binomial(link = "logit"),
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_foodautumn <- glmmTMB(
  case_ ~  + cos_ta_ + log_sl_ + etf_abundance_r*swi_t1 + (1 | id/step_id_),
  data = autumn,family = binomial(link = "logit"),
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_foodwinter <- glmmTMB(
  case_ ~  + cos_ta_ + log_sl_ + etf_abundance_r*swi_t1 + (1 | id/step_id_),
  data = winter,family = binomial(link = "logit"),
  control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Seasonal variation in weather
ssf_weatherspring <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                         temp_2m + t_precipitation + wind_speed_10m + 
                         (1 | id/step_id_),
                       data = spring, family = binomial(link = "logit"))

ssf_weathersummer <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                         temp_2m + t_precipitation + wind_speed_10m + 
                         (1 | id/step_id_),
                       data = summer, family = binomial(link = "logit"))

ssf_weatherautumn <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                         temp_2m + t_precipitation + wind_speed_10m + 
                         (1 | id/step_id_),
                       data = autumn, family = binomial(link = "logit"))

ssf_weatherwinter <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                         temp_2m + t_precipitation + wind_speed_10m + 
                         (1 | id/step_id_),
                       data = winter, family = binomial(link = "logit"))

# Seasonal variation in soil resources
ssf_soilspring <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                      swi_t1 + nitrog + org_carbon + (1 | id/step_id_),
                    data = spring , family = binomial(link = "logit"))

ssf_soilsummer <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                      swi_t1 + nitrog + org_carbon + (1 | id/step_id_),
                    data = summer , family = binomial(link = "logit"))

ssf_soilautumn <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                      swi_t1 + nitrog + org_carbon + (1 | id/step_id_),
                    data = autumn , family = binomial(link = "logit"))

ssf_soilwinter <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                      swi_t1 + nitrog + org_carbon + (1 | id/step_id_),
                    data = winter , family = binomial(link = "logit"))

# Seasonal variation in human resources
ssf_humansspring <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        pop_dens + thp + crop_intens + p_glyphosate + p_propiconazole + 
                        (1 | id/step_id_),
                      data = spring, family = binomial(link = "logit"), 
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_humanssummer <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        pop_dens + thp + crop_intens + p_glyphosate + p_propiconazole + 
                        (1 | id/step_id_),
                      data = summer, family = binomial(link = "logit"), 
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_humansautumn <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        pop_dens + thp + crop_intens + p_glyphosate + p_propiconazole + 
                        (1 | id/step_id_),
                      data = autumn, family = binomial(link = "logit"), 
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_humanswinter <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                        pop_dens + thp + crop_intens + p_glyphosate + p_propiconazole + 
                        (1 | id/step_id_),
                      data = winter, family = binomial(link = "logit"), 
                      control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

#Seasonal variation in vegetation resources
ssf_vegspring <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                     NDVI + crop_type + p_glyphosate + p_propiconazole + 
                     (1 | id/step_id_),
                   data = spring, family = binomial(link = "logit"),
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_vegsummer <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                     NDVI + crop_type + p_glyphosate + p_propiconazole + 
                     (1 | id/step_id_),
                   data = summer, family = binomial(link = "logit"),
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_vegautumn <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                     NDVI + crop_type + p_glyphosate + p_propiconazole + 
                     (1 | id/step_id_),
                   data = autumn, family = binomial(link = "logit"),
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

ssf_vegwinter <- glmmTMB(case_ ~ cos_ta_ + log_sl_ + 
                     NDVI + crop_type + p_glyphosate + p_propiconazole + 
                     (1 | id/step_id_),
                   data = winter, family = binomial(link = "logit"),
                   control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

#######################################################
# Plot the seasonal variation
#######################################################
# Function to extract fixed effects with 95% CI and add season label
extract_coefs <- function(model, season_name) {
  tidy(model, effects = "fixed", conf.int = TRUE, conf.level = 0.95) %>%
    mutate(season = season_name) %>%
    select(term, estimate, std.error, conf.low, conf.high, season)}

# Define custom colors for seasons
season_colors <- c("Spring" = "#93C963", "Summer" = "#E58E00","Autumn" = "#87503B","Winter" = "#6C8CA2")

# Extract Food model for each season
spring_coefs <- extract_coefs(ssf_foodspring, "Spring")
summer_coefs <- extract_coefs(ssf_foodsummer, "Summer")
autumn_coefs <- extract_coefs(ssf_foodautumn, "Autumn")
winter_coefs <- extract_coefs(ssf_foodwinter, "Winter")

# Combine all for food
all_coefs <- bind_rows(spring_coefs, summer_coefs, autumn_coefs, winter_coefs)

# Filter out intercept, cos_ta_, and log_sl_ terms
exclude_terms <- c("(Intercept)", "cos_ta_", "log_sl_")
all_coefs_filtered <- all_coefs %>% filter(!term %in% exclude_terms)

# Order terms and seasons for clarity in plot
all_coefs_filtered$term <- factor(all_coefs_filtered$term, levels = unique(all_coefs_filtered$term))
all_coefs_filtered$season <- factor(all_coefs_filtered$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot vertical forest plot of fixed effects by season with custom colors
s1 <- ggplot(all_coefs_filtered, aes(x = estimate, y = term, color = season)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  labs(x = "Estimate (log-odds scale)",y = "Predictor", color = "Season") + theme_minimal() +
  coord_cartesian(xlim = c(-1, 1)) + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
s1

# Extract weather model for each season
spring_coefs <- extract_coefs(ssf_weatherspring, "Spring")
summer_coefs <- extract_coefs(ssf_weathersummer, "Summer")
autumn_coefs <- extract_coefs(ssf_weatherautumn, "Autumn")
winter_coefs <- extract_coefs(ssf_weatherwinter, "Winter")

# Combine all 
all_coefs <- bind_rows(spring_coefs, summer_coefs, autumn_coefs, winter_coefs)

# Filter out intercept, cos_ta_, and log_sl_ terms
exclude_terms <- c("(Intercept)", "cos_ta_", "log_sl_")
all_coefs_filtered <- all_coefs %>% filter(!term %in% exclude_terms)

# Order terms and seasons for clarity in plot
all_coefs_filtered$term <- factor(all_coefs_filtered$term, levels = unique(all_coefs_filtered$term))
all_coefs_filtered$season <- factor(all_coefs_filtered$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot vertical forest plot of fixed effects by season with custom colors
s2 <- ggplot(all_coefs_filtered, aes(x = estimate, y = term, color = season)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  labs(x = "Estimate (log-odds scale)",y = "Predictor", color = "Season") + theme_minimal() +
  coord_cartesian(xlim = c(-0.25, 0.25)) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
s2

# Extract soil model for each season
spring_coefs <- extract_coefs(ssf_soilspring, "Spring")
summer_coefs <- extract_coefs(ssf_soilsummer, "Summer")
autumn_coefs <- extract_coefs(ssf_soilautumn, "Autumn")
winter_coefs <- extract_coefs(ssf_soilwinter, "Winter")

# Combine all 
all_coefs <- bind_rows(spring_coefs, summer_coefs, autumn_coefs, winter_coefs)

# Filter out intercept, cos_ta_, and log_sl_ terms
exclude_terms <- c("(Intercept)", "cos_ta_", "log_sl_")
all_coefs_filtered <- all_coefs %>% filter(!term %in% exclude_terms)

# Order terms and seasons for clarity in plot
all_coefs_filtered$term <- factor(all_coefs_filtered$term, levels = unique(all_coefs_filtered$term))
all_coefs_filtered$season <- factor(all_coefs_filtered$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot vertical forest plot of fixed effects by season with custom colors
s3 <- ggplot(all_coefs_filtered, aes(x = estimate, y = term, color = season)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  labs(x = "Estimate (log-odds scale)",y = "Predictor", color = "Season") + theme_minimal() +
  coord_cartesian(xlim = c(-1, 1)) + 
  theme(legend.position = "none") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
s3
# Extract soil model for each season
spring_coefs <- extract_coefs(ssf_humansspring, "Spring")
summer_coefs <- extract_coefs(ssf_humanssummer, "Summer")
autumn_coefs <- extract_coefs(ssf_humansautumn, "Autumn")
winter_coefs <- extract_coefs(ssf_humanswinter, "Winter")

# Combine all 
all_coefs <- bind_rows(spring_coefs, summer_coefs, autumn_coefs, winter_coefs)

# Filter out intercept, cos_ta_, and log_sl_ terms
exclude_terms <- c("(Intercept)", "cos_ta_", "log_sl_")
all_coefs_filtered <- all_coefs %>% filter(!term %in% exclude_terms)

# Order terms and seasons for clarity in plot
all_coefs_filtered$term <- factor(all_coefs_filtered$term, levels = unique(all_coefs_filtered$term))
all_coefs_filtered$season <- factor(all_coefs_filtered$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot vertical forest plot of fixed effects by season with custom colors
s4 <- ggplot(all_coefs_filtered, aes(x = estimate, y = term, color = season)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  labs(x = "Estimate (log-odds scale)",y = "Predictor", color = "Season") + theme_minimal() +
  coord_cartesian(xlim = c(-20, 20)) + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
s4

# Extract soil model for each season
spring_coefs <- extract_coefs(ssf_vegspring, "Spring")
summer_coefs <- extract_coefs(ssf_vegsummer, "Summer")
autumn_coefs <- extract_coefs(ssf_vegautumn, "Autumn")
winter_coefs <- extract_coefs(ssf_vegwinter, "Winter")

# Combine all 
all_coefs <- bind_rows(spring_coefs, summer_coefs, autumn_coefs, winter_coefs)

# Filter out intercept, cos_ta_, and log_sl_ terms
exclude_terms <- c("(Intercept)", "cos_ta_", "log_sl_")
all_coefs_filtered <- all_coefs %>% filter(!term %in% exclude_terms)

# Order terms and seasons for clarity in plot
all_coefs_filtered$term <- factor(all_coefs_filtered$term, levels = unique(all_coefs_filtered$term))
all_coefs_filtered$season <- factor(all_coefs_filtered$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot vertical forest plot of fixed effects by season with custom colors
s5 <- ggplot(all_coefs_filtered, aes(x = estimate, y = term, color = season)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.7), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = season_colors) +
  labs(x = "Estimate (log-odds scale)",y = "Predictor", color = "Season") + theme_minimal() +
  coord_cartesian(xlim = c(-25, 25)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
s5

combined_plot <- plot_grid(s1, s2, s3, s4, labels = c("A) Food", "C) Weather", "B) Soil", "D) Humans"), nrow = 2)
combined_plot
combined_plot2 <- plot_grid(combined_plot, s5, nrow = 1)
combined_plot2



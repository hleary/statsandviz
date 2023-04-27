# ======================================================================
# Global Spatial Autocorrelation =======================================
# ======================================================================

# Computing Moran's I

# Create a neighbor list and check for empty neighbor sets
urbanforest_nb <- poly2nb(urbanforest_geom, queen = TRUE)
empty_nb <- which(card(urbanforest_nb) == 0)
cat("Polygons with empty neighbor sets:", paste(empty_nb, collapse = ", "))

# Remove polygons with empty neighbor sets from the spatial data
urbanforest_geom_subset <- urbanforest_geom[-empty_nb, ]

# Create a spatial weights matrix for the subset
nb_subset <- poly2nb(urbanforest_geom_subset, queen = TRUE)
wt <- nb2listw(nb_subset, style = "B")

# Compute the Moran's I statistic to test for spatial autocorrelation
moran.test(urbanforest_geom_subset$treEqty, wt)
moran.test(urbanforest_geom_subset$spcsRch, wt)

# Visualize the spatial autocorrelation using Moran scatterplots
moran.plot(urbanforest_geom_subset$treEqty, wt)
moran.plot(urbanforest_geom_subset$spcsRch, wt)
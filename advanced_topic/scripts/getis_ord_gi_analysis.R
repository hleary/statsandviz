# Project: Advanced Topic
# Script: Tutorial of Hotspot Analysis in R
# Author: Heatherlee Leary, University of Arizona, hleary@arizona.edu


# Load Libraries
require(sf)        # Supports spatial data types 
require(sfdep)     # Spatial analyses 
require(spdep)     # Spatial analyses 
require(dplyr)     # Data manipulation
require(tidyr)     # Data manipulation
require(ggplot2)   # Data visualization   


# Read in data
tes_data <- st_read("advanced_topic/data/tree_equity_data.shp")

# View first few rows
head(tes_data)

# Visualize tree equity as histogram
hist(tes_data$treEqty, main = "Distribution of Tree Equity Scores", xlab = "Tree Equity Score", ylab = "Frequency")

# Visualize tree equity across neighborhoods
ggplot(tes_data) +
  geom_sf(aes(fill = treEqty), color = "black", lwd = 0.15) +
  scale_fill_gradient(name = "Tree Equity Score",
                      low = "white",
                      high = "darkgreen") +
  ggtitle("Tree Equity Scores of Tucson Neighborhoods") +
  labs(caption = "Data source: City of Tucson") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")


# ===========================================================================
# Tree Equity Hotspot Analysis ==============================================
# ===========================================================================

# First need to identify neighbors and create weights for polygons
# Row-standardized weights ensures equal weight to each neighborhood polygon

tes_nbs <- tes_data |> 
  mutate(
    nb = st_contiguity(geometry), # ERROR: empty neighbor sets
    wt = st_weights(nb),
    tes_lag = st_lag(treEqty, nb, wt)
  ) 


# RESOLVE ERROR: 
# The poly2nb() function creates a neighbors list based on the spatial 
# relationships between the polygons. The queen = TRUE argument specifies 
# that the neighbors list should be based on a queen contiguity criterion, 
# which considers two polygons as neighbors if they share a boundary or a vertex.


# Create a neighbor list and check for empty neighbor sets
list_nb <- poly2nb(tes_data, queen = TRUE)
empty_nb <- which(card(list_nb) == 0)
empty_nb

# Which neighborhoods are the ones with empty neighbor sets? 
# Subset the urbanforest_geom object to extract polygons with empty neighbor sets
empty_polygons <- tes_data[empty_nb, ]
empty_polygons$nghbrhd # Antigua Village, Mortimore, Sycamore Park


# Remove polygons with empty neighbor sets from the spatial data
tes_subset <- tes_data[-empty_nb, ]

# Error should be resolved, so
# Try again to identify neighbors and create weights
tes_nbs <- tes_subset |> 
  mutate(
    nb = st_contiguity(geometry),        # neighbors share border/vertex
    wt = st_weights(nb),                 # row-standardized weights
    tes_lag = st_lag(treEqty, nb, wt)   # calculate spatial lag of TreEqty
  ) 


# Visualize spatial lag of tree equity
tes_nbs |> 
  ggplot(aes(fill = tes_lag)) +
  geom_sf(color = "black", lwd = 0.15)

# The lag shows some smoothing and tree equity being a smoother gradient



# Is there any global clustering? 

# The Getis-Ord global G statistic is a measure of spatial autocorrelation, 
# which assesses whether there is clustering. It tells us if nearby values 
# are more simlar than expected under the null. 
global_g_test(tes_nbs$treEqty, tes_nbs$nb, tes_nbs$wt)




# Now looking at local clustering:

# We can calciulate the Gi using local_g_perm().
# Gi is ratio of spatial lag to the sum of values for neighborhood.
# Helps us to find the local clusters.

# Create a new column called "Gi" which is the output of the local_g_perm
# It's a dataframe column, which we can't do any work in, so we need to unnest.
# So, its a column that contains a dataframe, and we're essentially unpacking it

tes_hot_spots <- tes_nbs |> 
  mutate(
    Gi = local_g_perm(treEqty, nb, wt, nsim = 499)
  ) |> 
  unnest(Gi) 

# Cursory visualization
# Plot looks at gi values for all locations
tes_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2() # makes the value 0 be the middle



# Lets classify these in 7 different categories
# very hot (cold), cold (hot), somewhat hot (cold), insignificant

# very = p < 0.01
# cold/hot = p <= 0.05
# somewhat = p <= 0.1

tes_hot_spots |> 
  select(gi, p_folded_sim) |> 
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # we now need to make it look better :) 
    # if we cast to a factor we can make diverging scales easier 
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Tree Equity Hot Spots in Tucson"
  )




# Problem Set: Diversity Analysis
# Question Number 4
# Status: Completed


# 4. Write a function that calculates both the observed richness 
# and the Shannon-Wiener diversity index for each community in a matrix, 
# and writes an output table with the results. 

# You can use the vegan built-in functions. 
# Test your function with the dune_bio.txt dataset.

diversity_summary <- function(community_matrix) {
  # Calculate observed richness
  richness <- apply(community_matrix, 1, function(row) sum(row > 0))
  
  # Calculate Shannon-Wiener diversity index
  shannon_wiener <- apply(community_matrix, 1, function(row) {
    # Use the vegan built-in function to calculate Shannon-Wiener diversity index
    diversity(row, index = "shannon")
  })
  
  # Create an output table with the results
  output_table <- data.frame(Observed_Richness = richness,
                             Shannon_Wiener = shannon_wiener)
  
  return(output_table)
}


# Test with dune.bio dataset
dune <- read.table("R09_diversity_analysis/data/dune_bio.txt", header = T)
head(dune)


diversity_summary(dune)






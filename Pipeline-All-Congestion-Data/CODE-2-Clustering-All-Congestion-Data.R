# Header ---------------------------------------------
# TS Clustering Steps
# 1) Reduction/Representation - transformation
# 2) Similarity/Distance Measures
# 3) Clustering Prototyping
# 4) Time-Series Clustering

# Libraries and paths ---------------------------------------------

library(data.table)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(TSdist) # For distance measurements 
#library(TSclust) # For time-series clustering - error loading due to file dependency issue
library(dtwclust)
library(cluster)
library(openxlsx)
library(clue) # cl_dissimilarity
library(rlist) # For subsetting the list by a condition

# No scientific notation
options(scipen = 999)

# The path 
path <- dummy_path

# Load the data ---------------------------------------------

# The reduced data, reduced data at a range of reduction coeffcients,and the raw data
#load(file.path(path, "Pipeline All Congestion Data", "1-Reduced-Time-Series-All-Congestion-Data.RData"))
load(file.path(path, "Pipeline All Congestion Data", "1-Reduced-Time-Series-All-Congestion-Data-Coeff-Range.RData"))

# Try hierarchical clustering ---------------------------------------------
# The clustering option from tsclust calls the hclust function
# There are three different linkage methods (here as hierarchical_control)
# "average", "single", and "complete"
# Complete - pairwise similarity between all observations in cluster 1 and 2
# And largest distance is used as the distance between the clusters (default in R)
# Average - pairwise similarity between all observations in cluster 1 and 2
# And average distance is used as the distance between the clusters (default in R)

# k-shape clustering algorithm 
# sbd - distance based on coefficient-normalized cross-correlation 
# Hierarchical clustering - zscore pre-processing
# Now this is an example - show 
hc_p <- tsclust(red_list[[1]], type = "h", k = 6, # chose the 
                    preproc = zscore, seed = 899,
                    distance = "sbd", centroid = shape_extraction,
                    control = hierarchical_control(method = "complete"))

# Plot the dendrogram
# Plot the series and the obtained prototypes
plot(hc_p)
plot(hc_p, type = "sc")

# Manually cut the hierarchical clustering results
#hc_cut_np <- data.table(cutree(hc_np , k = 8))
hc_cut_p <- data.table(cutree(hc_p , k = 6))

# Look at the counts for the groupings
#table(hc_cut_np)
table(hc_cut_p)

# Try simple partitional clustering with Euclidean distance and PAM centroids------------------------------------------------

# Create list to store the partitional resylts 
pc_list <- list()

# Loop to generate the partitional clusters at various reduction granularities (100 to 1000, by = 100)
# And k values (3:15)
for(k in 3:15)
 {
  for(element_i in 1:length(red_list))
   {
     pc_p <- tsclust(red_list[[element_i]], type = "partitional", k = k, preproc = zscore,
                     distance = "L2", centroid = "pam",
                     seed = 899)
     title <- paste0("red", element_i,"00", "_", "k", k)
     pc_list[[title]] <- pc_p
    }
  }

# Save the list containing the clusters before you break your code with your schenanegans
#save(pc_list, file = file.path(path, "Pipeline All Congestion Data","pc_list.RData"))
load(file.path(path, "Pipeline All Congestion Data","pc_list.RData"))

# Create plots from a subset of the clustering data
plot_list <- list()
for (i in seq(5,130, by = 5)){
  local({
    i <- i
    plot <- plot(pc_list[[i]])
    title <- names(pc_list[i])
    plot_list[[title]] <<- plot
  })
}
  
# Do not save the plots - cannot reload them back due to lack of memory
#load(file.path(path, "Pipeline All Congestion Data","plot_list.RData"))
#save(plot_list, file = file.path(path, "Pipeline All Congestion Data","plot_list.RData"))

# THINKING THAT A COEF OF 200 OR 300 IS SUFFICIENT - for preserving the shapes
plot_list[82]

# Initiate a list to collect the extractions of the cluster groupings by k  
group_list <- list()

# Write a loop to first just pull out the cluster groupings by k  
for (i in 1:length(pc_list)){
  #dt <- data.table(pnode_name = names(pc_list[[i]]@datalist), group_pc = pc_list[[i]]@cluster)
  grouping <- pc_list[[i]]@cluster
  title <- names(pc_list[i])
  group_list[[title]] <- grouping
}

# Initiate a list to capture the groupings and unique
clus_count_list <- list()

for (i in seq(1,121, by = 10)){
  print(i)
  end <- i + 9 # Get the upper bound for the subsetting range
  clus <- setDT(group_list[c(i:end)])
  clus$unique_entry <- apply(clus, 1, function(x) length(unique(x)))
  clus$k_val <- stringr::str_sub(colnames(clus)[1], start = -2)
  clus_count_list[[i]] <- clus
}

# Remove all the NULL values list items - I don't know where these came from
clus_count_list <- clus_count_list[lengths(clus_count_list) != 0]

# Initiate percentage list for 
percentage_list <- list()

for (i in 1:length(clus_count_list)){
  print(i)
  data <- data.table(clus_count_list[[i]]$unique_entry)
  data <- data[V1 != 1, (.N/1184)*100]
  percentage_list[[i]] <- data
}

# Remove all the NULL values list items - I don't know where these came from
# And get a table of percentages
percentage_list <- percentage_list[lengths(percentage_list) != 0]
percentage_table <- setDT(percentage_list)
colnames(percentage_table) <- c("k=3","k=4","k=5","k=6","k=7","k=8","k=9","k=10",
                                "k=11","k=12","k=13","k=14","k=15")
  


# Get the final groupings that will go into the Power BI dashboard------------------------------------------------
# The lats and longs will be merged in in the next script

# Pull out the k = 3, k = 4, k = 5, k = 6, k = 7, and k = 8
# List element 1, 2, 3, 4, 5 and 6
# Turn into data.table
# Add the pnode names back
# Keep only the reductions with coef 500
final_groupings <- clus_count_list[1:6]
final_groupings <- rbindlist(final_groupings)
final_groupings[, pnode_name := names(pc_list[[i]]@datalist)]
final_groupings <- final_groupings[, .(pnode_name, k_val, red500_k3)]

# Merge in the 
# Calculate the internal metrics for the clusterings------------------------------------------------

# Sil - Max
# D - Max
# COP - Min
# DB - Min
# DBStar - Min
# CH - Max
# SF - Max

# Intiate the internal metric loop
internal_list <- list()

# Internal metric loop
for (i in seq(1,121, by = 10)){
  end <- i + 9 # Get the upper bound for the subsetting range
  cvi_mat <- sapply(pc_list[i:end], cvi, type = "internal")
  table <- data.table(cvi_mat)
  title <- stringr::str_sub(names(pc_list[i]), start = -2)
  table[, cvi := c("Sil-Max","D-Max", "COP-Min", "DB-Min", "DBStar-Min", "CH-Max", "SF-Max")]
  internal_list[[title]] <- table
}

#save(internal_list, file = file.path(path, "Pipeline All Congestion Data","internal_metrics_list.RData"))

# Export the cvi metrics and the data needed for summary stats  ------------------------------------------------

# Write loop to export the internal metrics - each as a tab in an excel sheet
# Create workbook object with format specs for the table lines
wb <- createWorkbook("LBro")

for (i in names(internal_list)){
  sheet <- i
  addWorksheet(wb, i)
  table <- internal_list[[i]]
  table[, cvi := c("Sil-Max","D-Max", "COP-Min", "DB-Min", "DBStar-Min", "CH-Max", "SF-Max")]
  writeData(wb, sheet, table)
}

# Save workbook
saveWorkbook(wb, file = file.path(path, "Pipeline All Congestion Data", "2-Internal-Metrics-Results.xlsx"),
             overwrite = TRUE)

save(final_groupings, file = file.path(path, "Pipeline All Congestion Data", 'DATA-2-Final-Groupings-for-Summary-Stats.RData'))




# Header ---------------------------------------------
# If looking for the hierarchial clustering code
# It is in  the "All Pipeline Data" folder

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

# No scientific notation
options(scipen = 999)

# Load the forecast data ---------------------------------------------

# Load the congestion cost forecast prices
# Update 1 (file_1) - Forecasted GMM and MCC difference value
file_1 <- file.path(path, "0-Raw-Data","Congestion and Losses Adder Update_1_20191108.xlsx")
sheets_1 <- getSheetNames(file_1)
sheet_list_1 <- lapply(sheets_1, read.xlsx, xlsxFile = file_1, startRow = 2)
names(sheet_list_1) <- sheets_1

# For now just import abd bind into one tavble the difference values - as opposed to the percents
list_1 <- list()
for (i in names(sheet_list_1)){
  dt <-setDT(sheet_list_1[i][[1]])
  colnames(dt) <- tolower(colnames(dt))
  dt[, year := i]
  list_1[[i]] <- dt
}

# Get the table of the difference values
diff_table <- rbindlist(list_1)

# Transform the forecast data ---------------------------------------------

# Change the column names in the forecast data
setnames(diff_table, old = c("apnode","on.peak.-.q1", "on.peak.-.q2", "on.peak.-.q3", "on.peak.-.q4",
                             "off.peak.-.q1", "off.peak.-.q2", "off.peak.-.q3","off.peak.-.q4"),
         new = c("pnode_name", "q1_on", "q2_on", "q3_on", "q4_on",
                 "q1_off", "q2_off", "q3_off", "q4_off"))

# Keep only the relavant variables
diff_table <- diff_table[,c(2,4:12)]

# Save the friendlier forecast data for loading into the next script 
#save(diff_table, file = file.path(path,"Pipeline Forecast Data", "DATA-1-Reformatted-Forecast-Data.RData"))

# Reformat the data for clustering - each node with all the forecast data in one row
data <- merge(diff_table[year == "2022"], diff_table[year == "2025"], by = "pnode_name")
data <- merge(data, diff_table[year == "2028"], by = "pnode_name")
data <- merge(data, diff_table[year == "2030"], by = "pnode_name")

# Delete the year columns
data <- data[, year.x := NULL]
data <- data[, year.x := NULL]
data <- data[, year.y := NULL]
data <- data[, year.y := NULL]

#row.names(data) <- data[, 1]
#test <- as.list(data)
#test <- as.list(unlist(t(data)))

# Try simple partitional clustering with Euclidean distance and PAM centroids------------------------------------------------
# Run the partitional clustering just on the reduction 1000 
# Re-running again because I want to not z-score pre-process

# Create list to store the partitional results 
pc_list <- list()

# Loop to generate the partitional clusters at various reduction granularities (100 to 1000, by = 100)
# And k values (3:10)
for(k in 3:20) {
     pc_p <- tsclust(data[,c(2:33)], type = "partitional", k = k, # preproc = zscore, --- no pre-processing
                     distance = "L2", centroid = "pam",
                     seed = 899)
     title <- paste0("k", k)
     pc_list[[title]] <- pc_p
  }

# Initiate a list to collect the extractions of the cluster groupings by k  
group_list <- list()

# Write a loop to first just pull out the cluster groupings by k  
for (i in 1:length(pc_list)){
  #dt <- data.table(pnode_name = names(pc_list[[i]]@datalist), group_pc = pc_list[[i]]@cluster)
  grouping <- pc_list[[i]]@cluster
  title <- names(pc_list[i])
  group_list[[title]] <- grouping
}

# Get the final groupings that will go into the Power BI dashboard------------------------------------------------
# The lats and longs will be merged in in the next script

# Turn into data.table
# Add the pnode names back
final_groupings <- setDT(group_list)
final_groupings[, pnode_name := data$pnode_name]

# Export  ------------------------------------------------

#Save the groupings
save(final_groupings, file = file.path(path,
    "Pipeline Forecast Data", "DATA-1-Final-Groupings-for-Summary-Forecast-Data-K20.RData"))




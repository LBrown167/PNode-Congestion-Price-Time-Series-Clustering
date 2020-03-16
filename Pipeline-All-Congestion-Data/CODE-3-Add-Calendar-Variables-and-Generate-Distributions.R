# Header ---------------------------------------------
# TS Clustering Steps
# 1) Reduction/Representation - transformation
# 2) Similarity/Distance Measures
# 3) Clustering Prototyping
# 4) Time-Series Clustering

# Libraries and paths ---------------------------------------------

library(data.table)
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

# Load the data  ------------------------------------------------

# Load the raw data and the final clustering results
load(file.path(path, "Pipeline All Congestion Data", "0-Congestion-Price-for-Trial-Clustering-All-Congestion-Data.RData"))
load(file.path(path, "Pipeline All Congestion Data - Min Sign Switch",
               "DATA-2-Final-Groupings-for-Summary-Stats-1000-Red-No-Pre.RData"))

# Deal with this later
# Load the NERC holdaus in - additional off-peak hours
# hol <- data.table(read.xlsx(file.path(path, "NERC-Holidays.xlsx"), sheet = "Holidays"))
# hol[, date := ymd(date)]

# Join the grouping values to the raw data ------------------------------------------------

# Obtain separate tables for the groupings - yes this is gross (no time)
# So can re-title the column headers appropriately
#k3 <- final_groupings[k_val == "k3"][, k_val := NULL]
#k4 <- final_groupings[k_val == "k4"][, k_val := NULL]
#k5 <- final_groupings[k_val == "k5"][, k_val := NULL]
#k6 <- final_groupings[k_val == "k6"][, k_val := NULL]
#k7 <- final_groupings[k_val == "k7"][, k_val := NULL]
#k8 <- final_groupings[k_val == "k8"][, k_val := NULL]

# Re-title the tables
#colnames(k4) <- gsub("k3","k4", colnames(k4))
#colnames(k5) <- gsub("k3","k5", colnames(k5))
#colnames(k6) <- gsub("k3", "k6", colnames(k6))
#colnames(k7) <- gsub("k3", "k7", colnames(k7))
#colnames(k8) <- gsub("k3", "k8", colnames(k8))

# Merge in the groups for
#dataset <- merge(dataset, k3, by = "pnode_name")
#dataset <- merge(dataset, k4, by = "pnode_name")
#dataset <- merge(dataset, k5, by = "pnode_name")
#dataset <- merge(dataset, k6, by = "pnode_name")
#dataset <- merge(dataset, k7, by = "pnode_name")
#dataset <- merge(dataset, k8, by = "pnode_name")

dataset <- merge(dataset, final_groupings, by = "pnode_name")

# Add the date variables,add on/off peak flag, and prepare data for graphing  ------------------------------------------------

# Off peak Monday through Saturday  HE 1-6 and HE 23-24 and all of Sunday
# On peak DA IFM SP-15 prices are from Monday through Saturday  HE 7-22. All of Sunday is considered off peak.
dataset[, dow := weekdays(date)]
dataset[, month := month(date)]
dataset[, year := year(date)]
dataset[, q := quarter(date)]
dataset[, month_year := paste0(year, "-", month)]
dataset[, q_year := paste0(year, "-Q", q)]

# Label the on peak hours - 1 on peak and 0 off peak
dataset[, on_peak := as.integer(((dow != "Sunday") & ((he >= 1 & he <= 6)| (he >= 23 & he <= 24))))]

# Save the dataset with the groupings and calendar variables
save(dataset, file = file.path(path,"Pipeline All Congestion Data", "DATA-3-Data-with-Groups-and-Calendar-Variables.RData"))
# Load in the data with the time variables added
#load(file.path(path, "Pipeline All Congestion Data","DATA-3-Data-with-Groups-and-Calendar-Variables.RData"))


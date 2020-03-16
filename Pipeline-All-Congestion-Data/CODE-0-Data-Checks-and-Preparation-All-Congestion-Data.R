# Header ---------------------------------------------
# Loads in data
# Appends all the congestion orice data
# Check for missing values - not really important just investigative
# Save the appended data congestion data with lat/longs 


# Libraries and paths ---------------------------------------------

library(data.table)
library(openxlsx)
library(lubridate)

path <- dummy_path

# Load data ---------------------------------------------

# Web scraped lat and long data
# List of AP nodes present in Hye Min's file (that she wants clustered)
load(file.path(path, "00-Web-Scrape-Lat-Long.RData"))
node_list <- read.xlsx(file.path(path,  "Locational Congestion Adders.xlsx"), startRow = 5)

# Load in the congestion price data
# Get file names 
# Initiate empty file list 
files <- dir(file.path(path = path), pattern = "Congestion-Price-", full.names = TRUE)
file_list <- list()

# Initiate the loop and load to store data in a list
for (i in files) {
  load(i)
  setDT(dataset)
  file_list[[i]] <- dataset 
}

# Conert the list to a data.table
dataset <- rbindlist(file_list)

# Check how many nodes there are - 1,458
uniqueN(dataset$`Node ID`)

# Make all the tables data.tables
setDT(data)
setDT(node_list)

# Reformatting ---------------------------------------------

# Rename the geographic data as "node_geo"
node_geo <- data

# Standardize the node name column
colnames(node_list) <- tolower(colnames(node_list))
colnames(dataset) <- tolower(colnames(dataset))
setnames(dataset, old = "node id", new = "pnode_name")

# Change the dataset node name type into a chacter
dataset[, pnode_name :=  as.character(pnode_name)]

# Change date to date format
dataset[, date:= ymd(date)]

# Missing Value Checks ---------------------------------------------

# Make the the list from Hye Min of nodes is unique
# Flag the nodes that do not have geographical data
# Flag the nodes that do not have congestion pricing data in PDR
#node_list <- unique(node_list, by = "pnode_name")
#node_list[, no_geo := as.integer(!(pnode_name %in%node_geo$pnode_name))]
#node_list[, no_price := as.integer(!(pnode_name %in%dataset$pnode_name))]

# Get a list of the unique pnodes within the PDR list
# Check which ones do not have geographical data
# There ate 875 which do not have geographical data
con_list <- data.table(pnode_name = dataset[, unique(pnode_name)])
con_list[, no_geo := as.integer(!(pnode_name %in% node_geo$pnode_name))]

# Checks for missing price values  ---------------------------------------------

# Get the statt and the end date
start <- range(dataset$date)[1]
end <- range(dataset$date)[2]

# The total number of days that each node should contain if dataset is full
# The total number of intervals that each node should contain if the dataset is full
days <- as.numeric(end - start + 1)
hours <- as.numeric(days*24)

# Check to see if there are any NA values - no NA values
range(dataset$cong)

# Calculate the number of unique dates by pnode
# Calcualte the number of hours by pnode 
# There are some duplicates
day_count <- dataset[ , .(day_count = uniqueN(date)), by = "pnode_name"]
hour_count <- dataset[ , .(hour_count = .N), by = "pnode_name"]

# Remove the duplicate values in the dataset
dataset <- unique(dataset, by = c("date", "he", "interval", "cong", "pnode_name"))

# Calculate the number of unique dates by pnode
# Calcualte the number of hours by pnode 
# There are some duplicates
day_count <- dataset[ , .(day_count = uniqueN(date)), by = "pnode_name"]
hour_count <- dataset[ , .(hour_count = .N), by = "pnode_name"]

# It seems there is one interval missing for all the data so
# The max number of intervals is one less than the "hours"
# Pull the list of pnodes with complete data
complete_dataset <- hour_count[hour_count == hours - 1, .(pnode_name)] 

# Get the final data for analysis ---------------------------------------------

# Pull only the nodes that have a complete dataset
# Merge in the lat/long for the data that have geographical location
dataset <- dataset[pnode_name %in% complete_dataset$pnode_name]
dataset <- merge(dataset, node_geo, by = "pnode_name", all.x = TRUE)

# Flag the nodes removed  due to missing data ---------------------------------------------

#node_list[, missing_price := as.integer(!(pnode_name %in%dataset$pnode_name))]

# Export the data for trial clustering ---------------------------------------------

# Export the node dataset with the data quality flags
wb <- createWorkbook("LBro")
addWorksheet(wb, "Sheet 1")
writeData(wb, "Sheet 1", con_list)
saveWorkbook(wb, file = file.path(path, "0-Flags-for-Missing-Data-All-Congestion-Data.xlsx"), overwrite = TRUE)

save(dataset, file = file.path(path, "0-Congestion-Price-for-Trial-Clustering-All-Congestion-Data.RData"))


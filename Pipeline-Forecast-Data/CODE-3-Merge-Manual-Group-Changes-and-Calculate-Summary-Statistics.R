# Header ---------------------------------------------
# Prepare the summary statistics
# And export the Excel spreadsheet that will serve as input to Power BI

# Libraries and paths ---------------------------------------------

library(data.table)
library(openxlsx)
library(lubridate)

# No scientific notation
options(scipen = 999)

# The path 
path <- dummy_path

# Load the data ---------------------------------------------

# Load the data that was created from the last script
# Data grouped the the clustering algorithm itself 
# With lat/longs and 
load(file.path(path, "Pipeline Forecast Data","DATA-2-Final-Grouped-Forecast-Data-RData-Version.RData"))
unmod_dataset <- copy(dataset)
rm(dataset)

# Load the
dataset <- read.xlsx(file.path(path, "Pipeline Forecast Data",
                               "DATA-2-Final-Grouped-Forecast-Data-for-PowerBI-Forecast-Clustering-AW.xlsx"), sheet = "Groupings - k = 7")

# Keep only the values needed from unmodified dataset ------------------------------------------------
# Remove alll the group calculations, sign switch counts, etc
# Because they will need to be calculated with the new group assignments

# Keep the columns of interest
# And retain only k = 7 because that is what we are going with
unmod_dataset <- unmod_dataset[, .(pnode_name, year, q_peak, for_cong, k, lat, long)]
unmod_dataset <- unmod_dataset[k == "k7"]
unmod_dataset[, k := NULL]

#  Merge the datasets ------------------------------------------------

dataset <- merge(unmod_dataset, dataset, by = "pnode_name", all.x = TRUE)

# Peform calculations ------------------------------------------------

# Group summaries: mean nodal congestion price by quarter/year  for on and off peak for each k 
dataset[, mean_quarterly_group := mean(for_cong),
          by = .(year, q_peak, k, group)]

# Get the absolute difference between the node forecast and the group forecast
dataset[, abs_diff := abs(for_cong - mean_quarterly_group)]

# Get columns indicating the sign of the node and group congestion prices
# For months and quarters
# Returns a 1 for + and a -1 for negative
dataset[, sign_quarterly_node := sign(for_cong)]
dataset[, sign_quarterly_group := sign(mean_quarterly_group)]

# Get a column that indicates whether there is a sign switch
# Between the individual node forecast and the group node forecast
# Jason/valuation are interested to know if the sign switches - that is one of the most important things
# 1 = sign switch (bad)
# 0 = no sign switch (good)
dataset[, sign_switch := as.integer(sign_quarterly_node != sign_quarterly_group)]

# Get the total proportion of sign switches
# Starting with the relvant total count - # of observations in one year and group (by k)
# You checked this multiple times
#dataset[, n_year_k_group := .N, by = .(year, k, group)] # This isn't necessary
dataset[, prop_switch_year_k_group  := sum(sign_switch)/.N, by = .(year, k, group)]
dataset[, prop_switch_year_k:= sum(sign_switch)/.N, by = .(year, k)]

# Export the data ------------------------------------------------

# Save the data
save(dataset, file = file.path(path, "Pipeline All Congestion Data",
                                     "DATA-3-Final-Grouped-Forecast-Data-Manual-Mod-RData-Version.RData"))

# Create the workboook structure
wb <- createWorkbook("LBro")
addWorksheet(wb, "Results - Forecast Clustering")

# Write the data to the sheet objects and export
writeData(wb, "Results - Forecast Clustering", dataset)
saveWorkbook(wb, file = "dummy_path_and_file.xlsx",
             overwrite = TRUE)
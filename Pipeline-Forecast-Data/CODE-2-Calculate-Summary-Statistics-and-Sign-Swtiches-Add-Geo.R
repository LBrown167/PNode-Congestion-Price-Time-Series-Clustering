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

# Load the raw historical congestion price data - contains lat/longs
# Keep only unique values then rename to avoid name conflicts
load(file.path(path, "Pipeline All Congestion Data", "0-Congestion-Price-for-Trial-Clustering-All-Congestion-Data.RData"))
dataset <- unique(dataset[, .(pnode_name, lat, long)], by = "pnode_name")
geo <- dataset
rm(dataset)

# Amy Li + team provided this data
# It is from a third-party vendor and I was told it is a bit outdated
geo_spop <- read.xlsx(file.path(path, "Geographical Data","DB80_Apnodes.xlsx"))
setnames(geo_spop, old = "Apnode", new = "pnode_name")

# Load the reformatted forcast data and the groupings resulted from the clustering analysis   
load(file.path(path, "Pipeline Forecast Data", "DATA-1-Reformatted-Forecast-Data.RData"))
load(file.path(path, "Pipeline Forecast Data", "DATA-1-Final-Groupings-for-Summary-Forecast-Data-K20.RData"))

# Transform the data to long format ------------------------------------------------

# Merge the groupings into the forecast data
dataset <- merge(diff_table, final_groupings, by = "pnode_name")

# Going to need to get it into long format 
# id.vars = c("pnode_name", "year","k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10")
dataset <- melt(dataset, id.vars = c("pnode_name", "year", "k3", "k4", "k5", "k6", "k7", "k8", "k9",
                                     "k10", "k11", "k12","k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"),
                      variable.name = "q_peak", value.name = "for_cong",
                       measure.vars = c("q1_on", "q2_on", "q3_on", "q4_on",
                                        "q1_off", "q2_off", "q3_off", "q4_off"))

# measure.vars = c("k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
dataset <- melt(dataset, id.vars = c("pnode_name", "year","q_peak", "for_cong"),
                    variable.name = "k", value.name = "group",
                    measure.vars = c("k3", "k4", "k5", "k6", "k7", "k8", "k9",
                                     "k10", "k11", "k12","k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
              
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

# Add the geographical information ------------------------------------------------

# Merge the geo info in and retain all the forecast values
# SPOP geo data is way better - only 23 lat/longs missing in "geo_spop"
# Use the spop data except for one node - removed for public upload
dataset <- merge(dataset, geo, by = "pnode_name", all.x = TRUE)
dataset <- merge(dataset, geo_spop, by = "pnode_name", all.x = TRUE, allow.cartesian = TRUE)


# Get rid of the old lat/long columns
dataset[, lat := NULL]
dataset[, long := NULL]

# And rename the columns again
setnames(dataset, old = c("Latitude", "Longitude"), new = c("lat", "long"))

# Get pivoted dataset for Power BI ------------------------------------------------

#t_dataset <- dcast(dataset, pnode_name + year + k + group ~ q_peak, value.var = "for_cong")

# Export the data ------------------------------------------------

# Save the data
save(dataset, file = file.path(path, "Pipeline Forecast Data",
                                     "DATA-2-Final-Grouped-Forecast-Data-RData-Version-K20.RData"))

# Create the workboook structure
wb <- createWorkbook("LBro")
addWorksheet(wb, "Results - Forecast Clustering")

# Write the data to the sheet objects and export
writeData(wb, "Results - Forecast Clustering", dataset)
saveWorkbook(wb, file = "dummy_path_and_file.xlsx",
             overwrite = TRUE)
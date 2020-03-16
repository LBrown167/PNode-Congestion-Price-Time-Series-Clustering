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

load(file.path(path, "Pipeline All Congestion Data","DATA-3-Data-with-Groups-and-Calendar-Variables.RData"))

# Obtain only relevant information  ------------------------------------------------

# Keep relevant columns proceeding with k = 7 for the clustering
dataset <- dataset[, c("pnode_name", "date", "he", "interval","cong", "lat", "long",
                       "red500_k3", "red500_k4", "red500_k5", "red500_k6", "red500_k7", "red500_k8",
                       "dow", "month", "year", "q", "month_year", "q_year","on_peak")]

# Obtain data only through Q2 of 2019 - Q3 is incomplete
dataset <- dataset[date < "2019-07-01"]

# Nodal summaries: mean nodal congestion price by quarter/year and month/year for on and off peak
dataset[, mean_quarterly_node :=  mean(cong), by =.(pnode_name, q_year, on_peak)]
dataset[, mean_monthly_node := mean(cong), by = .(pnode_name, month_year, on_peak)]

# Group summaries: mean nodal congestion price by quarter/year and month/year for on and off peak for each k 
dataset[, mean_quarterly_group := mean(cong),
         by = .(q_year, on_peak, red500_k3, red500_k4, red500_k5, red500_k6, red500_k7, red500_k8)]
dataset[, mean_monthly_group := mean(cong),
         by = .(month_year, on_peak, red500_k3, red500_k4, red500_k5, red500_k6, red500_k7, red500_k8)]

# Get the unique values - only need the values by month
dataset <- unique(dataset, by = c("pnode_name", "month_year", "on_peak"))

# Export the data ------------------------------------------------

# Save the data
save(dataset, file = file.path(path,"Pipeline All Congestion Data","DATA-4-Data-with-Calculations-by-Group-and-Node.RData"))

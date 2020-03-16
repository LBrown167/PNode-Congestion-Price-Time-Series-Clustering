# Header ---------------------------------------------
# Adding weights to the years of the forecast values
# 2022 - 40%
# 2025 - 30%
# 2028 - 20%
# 2030 - 10%


# Libraries and paths ---------------------------------------------

library(data.table)
library(openxlsx)
library(lubridate)

# No scientific notation
# Digits
options(scipen = 999)
options(digits = 4)

# The path 
path <- dummy_path


# Load data ---------------------------------------------

w_tab <- fread(file.path(path, "Pipeline Forecast Data", "0-Weights-Table.csv"))
w_tab[, year := as.character(year)]

# Load the final grouping data - only want the final groups and the node names
# Keep only the pnode_name and the group
# Get unique values and filter out the nodes that are excluded (group = 0)
load(file.path(path, "Pipeline All Congestion Data","DATA-3-Final-Grouped-Forecast-Data-Manual-Mod-RData-Version.RData"))
dataset <- dataset[, .(pnode_name, group)]
dataset <- unique(dataset)
dataset <- dataset[group != 0]

# Load the congestion cost forecast prices
# Update 2 (file_2) - Forecasted GMM and MCC difference value
file_2 <- file.path(path, "0-Raw-Data","Congestion and Losses Adder Update_2_20191108.xlsx")
sheets_2 <- getSheetNames(file_2)
sheet_list_2 <- lapply(sheets_2, read.xlsx, xlsxFile = file_2, startRow = 2)
names(sheet_list_2) <- sheets_2

# For now just import abd bind into one table the difference values - as opposed to the percents
list_2 <- list()
for (i in names(sheet_list_2)){
  dt <-setDT(sheet_list_2[i][[1]])
  colnames(dt) <- tolower(colnames(dt))
  dt[, year := i]
  list_2[[i]] <- dt
}

# Get the table of the difference values
perc_table <- rbindlist(list_2)

# Merge in the weights 
perc_table <- merge(perc_table, w_tab, by = "year", all.x = TRUE)

# Transform the percentage data ---------------------------------------------

# Change the column names in the forecast data
setnames(perc_table, old = c("apnode","on.peak.-.q1", "on.peak.-.q2", "on.peak.-.q3", "on.peak.-.q4",
                             "off.peak.-.q1", "off.peak.-.q2", "off.peak.-.q3","off.peak.-.q4"),
         new = c("pnode_name", "q1_on", "q2_on", "q3_on", "q4_on",
                 "q1_off", "q2_off", "q3_off", "q4_off"))

# Keep only the relavant variables
perc_table$no. <- NULL

# Merge, melt, calculate, dcast---------------------------------------------

# Merge the groups in
data <- merge(perc_table, dataset, by = "pnode_name")

# Melt
data <- melt(data, id.vars = c("pnode_name", "year", "group", "weight"),
                   variable.name = "q", value.name = "forecast",
                   measure.vars = c("gmm", "q1_on", "q2_on", "q3_on", "q4_on", "q1_off", 
                                    "q2_off", "q3_off", "q4_off"))

# Get the adjusted forecast value that has weights 
data[, adj_forecast := forecast * weight]
data[, adj_nod_for := sum(adj_forecast), by = .(pnode_name, q)]
data <- data[, .(weighted_forecast = mean(adj_nod_for)), by = .(group, q)]


# Round to four digits
data[, weighted_forecast := round(weighted_forecast, 3)]

# Re-widen the dataset
data <- dcast(data, group ~ q, value.var = "weighted_forecast")

# Export the data ---------------------------------------------

# Create the workboook structure
wb <- createWorkbook("LBro")
addWorksheet(wb, "Forecast Values")
addWorksheet(wb, "Groupings")

# Write the data to the sheet objects and export
writeData(wb, "Forecast Values", data)
writeData(wb, "Groupings", dataset[order(group, pnode_name)])
saveWorkbook(wb, file = file.path(path, "Pipeline Forecast Data", "dummy_file.xlsx"),
             overwrite = TRUE)

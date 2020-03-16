# Header ---------------------------------------------

# Libraries and paths ---------------------------------------------

library(data.table)
library(lubridate)
library(TSrepr)
library(ggplot2)
library(gridExtra) # For grid arrange
library(egg)

path <- dummy_path

# Load and reformat the data ---------------------------------------------

# AP node price data
load(file.path(path, "Pipeline All Congestion Data", "0-Congestion-Price-for-Trial-Clustering-All-Congestion-Data.RData"))

# Keep only the columns needed
# Rename a few of the columns
dataset <- dataset[, .(pnode_name, date, he, cong, type, lat, long)]

# Test a representation/reduction method ---------------------------------------------
# Look at the effect of reducing the time series to a range 
# Of different coefficient values 

# Ok - we have 1184 time series
uniqueN(dataset$pnode_name)

# Pull out one of the time series for reduction testing
test_data <- dataset[pnode_name == "AGUCALG1_7_B1"]

# Get a plot of the raw congestion before
raw <- ggplot(data.frame(Time = 1:nrow(test_data), Value = test_data$cong), aes(Time, Value)) +
       geom_line() +
       theme_bw()

# Define coefficients to try and initiate the plot list
coef_range <- seq(100, 1000, by = 100)
plot_list <- list()

for (i in coef_range)
  local({
  i <- i
  data <- repr_dft(as.numeric(test_data$cong), coef = i)
  plot <- ggplot(data.frame(Time = 1:i, Value = data), aes(Time, Value)) +
    ggtitle(paste0("DFT Coef:"," ", i)) +
    geom_line() +
    theme_bw()
  graph_title <- paste0("p_", i)
  plot_list[[graph_title]] <<- plot
})

# Remove all the NULL values list items - I don't know where these came from
plot_list <- plot_list[lengths(plot_list) != 0]

# Display the plots in a grid
# Exported this manually just as an example
display <- grid.arrange(grobs = lapply(plot_list,
                                       set_panel_size,
                                       width = unit(2.5, "in"),
                                       height = unit(3, "in")),
                                       ncol = 5)

# Perform the same reduction method on all the time series  ---------------------------------------------
# Peform a Discrete Fourier Tranform reduction in dimensionality
# Do it for thwe range of reductions outlined above 

# Pivot the dataset for easier reduction
dataset_p <- dcast(dataset, date + he ~ pnode_name, value.var = "cong")

# Initiate the list to store the reduced time series 
red_list <- list()

# For the range of coefficients outlined in coef_range
# Reduce the time series and
# Store each data.table containing reduced values into a list
for (coef_i in coef_range){
    print(coef_i)
    red_table <- lapply(dataset_p[, 3:length(dataset_p)], repr_dft, coef = coef_i)
    #red_table <- setDT(red_table)
    red_list[[coef_i]] <- red_table 
  }

# Remove the null values (again not sure why this is happening)
red_list <- red_list[lengths(red_list) != 0]

# Export the data  ---------------------------------------------

save(red_list, file = file.path(path, "Pipeline All Congestion Data", "1-Reduced-Time-Series-All-Congestion-Data-Coeff-Range.RData"))


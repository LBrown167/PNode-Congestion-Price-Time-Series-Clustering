#####################
# Load packages 
#####################
 
library(httr)
library(jsonlite)
library(rlist)
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)

#####################
# Import CAISO Price Contour Map JSON data from url
#####################

Link = "http://wwwmobile.caiso.com/Web.Service.Chart/api/v3/ChartService/PriceContourMap1"

#Pull the information from the link and parse JSON data
Response = GET(Link)
Data = fromJSON(content(Response, as= "text"))

#####################
# Get the nodal geographic information
#####################

# Get the table that contains the lat and long of the nodes
# And reformat the lat/long information
data <- data.table(Data$l$m[[3]])
data[, c := as.character(c)]
data[, c := gsub("\\(", "", c)]
data[, c := gsub("c", "", c)]
data[, c := gsub("\\)", "", c)]
data[, paste0(c("lat", "long")) := tstrsplit(c, ",")]
data[, long := trimws(long, "both")]

# Get and rename the columns at the same time
data <- data[, .(type = t, pnode_name = n, region = a, lat, long)]

#####################
# Export the nodal geographic information
#####################

save(data, file = file.path(dummy_path, "file.RData"))
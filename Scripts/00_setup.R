require(naturecounts)
require(INLA)
require(tidyverse)
require(sf)
require(dplyr)
require(ggplot2)
require(rnaturalearth)
require(rnaturalearthdata)

# Create folders as necessary
if(!dir.exists("Data")) dir.create("Data")
if(!dir.exists("Output")) dir.create("Output")
if(!dir.exists("Output/Plots")) dir.create("Output/Plots")

# set output directory for analysis files; create if not already there
out.dir <- paste("./Output/", max.year, "/", sep = "")
dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
data.dir <- paste("./Data/", max.year, "/", sep = "")
dir.create(data.dir, showWarnings=FALSE, recursive=TRUE)

# set data directory for analysis files; create if not already there
plot.dir <- paste("./Output/Plots/", max.year, "/", sep = "")
dir.create(plot.dir, showWarnings=FALSE, recursive=TRUE)

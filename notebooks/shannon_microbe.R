##Jackleen Guo
##SSRP Graphs

##-----setup and attach data
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(fs)
library(here)
library(patchwork)
set.seed(1234)

source("notebooks/load_data.R")
metadata <- load_metadata()
microdata <- load_microdata()


##-----shannon diversity index

num_patients <- nrow(microdata$microbe_only_data)
shannon_div <- numeric(num_patients)

for (i in 1:num_patients) { #go through all subjects 
  species_counts = microdata$microbe_only_data[i, ] #do for 1 subject
  species_proportions = species_counts / sum(species_counts) #get propotion, how many species div sum counts (100?)
  shannon_div[i] = -sum(species_proportions * log(species_proportions), na.rm = TRUE) #formula for each subject
}

microdata$subject_microdata$shannon_div <- shannon_div
#why does this take so long
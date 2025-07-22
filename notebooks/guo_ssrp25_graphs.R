##Jackleen Guo
##SSRP Metadata Graphs

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

source("notebooks/microdata_max_microbe.R")

source("notebooks/shannon_microbe.R")

source("notebooks/table1.R")

source("notebooks/pie_chart_figures.R")

source("notebooks/linear_models.R")

source("notebooks/barcode_graph_figures.R")

source("notebooks/pcoa_graph_figures.R")

source("notebooks/permanova_values.R")


#source("notebooks/feeding_graph.R")


##-----delivery type
#table
table(metadata$delivery_6m)




##-----Gestational weeks
metadataGAKnown <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header = TRUE)
data_gaKnown <- dplyr::filter(metadataGAKnown, ga_known == 1)

gwWeeks_mean <- mean(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_sd <- sd(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_median <- median(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_min <- min (data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_max <- max (data_gaKnown$ga_weeks, na.rm = TRUE)

print(paste0("Gestational mean: ", gwWeeks_mean))
print(paste0("Gestational sd: ", gwWeeks_sd))
print(paste0("Gestational median:", gwWeeks_median))
print(paste0("Gestational min:", gwWeeks_min))
print(paste0("Gestational max:", gwWeeks_max))

gestw_metadata_POS <- metadata_POS$gest_weeks
summary(gestw_metadata_POS)
sd_gestw_metadata_POS = sd(gestw_metadata_POS, na.rm = TRUE)
sd_gestw_metadata_POS

gestw_metadata_NEG <- metadata_NEG$gest_weeks
summary(gestw_metadata_NEG)
sd_gestw_metadata_NEG = sd(gestw_metadata_NEG, na.rm = TRUE)
sd_gestw_metadata_NEG


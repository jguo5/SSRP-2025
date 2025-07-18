##Jackleen Guo
##SSRP Metadata Graphs
## 6-17-2025

##-----setup and attach data
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(gridExtra)
library(fs)
library(here)
library(patchwork)
set.seed(1234)

metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
attach(metadata)
head(metadata, 3)
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

## Build Table 1

table1 <- bind_rows(
  ### Number of samples
  data.frame(
    "block" = "Number of samples",
    "sublock" = "Number of samples",
    "stat" = "N",
    "value" = nrow(metadata$subject_metadata)
  ),
  ### Sex
  data.frame(
    "block" = "Sex",
    "sublock" = "M",
    "stat" = "N",
    "value" = sum(metadata$subject_metadata$child_sex == "M")
  ),
  data.frame(
    "block" = "Sex",
    "sublock" = "M",
    "stat" = "proportion",
    "value" = sum(metadata$subject_metadata$child_sex == "M")/nrow(metadata$subject_metadata)
  ),
  data.frame(
    "block" = "Sex",
    "sublock" = "F",
    "stat" = "N",
    "value" = sum(metadata$subject_metadata$child_sex == "F")
  ),
  data.frame(
    "block" = "Sex",
    "sublock" = "F",
    "stat" = "proportion",
    "value" = sum(metadata$subject_metadata$child_sex == "F")/nrow(metadata$subject_metadata)
  ),
  ### Delivery Mode
  data.frame(
    "block" = "Vaginal",
    "sublock" = "Vaginal",
    "stat" = "proportion",
    "value" = sum(metadata$subject_metadata$delivery_mode == "Vaginal")/nrow(metadata$subject_metadata)
  ),
  data.frame(
    "block" = "Vaginal",
    "sublock" = "Vaginal",
    "stat" = "N",
    "value" = sum(metadata$subject_metadata$delivery_mode == "Vaginal")/nrow(metadata$subject_metadata)
  ),
  data.frame(
    "block" = "Cesearan",
    "sublock" = "Cesearan",
    "stat" = "N",
    "value" = sum(metadata$subject_metadata$delivery_mode == "Cesearan")/nrow(metadata$subject_metadata)
  ),
  data.frame(
    "block" = "Cesearan",
    "sublock" = "Cesearan",
    "stat" = "proportion",
    "value" = sum(metadata$subject_metadata$delivery_mode == "Cesearan")/nrow(metadata$subject_metadata)
  ),
  ### Age
  {
    metadata$sample_metadata %>%
      group_by(timepoint) %>%
      summarise(
        N = n(),
        min = min(zymo_child_agemonths),
        max = max(zymo_child_agemonths),
        mean = mean(zymo_child_agemonths),
        SD = sd(zymo_child_agemonths)
      ) %>%
      pivot_longer(
        cols = all_of(c("N", "min", "max", "mean", "SD"))
      ) %>%
      rename(sublock = timepoint, stat = name) %>%
      mutate(block = "AgeMonths", .keep = "all", .before = 2)
  }
  
  ### Feed?
)
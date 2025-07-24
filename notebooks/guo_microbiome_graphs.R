##Jackleen Guo
##SSRP Microbiome Data Graphs
## 6-25-2025

##-----Setup and attach data
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(fs)
library(here)
library(vegan)
library(patchwork)
set.seed(123)

source("notebooks/load_data.R")
metadata <- load_metadata()







# #metadata maternal hiv import
# metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
# attach(metadata)
# head(metadata, 3)
# 
# processed_metadata <- metadata %>%
#   select(
#     "subject_id",
#     "zymo_code_3m",
#     "zymo_code_6m",
#     "zymo_code_12m",
#     "zymo_code_18m",
#     "zymo_code_24m",
#     
#     "subject_id",
#     "child_sex",
#     "delivery_6m",
#     "medhx_mom___1_selfreport",
#     "mat_edu_years",
#     "ga_weeks",
#     "solids_age_6m",
#     
#     "feeding_type_3m",
#     "feeding_type_6m",
#     "feeding_type_12m___1",
#     "feeding_type_12m___2",
#     "feeding_type_12m___3",
#     "feeding_type_18m___1",
#     "feeding_type_18m___2",
#     "feeding_type_18m___3"
#   )%>%
#   filter(
#     !(zymo_code_3m == "" &
#         zymo_code_6m == "" &
#         zymo_code_12m == "" &
#         zymo_code_18m == "" &
#         zymo_code_24m == "")
#   )%>%
#   rename(
#     delivery_mode = delivery_6m,
#     mom_hiv_status = medhx_mom___1_selfreport,
#     gest_weeks = ga_weeks,
#     feeding_3m = feeding_type_3m,
#     feeding_6m = feeding_type_6m,
#     solids_age = solids_age_6m,
#     
#   ) %>%
#   mutate(child_sex = factor(child_sex, ordered = FALSE, levels = c(0,1), labels = c("F", "M"))) %>%
#   mutate(delivery_mode = factor(delivery_mode, ordered = FALSE, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))) %>%
#   mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
#   arrange(mom_hiv_status) %>%
#   mutate(feeding_3m = factor(feeding_3m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed"))) %>%
#   mutate(feeding_6m = factor(feeding_6m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed"))) %>%
#   mutate(
#     feeding_12m = if_else(feeding_type_12m___3 == 1, "Solid",
#                           if_else(feeding_type_12m___1 == 1, "B reast",
#                                   if_else(feeding_type_12m___2 == 1, "Formula", "N/A"))),
#     #feeding_12m = ifelse(is.na(feeding_12m), "N/A", feeding_12m),
#     feeding_12m = factor(feeding_12m, ordered = FALSE, levels = c("Breast", "Formula", "Solid"))
#   ) %>%
#   mutate(
#     feeding_18m = if_else(feeding_type_18m___3 == 1, "Solid",
#                           if_else(feeding_type_18m___1 == 1, "Breast",
#                                   if_else(feeding_type_18m___2 == 1, "Formula", "N/A"))),
#     #feeding_18m = ifelse(is.na(feeding_18m), "N/A", feeding_18m),
#     feeding_18m = factor(feeding_18m, ordered = FALSE, levels = c("Breast", "Formula", "Solid"))
#   )%>%
#   mutate( . , master_idx = 1:nrow(.))












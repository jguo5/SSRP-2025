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

#-----Permanova
micro_permanova <- microdata$subject_microdata %>%
  select(
    -sample,
    -master_idx
  ) %>%
  mutate(
    subject_id = gsub("khula-", "", subject_id)
  )

micro_permanova <- micro_permanova %>%
  left_join(
    metadata$subject_metadata %>% select(
      subject_id, 
      mom_hiv_status,
      child_sex,
      delivery_mode
      # feeding_12m,
      # feeding_18m
    ),
    by = "subject_id"
  ) %>%
  mutate(
    mom_hiv_status = factor(mom_hiv_status)
  )




#%>%
#mutate( . , master_idx = 1:nrow(.))

dist_matrix <- vegdist(microdata$microbe_only_data, method = "bray")

adonis2(dist_matrix ~ ageMonths, data = micro_permanova, permutations = 1000, na.action = na.omit)

adonis2(dist_matrix ~ child_sex, data = micro_permanova, permutations = 1000, na.action = na.omit)

adonis2(dist_matrix ~ mom_hiv_status, data = micro_permanova, permutations = 1000, na.action = na.omit)

adonis2(dist_matrix ~ delivery_mode, data = micro_permanova, permutations = 1000, na.action = na.omit)


#--adonis2 with ageMonths3, ageMonths6, ageMonths12

meta3m <- micro_permanova %>%
  filter(ageMonths < 4.5) 
micro3m <- micro_permanova %>%
  filter(ageMonths < 4.5) %>%
  select(
    -subject_id, 
    -mom_hiv_status,
    -child_sex,
    -delivery_mode,
    -ageMonths
  )
dist_matrix3 <- vegdist(micro3m, method = "bray")
adonis2(dist_matrix3 ~ ageMonths, data = meta3m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix3 ~ child_sex, data = meta3m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix3 ~ delivery_mode, data = meta3m, permutations = 1000, na.action = na.omit)


meta6m <- micro_permanova %>%
  filter(ageMonths >= 4.5) %>%
  filter(ageMonths < 9.0)
micro6m <- micro_permanova %>%
  filter(ageMonths >= 4.5) %>%
  filter(ageMonths < 9.0) %>%
  select(
    -subject_id, 
    -mom_hiv_status,
    -child_sex,
    -delivery_mode,
    -ageMonths
  )
dist_matrix6 <- vegdist(micro6m, method = "bray")
adonis2(dist_matrix6 ~ ageMonths, data = meta6m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix6 ~ child_sex, data = meta6m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix6 ~ delivery_mode, data = meta6m, permutations = 1000, na.action = na.omit)


meta12m <- micro_permanova %>%
  filter(ageMonths >= 9.0) %>%
  filter(ageMonths < 15.0)
micro12m <- micro_permanova %>%
  filter(ageMonths >= 9.0) %>%
  filter(ageMonths < 15.0) %>%
  select(
    -subject_id, 
    -mom_hiv_status,
    -child_sex,
    -delivery_mode,
    -ageMonths
  )
dist_matrix12 <- vegdist(micro12m, method = "bray")
adonis2(dist_matrix12 ~ ageMonths, data = meta12m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix12 ~ child_sex, data = meta12m, permutations = 1000, na.action = na.omit)
adonis2(dist_matrix12 ~ delivery_mode, data = meta12m, permutations = 1000, na.action = na.omit)


meta18m <- micro_permanova %>%
  filter(ageMonths >= 15.0)
micro18m <- micro_permanova %>%
  filter(ageMonths >= 15.0) %>%
  select(
    -subject_id, 
    -mom_hiv_status,
    -child_sex,
    -delivery_mode,
    -ageMonths
  )
dist_matrix18 <- vegdist(micro18m, method = "bray")
adonis2(dist_matrix18 ~ ageMonths, data = meta18m, permutations = 1000, na.action = na.omit)

#---maternal HIV across different ages
# 3 months
adonis2(dist_matrix3 ~ ageMonths + mom_hiv_status, data = meta3m, permutations = 1000, na.action = na.omit, by = "margin")

# 6 months
adonis2(dist_matrix6 ~ ageMonths + mom_hiv_status, data = meta6m, permutations = 1000, na.action = na.omit, by = "margin")

# 12 months
adonis2(dist_matrix12 ~ ageMonths + mom_hiv_status, data = meta12m, permutations = 1000, na.action = na.omit, by = "margin")

# 18 months
adonis2(dist_matrix18 ~ ageMonths + mom_hiv_status, data = meta18m, permutations = 1000, na.action = na.omit, by = "margin")

#all
adonis2(dist_matrix ~ ageMonths + mom_hiv_status, data = micro_permanova, permutations = 1000, na.action = na.omit, by = "margin")



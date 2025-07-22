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
library(vegan)
set.seed(1234)

source("notebooks/load_data.R")
metadata <- load_metadata()
microdata <- load_microdata()
source("notebooks/microdata_max_microbe.R")
source("notebooks/graph_colors.R")

#-----PCoA Plot
#--Plot by HIV status

distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") #lots of 0's, should i remove?
pcoa_result <- cmdscale(distance_matrix, k = 2)

pcoa_df <- as.data.frame(pcoa_result)

pcoa_df <- pcoa_df %>%
  rename(PCoA1 = V1, PCoA2 = V2)


pcoa_df$subject_id <- microdata$subject_microdata$subject_id
meta_subset2 <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id")
pcoa_df <- pcoa_df %>%
  arrange(mom_hiv_status)%>%
  filter(!is.na(mom_hiv_status))

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = mom_hiv_status)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Microbiome PCoA Plot",
    x = "PCoA1",
    y = "PCoA2",
    color = "Mom HIV Status"
  ) +
  scale_color_manual(values = c("Negative" = "#73baf3", "Positive" = "#f37373")) +
  theme(plot.title = element_text(hjust = 0.5))



#--Plot by top 8 microbes

#microbe_bray <- microdata$microbe_only_data[rowSums(microdata$microbe_only_data) > 0, ]

#lots of 0's, should i remove?
distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") 
pcoa_result <- cmdscale(distance_matrix, k = 2)
pcoa_df <- as.data.frame(pcoa_result)
pcoa_df <- pcoa_df %>%
  rename(PCoA1 = V1, PCoA2 = V2)

maxdata <- max_microbes_data %>% select (subject_id, max_subject_microbe)

pcoa_df$subject_id <- microdata$subject_microdata$subject_id

meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id")
max_microbe_vec <- setNames(maxdata$max_subject_microbe, maxdata$subject_id)
pcoa_df$max_subject_microbe <- max_microbe_vec[pcoa_df$subject_id]
pcoa_df <- pcoa_df %>%
  arrange(mom_hiv_status)

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = microbe_colors) +      
  labs(
    title = "PCoA Colored by Microbe",
    x = "PCoA1",
    y = "PCoA2",
    color = "Microbes"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#plot by top 5 microbes
distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") 
pcoa_result <- cmdscale(distance_matrix, k = 2)
pcoa_df <- as.data.frame(pcoa_result)
pcoa_df <- pcoa_df %>%
  rename(PCoA1 = V1, PCoA2 = V2)

maxdata <- max_microbes_data %>% select (subject_id, max_subject_microbe_5)

pcoa_df$subject_id <- microdata$subject_microdata$subject_id

meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id")
max_microbe_vec <- setNames(maxdata$max_subject_microbe_5, maxdata$subject_id)
pcoa_df$max_subject_microbe_5 <- max_microbe_vec[pcoa_df$subject_id]
pcoa_df <- pcoa_df %>%
  arrange(mom_hiv_status)

ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe_5)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = microbe_colors) +      
  labs(
    title = "PCoA Colored by Top 5 Microbe",
    x = "PCoA1",
    y = "PCoA2",
    color = "Microbes"
  ) +
  theme(plot.title = element_text(hjust = 0.5))



#-- 2 separate for microbes
distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") 
pcoa_result <- cmdscale(distance_matrix, k = 2)
pcoa_df <- as.data.frame(pcoa_result) %>%
  rename(PCoA1 = V1, PCoA2 = V2)

pcoa_df$subject_id <- microdata$subject_microdata$subject_id

meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id") %>%
  filter(!is.na(mom_hiv_status))



maxdata <- max_microbes_data %>% select(subject_id, max_subject_microbe)
max_microbe_vec <- setNames(maxdata$max_subject_microbe, maxdata$subject_id)
pcoa_df$max_subject_microbe <- max_microbe_vec[pcoa_df$subject_id]

pcoa_df_POS <- pcoa_df %>% filter(mom_hiv_status == "Positive")
pcoa_df_NEG <- pcoa_df %>% filter(mom_hiv_status == "Negative")

p_microbe_POS <- ggplot(pcoa_df_POS, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "PCoA HIV Positive",
       x = "PCoA1", y = "PCoA2", color = "Top Microbe") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = microbe_colors)

p_microbe_NEG <- ggplot(pcoa_df_NEG, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = "PCoA HIV Negative",
       x = "PCoA1", y = "PCoA2", color = "Top Microbe") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = microbe_colors)
p_microbe_NEG + p_microbe_POS

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

library(showtext)
showtext_auto() 
font_add_google(name = "Source Sans Pro", family = "Source Sans")


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

hiv_pcoa <- ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = mom_hiv_status)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "PCoA Colored by Mom HIV Status",
    x = "PCoA1",
    y = "PCoA2",
    color = "Mom HIV Status"
  ) +
  scale_color_manual(values = c("Negative" = "#73baf3", "Positive" = "#f37373")) +
  #theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  theme(
    plot.title = element_text(family = "Untitled Sans", size = 20, hjust = 0.5),
    legend.position = "none",
    text = element_text(family = "Untitled Sans")
  )
print(hiv_pcoa)

ggsave(
  plot = hiv_pcoa,
  filename = "hiv_pcoa.svg",
  width = 30,
  height = 15,
  units = "cm"
)




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

top8_pcoa <- ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = microbe_colors) +      
  labs(
    title = "PCoA Colored by Top 8 Microbes",
    x = "PCoA1",
    y = "PCoA2",
    color = "Microbes"
  ) +
  #theme(plot.title = element_text(hjust = 0.5), legend.position = "none") 
  theme(
    plot.title = element_text(family = "Untitled Sans", size = 20, hjust = 0.5),
    legend.position = "none",
    text = element_text(family = "Untitled Sans")
  )
print(top8_pcoa)

ggsave(
  plot = top8_pcoa,
  filename = "top8_pcoa.svg",
  width = 25,
  height = 15,
  units = "cm"
)


# 
# #plot by top 5 microbes
# distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") 
# pcoa_result <- cmdscale(distance_matrix, k = 2)
# pcoa_df <- as.data.frame(pcoa_result)
# pcoa_df <- pcoa_df %>%
#   rename(PCoA1 = V1, PCoA2 = V2)
# 
# maxdata <- max_microbes_data %>% select (subject_id, max_subject_microbe_5)
# 
# pcoa_df$subject_id <- microdata$subject_microdata$subject_id
# 
# meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
# pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id")
# max_microbe_vec <- setNames(maxdata$max_subject_microbe_5, maxdata$subject_id)
# pcoa_df$max_subject_microbe_5 <- max_microbe_vec[pcoa_df$subject_id]
# pcoa_df <- pcoa_df %>%
#   arrange(mom_hiv_status)
# 
# ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe_5)) +
#   geom_point(size = 3, alpha = 0.8) +
#   theme_minimal() +
#   scale_color_manual(values = microbe_colors) +      
#   labs(
#     title = "PCoA Colored by Top 5 Microbe",
#     x = "PCoA1",
#     y = "PCoA2",
#     color = "Microbes"
#   ) +
#   theme(plot.title = element_text(hjust = 0.5))


# 
# #-- 2 separate for microbes
# distance_matrix <- vegdist(microdata$microbe_only_data, method = "bray") 
# pcoa_result <- cmdscale(distance_matrix, k = 2)
# pcoa_df <- as.data.frame(pcoa_result) %>%
#   rename(PCoA1 = V1, PCoA2 = V2)
# 
# pcoa_df$subject_id <- microdata$subject_microdata$subject_id
# 
# meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)
# pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id") %>%
#   filter(!is.na(mom_hiv_status))
# 
# 
# 
# maxdata <- max_microbes_data %>% select(subject_id, max_subject_microbe)
# max_microbe_vec <- setNames(maxdata$max_subject_microbe, maxdata$subject_id)
# pcoa_df$max_subject_microbe <- max_microbe_vec[pcoa_df$subject_id]
# 
# pcoa_df_POS <- pcoa_df %>% filter(mom_hiv_status == "Positive")
# pcoa_df_NEG <- pcoa_df %>% filter(mom_hiv_status == "Negative")
# 
# p_microbe_POS <- ggplot(pcoa_df_POS, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
#   geom_point(size = 3, alpha = 0.8) +
#   theme_minimal() +
#   labs(title = "PCoA HIV Positive",
#        x = "PCoA1", y = "PCoA2", color = "Top Microbe") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_color_manual(values = microbe_colors)
# 
# p_microbe_NEG <- ggplot(pcoa_df_NEG, aes(x = PCoA1, y = PCoA2, color = max_subject_microbe)) +
#   geom_point(size = 3, alpha = 0.8) +
#   theme_minimal() +
#   labs(title = "PCoA HIV Negative",
#        x = "PCoA1", y = "PCoA2", color = "Top Microbe") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   scale_color_manual(values = microbe_colors)
# p_microbe_NEG + p_microbe_POS




#--------Plot by HIV status separated by 3m, 6m, 12m
pcoa_microdata <- microdata$subject_microdata %>%
  select(
    -master_idx,
    -sample,
    #-species_richness 
  )
micro_subset3 <- metadata$subject_metadata %>%
  select(
    mom_hiv_status,
    subject_id
  )
pcoa_microdata <- left_join(
    pcoa_microdata, 
    micro_subset3,
    by = "subject_id"
  )

micro3m <- pcoa_microdata %>% filter(ageMonths < 4.5)
micro6m <- pcoa_microdata %>% filter(ageMonths >= 4.5, ageMonths < 9)
micro12m <- pcoa_microdata %>% filter(ageMonths >= 9, ageMonths < 15)

m3microbeonly <- micro3m %>%
  select(
    -subject_id,
    -mom_hiv_status,
    -ageMonths
  )
m6microbeonly <- micro6m %>%
  select(
    -subject_id,
    -mom_hiv_status,
    -ageMonths
  )
m12microbeonly <- micro12m %>%
  select(
    -subject_id,
    -mom_hiv_status,
    -ageMonths
  )

build_matrix <- function(matrix_data, data) {
  distance_matrix <- vegdist(matrix_data, method = "bray")
  
  pcoa_result <- cmdscale(distance_matrix, k = 2)
  pcoa_df <- as.data.frame(pcoa_result) %>%
    rename(PCoA1 = V1, PCoA2 = V2)
  
  pcoa_df$subject_id <- data$subject_id

  meta_subset <- metadata$subject_metadata %>%
    select(subject_id, mom_hiv_status)
  
  pcoa_df <- left_join(pcoa_df, meta_subset, by = "subject_id") %>%
    arrange(mom_hiv_status) %>%
    filter(!is.na(mom_hiv_status))
  
  return(pcoa_df)
}

pcoa_3m_df <- build_matrix(m3microbeonly, micro3m)
pcoa_6m_df <- build_matrix(m6microbeonly, micro6m)
pcoa_12m_df <- build_matrix(m12microbeonly, micro12m)

plot_pcoa_by_hiv <- function(dataframe, title) { 
  ggplot(dataframe, aes(x = PCoA1, y = PCoA2, color = mom_hiv_status)) +
  geom_point(size = 3, alpha = 0.8) +
  theme_minimal() +
  labs(title = title, x = "PCoA1", y = "PCoA2", color = "Mom HIV Status") +
  scale_color_manual(values = c("Negative" = "#73baf3", "Positive" = "#f37373")) +
  #theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
  theme(
    plot.title = element_text(family = "Untitled Sans", size = 20, hjust = 0.5),
    legend.position = "none",
    text = element_text(family = "Untitled Sans")
  )
}

pcoa_3m <- plot_pcoa_by_hiv(pcoa_3m_df, "PCoA by HIV (3 months)")

pcoa_6m <- plot_pcoa_by_hiv(pcoa_6m_df, "PCoA by HIV (6 months)")

pcoa_12m <- plot_pcoa_by_hiv(pcoa_12m_df, "PCoA by HIV (12 months)")

ggsave(
  plot = pcoa_3m,
  filename = "pcoa_3m.svg",
  width = 30,
  height = 15,
  units = "cm"
)

ggsave(
  plot = pcoa_6m,
  filename = "pcoa_6m.svg",
  width = 30,
  height = 15,
  units = "cm"
)

ggsave(
  plot = pcoa_12m,
  filename = "pcoa_12m.svg",
  width = 30,
  height = 15,
  units = "cm"
)


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
source("notebooks/graph_colors.R")



#-----Scatterplot 
# microdata$subject_microdata$ageMonths <- as.numeric(microdata$subject_microdata$ageMonths)
# microdata$subject_microdata$shannon_div <- as.numeric(microdata$subject_microdata$shannon_div)

scatter_data <- microdata$subject_microdata %>%
  select(subject_id, (all_of(c("ageMonths", "shannon_div", "species_richness")))) %>% 
  mutate(
    subject_id = gsub("khula-", "", subject_id)
  )

scatter_data <- scatter_data %>%
  left_join(
    metadata$subject_metadata %>% select(subject_id, mom_hiv_status),
    by = "subject_id"
  )



p_shannon <- ggplot(scatter_data, aes(x = ageMonths, y = shannon_div, color = mom_hiv_status)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = 'lm', colour = "black", linewidth = 0.5, se = FALSE) +
  theme_minimal() +
  xlab("Child Age (Months)") +
  ylab("Shannon Diversity") +
  scale_color_manual(values = mom_hiv_status_colors) +
  

p_richness <- ggplot(scatter_data, aes(x = ageMonths, y = species_richness, color = mom_hiv_status)) +
  geom_point(size = 1, shape = 16) +
  geom_smooth(method = 'lm', colour = "black", linewidth = 0.5, se = FALSE) +
  theme_minimal() +
  xlab("Child Age (Months)") +
  ylab("Species Richness") +
  scale_color_manual(values = mom_hiv_status_colors)

joint_plot <- (p_shannon /
    p_richness
) + plot_layout(heights = rep(1, 2)) & theme(legend.position = "none")

ggsave(
  plot = joint_plot,
  filename = "linear.svg",
  width = 20,
  height = 20,
  units = "cm"
)



#-- separated HIV status
# 
# scatterdata_POS <- scatter_data %>% filter(mom_hiv_status == "Positive")
# scatterdata_NEG <- scatter_data %>% filter(mom_hiv_status == "Negative")
# 
# p_shannon_scatter_POS <- ggplot(scatterdata_POS, aes(x = ageMonths, y = shannon_div)) +
#   geom_point(size = 1, shape = 1) +
#   geom_smooth(method = 'lm', colour = "#f37373", linewidth = 0.5, se = TRUE) +
#   theme_minimal() +
#   xlab("Age (Months)") +
#   ylab("Shannon Diversity") +
#   labs(title = "Shannon Diversity HIV Positive")
# 
# p_shannon_scatter_NEG <- ggplot(scatterdata_NEG, aes(x = ageMonths, y = shannon_div)) +
#   geom_point(size = 1, shape = 1) +
#   geom_smooth(method = 'lm', colour = "#73baf3", linewidth = 0.5, se = TRUE) +
#   theme_minimal() +
#   xlab("Age (Months)") +
#   ylab("Shannon Diversity") +
#   labs(title = "Shannon Diversity HIV Negative")
# 
# 
# p_rich_scatter_POS <- ggplot(scatterdata_POS, aes(x = ageMonths, y = species_richness)) +
#   geom_point(size = 1, shape = 1) +
#   geom_smooth(method = 'lm', colour = "#f37373", linewidth = 0.5, se = TRUE) +
#   theme_minimal() +
#   xlab("Age (Months)") +
#   ylab("Species Richness") +
#   labs(title = "Species Richness HIV Positive")
# 
# p_rich_scatter_NEG <- ggplot(scatterdata_NEG, aes(x = ageMonths, y = species_richness)) +
#   geom_point(size = 1, shape = 1) +
#   geom_smooth(method = 'lm', colour = "#73baf3", linewidth = 0.5, se = TRUE) +
#   theme_minimal() +
#   xlab("Age (Months)") +
#   ylab("Species Richness") +
#   labs(title = "Species Richness HIV Negative")
# 
# (p_shannon_scatter_POS / 
#     p_shannon_scatter_NEG / 
#     p_rich_scatter_POS / 
#     p_rich_scatter_NEG
# ) + plot_layout (ncol = 2)
# 
# 

#-----linear regression
# scatter_data <- scatter_data %>%
#   mutate(
#     max_subject_microbe = max_microbes_data$max_subject_microbe[match(subject_id, max_microbes_data$subject_id)],
#     max_abundance = max_microbes_data$max_abundance[match(subject_id, max_microbes_data$subject_id)],
#     child_sex = metadata$subject_metadata$child_sex[match(subject_id, metadata$subject_metadata$subject_id)],
#     mat_edu_years = metadata$subject_metadata$mat_edu_years[match(subject_id, metadata$subject_metadata$subject_id)]
#   )
# 
# func_lm <- function(microbe) {
#   microbe_set <- scatter_data %>%
#     filter(max_microbes_data == microbe) %>%
#     filter(
#       !is.na(mom_hiv_status),
#       !is.na(max_abundance),
#       !is.na(child_sex),
#       !is.na(ageMonths)
#     )
#   
#   model_microbe <- lm(max_abundance ~ ageMonths + mom_hiv_status + child_sex, data = microbe_set)
#   return(summary(model_microbe))
# }
# 
# func_lm("Escherichia_coli")
# func_lm("Bifidobacterium_longum")
# func_lm("Prevotella_copri")
# func_lm("Bifidobacterium_breve")
# func_lm("Bifidobacterium_pseudocatenulatum")
# func_lm("Bifidobacterium_kashiwanohense")
# func_lm("Ruminococcus_gnavus")
# func_lm("Bifidobacterium_bifidum")
# 
# plot_microbe_func <- function(scatter_data, microbe) {
#   microbe_set <- scatter_data %>%
#     # filter(max_microbe == microbe) %>%
#     filter(
#       !is.na(mom_hiv_status),
#       !is.na(max_abundance),
#       !is.na(child_sex),
#       !is.na(ageMonths)
#     )
#   ggplot(data = microbe_set, aes(x = ageMonths, y = .data[[microbe]], color = mom_hiv_status)) +
#     geom_point(size = 1, shape = 1) +
#     geom_smooth(method = "lm", linewidth = 0.5, se = TRUE) +
#     theme_minimal() +
#     xlab("Age (Months)") +
#     ylab("Microbe Abundance") +
#     labs(title = paste("Linear Regression of", microbe))+
#     theme_minimal()
# }
# 
# plot_microbe_func(scatter_data, "Escherichia_coli")
# plot_microbe_func(scatter_data, "Bifidobacterium_longum")
# plot_microbe_func(scatter_data, "Prevotella_copri")
# plot_microbe_func(scatter_data, "Bifidobacterium_breve")
# plot_microbe_func(scatter_data, "Bifidobacterium_pseudocatenulatum")
# plot_microbe_func(scatter_data, "Bifidobacterium_kashiwanohense")
# plot_microbe_func(scatter_data, "Ruminococcus_gnavus")
# plot_microbe_func(scatter_data, "Bifidobacterium_bifidum")
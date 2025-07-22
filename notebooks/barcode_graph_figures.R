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
source("notebooks/graph_colors.R")



#-----heatmap/barcode plot

func_barcode <- function(data, x, fill, scale_fill, title) {
  ggplot(data, aes(x = {{x}}, fill = {{fill}})) +
    geom_tile(aes(y = 1), height = 0.4) + 
    {{scale_fill}} +
    coord_cartesian(ylim = c(0, 1)) + 
    theme_void() +
    labs(title = title) +
    theme(
      plot.title = element_text(size = 10),
      legend.position = "none", #remove legend
      theme(plot.margin = margin(0, 0, 0, 0))
    )
}

p_hiv <- func_barcode(metadata$subject_metadata, master_idx, mom_hiv_status, 
                      scale_fill_manual(values = mom_hiv_status_colors),
                      "Maternal HIV Status")

p_mat_edu <- func_barcode(metadata$subject_metadata, master_idx, mat_edu_years, 
                          mat_edu_colors, "Maternal Education (years)")

p_gest_weeks <- func_barcode(metadata$subject_metadata, master_idx, ga_weeks, 
                             gestational_wks_colors, "Gestational Time (weeks)")

p_delivery <- func_barcode(metadata$subject_metadata, master_idx, delivery_mode, 
                           scale_fill_manual(values = delivery_colors),
                           "Delivery Mode")

p_sex <- func_barcode(metadata$subject_metadata, master_idx, child_sex, 
                      scale_fill_manual(values = child_sex_colors),
                      "Child Sex")

(p_hiv/
    p_mat_edu/
    p_delivery/
    p_sex/
    p_gest_weeks
) + plot_layout(ncol = 1, heights = rep(1, 5), guides = "collect")


  plot_layout(ncol = 1) +      
  plot_annotation(theme = theme(plot.margin = margin(0,0,0,0))) &
  theme(plot.margin = margin(0,0,0,0))

# p_microbes <- ggplot(max_microbes_data, aes(x = subject_id, y = 1, fill = max_microbe)) +
#   geom_tile(height = 1) +
#   scale_fill_manual(values = microbe_colors) +
#   theme_void() +
#   labs(title = "Top 5 Microbes")
# 
# p_hiv / p_microbes + plot_layout(heights = c(1, 1))


#add for feeding, diff colors for breast, formula, other
# 
# 
# p_3mfeed <- func_barcode(metadata$subject_metadata, master_idx, feeding_3m, 
#                          scale_fill_manual(values = feeding_colors), "Feeding Types 3m")
# 
# p_6mfeed <- func_barcode(metadata$subject_metadata, master_idx, feeding_6m, 
#                          scale_fill_manual(values = feeding_colors), "Feeding Types 6m")
# 
# p_12mfeed <- func_barcode(metadata$subject_metadata, master_idx, feeding_12m, 
#                           scale_fill_manual(values = feeding_colors, drop = FALSE),
#                           "Feeding Types 12m")
# 
# p_18mfeed <- func_barcode(metadata$subject_metadata, master_idx, feeding_18m, 
#                           scale_fill_manual(values = feeding_colors, drop=FALSE), 
#                           "Feeding Types 18m")
# 
# #plot for positive maternal HIV
# 
# #sort by hiv and then sort by feed type
# pos_metadata_feed3m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Positive") %>%
#   arrange(feeding_3m) %>%
#   mutate(master_idx = row_number())
# 
# pos_metadata_feed6m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Positive") %>%
#   arrange(feeding_6m) %>%
#   mutate(master_idx = row_number())
# 
# pos_metadata_feed12m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Positive") %>%
#   arrange(feeding_12m) %>%
#   mutate(master_idx = row_number())
# 
# pos_metadata_feed18m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Positive") %>%
#   arrange(feeding_18m) %>%
#   mutate(master_idx = row_number())
# 
# p_feed3mPOS <- func_barcode(pos_metadata_feed3m, master_idx, feeding_3m, 
#                             scale_fill_manual(values = feeding_colors), 
#                             "Feeding Types 3m Positive HIV")
# 
# p_feed6mPOS <- func_barcode(pos_metadata_feed6m, master_idx, feeding_6m, 
#                             scale_fill_manual(values = feeding_colors), 
#                             "Feeding Types 6m Positive HIV")
# 
# p_feed12mPOS <- func_barcode(pos_metadata_feed12m, master_idx, feeding_12m, 
#                              scale_fill_manual(values = feeding_colors), 
#                              "Feeding Types 12m Positive HIV")
# 
# p_feed18mPOS <- func_barcode(pos_metadata_feed18m, master_idx, feeding_18m, 
#                              scale_fill_manual(values = feeding_colors), 
#                              "Feeding Types 18m Positive HIV")
# 
# #plot for negative maternal HIV
# neg_metadata_feed3m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Negative") %>%
#   arrange(feeding_3m) %>%
#   mutate(master_idx = row_number())
# 
# neg_metadata_feed6m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Negative") %>%
#   arrange(feeding_6m) %>%
#   mutate(master_idx = row_number())
# 
# neg_metadata_feed12m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Negative") %>%
#   arrange(feeding_12m) %>%
#   mutate(master_idx = row_number())
# 
# neg_metadata_feed18m <- metadata$subject_metadata %>%
#   filter(mom_hiv_status == "Negative") %>%
#   arrange(feeding_18m) %>%
#   mutate(master_idx = row_number())
# 
# p_feed3mNEG <- ggplot(neg_metadata_feed3m, 
#                       aes(x = master_idx, y = 1, fill = feeding_3m)) +
#   geom_tile(height = 1) +
#   scale_fill_manual(values = feeding_colors) +
#   theme_void() +
#   labs(title = "Feeding Types 3m Negative HIV")
# 
# p_feed6mNEG <- ggplot(neg_metadata_feed6m, 
#                       aes(x = master_idx, y = 1, fill = feeding_6m)) +
#   geom_tile(height = 1) +
#   scale_fill_manual(values = feeding_colors) +
#   theme_void() +
#   labs(title = "Feeding Types 6m Negative HIV")
# 
# p_feed12mNEG <- ggplot(neg_metadata_feed12m, 
#                        aes(x = master_idx, y = 1, fill = feeding_12m)) +
#   geom_tile(height = 1) +
#   scale_fill_manual(values = feeding_colors) +
#   theme_void() +
#   labs(title = "Feeding Types 12m Negative HIV")
# 
# p_feed18mNEG <- ggplot(neg_metadata_feed18m, 
#                        aes(x = master_idx, y = 1, fill = feeding_18m)) +
#   geom_tile(height = 1) +
#   scale_fill_manual(values = feeding_colors) +
#   theme_void() +
#   labs(title = "Feeding Types 18m Negative HIV")
# 
# 
# 
# (p_feed3mPOS /
#     p_feed3mNEG /
#     p_feed6mPOS /
#     p_feed6mNEG /
#     p_feed12mPOS /
#     p_feed12mNEG /
#     p_feed18mPOS /
#     p_feed18mNEG
# ) + plot_layout(heights = rep(1, 8))


# 
# (p_hiv/
#     p_3mfeed/
#     p_6mfeed/
#     p_12mfeed/
#     p_18mfeed
# ) + plot_layout(heights = rep(1, 5))



#--barcode graphs


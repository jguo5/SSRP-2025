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
library(grid)
set.seed(1234)

source("notebooks/load_data.R")
metadata <- load_metadata()
microdata <- load_microdata()
source("notebooks/microdata_max_microbe.R")
source("notebooks/graph_colors.R")

#-----heatmap/barcode plot

func_barcode <- function(data, x, fill, scale_fill, title, guideopts) {
  p <- ggplot(data, aes(x = {{x}}, fill = {{fill}})) +
    geom_tile(aes(y = 1)) + 
    {{scale_fill}} +
    # coord_cartesian(ylim = c(0, 1)) + 
    theme_void(base_size = 16) +
    theme(
      legend.position = "none", #remove legend
      legend.title = element_text(size=12),
      plot.margin = margin(0, 0, 0, 40),
      axis.title.y = element_text(color = "black", size = 12, face = "bold")
    ) +
    ylab(title) +
    guideopts
}

p_hiv <- func_barcode(metadata$subject_metadata, master_idx, mom_hiv_status, 
                      scale_fill_manual(values = mom_hiv_status_colors),
                      "Maternal\nHIV\nStatus",
                      guideopts = guides(fill=guide_legend(ncol=1, title.position = "left", title="Maternal\nHIV")))

p_mat_edu <- func_barcode(metadata$subject_metadata, master_idx, mat_edu_years, 
                          mat_edu_colors, "Maternal\nEducation\n(years)",
                          guideopts = guides(fill=guide_colorbar(direction = "vertical", ncol=1, title.position = "left", title="Maternal\nEducation")))

p_gest_weeks <- func_barcode(metadata$subject_metadata, master_idx, ga_weeks, 
                             gestational_wks_colors, "Gestational\nTime\n(weeks)",
                             guideopts = guides(fill=guide_colorbar(direction = "vertical", ncol=1, title.position = "left", title="Gestational\nTime")))

p_delivery <- func_barcode(metadata$subject_metadata, master_idx, delivery_mode, 
                           scale_fill_manual(values = delivery_colors),
                           "Delivery\nMode",
                           guideopts = guides(fill=guide_legend(ncol=1, title.position = "left", title="Delivery\nMode")))

p_sex <- func_barcode(metadata$subject_metadata, master_idx, child_sex, 
                      scale_fill_manual(values = child_sex_colors),
                      "Child\nSex",
                      guideopts = guides(fill=guide_legend(ncol=1, title.position = "left", title="Child\nSex")))

joint_plot <- (p_hiv/
    p_mat_edu/
    p_delivery/
    p_sex/
    p_gest_weeks
) +
  plot_layout(ncol = 1, heights=rep(-1,5), guides = "collect") &
  theme(legend.position='bottom', plot.margin = margin(0, 0, 0, 0), legend.margin = margin(0,10,0,10))

ggsave(
  plot = joint_plot,
  filename = "barcodes.png",
  width = 40,
  height = 14,
  units = "cm"
)

#--barcode graphs


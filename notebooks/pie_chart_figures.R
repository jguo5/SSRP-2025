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
library(showtext)
set.seed(1234)

source("notebooks/load_data.R")
metadata <- load_metadata()
microdata <- load_microdata()
source("notebooks/microdata_max_microbe.R")

showtext_auto() 
font_add_google(name = "Source Sans Pro", family = "Source Sans")


#---Child Sex Pie Chart
pie_child_sex_df <- metadata$subject_metadata %>%
  group_by(child_sex) %>%
  summarise(N = n()) %>%
  mutate(
    proportion = N / sum(N), #from plot_df$proportion <- plot_df$N  / sum(plot_df$N)
  ) %>%
  arrange(desc(child_sex)) %>% #arrange df in desc order (1->0) to match graph w. labels
  mutate(
    csum = cumsum(proportion), #top of each slice
    pos = csum - proportion / 2, #where label goes
    labelSex = paste0(child_sex, ": ", N, " (", round(proportion * 100, 2), "%)")
  )

pie_child_sex <- ggplot(pie_child_sex_df, aes(y = proportion, fill = child_sex)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1, y = pos, label = labelSex),
    size = 4,
    color = "black") +
  theme_void(base_family = "Source Sans") +
  scale_fill_manual(values = c(F = "orange", M = "lightblue")) +
  labs(title = "Child Sex", fill = "Sex") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  plot = pie_child_sex,
  filename = "pie_child_sex.svg",
  width = 20,
  height = 20,
  units = "cm"
)

print(pie_child_sex)



#---Delivery Mode Pie Chart
pie_delivery_df <- metadata$subject_metadata %>%
  group_by(delivery_mode) %>%
  summarise(N = n()) %>%
  #na.omit() %>%     #omit na values (65...)
  mutate(
    proportion = N / sum(N), #from plot_df$proportion <- plot_df$N  / sum(plot_df$N)
  ) %>%
  arrange(desc(delivery_mode)) %>% #arrange df in desc order (1->0) to match graph w. labels
  mutate(
    csum = cumsum(proportion), #top of each slice
    pos = csum - proportion / 2, #where label goes
    labelDeli = paste0(delivery_mode, ": ", N, " (", round(proportion * 100, 2), "%)")
  )

pie_delivery <- ggplot(pie_delivery_df, aes(y = proportion, fill = delivery_mode)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1, y = pos, label = labelDeli),
    size = 4,
    color = "black") +
  theme_void() +
  scale_fill_manual(values = c("Vaginal" = "#9df373", "Cesarean" = "#c1acf5")) +
  labs(title = "Delivery Mode", fill = "Mode") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  plot = pie_delivery,
  filename = "pie_delivery.svg",
  width = 20,
  height = 20,
  units = "cm"
)



print(pie_delivery) 



#--pie chart

pie_data <- max_microbes_data %>%
  count(max_subject_microbe)%>%
  mutate(
    prevalence = n / sum(n),
    label = paste0(round(prevalence * 100, 1), "%"),
  ) %>%
  arrange(desc(-n))

pie_data <- pie_data %>%
  mutate(
    max_subject_microbe = factor(max_subject_microbe, levels = max_subject_microbe)
  )

microbe_pie <- ggplot(pie_data, aes(x = "", y = prevalence, fill = max_subject_microbe)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void(base_family = "Source Sans") +
  scale_fill_manual(values = microbe_colors)            
  #labs(title = "Microbe Prevalence Pie Chart")

ggsave(
  plot = microbe_pie,
  filename = "microbe_pie.svg",
  width = 20,
  height = 20,
  units = "cm"
)
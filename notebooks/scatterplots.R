
#-----Scatterplot 
microdata$ageMonths <- as.numeric(microdata$ageMonths)
microdata$shannon_div <- as.numeric(microdata$shannon_div)

p_shannon <- ggplot(microdata, aes(x = ageMonths, y = shannon_div)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  theme_minimal() +
  xlab("Child Age (Months)") +
  ylab("Shannon Diversity")

p_richness <- ggplot(microdata, aes(x = ageMonths, y = species_richness)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  theme_minimal() +
  xlab("Child Age (Months)") +
  ylab("Species Richness")

grid.arrange (p_shannon, p_richness, ncol = 2)


#-- separated HIV status
scatter_data <- microdata %>%
  select(subject_id, (all_of(c("ageMonths", "shannon_div", "species_richness")))) %>% 
  mutate(
    subject_id = gsub("khula-", "", subject_id)
  )

scatter_data <- scatter_data %>%
  left_join(
    processed_metadata %>% select(subject_id, mom_hiv_status),
    by = "subject_id"
  )

scatterdata_POS <- scatter_data %>% filter(mom_hiv_status == "Positive")
scatterdata_NEG <- scatter_data %>% filter(mom_hiv_status == "Negative")

p_shannon_scatter_POS <- ggplot(scatterdata_POS, aes(x = ageMonths, y = shannon_div)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "#f37373", linewidth = 0.5, se = TRUE) +
  theme_minimal() +
  xlab("Age (Months)") +
  ylab("Shannon Diversity") +
  labs(title = "Shannon Diversity HIV Positive")

p_shannon_scatter_NEG <- ggplot(scatterdata_NEG, aes(x = ageMonths, y = shannon_div)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "#73baf3", linewidth = 0.5, se = TRUE) +
  theme_minimal() +
  xlab("Age (Months)") +
  ylab("Shannon Diversity") +
  labs(title = "Shannon Diversity HIV Negative")


p_rich_scatter_POS <- ggplot(scatterdata_POS, aes(x = ageMonths, y = species_richness)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "#f37373", linewidth = 0.5, se = TRUE) +
  theme_minimal() +
  xlab("Age (Months)") +
  ylab("Species Richness") +
  labs(title = "Species Richness HIV Positive")

p_rich_scatter_NEG <- ggplot(scatterdata_NEG, aes(x = ageMonths, y = species_richness)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "#73baf3", linewidth = 0.5, se = TRUE) +
  theme_minimal() +
  xlab("Age (Months)") +
  ylab("Species Richness") +
  labs(title = "Species Richness HIV Negative")

(p_shannon_scatter_POS / 
    p_shannon_scatter_NEG / 
    p_rich_scatter_POS / 
    p_rich_scatter_NEG
) + plot_layout (ncol = 2)





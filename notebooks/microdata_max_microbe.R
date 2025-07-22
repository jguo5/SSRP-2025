##Jackleen Guo
##SSRP Graphs

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



##-----microbe frequencies
count_max_microbe_df <- data.frame(
  num_subjects = colSums(microdata$microbe_only_data > 0, na.rm = TRUE) #number of subjects with microbe % >0
)

head(count_max_microbe_df)


#--microbe with most subjects--max of count_max_microbe_df
max_subject_microbe <- rownames(count_max_microbe_df)[which.max(count_max_microbe_df[[1]])] #extract 1 microbe
max_microbe_count <- max(count_max_microbe_df[[1]]) #extract one microbe's count
print(paste0("Microbe with most occurences: ", max_subject_microbe))
print(paste0("Subject count of max_microbe: ", max_microbe_count))


#--microbe greatest for each subject
#subject_microdata, return the subject_id
max_microbes_data <- microdata$subject_microdata %>%
  select(
    "subject_id"
  )
indiv_index <- apply(microdata$microbe_only_data, 1, which.max) #return index of max microbe column
indiv_name <- colnames(microdata$microbe_only_data)[indiv_index] #get name of indexed column
head(indiv_index)
head(indiv_name)

max_microbes_data$microbe_name <- indiv_name

max_abundance <- nrow(microdata$microbe_only_data) #add abundance as another column

for (i in 1:max_abundance) {
  max_abundance[i] <- microdata$microbe_only_data[i, indiv_index[i]]
}

max_microbes_data$max_abundance <- max_abundance

#--species richness
#for each subject, which microbes>0, add to count
#num_subjects = colSums(microbe_only_data > 0, na.rm = TRUE)

num_microbe <- rowSums(microdata$microbe_only_data > 0, na.rm = TRUE) #how many true (>0) in each row
richness <- data.frame(
  subject_id = microdata$subject_microdata$subject_id, species_richness = num_microbe #create df of richness + subject id
)
microdata$subject_microdata$species_richness <- num_microbe #add to microdata



#-----Mean relative abundance

row_totals <- rowSums(microdata$microbe_only_data)
relative_abundance <- microdata$microbe_only_data

for (i in 1:nrow(microdata$microbe_only_data)) {
  relative_abundance[i, ] <- microdata$microbe_only_data[i, ] / 100
}

mean_abundance <- colMeans(relative_abundance)

mean_abundance_df <- data.frame(
  Microbe = names(mean_abundance),
  mean_relative_abundance = as.vector(mean_abundance)
)


#-----Max counts

max_counts <- data.frame(table(max_microbes_data$microbe_name))
max_counts <- max_counts %>% arrange(desc(Freq))
top8_names <- as.character(head((max_counts$Var1), 8))
top5_names <- as.character(head((max_counts$Var1), 5))

meta_subset <- metadata$subject_metadata %>% select(subject_id, mom_hiv_status)

subject_order <- meta_subset %>%
  filter(!is.na(mom_hiv_status)) %>%
  arrange(mom_hiv_status, subject_id) %>% #arrange hiv and then subjid
  pull(subject_id) #extract as vector

meta_subset <- meta_subset %>%
  filter(subject_id %in% subject_order) %>%
  mutate(subject_id = factor(subject_id, levels = subject_order))

max_microbes_data <- max_microbes_data %>%
  mutate(
    max_subject_microbe = ifelse(microbe_name %in% top8_names, microbe_name, "Other")
  ) %>%
  mutate(
    max_subject_microbe_5 = ifelse(microbe_name %in% top5_names, microbe_name, "Other")
  ) %>%
  left_join(meta_subset, by = "subject_id") %>%
  mutate(subject_id = factor(subject_id, levels = subject_order)) %>% 
  arrange (subject_id) %>%
  filter(!is.na(mom_hiv_status))

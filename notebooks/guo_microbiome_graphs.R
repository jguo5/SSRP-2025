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

microdata <- read.csv(fs::path(here::here(), "ext", "2025-06-25-JGuo-taxonomic_inputs.csv"), header=TRUE)
attach(microdata)
head(microdata, 3)

processed_micro <- microdata %>%
  select(
    -datasource,
    -study_name,
    -datagroup,
    -site,
    -datacolor,
    -visit,
    -westernized_cat, 
    -ageMonths,
    -sample  #should we do sample number or subject ID
  )%>%
  mutate(
    subject_id = gsub("khula-", "", subject_id)
  )%>%
  mutate( . , master_idx = 1:nrow(.))

#metadata maternal hiv import
metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
attach(metadata)
head(metadata, 3)

processed_metadata <- metadata %>%
  select(
    "subject_id",
    "zymo_code_3m",
    "zymo_code_6m",
    "zymo_code_12m",
    "zymo_code_18m",
    "zymo_code_24m",
    
    "medhx_mom___1_selfreport"
  )%>%
  filter(
    !(zymo_code_3m == "" &
        zymo_code_6m == "" &
        zymo_code_12m == "" &
        zymo_code_18m == "" &
        zymo_code_24m == "")
  )%>%
  rename(
    mom_hiv_status = medhx_mom___1_selfreport
  )%>%
  mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
  arrange(mom_hiv_status) %>%
  mutate( . , master_idx = 1:nrow(.))



##-----shannon diversity index
microbe_only_data <- processed_micro %>%
  select(
    -subject_id, 
    -master_idx
  )

num_patients <- nrow(microbe_only_data)
shannon_div <- numeric(num_patients)

for (i in 1:num_patients) { #go through all subjects 
  species_counts = microbe_only_data[i, ] #do for 1 subject
  species_proportions = species_counts / sum(species_counts) #get propotion, how many species div sum counts (100?)
  shannon_div[i] = -sum(species_proportions * log(species_proportions), na.rm = TRUE) #formula for each subject
}

shannon_only <- data.frame(shannon_div) #see shannon values by itself
microdata$shannon_div <- shannon_div #insert into data



##-----microbe frequencies
df_micro_count <- data.frame(
  num_subjects = colSums(microbe_only_data > 0, na.rm = TRUE) #number of subjects with microbe % >0
)

head(df_micro_count)

#--microbe with most subjects--max of df_micro_count
max_microbe <- rownames(df_micro_count)[which.max(df_micro_count[[1]])] #extract 1 microbe
max_count <- max(df_micro_count[[1]]) #extract one microbe's count
print(paste0("Microbe with most occurences: ", max_microbe))
print(paste0("Subject count of max_microbe: ", max_count))

#--microbe greatest for each subject
#processed_micro, return the subject_id
microbe_maxdata <- processed_micro %>%
  select(
    "subject_id"
  )
indiv_index <- apply(microbe_only_data, 1, which.max) #return index of max microbe column
indiv_name <- colnames(microbe_only_data)[indiv_index] #get name of indexed column
head(indiv_index)
head(indiv_name)

microbe_maxdata$microbe_name <- indiv_name

max_abundance <- nrow(microbe_only_data) #add abundance as another column

for (i in 1:max_abundance) {
  max_abundance[i] <- microbe_only_data[i, indiv_index[i]]
}

microbe_maxdata$max_abundance <- max_abundance

#--species richness
#for each subject, which microbes>0, add to count
#num_subjects = colSums(microbe_only_data > 0, na.rm = TRUE)

num_microbe <- rowSums(microbe_only_data > 0, na.rm = TRUE) #how many true (>0) in each row
richness <- data.frame(
  subject_id = microdata$subject_id, species_richness = num_microbe #create df of richness + subject id
)
microdata$species_richness <- num_microbe #add to microdata


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



#-----Barcode Plots

# naonly_metadata <- metadata %>%
#   select(
#     "subject_id",
#     "zymo_code_3m",
#     "zymo_code_6m",
#     "zymo_code_12m",
#     "zymo_code_18m",
#     "zymo_code_24m"
#     
#   )%>%
#   filter(zymo_code_3m == "") %>%
#   filter(zymo_code_6m == "") %>%
#   filter(zymo_code_12m == "") %>%
#   filter(zymo_code_18m == "") %>%
#   filter(zymo_code_24m == "")

#remove naonly_metadata$subject_id from microbe_maxdata
df_micro_count <- df_micro_count %>% arrange(desc(num_subjects))
top5_names <- head(rownames(df_micro_count), 5)

meta_subset <- processed_metadata %>% select(subject_id, mom_hiv_status)

subject_order <- meta_subset %>%
  filter(!is.na(mom_hiv_status)) %>%
  arrange(mom_hiv_status, subject_id) %>% #arrange hiv and then subjid
  pull(subject_id) #extract as vector

meta_subset <- meta_subset %>%
  filter(subject_id %in% subject_order) %>%
  mutate(subject_id = factor(subject_id, levels = subject_order))

microbe_maxdata <- microbe_maxdata %>%
  mutate(
    max_microbe = ifelse(microbe_name %in% top5_names, microbe_name, "Other")
  ) %>%
  left_join(meta_subset, by = "subject_id") %>%
  mutate(subject_id = factor(subject_id, levels = subject_order)) %>% 
  arrange (subject_id) %>%
  filter(!is.na(mom_hiv_status))





#--barcode graphs
p_hiv <- ggplot(meta_subset, aes(x = subject_id, y = 1, fill = mom_hiv_status)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Negative" = "blue", "Positive" = "red")) +
  theme_void() +
  labs(title = "Maternal HIV")

p_microbes <- ggplot(microbe_maxdata, aes(x = subject_id, y = 1, fill = max_microbe)) +
    geom_tile(height = 1) +
    scale_fill_viridis_d() +
    theme_void() +
    labs(title = "Top 5 Microbes")

p_hiv / p_microbes + plot_layout(heights = c(1, 1))






#--pie chart
pie_data <- microbe_maxdata %>%
  count(max_microbe) %>%
  mutate(
    prevalence = n / sum(n),
    label = paste0(max_microbe, "\n", round(prevalence * 100, 1), "%")
  )

ggplot(pie_data, aes(x = "", y = prevalence, fill = max_microbe)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Microbe Prevalence Pie Chart")


#--PCoA Plot

distance_matrix <- vegdist(microbe_only_data, method = "bray") #lots of 0's, should i remove?
pcoa_result <- cmdscale(distance_matrix, k = 2)

pcoa_df <- as.data.frame(pcoa_result)

pcoa_df <- pcoa_df %>%
  rename(PCoA1 = V1, PCoA2 = V2)


pcoa_df$subject_id <- processed_micro$subject_id
meta_subset <- processed_metadata %>% select(subject_id, mom_hiv_status)
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
  theme(plot.title = element_text(hjust = 0.5))







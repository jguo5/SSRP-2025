##Jackleen Guo
##SSRP Microbiome Data Graphs
## 6-25-2025

##Setup and attach data
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(fs)
library(here)
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
  mutate( . , master_idx = 1:nrow(.))



##------shannon diversity index
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

 
# # Sample data: species counts
# species_counts <- table(df_micro_count$Num_Subjects)
# 
# # Calculate proportions
# species_proportions <- species_counts / sum(species_counts)
# 
# # Calculate Shannon Diversity Index
# shannon_diversity = -sum(species_proportions * log(species_proportions))
# print(shannon_diversity)
# 



##-----microbe frequencies


df_micro_count <- data.frame(
  num_subjects = colSums(microbe_only_data > 0, na.rm = TRUE) #number of subjects with microbe % >0
)

head(df_micro_count)

#microbe with most subjects--max of df_micro_count
max_microbe <- rownames(df_micro_count)[which.max(df_micro_count[[1]])] #extract 1 microbe
max_count <- max(df_micro_count[[1]]) #extract one microbe's count
print(paste0("Microbe with most occurences: ", max_microbe))
print(paste0("Subject count of max_microbe: ", max_count))


#microbe greatest for each subject
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

#add abundance as another column
max_abundance <- nrow(microbe_only_data)

for (i in 1:max_abundance) {
  max_abundance[i] <- microbe_only_data[i, indiv_index[i]]
}

microbe_maxdata$max_abundance <- max_abundance

#species richness
#for each subject, which microbes>0, add to count
#num_subjects = colSums(microbe_only_data > 0, na.rm = TRUE)

num_microbe <- rowSums(microbe_only_data > 0, na.rm = TRUE) #how many true (>0) in each row

richness <- data.frame(
  subject_id = microdata$subject_id, species_richness = num_microbe #create df of richness + subject id
)

microdata$species_richness <- num_microbe #add to microdata


#---Scatterplot 
microdata$ageMonths <- as.numeric(microdata$ageMonths)
microdata$shannon_div <- as.numeric(microdata$shannon_div)

p_shannon <- ggplot(microdata, aes(x = ageMonths, y = shannon_div)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  xlab("Child Age (Months)") +
  ylab("Shannon Diversity")
  
p_richness <- ggplot(microdata, aes(x = ageMonths, y = species_richness)) +
  geom_point(size = 1, shape = 1) +
  geom_smooth(method = 'lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  xlab("Child Age (Months)") +
  ylab("Species Richness")

grid.arrange (p_shannon, p_richness, ncol = 2)


#---Barcode 

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

p_hiv <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = mom_hiv_status)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Negative" = "blue", "Positive" = "red")) +
  theme_void() +
  labs(title = "Maternal HIV")

naonly_metadata <- metadata %>%
  select(
    "subject_id",
    "zymo_code_3m",
    "zymo_code_6m",
    "zymo_code_12m",
    "zymo_code_18m",
    "zymo_code_24m"
    
  )%>%
  filter(zymo_code_3m == "") %>%
  filter(zymo_code_6m == "") %>%
  filter(zymo_code_12m == "") %>%
  filter(zymo_code_18m == "") %>%
  filter(zymo_code_24m == "")

#remove naonly_metadata$subject_id from microbe_maxdata

#df_micro_count$prevalence <- (df_micro_count$num_subjects / num_patients) * 100 

df_micro_count <- df_micro_count %>% arrange(desc(num_subjects))
# top5_microbe <- sum(head(df_micro_count$prevalence, 5))
# filtered_df <- df_micro_count %>% filter(row_number() > 5)
# other_sum <- sum(filtered_df$prevalence)
# total = top5_microbe + other_sum

top5_names <- head(rownames(df_micro_count), 5)

microbe_maxdata <- microbe_maxdata %>%
  mutate(
    max_microbe = ifelse(microbe_name %in% top5_names, microbe_name, "Other"),
    subject_id = gsub("khula-", "", subject_id)
  ) %>%
  mutate( . , master_idx = 1:nrow(.))

#pie chart
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


  #filter(!microbe_maxdata$subject_id %in% naonly_metadata$subject_id)

#barcode graph
p_microbes <- ggplot(microbe_maxdata, aes(x = master_idx, y = 1, fill = max_microbe)) +
    geom_tile(height = 1) +
    scale_fill_viridis_d() +
    theme_void() +
    labs(title = "Top 5 Microbes")

grid.arrange(p_hiv, p_microbes, nrow = 2)






#---Pie Chart

# pie_df <- data.frame(
#   microbes = c(top5_names, "Other"),
#   prevalence = c(head(df_micro_count$prevalence, 5), other_sum)
# )
# total <- sum(pie_df$prevalence)
# pie_df$slices <- pie_df$prevalence / total
# pie_df$label <- paste0(pie_df$microbe, "\n", round(pie_df$slices * 100, 1), "%")
# 
# ggplot(pie_df, aes(x = "", y = slices, fill = microbes)) +
#   geom_bar(stat = "identity", width = 1, color = "white") +
#   coord_polar("y", start = 0) +
#   geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
#   theme_void() +
#   labs(title = "Microbe Prevalence") +
#   theme(plot.title = element_text(hjust = 0.5))
#   #scale_fill_discrete(labels = label)

#------------------prevalence --> 300/400
# df_micro_count$microbe <- rownames(df_micro_count)
# num_patients <- nrow(microbe_only_data)
# 
# df_micro_count <- df_micro_count %>%
#   arrange(desc(num_subjects))%>%
#   mutate(prevalence = (num_subjects / num_patients))
# 
# top5_names <- head(df_micro_count$microbe, 5)
# 
# filtered_df <- df_micro_count %>% 
#   filter(row_number() > 5)
# other_sum <- sum(filtered_df$prevalence)
# 
# df_pie <- data.frame(
#   microbes = c(top5_names, "Other"),
#   prevalence = c(head(df_micro_count$prevalence, 5), other_sum)
# )
# df_pie$label <- paste0(df_pie$microbes, "\n", round(df_pie$prevalence * 100, 1), "%")
# 
# ggplot(df_pie, aes(x = "", y = prevalence, fill = microbes)) +
#   geom_bar(stat = "identity", width = 1, color = "white") +
#   coord_polar("y", start = 0) +
#   geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
#   theme_void() +
#   labs(title = "Microbe Prevalence") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# 
# sum_pie <- sum(df_pie$prevalence)
#  
#  
# 
# 
# 
# df_pie <- df_pie %>%
#   mutate (pie_slice = (prevalence/sum_pie))
# 
# 
# 


#every num_subjects/total = prevalence for each
#sum all prevalences for pie slices
#prevalence/sum = pie_slice
#labels -> microbe + pie*100 + %

# 
# pie_df <- data.frame(
#   microbes = c(top5_names, "Other"),
#   prevalence = c(head(df_micro_count$num_subjects, 5), other_sum)
# )
# 
# 

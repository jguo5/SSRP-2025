##Jackleen Guo
##SSRP Microbiome Data Graphs
## 6-25-2025

##Setup and attach data
library(dplyr)
library(tidyr)


data <- read.csv(fs::path(here::here(), "ext", "2025-06-25-JGuo-taxonomic_inputs.csv"), header=TRUE)
attach(data)
head(data, 3)

processed_micro <- data %>%
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
  )

##------shannon diversity index
microbe_only_data <- processed_micro %>%
  select(
    -subject_id 
  )

num_patients <- nrow(microbe_only_data)
shannon_div <- numeric(num_patients)

for (i in 1:num_patients) { #go through all subjects 
  species_counts = microbe_only_data[i, ] #do for 1 subject
  species_proportions = species_counts / sum(species_counts) #formula
  shannon_div[i] = -sum(species_proportions * log(species_proportions), na.rm = TRUE) #formula
}

shannon_only <- data.frame(shannon_div) #see shannon values by itself
data$shannon_div <- shannon_div #insert into data

 
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


#which subject has the most amount of each mircobe
#is this even necessary.......


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


#table(microbe_maxdata)




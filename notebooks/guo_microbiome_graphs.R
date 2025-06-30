##Jackleen Guo
##SSRP Microbiome Data Graphs
## 6-25-2025

##Setup and attach data
library(dplyr)
library(tidyr)
library(ggplot2)

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
microdata$ageMonths <- as.factor(microdata$ageMonths)
microdata$shannon_div <- as.factor(microdata$shannon_div)

p_shannon <- ggplot(microdata, aes(x=ageMonths, y=shannon_div, )) +
  geom_point(size=1, shape=1) +
  geom_smooth(method='lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  xlab("Child Age (Months)") +
  ylab("Shannon Diversity")
  #species richness --> how many different species per subject
  
  # scale_y_continuous(
  #   name = "Shannon Diversity",
  #   sec.axis = sec_axis(~.*coeff, name="Richness)
  # )
  
p_richness <- ggplot(microdata, aes(x=ageMonths, y=species_richness, )) +
  geom_point(size=1, shape=1) +
  geom_smooth(method='lm', colour = "blue", linewidth = 0.5, se = FALSE) +
  xlab("Child Age (Months)") +
  ylab("Species Richness")

grid.arrange (p_shannon, p_richness, ncol = 2)



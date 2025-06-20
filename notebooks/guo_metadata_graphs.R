##Jackleen Guo
##SSRP Metadata Graphs
## 6-17-2025

##-----setup and attach data
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(gridExtra)
library(fs)
library(here)
set.seed(1234)

metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
attach(metadata)
head(metadata, 3)

##-----Processing the Metadata

processed_metadata <- metadata %>%
  select(
    "subject_id",
    "child_sex",
    "delivery_6m",
    "medhx_mom___1_selfreport",
    "mat_edu_years",
    "ga_weeks",
    
    "feeding_type_3m",
    "feeding_type_6m",
    # "feeding_type_12m___1",
    # "feeding_type_12m___2",
    # "feeding_type_12m___3",
    # "feeding_type_18m___1",
    # "feeding_type_18m___2",
    # "feeding_type_18m___3",
  ) %>%
  rename(
    delivery_mode = delivery_6m,
    mom_hiv_status = medhx_mom___1_selfreport,
    gest_weeks = ga_weeks,
    feed_3m = feeding_type_3m,
    feed_6m = feeding_type_6m,
    
    # feed_breast = c(feeding_3m[1], feeding_6m[1], feeding_12m1, feeding_18m1),
    # feed_formula = c(feeding_3m[2], feeding_6m[2], feeding12m2, feeding18m2),
    # feed_mixed = c(feeding_3m[3], feeding_6m[3]),
    # feed_solid = c(feeding_12m3, feeding_18m3)
  ) %>%
  mutate(child_sex = factor(child_sex, ordered = FALSE, levels = c(0,1), labels = c("F", "M"))) %>%
  mutate(delivery_mode = factor(delivery_mode, ordered = FALSE, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))) %>%
  mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
  arrange(mom_hiv_status) %>%
  mutate(feed_3m = factor(feed_3m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed"))) %>%
  mutate(feed_6m = factor(feed_6m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed"))) %>%
  mutate(
    feed_12m = if_else(feeding_type_12m___1 == 1, "Breast",
               if_else(feeding_type_12m___2 == 1, "Formula",
               if_else(feeding_type_12m___3 == 1, "Solid", "N/A"))),
    feed_12m = factor(feed_12m, ordered = FALSE, levels = c("Breast", "Formula", "Solid"))
  ) %>%
  mutate(
    feed_18m = if_else(feeding_type_18m___1 == 1, "Breast",
               if_else(feeding_type_18m___2 == 1, "Formula",
               if_else(feeding_type_18m___3 == 1, "Solid", "N/A"))),
    feed_18m = factor(feed_18m, ordered = FALSE, levels = c("Breast", "Formula", "Solid"))
  ) %>%
  
  mutate( . , master_idx = 1:nrow(.))

  # mutate(
  #   feed_breast = if_else(
  #     feeding_type_3m == 1 | 
  #     feeding_type_6m == 1 | 
  #     feeding_type_12m___1 == 1 | 
  #     feeding_type_18m___1 == 1,
  #     1, 0
  #   ),
  #   feed_formula = if_else(
  #     feeding_type_3m == 2 | 
  #     feeding_type_6m == 2 | 
  #     feeding_type_12m___2 == 1 | 
  #     feeding_type_18m___2 == 1,
  #     1, 0
  #   ),
  #   feed_mixed = if_else(
  #     feeding_type_3m == 3 | 
  #     feeding_type_6m == 3,
  #     1, 0
  #   ),
  #   feed_solid = if_else(
  #     feeding_type_12m___3 == 1 | 
  #     feeding_type_18m___3 == 1,
  #     1, 0
  #   )
  # )%>%
  # mutate(
  #   feed_type = case_when(
  #     feed_breast == 1 ~ "Breast",
  #     feed_formula == 1 ~ "Formula",
  #     feed_mixed == 1 ~ "Mixed",
  #     feed_solid == 1 ~ "Solid",
  #     TRUE ~ "None"
  #   )
  # )%>%
  # mutate(
  #   feed_type = factor(feed_type, ordered = FALSE, levels = c("Breast", "Formula", "Mixed", "Solid"), labels = c("Breast", "Formula", "Mixed", "Solid"))
  # )%>%
  


##-----mom education
edu_mean <- mean(metadata$mat_edu_years, na.rm = TRUE)
edu_sd <- sd(metadata$mat_edu_years, na.rm = TRUE)
edu_median <- median(metadata$mat_edu_years, na.rm = TRUE)
edu_min <- min (metadata$mat_edu_years, na.rm = TRUE)
edu_max <- max (metadata$mat_edu_years, na.rm = TRUE)

print(paste0("Maternal education MEAN: ", edu_mean))
print(paste0("Maternal education SD: ", edu_sd))
print(paste0("Maternal education MEDIAN: ", edu_median))
print(paste0("Maternal education MIN: ", edu_min))
print(paste0("Maternal education MAX: ", edu_max))

table(metadata$mom_edu_en) #mom edu level



##-----birth weight (grams)
bw_mean <- mean(metadata$bw, na.rm = TRUE)
bw_sd <- sd(metadata$bw, na.rm = TRUE)
bw_median <- median(metadata$bw, na.rm = TRUE)

bw_min <- min (metadata$bw, na.rm = TRUE)
bw_max <- max (metadata$bw, na.rm = TRUE)

print(paste0("Birth weight MEAN: ", bw_mean))
print(paste0("Birth weight SD: ", bw_sd))
print(paste0("Birth weight MEDIAN: ", bw_median))
print(paste0("Birth weight MIN: ", bw_min))
print(paste0("Birth weight MAX: ", bw_max))



##-----ARV status
#baby
arv_baby <- table(metadata$baby_arv_selfreport)
print(arv_baby)
arv_df <- as.data.frame(arv_baby)
print(arv_df)
#mom
arv_mom <- table(metadata$mom_arv_selfreport)
print(arv_mom)
arv_dfm <- as.data.frame(arv_mom)
print(arv_dfm)



##-----HIV status
#baby
table(metadata$baby_hiv_selfreport)
#mom
table(metadata$hiv_mom_diagnosed_selfreport)



##-----child sex
#table
table(metadata$child_sex)

#pie chart
#metadata$child_sex <- factor(metadata$child_sex, levels = c(0,1), labels = c("Female", "Male"))

plot_df <- processed_metadata %>%
  group_by(child_sex) %>%
  summarise(N = n()) %>%
  na.omit() %>%     #omit na values (65...)
  mutate(
    proportion = N / sum(N), #from plot_df$proportion <- plot_df$N  / sum(plot_df$N)
    ) %>%
  arrange(desc(child_sex)) %>% #arrange df in desc order (1->0) to match graph w. labels
  mutate(
    csum = cumsum(proportion), #top of each slice
    pos = csum - proportion / 2, #where label goes
    labelSex = paste0(child_sex, ": ", N, " (", round(proportion * 100, 2), "%)")
    )

p <- ggplot(plot_df, aes(y = proportion, fill = child_sex)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1, y = pos, label = labelSex),
    size = 4,
    color = "black") +
  theme_void() +
  labs(title = "Child Sex", fill = "Sex") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)



##-----delivery type
#table
table(metadata$delivery_6m)

#pie chart
#metadata$delivery_6m <- factor(metadata$delivery_6m, levels = c(0,1))

plot_df <- processed_metadata %>%
  group_by(delivery_mode) %>%
  summarise(N = n()) %>%
  na.omit() %>%     #omit na values (65...)
  mutate(
    proportion = N / sum(N), #from plot_df$proportion <- plot_df$N  / sum(plot_df$N)
  ) %>%
  arrange(desc(delivery_mode)) %>% #arrange df in desc order (1->0) to match graph w. labels
  mutate(
    csum = cumsum(proportion), #top of each slice
    pos = csum - proportion / 2, #where label goes
    labelDeli = paste0(delivery_mode, ": ", N, " (", round(proportion * 100, 2), "%)")
  )

p <- ggplot(plot_df, aes(y = proportion, fill = delivery_mode)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1, y = pos, label = labelDeli),
    size = 4,
    color = "black") +
  theme_void() +
  labs(title = "Delivery Mode", fill = "Mode") +
  theme(plot.title = element_text(hjust = 0.5))

print(p) 



##-----feeding

feeding_3m <- table(metadata$feeding_type_3m)

feeding_6m <- table(metadata$feeding_type_6m)

feeding_12m1 <- table(metadata$feeding_type_12m___1)
feeding_12m2 <- table(metadata$feeding_type_12m___2)
feeding_12m3 <- table(metadata$feeding_type_12m___3)

feeding_18m1 <- table(metadata$feeding_type_18m___1)
feeding_18m2 <- table(metadata$feeding_type_18m___2)
feeding_18m3 <- table(metadata$feeding_type_18m___3)

feeding_3m

feeding_6m

feeding_12m1
feeding_12m2
feeding_12m3

feeding_18m1
feeding_18m2
feeding_18m3

#Feeding graph
data3m <- c(feeding_3m[1], feeding_3m[2], feeding_3m[3])
data6m <- c(feeding_6m[1], feeding_6m[2], feeding_6m[3])
data12m <- c(feeding_12m1[1], feeding_12m2[1], feeding_12m3[1])
data18m <- c(feeding_18m1[1], feeding_18m2[1], feeding_18m3[1])

allFeed <- c(data3m, data6m, data12m, data18m)

month <- rep(c("3m", "6m", "12m", "18m"), each = 3)

feedType <- rep(c("Breast", "Formula", "Mixed (3m, 6m)/Solid (12m, 18m)"), times = 4)

data_feed <- data.frame(Month = month, Type = feedType, Value = allFeed)

ggplot(data_feed, aes(x = Month, y = Value, fill = Type)) +
geom_bar(stat = "identity", position = "stack") +  labs(x = "Month", y = "Number of Children", title = "Feeding Types by Month")



##-----Gestational weeks
metadataGAKnown <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header = TRUE)
data_gaKnown <- dplyr::filter(metadataGAKnown, ga_known == 1)

gwWeeks_mean <- mean(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_sd <- sd(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_median <- median(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_max <- max (data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_min <- min (data_gaKnown$ga_weeks, na.rm = TRUE)

print(gwWeeks_mean)
print(gwWeeks_sd)
print(gwWeeks_median)
print(gwWeeks_max)
print(gwWeeks_min)


#-----heatmap/barcode plot

p_hiv <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = mom_hiv_status)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Negative" = "blue", "Positive" = "red")) +
  theme_void() +
  labs(title = "Maternal HIV")

p_mat_edu <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = mat_edu_years)) +
  geom_tile(height = 1) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(title = "Maternal Education (years)")

p_gest_weeks <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = gest_weeks)) +
  geom_tile(height = 1) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(title = "Gestational Time (weeks)")

p_delivery <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = delivery_mode)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Vaginal" = "darkgreen", "Cesarean" = "lightyellow")) +
  theme_void() +
  labs(title = "Delivery Mode")

p_sex <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = child_sex)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("F" = "orange", "M" = "lightblue")) +
  theme_void() +
  labs(title = "Child Sex")

#add for feeding, diff colors for breast, formula, other

p_3mfeed <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = feed_3m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Breast" = "pink", "Formula" = "brown", "Mixed" = "violet")) +
  theme_void() +
  labs(title = "Feeding Types 3m")

p_6mfeed <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = feed_6m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Breast" = "pink", "Formula" = "brown", "Mixed" = "violet")) +
  theme_void() +
  labs(title = "Feeding Types 6m")

p_12mfeed <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = feed_12m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Breast" = "pink", "Formula" = "brown", "Solid" = "cyan")) +
  theme_void() +
  labs(title = "Feeding Types 12m")

p_18mfeed <- ggplot(processed_metadata, aes(x = master_idx, y = 1, fill = feed_18m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Breast" = "pink", "Formula" = "brown", "Solid" = "cyan")) +
  theme_void() +
  labs(title = "Feeding Types 18m")


grid.arrange(p_hiv, p_mat_edu, p_delivery, p_sex, p_gest_weeks, nrow = 5)

grid.arrange(p_hiv, p_3mfeed, p_6mfeed, p_12mfeed, p_18mfeed, nrow = 5)

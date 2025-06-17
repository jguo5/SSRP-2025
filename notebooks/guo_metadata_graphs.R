##Jackleen Guo
##SSRP Metadata Graphs
## 6-16-2025

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

## Processing the Metadata

processed_metadata <- metadata %>%
  select(
    "subject_id",
    "child_sex",
    "delivery_6m",
    "medhx_mom___1_selfreport",
    "mat_edu_years"
  ) %>%
  rename(
    delivery_mode = delivery_6m,
    mom_hiv_status = medhx_mom___1_selfreport
  ) %>%
  mutate(child_sex = factor(child_sex, ordered = FALSE, levels = c(0,1), labels = c("F", "M"))) %>%
  mutate(delivery_mode = factor(delivery_mode, ordered = FALSE, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))) %>%
  mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
  arrange(mom_hiv_status) %>%
  mutate( . , master_idx = 1:nrow(.))

##-----mom education
edu_mean <- mean(metadata$mat_edu_years, na.rm = TRUE)
edu_sd <- sd(metadata$mat_edu_years, na.rm = TRUE)
edu_median <- median(metadata$mat_edu_years, na.rm = TRUE)
edu_min <- min (metadata$mat_edu_years, na.rm = TRUE)
edu_max <- max (metadata$mat_edu_years, na.rm = TRUE)

print(paste0("Maternal education MEAN: ", edu_mean))
print(edu_sd)
print(edu_median)
print(edu_min)
print(edu_max)

table(metadata$mom_edu_en) #mom edu level



##-----birth weight (grams)
bw_mean <- mean(metadata$bw, na.rm = TRUE)
bw_sd <- sd(metadata$bw, na.rm = TRUE)
bw_median <- median(metadata$bw, na.rm = TRUE)

bw_min <- min (metadata$bw, na.rm = TRUE)
bw_max <- max (metadata$bw, na.rm = TRUE)

print(bw_mean)
print(bw_sd)
print(bw_median)
print(bw_min)
bw_max



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

#HIV barcode plot
hiv_vector <- metadata$baby_hiv_selfreport
hiv_vector <- hiv_vector[!is.na(hiv_vector)]
hiv_factor <- factor(hiv_vector, levels = c(0, 1, 555), labels = c("Yes", "No", "Unknown"))

dfhiv <- data.frame(id = seq_along(hiv_factor), Mode = hiv_factor)

ggplot(dfhiv, aes(x = id, y = 1, fill = Mode)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Yes" = "pink", "No" = "red", "Unknown" = "purple")) +
  theme_void() +
  labs(title = "HIV Diagnosis")



##-----child sex
#table
table(metadata$child_sex)

#pie chart
metadata$child_sex <- factor(metadata$child_sex, levels = c(0,1))

plot_df <- metadata %>%
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
    label = paste0(ifelse(child_sex == 0, "Female: ", "Male: "), N, " (", round(proportion * 100, 2), "%)") #text on slice
    )

p <- ggplot(plot_df, aes(y = proportion, fill = child_sex)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1, y = pos, label = label),
    size = 4,
    color = "black") +
  scale_fill_discrete(labels = c("Female", "Male")) +
  theme_void() +
  labs(title = "Child Sex", fill = "Sex") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

##-----delivery type
#table
table(metadata$delivery_6m)

#pie chart
metadata$delivery_6m <- factor(metadata$delivery_6m, levels = c(0,1))

plot_df <- metadata %>%
  group_by(delivery_6m) %>%
  summarise(N = n()) %>%
  na.omit() %>%     #omit na values (65...)
  mutate(
    proportion = N / sum(N), #from plot_df$proportion <- plot_df$N  / sum(plot_df$N)
  ) %>%
  arrange(desc(delivery_6m)) %>% #arrange df in desc order (1->0) to match graph w. labels
  mutate(
    csum = cumsum(proportion), #top of each slice
    pos = csum - proportion / 2, #where label goes
    label = paste0(ifelse(delivery_6m == 0, "Vaginal: ", "Cesearan: "), N, " (", round(proportion * 100, 2), "%)") #text on slice
  )

p <- ggplot(plot_df, aes(y = proportion, fill = delivery_6m)) +
  geom_bar(aes(x = "", y = proportion), stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(x = 1.06, y = pos, label = label),
    size = 4,
    color = "black") +
  scale_fill_discrete(labels = c("Vaginal", "Cesearan")) +
  theme_void() +
  labs(title = "Delivery Types", fill = "Delivery Type") +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

#barcode plot

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

grid.arrange(p_hiv, p_mat_edu, p_delivery, p_sex, nrow = 4)

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
#table
metadataGAKnown <- read.csv("C:\\Users\\iwuba\\Downloads\\VKCLab\\2025-03-07-KhulaDataRequest_SA_ClinicalMdata - Sheet1.csv", header=TRUE)
data_gaKnown <- dplyr::filter(metadataGAKnown, ga_known == 1)

gwWeeks_mean <- mean(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_sd <- sd(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_median <- median(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_max <- max (data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_min <- min (data_gaKnown$ga_weeks, na.rm = TRUE)

gwWeeks_mean
gwWeeks_sd
gwWeeks_median
gwWeeks_max
gwWeeks_min

#barcode plot
gest_vector <- data_gaKnown$ga_weeks
gest_vector <- gest_vector[!is.na(gest_vector)]

sorted_gest <- sort(gest_vector)

dfgest <- data.frame(Weeks = sorted_gest)
dfgest$id <- seq_along(dfgest$Weeks)


ggplot(dfgest, aes(x = id, y = 1, fill = Weeks)) +
  geom_raster() + scale_fill_gradientn(colours = c("red", "darkorange", "orange", "yellow", "lightgreen", "green"), name = "Weeks") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Gestational Weeks")



##-----setup and attach data
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)
set.seed(1234)

metadata <- read.csv("C:\\Users\\iwuba\\Downloads\\VKCLab\\2025-03-07-KhulaDataRequest_SA_ClinicalMdata - Sheet1.csv", header=TRUE)
attach(metadata)
head(metadata, 3)



##-----mom education
edu_mean <- mean(metadata$mat_edu_years, na.rm = TRUE)
edu_sd <- sd(metadata$mat_edu_years, na.rm = TRUE)
edu_median <- median(metadata$mat_edu_years, na.rm = TRUE)
edu_min <- min (metadata$mat_edu_years, na.rm = TRUE)
edu_max <- max (metadata$mat_edu_years, na.rm = TRUE)

edu_mean
edu_sd 
edu_median
edu_min
edu_max

table(metadata$mom_edu_en) #mom edu level



##-----birth weight (grams)
bw_mean <- mean(metadata$bw, na.rm = TRUE)
bw_sd <- sd(metadata$bw, na.rm = TRUE)
bw_median <- median(metadata$bw, na.rm = TRUE)

bw_min <- min (metadata$bw, na.rm = TRUE)
bw_max <- max (metadata$bw, na.rm = TRUE)

bw_mean
bw_sd
bw_median
bw_min
bw_max



##-----ARV status
#baby
arv_baby = table(metadata$baby_arv_selfreport)
arv_baby
arv_df = as.data.frame(arv_baby)
arv_df
#mom
arv_mom = table(metadata$mom_arv_selfreport)
arv_mom
arv_dfm = as.data.frame(arv_mom)
arv_dfm



##-----HIV status
#baby
table(metadata$baby_hiv_selfreport)
#mom
table(metadata$hiv_mom_diagnosed_selfreport)

#HIV barcode plot
hiv_vector <- metadata$baby_hiv_selfreport
hiv_vector = hiv_vector[!is.na(hiv_vector)]
hiv_factor = factor(hiv_vector, levels = c(0, 1, 555), labels = c("Yes", "No", "Unknown"))

dfhiv = data.frame(id = seq_along(hiv_factor), Mode = hiv_factor)

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

#barcode plot
sex_vector = metadata$child_sex
sex_vector = sex_vector[!is.na(sex_vector)]
sex_factor = factor(sex_vector, levels = c(0, 1), labels = c("Female", "Male"))

df = data.frame(id = seq_along(sex_factor), sex = sex_factor)

ggplot(df, aes(x = id, y = 1, fill = sex)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Female" = "orange", "Male" = "lightblue")) +
  theme_void() +
  labs(title = "Child Sex")



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
delivery_vector = metadata$delivery_6m
delivery_vector = delivery_vector[!is.na(delivery_vector)]
delivery_factor = factor(delivery_vector, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))

dfdeli = data.frame(id = seq_along(delivery_factor), Mode = delivery_factor)

ggplot(dfdeli, aes(x = id, y = 1, fill = Mode)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = c("Vaginal" = "darkgreen", "Cesarean" = "lightyellow")) +
  theme_void() +
  labs(title = "Delivery Mode")


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

##-----Feeding graph
# data3m = c(123, 85, 81)
# data6m = c(97, 135, 56)
# data12m = c(114, 176, 264)
# data18m = c(75, 125, 209)

# allFeed <- c(data3m, data6m, data12m, data18m)
# 
# 
# month <- rep(c("3m", "6m", "12m", "18m"), each = 3)
# 
# feedType <- rep(c("Breast", "Formula", "Mixed (3m, 6m)/Solid (12m, 18m)"), times = 4)
# 
# data_feed <- data.frame(Month = month, Type = feedType, Value = allFeed)
# 
# ggplot(data_feed, aes(x = Month, y = Value, fill = Type)) +
#   geom_bar(stat = "identity", position = "stack") +  labs(x = "Month", y = "Number of Children", title = "Feeding Types by Month")


##-----Gestational weeks
#table
metadataGAKnown = read.csv("C:\\Users\\iwuba\\Downloads\\VKCLab\\2025-03-07-KhulaDataRequest_SA_ClinicalMdata - Sheet1.csv", header=TRUE)
data_gaKnown = dplyr::filter(metadataGAKnown, ga_known == 1)

gwWeeks_mean = mean(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_sd = sd(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_median = median(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_max = max (data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_min = min (data_gaKnown$ga_weeks, na.rm = TRUE)

gwWeeks_mean
gwWeeks_sd
gwWeeks_median
gwWeeks_max
gwWeeks_min

#barcode plot
gest_vector = data_gaKnown$ga_weeks
gest_vector = gest_vector[!is.na(gest_vector)]

sorted_gest = sort(gest_vector)

dfgest = data.frame(Weeks = sorted_gest)
dfgest$id = seq_along(dfgest$Weeks) 


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



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
library(patchwork)
set.seed(1234)

metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
attach(metadata)
head(metadata, 3)

##-----Processing metadata

processed_metadata <- metadata %>%
  select(
    "subject_id",
    "zymo_code_3m",
    "zymo_code_6m",
    "zymo_code_12m",
    "zymo_code_18m",
    "zymo_code_24m",
    
    "subject_id",
    "child_sex",
    "delivery_6m",
    "medhx_mom___1_selfreport",
    "mat_edu_years",
    "ga_weeks",
    "solids_age_6m",
    
    "feeding_type_3m",
    "feeding_type_6m",
    "feeding_type_12m___1",
    "feeding_type_12m___2",
    "feeding_type_12m___3",
    "feeding_type_18m___1",
    "feeding_type_18m___2",
    "feeding_type_18m___3"
  )%>%
  
  filter(
    !(zymo_code_3m == "" &
        zymo_code_6m == "" &
        zymo_code_12m == "" &
        zymo_code_18m == "" &
        zymo_code_24m == "")
  )%>%
  
  rename(
    delivery_mode = delivery_6m,
    mom_hiv_status = medhx_mom___1_selfreport,
    gest_weeks = ga_weeks,
    feeding_3m = feeding_type_3m,
    feeding_6m = feeding_type_6m,
    solids_age = solids_age_6m
  ) %>%
  
  mutate(child_sex = factor(child_sex, ordered = FALSE, levels = c(0,1), labels = c("F", "M"))) %>%
  mutate(delivery_mode = factor(delivery_mode, ordered = FALSE, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))) %>%
  mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
  arrange(mom_hiv_status) %>%
  
  mutate(
    feeding_12m = if_else(feeding_type_12m___3 == 1, 4,  # Solid
                  if_else(feeding_type_12m___1 == 1, 1,  # Breast
                  if_else(feeding_type_12m___2 == 1, 2,  # Formula
                                          0))), #N/A
    feeding_18m = if_else(feeding_type_18m___3 == 1, 4,
                  if_else(feeding_type_18m___1 == 1, 1,
                  if_else(feeding_type_18m___2 == 1, 2,
                                          0))), #N/A
    
    feeding_3m  = ifelse(solids_age <= 3, 4, feeding_3m),
    feeding_6m  = ifelse(solids_age > 3 & solids_age <= 6, 4, feeding_6m),
    feeding_12m = ifelse(solids_age > 6 & solids_age <= 12, 4, (feeding_12m)),
    feeding_18m = ifelse(solids_age > 12 & solids_age <= 18, 4, (feeding_18m))
  ) %>%
  
  mutate(
    feeding_3m  = factor(feeding_3m,  levels = c(1, 2, 3, 4), labels = c("Breast", "Formula", "Mixed", "Solid")),
    feeding_6m  = factor(feeding_6m,  levels = c(1, 2, 3, 4), labels = c("Breast", "Formula", "Mixed", "Solid")),
    feeding_12m = factor(feeding_12m, levels = c(1, 2, 3, 4), labels = c("Breast", "Formula", "Mixed", "Solid")),
    feeding_18m = factor(feeding_18m, levels = c(1, 2, 3, 4), labels = c("Breast", "Formula", "Mixed", "Solid"))
  ) %>%
  
  mutate( . , master_idx = 1:nrow(.))
  
  
  # mutate(
  #   feeding_3m = case_when(
  #     feeding_3m == 1 ~ "Breast",
  #     feeding_3m == 2 ~ "Formula",
  #     feeding_3m == 3 ~ "Mixed",
  #     feeding_3m == 4 ~ "Solid"
  #   ),
  #   feeding_6m = case_when(
  #     feeding_6m == 1 ~ "Breast",
  #     feeding_6m == 2 ~ "Formula",
  #     feeding_6m == 3 ~ "Mixed",
  #     feeding_3m == 4 ~ "Solid"
  #   )
  # ) %>%
  # 
  # mutate(
  #   feeding_3m  = ifelse(solids_age <= 3, 4, feeding_3m),
  #   feeding_6m  = ifelse(solids_age > 3 & solids_age <= 6, 4, feeding_6m),
  #   feeding_12m = ifelse(solids_age > 6 & solids_age <= 12, 4, feeding_12m),
  #   feeding_18m = ifelse(solids_age > 12, 4, feeding_18m)
  # ) %>%
 
  
  
  # mutate(
  #   feeding_3m = factor(feeding_3m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed")),
  #   feeding_6m = factor(feeding_6m, ordered = FALSE, levels = c(1, 2, 3), labels = c("Breast", "Formula", "Mixed")),
  #   feeding_12m = factor(feeding_12m, ordered = FALSE, levels = c("Breast", "Formula", "Solid", "N/A")),
  #   feeding_18m = factor(feeding_18m, ordered = FALSE, levels = c("Breast", "Formula", "Solid", "N/A"))
  # ) %>%
#   feeding_12m = if_else(feeding_type_12m___3 == 1, "Solid",
#                         if_else(feeding_type_12m___1 == 1, "Breast",
#                                 if_else(feeding_type_12m___2 == 1, "Formula", "N/A"))),
# 
# feeding_18m = if_else(feeding_type_18m___3 == 1, "Solid",
#                       if_else(feeding_type_18m___1 == 1, "Breast",
#                               if_else(feeding_type_18m___2 == 1, "Formula", "N/A"))),
#     
 


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

metadata_POS <- processed_metadata %>%
  filter(mom_hiv_status == "Positive") 

metadata_NEG <- processed_metadata %>%
  filter(mom_hiv_status == "Negative")

edu_metadata_POS <- metadata_POS$mat_edu_years
summary(edu_metadata_POS)
sd_edu_metadata_POS = sd(edu_metadata_POS)
sd_edu_metadata_POS

edu_metadata_NEG <- metadata_NEG$mat_edu_years
summary(edu_metadata_NEG)
sd_edu_metadata_NEG = sd(edu_metadata_NEG)
sd_edu_metadata_NEG



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
print("Child ARV Table: ")
print(arv_df)
#mom
arv_mom <- table(metadata$mom_arv_selfreport)
print(arv_mom)
arv_dfm <- as.data.frame(arv_mom)
print("Maternal ARV Table: ")
print(arv_dfm)



##-----HIV status
#baby
hiv_baby <- table(metadata$baby_hiv_selfreport)
print("Child HIV Table: ")
print(hiv_baby)
#mom
print("Maternal HIV Table: ")
table(metadata$hiv_mom_diagnosed_selfreport)



##-----child sex
#table
print("Child Sex Table: ")
table(metadata$child_sex)

#pie chart
#metadata$child_sex <- factor(metadata$child_sex, levels = c(0,1), labels = c("Female", "Male"))

plot_df <- processed_metadata %>%
  group_by(child_sex) %>%
  summarise(N = n()) %>%
  #na.omit() %>%     #omit na values (65...)
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
  scale_fill_manual(values = c(F = "orange", M = "lightblue")) +
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

p <- ggplot(plot_df, aes(y = proportion, fill = delivery_mode)) +
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

print(p) 




##-----feeding

# feeding_3m <- table(metadata$feeding_type_3m)
# 
# feeding_6m <- table(metadata$feeding_type_6m)
# 
# feeding_12m1 <- table(metadata$feeding_type_12m___1)
# feeding_12m2 <- table(metadata$feeding_type_12m___2)
# feeding_12m3 <- table(metadata$feeding_type_12m___3)
# 
# feeding_18m1 <- table(metadata$feeding_type_18m___1)
# feeding_18m2 <- table(metadata$feeding_type_18m___2)
# feeding_18m3 <- table(metadata$feeding_type_18m___3)
# 
# feeding_3m
# 
# feeding_6m
# 
# feeding_12m1
# feeding_12m2
# feeding_12m3
# 
# feeding_18m1
# feeding_18m2
# feeding_18m3

#Feeding graph
# data3m <- c(feeding_3m[1], feeding_3m[2], feeding_3m[3])
# data6m <- c(feeding_6m[1], feeding_6m[2], feeding_6m[3])
# 
# data3m <- data.frame(
#   Month = "3m",
#   Type = c("Breast", "Formula", "Mixed"),
#   Value = c(feeding_3m[1], feeding_3m[2], feeding_3m[3])
# )
# 
# data6m <- data.frame(
#   Month = "6m",
#   Type = c("Breast", "Formula", "Mixed"),
#   Value = c(feeding_6m[1], feeding_6m[2], feeding_6m[3])
# )
# 
# data3m <- data3m %>% filter(!is.na(Value))
# data6m <- data6m %>% filter(!is.na(Value))
# 
# data12m <- processed_metadata %>%
#   mutate(feed_12m = case_when(
#     feeding_type_12m___3 == 1 ~ "Solid",
#     feeding_type_12m___1 == 1 ~ "Breast",
#     feeding_type_12m___2 == 1 ~ "Formula",
#     TRUE ~ "N/A"
#   )) %>%
#   filter(feed_12m != "N/A") %>% 
#   count(feed_12m, name = "Value") %>%
#   mutate(Month = "12m", Type = feed_12m) %>%
#   select(Month, Type, Value)
# 
# data18m <- processed_metadata %>%
#   mutate(feed_18m = case_when(
#     feeding_type_18m___3 == 1 ~ "Solid",
#     feeding_type_18m___1 == 1 ~ "Breast",
#     feeding_type_18m___2 == 1 ~ "Formula",
#     TRUE ~ "N/A"
#   )) %>%
#   filter(feed_18m != "N/A") %>% 
#   count(feed_18m, name = "Value") %>%
#   mutate(Month = "18m", Type = feed_18m) %>%
#   select(Month, Type, Value)
# 
# Month <- c(data3m$Month, data6m$Month, data12m$Month, data18m$Month)
# Type <- c(data3m$Type, data6m$Type, data12m$Type, data18m$Type)
# Value <- c(data3m$Value, data6m$Value, data12m$Value, data18m$Value)
# 
# data_feed <- data.frame(Month = Month, Type = Type, Value = Value)
# data_feed$Month <- factor(data_feed$Month, levels = c("3m", "6m", "12m", "18m"))
# 
# ggplot(data_feed, aes(x = Month, y = Value, fill = Type)) +
# geom_bar(stat = "identity", position = "stack") + 
#   scale_fill_manual (values=c(
#     "Breast" = "pink",
#     "Formula" = "brown",
#     "Mixed" = "violet",
#     "Solid" = "cyan"
#   )) +
#   labs(x = "Month", y = "Number of Children", title = "Feeding Types by Month")
# 
# 

##-----Gestational weeks
metadataGAKnown <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header = TRUE)
data_gaKnown <- dplyr::filter(metadataGAKnown, ga_known == 1)

gwWeeks_mean <- mean(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_sd <- sd(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_median <- median(data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_min <- min (data_gaKnown$ga_weeks, na.rm = TRUE)
gwWeeks_max <- max (data_gaKnown$ga_weeks, na.rm = TRUE)

print(paste0("Gestational mean: ", gwWeeks_mean))
print(paste0("Gestational sd: ", gwWeeks_sd))
print(paste0("Gestational median:", gwWeeks_median))
print(paste0("Gestational min:", gwWeeks_min))
print(paste0("Gestational max:", gwWeeks_max))

gestw_metadata_POS <- metadata_POS$gest_weeks
summary(gestw_metadata_POS)
sd_gestw_metadata_POS = sd(gestw_metadata_POS, na.rm = TRUE)
sd_gestw_metadata_POS

gestw_metadata_NEG <- metadata_NEG$gest_weeks
summary(gestw_metadata_NEG)
sd_gestw_metadata_NEG = sd(gestw_metadata_NEG, na.rm = TRUE)
sd_gestw_metadata_NEG


#-----heatmap/barcode plot

func_barcode <- function(data, x, fill, scale_fill, title) {
  ggplot(data, aes(x = {{x}}, fill = {{fill}})) +
    geom_tile(aes(y = 1), height = 1) + 
    {{scale_fill}} +
    theme_void() +
    labs(title = title)
}

p_hiv <- func_barcode(processed_metadata, master_idx, mom_hiv_status, 
                      scale_fill_manual(values = c("Negative" = "#73baf3", "Positive" = "#f37373")),
                      "Maternal HIV Status")

p_mat_edu <- func_barcode(processed_metadata, master_idx, mat_edu_years, 
                          scale_fill_viridis_c(option = "rocket"), "Maternal Education (years)")

p_gest_weeks <- func_barcode(processed_metadata, master_idx, gest_weeks, 
                             scale_fill_viridis_c(), "Gestational Time (weeks)")

p_delivery <- func_barcode(processed_metadata, master_idx, delivery_mode, 
                           scale_fill_manual(values = c("Vaginal" = "#9df373", "Cesarean" = "#c1acf5")),
                           "Delivery Mode")

p_sex <- func_barcode(processed_metadata, master_idx, child_sex, 
                           scale_fill_manual(values = c("F" = "orange", "M" = "lightblue")),
                           "Child Sex")


#add for feeding, diff colors for breast, formula, other
feeding_colors = c("Breast"="pink", "Formula"="brown", 
                   "Mixed"="violet", "Solid"="cyan", "N/A"="gray")

p_3mfeed <- func_barcode(processed_metadata, master_idx, feeding_3m, 
                         scale_fill_manual(values = feeding_colors), "Feeding Types 3m")

p_6mfeed <- func_barcode(processed_metadata, master_idx, feeding_6m, 
                         scale_fill_manual(values = feeding_colors), "Feeding Types 6m")

p_12mfeed <- func_barcode(processed_metadata, master_idx, feeding_12m, 
                          scale_fill_manual(values = feeding_colors, drop = FALSE),
                          "Feeding Types 12m")

p_18mfeed <- func_barcode(processed_metadata, master_idx, feeding_18m, 
                          scale_fill_manual(values = feeding_colors, drop=FALSE), 
                          "Feeding Types 18m")

#plot for positive maternal HIV

#sort by hiv and then sort by feed type
pos_metadata_feed3m <- processed_metadata %>%
  filter(mom_hiv_status == "Positive") %>%
  arrange(feeding_3m) %>%
  mutate(master_idx = row_number())

pos_metadata_feed6m <- processed_metadata %>%
  filter(mom_hiv_status == "Positive") %>%
  arrange(feeding_6m) %>%
  mutate(master_idx = row_number())

pos_metadata_feed12m <- processed_metadata %>%
  filter(mom_hiv_status == "Positive") %>%
  arrange(feeding_12m) %>%
  mutate(master_idx = row_number())

pos_metadata_feed18m <- processed_metadata %>%
  filter(mom_hiv_status == "Positive") %>%
  arrange(feeding_18m) %>%
  mutate(master_idx = row_number())
  
p_feed3mPOS <- func_barcode(pos_metadata_feed3m, master_idx, feeding_3m, 
                            scale_fill_manual(values = feeding_colors), 
                            "Feeding Types 3m Positive HIV")

p_feed6mPOS <- func_barcode(pos_metadata_feed6m, master_idx, feeding_6m, 
                            scale_fill_manual(values = feeding_colors), 
                            "Feeding Types 6m Positive HIV")

p_feed12mPOS <- func_barcode(pos_metadata_feed12m, master_idx, feeding_12m, 
                             scale_fill_manual(values = feeding_colors), 
                             "Feeding Types 12m Positive HIV")

p_feed18mPOS <- func_barcode(pos_metadata_feed18m, master_idx, feeding_18m, 
                             scale_fill_manual(values = feeding_colors), 
                             "Feeding Types 18m Positive HIV")


#plot for negative maternal HIV
neg_metadata_feed3m <- processed_metadata %>%
  filter(mom_hiv_status == "Negative") %>%
  arrange(feeding_3m) %>%
  mutate(master_idx = row_number())

neg_metadata_feed6m <- processed_metadata %>%
  filter(mom_hiv_status == "Negative") %>%
  arrange(feeding_6m) %>%
  mutate(master_idx = row_number())

neg_metadata_feed12m <- processed_metadata %>%
  filter(mom_hiv_status == "Negative") %>%
  arrange(feeding_12m) %>%
  mutate(master_idx = row_number())

neg_metadata_feed18m <- processed_metadata %>%
  filter(mom_hiv_status == "Negative") %>%
  arrange(feeding_18m) %>%
  mutate(master_idx = row_number())

p_feed3mNEG <- ggplot(neg_metadata_feed3m, 
                      aes(x = master_idx, y = 1, fill = feeding_3m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = feeding_colors) +
  theme_void() +
  labs(title = "Feeding Types 3m Negative HIV")

p_feed6mNEG <- ggplot(neg_metadata_feed6m, 
                      aes(x = master_idx, y = 1, fill = feeding_6m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = feeding_colors) +
  theme_void() +
  labs(title = "Feeding Types 6m Negative HIV")

p_feed12mNEG <- ggplot(neg_metadata_feed12m, 
                       aes(x = master_idx, y = 1, fill = feeding_12m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = feeding_colors) +
  theme_void() +
  labs(title = "Feeding Types 12m Negative HIV")

p_feed18mNEG <- ggplot(neg_metadata_feed18m, 
                       aes(x = master_idx, y = 1, fill = feeding_18m)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = feeding_colors) +
  theme_void() +
  labs(title = "Feeding Types 18m Negative HIV")


#aaaaa
# 
# data_gaKnown = dplyr::filter(metadata, ga_known == 1)
# 
# gest_vector = data_gaKnown$ga_weeks
# gest_vector = gest_vector[!is.na(gest_vector)]
# 
# sorted_gest = sort(gest_vector)
# 
# dfgest = data.frame(Weeks = sorted_gest)
# dfgest$id = seq_along(dfgest$Weeks) 
# 
# 
# ggplot(dfgest, aes(x = id, y = 1, fill = Weeks)) +
#   geom_raster() + scale_fill_gradient(colours = c("red", "darkorange", "orange", "yellow", "lightgreen", "green"), name = "Weeks") + 
#   theme_minimal() +
#   theme(
#     axis.title = element_blank(),
#     axis.text = element_blank(),
#     axis.ticks = element_blank(),
#     panel.grid = element_blank()
#   ) +
#   labs(title = "Gestational Weeks")

(p_feed3mPOS /
    p_feed3mNEG /
    p_feed6mPOS /
    p_feed6mNEG /
    p_feed12mPOS /
    p_feed12mNEG /
    p_feed18mPOS /
    p_feed18mNEG
) + plot_layout(heights = rep(1, 8))

(p_hiv/
    p_mat_edu/
    p_delivery/
    p_sex/
    p_gest_weeks
) + plot_layout(heights = rep(1, 5))

(p_hiv/
    p_3mfeed/
    p_6mfeed/
    p_12mfeed/
    p_18mfeed
) + plot_layout(heights = rep(1, 5))



##-----Data cleaning... again!

#check if has zymo code --true--> remove

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
  
#write.csv(naonly_metadata, (fs::path(here::here(), "ext", "na-zymo_only_2025-03-07-KhulaSA_ClinicalMdata.csv")), row.names = TRUE)


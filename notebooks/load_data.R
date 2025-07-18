#####
# Notebook to parse metadata for SSRP HIV project
#####
load_metadata <- function() {
  
  ## 1. Load the master metadata table
  master_metadata <- read.csv(fs::path(here::here(), "ext", "2025-03-07-KhulaSA_ClinicalMdata.csv"), header=TRUE)
  attach(master_metadata)
  
  head(master_metadata, 3)
  summary(master_metadata)
  
  ##-----Processing metadata
  
  ## Gathering only the relevant columns
  processed_subject_metadata <- master_metadata %>%
    select(
      "subject_id",                       ## Primary key - SUbuject ID
      "zymo_code_3m",                     ## Biospecimen ID at TP 3m
      "zymo_code_6m",                     ## Biospecimen ID at TP 6m
      "zymo_code_12m",                    ## Biospecimen ID at TP 12m
      "zymo_code_18m",                    ## Biospecimen ID at TP 18m
      "zymo_code_24m",                    ## Biospecimen ID at TP 24m
      "zymo_child_agemonths_3m",          ## Biospecimen collection age (weeks) TP 3m
      "zymo_child_agemonths_6m",          ## Biospecimen collection age (weeks) TP 6m
      "zymo_child_agemonths_12m",         ## Biospecimen collection age (weeks) TP 12m
      "zymo_child_agemonths_18m",         ## Biospecimen collection age (weeks) TP 18m
      "zymo_child_agemonths_24m",         ## Biospecimen collection age (weeks) TP 24m
      "child_sex",                        ## Child sex (M/F)
      "delivery_6m",                      ## Delivery mode (questionnaire @ 6m)
      "medhx_mom___1_selfreport",         ## Maternal HIV (self-reported)
      "mat_edu_years",                    ## Years of formal education (maternal)
      "ga_weeks",                         ## Gestetional age (weeks)
      "solids_age_6m",                    ## Age when solid diet was introduced
      "feeding_type_3m",                  ## Feeding practice @ 3m (1 = BF, 2 = FF, 3 = MX)
      "feeding_type_6m"# ,                  ## Feeding practice @ 6m (1 = BF, 2 = FF, 3 = MX)
      # "feeding_type_12m___1",             ## Feeding practice @ 3m BF (0 = false, 1 = true)
      # "feeding_type_12m___2",             ## Feeding practice @ 3m FF (0 = false, 1 = true)
      # "feeding_type_12m___3",             ## Feeding practice @ 3m SOLIDS (0 = false, 1 = true)
      # "feeding_type_18m___1",             ## Feeding practice @ 3m BF (0 = false, 1 = true)
      # "feeding_type_18m___2",             ## Feeding practice @ 3m FF (0 = false, 1 = true)
      # "feeding_type_18m___3"              ## Feeding practice @ 3m SOLIDS (0 = false, 1 = true)
    )
  
  ## Filtering out subjects without any stool samples
  processed_subject_metadata <- processed_subject_metadata %>%
    filter(
      !(zymo_code_3m == "" &
          zymo_code_6m == "" &
          zymo_code_12m == "" &
          zymo_code_18m == "" &
          zymo_code_24m == "")
    )
  
  ## Dealing with feeding for 3m and 6m
  
  for (i in 1:nrow(processed_subject_metadata)) {
    
    if (!is.na(processed_subject_metadata[i, "zymo_child_agemonths_3m"]) & !is.na(processed_subject_metadata[i, "solids_age_6m"]) ) {
      if ( processed_subject_metadata[i, "zymo_child_agemonths_3m"] >  processed_subject_metadata[i, "solids_age_6m"] * (7/30.5) ) {
        processed_subject_metadata[i, "feeding_type_3m"] = 4
        processed_subject_metadata[i, "feeding_type_6m"] = 4
      }
    }
    
    if (!is.na(processed_subject_metadata[i, "zymo_child_agemonths_6m"]) & !is.na(processed_subject_metadata[i, "solids_age_6m"]) ) {
      if (processed_subject_metadata[i, "zymo_child_agemonths_6m"] >  processed_subject_metadata[i, "solids_age_6m"] * (7/30.5) ) {
        processed_subject_metadata[i, "feeding_type_6m"] = 4
      }
    }
    
  }
  
  processed_subject_metadata$feeding_type_3m <- recode(
    processed_subject_metadata$feeding_type_3m,
    `1` = "BF",
    `2` = "FF",
    `3` = "MX",
    `4` = "SL"
  )
  
  
  processed_subject_metadata$feeding_type_6m <- recode(
    processed_subject_metadata$feeding_type_6m,
    `1` = "BF",
    `2` = "FF",
    `3` = "MX",
    `4` = "SL"
  )
  
  ## Renaming
  
  processed_subject_metadata <- processed_subject_metadata %>%
    rename(
      delivery_mode = delivery_6m,
      mom_hiv_status = medhx_mom___1_selfreport,
    ) %>%
    
    mutate(child_sex = factor(child_sex, ordered = FALSE, levels = c(0,1), labels = c("F", "M"))) %>%
    mutate(delivery_mode = factor(delivery_mode, ordered = FALSE, levels = c(0, 1), labels = c("Vaginal", "Cesarean"))) %>%
    mutate(mom_hiv_status = factor(mom_hiv_status, ordered = FALSE, levels = c(0, 1), labels = c("Negative", "Positive"))) %>%
    arrange(mom_hiv_status) %>%
    
    mutate( . , master_idx = 1:nrow(.))
  
  processed_sample_metadata <- processed_subject_metadata %>%
    pivot_longer(
      cols = matches("zymo_code_\\d+m|zymo_child_agemonths_\\d+m"),
      names_to = c(".value", "timepoint"),
      names_pattern = "(zymo_code|zymo_child_agemonths)_(\\d+m)"
    ) %>% na.omit(zymo_child_agemonths)
  
  ## Microbiome data
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
  
  return(
    list(
      "sample_metadata" = processed_sample_metadata,
      "subject_metadata" = processed_subject_metadata
      )
    )
    
}

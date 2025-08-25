rm(list=ls())

if (!require("devtools")) install.packages("devtools")
devtools::install_github("IMNMV/ClaudeR")
#puis appuyer sur 1 

# Installation des packages nécessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary",
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", 
  "survminer", "summarytools", "MatchIt", "optmatch", 
  "officer", "flextable", "gt", "mice", "googlesheets4", "cards"
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats",
  "parameters", "broom.helpers", "forestplot", "kableExtra", "rsconnect", "pacman", "stringr", "knitr", "purr", "lubridate"
))


# Chargement des librairies
pacman::p_load(
  cardx, dplyr, readxl, openxlsx, tidyverse, gtsummary, claudeR,
  magrittr, ggplot2, lubridate, ggpubr, survival, 
  survminer, summarytools, MatchIt, optmatch, 
  officer, flextable, gt, mice, googlesheets4, cards, stringr, purr, lubridate
  RItools, epiR, tableone, cobalt, broom, gridExtra,
  forcats, dlstats, pkgsearch, pROC, forcats,
  stats, parameters, broom.helpers, knitr, 
  forestplot, kableExtra, rsconnect, shiny, googlesheets4
)


import_and_clean_logbook_data <- function() {
  
  # Configuration des URLs Google Sheets
  sheets_urls <- list(
    Cochin = "https://docs.google.com/spreadsheets/d/1ZWEY6L2vRm6VHkOw_ytbFpaeiv6h_FRQOJCVl7c1t4k/edit?usp=sharing",
    Paul_Brousse = "https://docs.google.com/spreadsheets/d/176ze81vIL38_HdT3XVThSyLbBXr4ZLCYkdjPJxAh1HI/edit?usp=sharing",
    St_Louis = "https://docs.google.com/spreadsheets/d/1w52ZALvJ2uOKgn1bcaILuQ6j0A2W1_oFzmf0hxeSUNE/edit?usp=sharing",
    HEGP = "https://docs.google.com/spreadsheets/d/1gXd9f2ZID3VL5oTTQ0j_JxtYjk9fb9C2s1LmTBhvTJg/edit?usp=sharing",
    PSL = "https://docs.google.com/spreadsheets/d/1CAfPAdzhKSbARkMZagJE5gmLMwAJn5WN4N3dJgijedE/edit?gid=0#gid=0",
    Cochin2 = "https://docs.google.com/spreadsheets/d/1bd7WkoZrHbfW3AhFfZgxgzCjJ7cv0tDgHUZ7BIahjoE/edit?gid=0#gid=0"
  )
  
  # Colonnes standardisées
  colonnes_standard <- c(
    "DATE", "NOM_interne", "INTERVENTION", "Garde_Programme", "Ambu", 
    "OPERATEUR", "OPERATEUR_2", "RANG_BOSS", "RANG_INTERNE", "Geste", 
    "QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout", "Geste_whole_text", 
    "Geste_a_l_aise", "Si_pas_de_geste_RESSENTI", "AMBIANCE", "PEDAGOGIE", 
    "SELF_ESTIME_SORTIE", "Hôpital"
  )
  
  # Fonction helper pour lecture et harmonisation d'une sheet
  read_and_clean_sheet <- function(sheet_url, hospital_name) {
    # Lecture et ajout de l'hôpital
    data <- read_sheet(sheet_url) %>%
      mutate(Hôpital = hospital_name) %>%
      rename_with(~ str_replace_all(., " ", "_"))
    
    # Renommage conditionnel des colonnes
    rename_mapping <- c(
      "Geste?" = "Geste",
      "Garde_Programme" = "Garde_Programme",
      "Ambu" = "Ambu",
      "OPERATEUR" = "OPERATEUR",
      "OPERATEUR_2" = "OPERATEUR_2",
      "AMBIANCE" = "AMBIANCE",
      "PEDAGOGIE" = "PEDAGOGIE",
      "SELF_ESTIME_SORTIE" = "SELF_ESTIME_SORTIE",
      "Si_pas_de_geste_RESSENTI" = "Si_pas_de_geste_RESSENTI",
      "Geste_a_l_aise" = "Geste_a_l_aise"
    )
    
    for (old_name in names(rename_mapping)) {
      if (old_name %in% colnames(data)) {
        data <- data %>% rename(!!sym(rename_mapping[old_name]) := !!sym(old_name))
      }
    }
    
    # Suppression colonne spécifique et ajout colonnes manquantes
    data <- data %>%
      select(-any_of("IPP patient.e"))
    
    # Ajouter colonnes manquantes
    missing_cols <- setdiff(colonnes_standard, colnames(data))
    for (col in missing_cols) {
      data[[col]] <- NA_character_
    }
    
    # Réorganiser et convertir
    data <- data %>%
      select(all_of(colonnes_standard)) %>%
      mutate(across(everything(), as.character))
    
    return(data)
  }
  
  # Import et fusion des données de tous les hôpitaux
  df <- map2_dfr(sheets_urls, names(sheets_urls), read_and_clean_sheet)
  
  # Nettoyage et harmonisation (ordre exact de l'ancien script)
  df <- df %>%
    # 1. Nettoyage SELF_ESTIME_SORTIE seulement
    mutate(SELF_ESTIME_SORTIE = str_trim(SELF_ESTIME_SORTIE)) %>%
    
    # 2. Harmonisation PEDAGOGIE
    mutate(PEDAGOGIE = case_when(
      PEDAGOGIE == "1" ~ "1-rien",
      PEDAGOGIE == "2" ~ "2-quasi rien",
      PEDAGOGIE == "3" ~ "3-ok",
      PEDAGOGIE == "4" ~ "4-bien",
      PEDAGOGIE == "5" ~ "5-incroyable!!",
      TRUE ~ PEDAGOGIE
    )) %>%
    mutate(PEDAGOGIE = factor(PEDAGOGIE, 
                              levels = c("1-rien", "2-quasi rien", "3-ok", "4-bien", "5-incroyable!!"), 
                              ordered = TRUE)) %>%
    
    # 3. Nettoyage AMBIANCE séparément  
    mutate(AMBIANCE = str_trim(AMBIANCE)) %>%
    
    # 4. Harmonisation AMBIANCE (avec case_when explicite comme l'original)
    mutate(AMBIANCE = case_when(
      AMBIANCE == "1 - je veux partir" ~ "1 - je veux partir",
      AMBIANCE == "2 - c'est ok" ~ "2 - c'est ok",
      AMBIANCE == "3 - on recommence" ~ "3 - on recommence",
      TRUE ~ AMBIANCE
    )) %>%
    mutate(AMBIANCE = factor(AMBIANCE, 
                             levels = c("1 - je veux partir", "2 - c'est ok", "3 - on recommence"), 
                             ordered = TRUE))
  
  # 5. Modifications RANG (style original avec $)
  df$RANG_BOSS <- gsub("Dr Junior", "DJ", df$RANG_BOSS)
  df$RANG_INTERNE <- gsub("^1e aide$", "1er aide", df$RANG_INTERNE)
  
  # 6. Filtrage (style original avec crochets)
  df <- df[!(df$RANG_BOSS == "Interne" | df$RANG_INTERNE == "Spectateur non habillé"), ]
  
  # 7. Modification Garde_Programme (après filtrage)
  df <- df %>%
    mutate(Garde_Programme = case_when(
      Garde_Programme == "Astreinte" ~ "Garde",
      TRUE ~ Garde_Programme
    ))
  
  return(df)
}

#import de tout le df
df <- import_and_clean_logbook_data()


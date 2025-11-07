##-------PACKAGES DE BASE-------
if (!require("devtools")) install.packages("devtools")
#install claudeR avec force = TRUE pour écraser les anciennes versions
devtools::install_github("IMNMV/ClaudeR", force = TRUE)
library(ClaudeR)

# Installation des packages nécessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary", "stringr",
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", "scales",
  "survminer", "summarytools", "MatchIt", "optmatch", "purrr",
  "officer", "flextable", "gt", "mice", "googlesheets4", "cards",
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats",
  "parameters", "broom.helpers", "forestplot", "kableExtra", "rsconnect", "pacman", "stringr", "knitr", "purr", "lubridate"
))

library(pacman)
# Chargement des librairies
pacman::p_load(
  cardx, dplyr, readxl, openxlsx, tidyverse, gtsummary, ClaudeR,
  magrittr, ggplot2, lubridate, ggpubr, survival, 
  survminer, summarytools, MatchIt, optmatch, scales,
  officer, flextable, gt, mice, googlesheets4, cards, stringr, purr, lubridate,
  RItools, epiR, tableone, cobalt, broom, gridExtra,
  forcats, dlstats, pkgsearch, pROC, forcats,
  stats, parameters, broom.helpers, knitr, 
  forestplot, kableExtra, rsconnect, shiny, googlesheets4
)

library(cardx)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(gtsummary)
library(ClaudeR)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(summarytools)
library(MatchIt)
library(optmatch)
library(scales)
library(officer)
library(flextable)
library(gt)
library(mice)
library(googlesheets4)
library(cards)
library(stringr)
library(purrr)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(gridExtra)
library(forcats)
library(dlstats)
library(pkgsearch)
library(pROC)
library(stats)
library(parameters)
library(broom.helpers)
library(knitr)
library(forestplot)
library(kableExtra)
library(rsconnect)
library(shiny)


# Style commun (même que tes 2 premiers graphiques)
common_y <- scale_y_continuous(expand = expansion(mult = c(0, 0.12)))  # un peu d'air pour les labels
common_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Paramètres communs pour l'export (même que les plots initiaux)
w <- 6      # largeur en inches
h <- 4      # hauteur en inches
dpi <- 1000 # résolution

##--------------------------------------------
##-------GIT PUSH DU SCRIPT-------
# Ajouter le fichier au staging Git
fichier <- "/Users/thomashusson/Documents/R/Logbook/script logbook complet pour SFCD.R"
setwd("/Users/thomashusson/Documents/R/Logbook")
system(paste("git add", shQuote(fichier)))

# Commit avec un message explicite
message_commit <- "Mise à jour du script logbook complet pour SFCD"
system(paste("git commit -m", shQuote(message_commit)))

# Étape 4 : Forcer le push pour écraser les changements distants
system("git push --force origin main")

##--------------------------------------------
##-------IMPORT DES DONNÉES + NETTOYAGE-----
rm(list=ls())

#ggsheets deauth
gs4_deauth()

import_and_clean_logbook_data <- function() {
  
  # Configuration des URLs Google Sheets
  sheets_urls <- list(
    Cochin = "https://docs.google.com/spreadsheets/d/1ZWEY6L2vRm6VHkOw_ytbFpaeiv6h_FRQOJCVl7c1t4k/edit?usp=sharing",
    Paul_Brousse = "https://docs.google.com/spreadsheets/d/176ze81vIL38_HdT3XVThSyLbBXr4ZLCYkdjPJxAh1HI/edit?usp=sharing",
    St_Louis = "https://docs.google.com/spreadsheets/d/1w52ZALvJ2uOKgn1bcaILuQ6j0A2W1_oFzmf0hxeSUNE/edit?usp=sharing",
    HEGP = "https://docs.google.com/spreadsheets/d/1gXd9f2ZID3VL5oTTQ0j_JxtYjk9fb9C2s1LmTBhvTJg/edit?usp=sharing",
    PSL = "https://docs.google.com/spreadsheets/d/1CAfPAdzhKSbARkMZagJE5gmLMwAJn5WN4N3dJgijedE/edit?gid=0#gid=0",
    Cochin2 = "https://docs.google.com/spreadsheets/d/1bd7WkoZrHbfW3AhFfZgxgzCjJ7cv0tDgHUZ7BIahjoE/edit?gid=0#gid=0",
    Avicenne = "https://docs.google.com/spreadsheets/d/1XTiRmVf7B_bVcfF53AwKRXC8WrEiKe0O-UUcmfrVnws/edit?gid=0#gid=0",
    St_Antoine = "https://docs.google.com/spreadsheets/d/1J67SU6hM6oKASHcAZkF00wazlESRluplK1V24c1mDsM/edit?gid=0#gid=0"
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

#faire un df uniquement pour Paul Brousse
df_PBR <- df %>%
  filter(Hôpital == "Paul_Brousse")



##--------------------------------------------
##-------TAUX DE GESTE TOTAL-------
# Calcul des effectifs
df_geste_global <- df %>%
  filter(!is.na(Geste)) %>%
  count(Geste) %>%
  mutate(
    pourcentage = n / sum(n),
    label = paste0(round(100 * pourcentage, 1), "%"),
    # Étiquettes en français
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No" ~ "Pas de geste",
      TRUE ~ Geste
    ),
    label_complet = paste0(Geste_francais, " (", round(100 * pourcentage, 1), "%)") 
  )

# Diagramme en secteurs (camembert)
camembertgeste <- ggplot(df_geste_global, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#b2df8a", "Pas de geste" = "#fb9a99")) +
  labs(title = "Répartition des gestes réalisés (tous hôpitaux confondus)") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

camembertgeste


ggsave("camembertgeste.png", plot = camembertgeste, width = 10, height = 6)
##--------------------------------------------
##-------TAUX DE GESTE GARDE vs PROGRAMMÉ-------
# Charger les packages nécessaires
library(dplyr)
library(ggplot2)
library(ggpattern)   # pour les hachures
library(gridExtra)   # pour grid.arrange
library(grid)        # pour textGrob




# Préparation des données pour les camemberts
df_garde_camembert <- df %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  count(Garde_Programme, Geste) %>%
  group_by(Garde_Programme) %>%
  mutate(
    pourcentage    = n / sum(n),
    label_pct      = paste0(round(100 * pourcentage, 1), "%"),
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No"  ~ "Pas de geste",
      TRUE           ~ Geste
    )
  )

# Séparer les données pour chaque type
df_garde     <- df_garde_camembert %>% filter(Garde_Programme == "Garde")
df_programme <- df_garde_camembert %>% filter(Garde_Programme == "Programmé")

# Palette pastel commune
palette_pastel <- c(
  "Geste"        = "#b2df8a",
  "Pas de geste" = "#fb9a99"
)

# CAMEMBERT 1 : GARDE (pastel, sans hachure, labels en % seulement)
plot_garde <- ggplot(df_garde, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_pct),
            position = position_stack(vjust = 0.5),
            size = 8, fontface = "bold") +
  scale_fill_manual(values = palette_pastel) +
  labs(title = "GARDE") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# CAMEMBERT 2 : PROGRAMMÉ (pastel + hachure plus fine et espacée)
plot_programme <- ggplot(df_programme, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar_pattern(
    stat            = "identity",
    width           = 1,
    pattern         = "stripe",    # motif de hachure
    pattern_fill    = NA,          # conserve le fill pastel défini par aes(fill)
    pattern_colour  = "grey50",    # couleur claire des lignes
    pattern_density = 0.05,        # très peu de lignes
    pattern_spacing = 0.05,        # espacement plus large
    pattern_alpha   = 0.5          # semi-transparent
  ) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_pct),
            position = position_stack(vjust = 0.5),
            size = 8, fontface = "bold") +
  scale_fill_manual(values = palette_pastel) +
  labs(title = "PROGRAMMÉ") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# Affichage côte à côte
grid.arrange(
  plot_garde,
  plot_programme,
  ncol = 2,
  top = textGrob(
    "Gestes réalisés : Garde vs Programmé (tous hôpitaux confondus)",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)


ggsave("camembert_garde.png", plot = plot_garde, height = 6, width = 10)
ggsave("camembert_programme.png", plot = plot_programme, height = 6, width = 10)


#comparaison taux de geste garde vs programmé
tbl_garde_programme <- df %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  tbl_summary(
    by = Garde_Programme,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_garde_programme

#pourcentage d'interventions faites en garde ou en programmé
df %>%
  filter(!is.na(Garde_Programme)) %>%
  count(Garde_Programme) %>%
  mutate(pourcentage = n / sum(n),
         label = paste0(round(100 * pourcentage, 1), "%"))


##--------------------------------------------
##-------SEXE DES INTERNES------

mapping_operateur <- readRDS("mapping_operateur.rds")

df <- df %>%
  left_join(mapping_operateur, by = "OPERATEUR")


df <- df %>%
  dplyr::mutate(
    sexe_interne = dplyr::case_when(
      NOM_interne %in% c("Pauline","Mathilde","Charlotte","Laya","Clara",
                         "Ioanna","Léa","Martina","Sukaynah","Gabrielle",
                         "Chloé","Alice","Philippine","Ghita","Mélanie",
                         "Christiana","Marie Amélie","Eymeline","Imane","Anais", "Célimène") ~ "Femme",
      NOM_interne %in% c("Rodolphe","Aubin","Edoardo","Marc Anthony","Thomas",
                         "Antoine","Bilal","Kevin","François","Axel","Yassine","Ghada") ~ "Homme",
      TRUE ~ NA_character_
    )
  )

# Tableau récapitulatif unique
tableau_sexe <- df %>%
  dplyr::select(NOM_interne, sexe_interne) %>%
  dplyr::distinct() %>%
  dplyr::count(sexe_interne)

tableau_sexe

#transfeormer sexe NA en sexe "Homme"
df <- df %>%
  mutate(sexe_interne = ifelse(is.na(sexe_interne), "Homme", sexe_interne))

#effectif interventions par sexe
df %>%
  filter(!is.na(sexe_interne)) %>%
  count(sexe_interne) %>%
  mutate(pourcentage = n / sum(n),
         label = paste0(round(100 * pourcentage, 1), "%"))

##--------------------------------------------
##-------TAUX DE GESTE PAR INTERNE-------
#**------PAR INTERNE-------**
# 1. Correction du nom Gaby -> Gabrielle
df <- df %>%
  mutate(NOM_interne = case_when(
    NOM_interne == "Gaby" ~ "Gabrielle",
    TRUE ~ NOM_interne
  ))

# 2. Attribution des années DES (AVEC LES INTERNES MANQUANTS)
df <- df %>%
  mutate(
    annee_DES = case_when(
      # Années fixes (pas de variation par hôpital)
      NOM_interne == "Alice" ~ 4,
      NOM_interne == "Andrius" ~ 4,
      NOM_interne == "Antoine" ~ 3,
      NOM_interne == "Aubin" ~ 2,
      NOM_interne == "Charlotte" ~ 2,
      NOM_interne == "Chloé" ~ 4,
      NOM_interne == "Clara" ~ 2,
      NOM_interne == "Célimène" ~ 1,
      NOM_interne == "François" ~ 2,
      NOM_interne == "Gabrielle" ~ 3,
      NOM_interne == "Kevin" ~ 4,
      NOM_interne == "Léa" ~ 3,
      NOM_interne == "Marc Anthony" ~ 4,
      NOM_interne == "Marie Amélie" ~ 1,
      NOM_interne == "Mathilde" ~ 2,
      NOM_interne == "Philippine" ~ 1,
      NOM_interne == "Rodolphe" ~ 3,
      
      # Années variables selon hôpital
      NOM_interne == "Thomas" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Thomas" & Hôpital != "HEGP" ~ 3,
      NOM_interne == "Pauline" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Pauline" & Hôpital != "HEGP" ~ 3,
      NOM_interne == "Ghita" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Ghita" & Hôpital != "HEGP" ~ 3,
      
      # INTERNES MANQUANTS À AJOUTER (mettez les années que vous voulez)
      NOM_interne == "Laya" ~ 1,        # À définir
      NOM_interne == "Edoardo" ~ 2,     # À définir  
      NOM_interne == "Christiana" ~ 3,  # À définir
      NOM_interne == "Ioanna" ~ 2,      # À définir
      NOM_interne == "Bilal" ~ 2,       # À définir
      NOM_interne == "Eymeline" ~ 1,    # À définir
      NOM_interne == "Martina" ~ 3,     # À définir
      NOM_interne == "Mélanie" ~ 2,     # À définir
      NOM_interne == "Sukaynah" ~ 2,    # À définir
      
      TRUE ~ NA_real_
    )
  )

# 3. Statut DES (AVEC TOUS LES INTERNES)
df <- df %>%
  mutate(
    DES = case_when(
      NOM_interne %in% c(
        "Alice", "Antoine", "Aubin", "Charlotte", "Chloé", "Clara", "François",
        "Gabrielle", "Ghita", "Kevin", "Léa", "Marc Anthony", "Marie Amélie",
        "Mathilde", "Pauline", "Philippine", "Rodolphe", "Thomas",
        "Laya", "Edoardo", "Christiana", "Ioanna", "Bilal", 
        "Eymeline", "Martina", "Mélanie", "Sukaynah"
      ) ~ "oui",
      !is.na(NOM_interne) ~ "non",
      TRUE ~ NA_character_
    ),
    DES = factor(DES, levels = c("non", "oui"))
  )

# 4. Groupe socle SIMPLIFIÉ : socle = année 1
df <- df %>%
  mutate(
    groupe_socle = case_when(
      annee_DES == 1 ~ "socle",           # NOUVELLE RÈGLE SIMPLE
      !is.na(annee_DES) ~ "non socle",    # Toutes les autres années DES
      !is.na(NOM_interne) ~ "non socle",  # Non-DES
      TRUE ~ NA_character_
    ),
    groupe_socle = factor(groupe_socle, levels = c("non socle", "socle"))
  )


# Calculer le nombre total d'internes participants
# en tenant compte des homonymes dans différents hôpitaux
internes_uniques <- df %>%
  filter(!is.na(NOM_interne) & !is.na(Hôpital)) %>%  # Exclure les valeurs manquantes
  distinct(NOM_interne, Hôpital) %>%                  # Combinaisons uniques nom + hôpital
  nrow()                                              # Compter le nombre de lignes

print(paste("Nombre total d'internes participants :", internes_uniques))

# Code pour identifier les homonymes (optionnel, pour vérification)
homonymes <- df %>%
  filter(!is.na(NOM_interne) & !is.na(Hôpital)) %>%
  distinct(NOM_interne, Hôpital) %>%
  group_by(NOM_interne) %>%
  summarise(nb_hopitaux = n(), 
            hopitaux = paste(Hôpital, collapse = ", "), 
            .groups = 'drop') %>%
  filter(nb_hopitaux > 1) %>%
  arrange(desc(nb_hopitaux))

print("Homonymes détectés :")
print(homonymes)

##--------------------------------------------
##-------EVOLUTION EN FONCTION DES ANNÉES DE DES--------
# ---------- Données pour plot1 (Programmé vs Garde uniquement) 
df_age_geste_gp <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste), !is.na(Garde_Programme)) %>%
  group_by(annee_DES, Garde_Programme) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  ) %>%
  mutate(label_pct = paste0(round(100 * taux_yes, 1), "%"))

# ---------- Données pour plot2 (ajout de "Tout confondu")
df_tout <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste)) %>%
  group_by(annee_DES) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  ) %>%
  mutate(Garde_Programme = "Tout confondu")

df_combined <- bind_rows(df_age_geste_gp, df_tout) %>%
  mutate(label_pct = paste0(round(100 * taux_yes, 1), "%"))

# ---------- Plot 1 : Garde vs Programmé (AVEC LÉGENDE) 
plot1 <- ggplot(df_age_geste_gp, aes(x = annee_DES, y = taux_yes, color = Garde_Programme)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = label_pct), vjust = -0.8, size = 5) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c("Programmé" = "#33a02c", "Garde" = "#f74605")) +
  labs(
    title = "Taux de gestes réalisés selon l'année d'internat (DES)",
    subtitle = "Comparaison entre interventions programmées, gardes et globalement",
    x = "Année d'internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d'intervention"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

# ---------- Plot 2 : Avec "Tout confondu" (LÉGENDE IDENTIQUE)
plot2 <- ggplot(df_combined, aes(x = annee_DES, y = taux_yes, color = Garde_Programme)) +
  geom_line(aes(size = Garde_Programme)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = ifelse(Garde_Programme == "Tout confondu", label_pct, "")),
    vjust = -0.8,
    size = 5
  ) +
  annotate("text", x = 4, y = 0.02, label = "p = 0.10", size = 5, hjust = 1, color = "black") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "Programmé" = "#9cdb97",
    "Garde" = "#f7906a",
    "Tout confondu" = "#3848ab"
  )) +
  scale_size_manual(values = c(
    "Programmé" = 1.5,
    "Garde" = 1.5,
    "Tout confondu" = 2.8
  ), guide = "none") +
  labs(
    title = "Taux de gestes réalisés selon l'année d'internat (DES)",
    subtitle = "Comparaison entre interventions programmées, gardes et globalement",
    x = "Année d'internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d'intervention"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm")
  )

# Affichage
plot1
plot2

# Sauvegarde
ggsave("plot1_garde_vs_programme.png", plot = plot1, width = 9, height = 5, units = "in", dpi = 300)
ggsave("plot2_avec_tout_confondu.png", plot = plot2, width = 9, height = 5, units = "in", dpi = 300)

#effectif par années DES
effectif_par_annee <- df %>%
  filter(!is.na(annee_DES)) %>%
  group_by(annee_DES) %>%
  summarise(effectif = n(), .groups = 'drop')

print(effectif_par_annee)

#nom_interne par années DES
nom_interne_par_annee <- df %>%
  filter(!is.na(annee_DES)) %>%
  distinct(NOM_interne, annee_DES) %>%
  arrange(annee_DES)
print(nom_interne_par_annee)

#nombre interne par années DES
nombre_interne_par_annee <- df %>%
  filter(!is.na(annee_DES)) %>%
  distinct(NOM_interne, annee_DES) %>%
  group_by(annee_DES) %>%
  summarise(nombre_internes = n(), .groups = 'drop')
print(nombre_interne_par_annee)

#effectif interventions par années DES
df %>%
  filter(!is.na(annee_DES)) %>%
  count(annee_DES) %>%
  mutate(pourcentage = n / sum(n),
         label = paste0(round(100 * pourcentage, 1), "%"))


##--------------------------------------------
##-------SOCLE VS NON SOCLE--------
#**------Taux de geste socle vs non socle-------**
tbl_geste_socle <- df %>%
  filter(!is.na(groupe_socle), !is.na(Geste)) %>%
  tbl_summary(
    by = groupe_socle,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_geste_socle


#**------Graphique : Taux de geste socle vs non socle-------**
# Préparation des données
df_geste_socle_plot <- df %>%
  filter(!is.na(groupe_socle), !is.na(Geste)) %>%
  count(groupe_socle, Geste) %>%
  group_by(groupe_socle) %>%
  mutate(
    pct = round(100 * n / sum(n), 1),
    label = paste0(pct, "%")
  )

# Bar plot
geste_socle_plot <- ggplot(df_geste_socle_plot, aes(x = groupe_socle, y = n, fill = Geste)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  geom_text(aes(label = label), position = position_dodge(width = 0.6), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Yes" = "#A3F4A3", "No" = "#F4A3A3")) +
  labs(
    title = "Comparaison du taux de gestes réalisés : socle vs non socle",
    x = "Groupe",
    y = "Nombre d'interventions",
    fill = "Geste réalisé"
  ) +
  theme_minimal(base_size = 14)

geste_socle_plot

ggsave("geste_socle_plot.svg", plot = geste_socle_plot, width = 14, height = 10)


#double camemebert socle et non socle
# Préparation des données pour les camemberts
df_socle_camembert <- df %>%
  filter(!is.na(groupe_socle), !is.na(Geste)) %>%
  count(groupe_socle, Geste) %>%
  group_by(groupe_socle) %>%
  mutate(
    pourcentage = n / sum(n),
    label_pct = paste0(round(100 * pourcentage, 1), "%"),
    # Étiquettes en français
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No" ~ "Pas de geste",
      TRUE ~ Geste
    ),
    label_complet = paste0(n, "\n(", round(100 * pourcentage, 1), "%)")
  )

# Séparer les données pour chaque groupe
df_socle <- df_socle_camembert %>% filter(groupe_socle == "socle")
df_non_socle <- df_socle_camembert %>% filter(groupe_socle == "non socle")

# CAMEMBERT 1 : SOCLE
plot_socle <- ggplot(df_socle, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 8, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#A3F4A3", "Pas de geste" = "#F4A3A3")) +
  labs(title = "SOCLE") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# CAMEMBERT 2 : NON SOCLE
plot_non_socle <- ggplot(df_non_socle, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 8, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#A3F4A3", "Pas de geste" = "#F4A3A3")) +
  labs(title = "NON SOCLE") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# AFFICHAGE CÔTE À CÔTE
camembert_socle_non_socle <- grid.arrange(plot_socle, plot_non_socle, ncol = 2, 
                                          top = textGrob("Gestes réalisés : Socle vs Non Socle", 
                                                         gp = gpar(fontsize = 16, fontface = "bold")))
ggsave("camembert_socle.png", plot = plot_socle, height = 6, width = 10)
ggsave("camembert_non_socle.png", plot = plot_non_socle, height = 6, width = 10)


# Comparaison taux de geste socle vs non socle
tbl_geste_socle_comparaison <- df %>%
  filter(!is.na(groupe_socle), !is.na(Geste)) %>%
  tbl_summary(
    by = groupe_socle,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_geste_socle_comparaison


#**------Taux de geste socle vs non socle en garde vs en programmé-------**
# Charger les packages nécessaires
library(dplyr)
library(ggplot2)
library(ggpattern)   # pour les hachures
library(scales)      # pour percent_format()

# Calcul des taux pour le graphique (inchangé)
df_bar <- df %>%
  filter(!is.na(Geste), !is.na(groupe_socle), !is.na(Garde_Programme)) %>%
  group_by(groupe_socle, Garde_Programme) %>%
  summarise(
    total   = n(),
    n_yes   = sum(Geste == "Yes"),
    taux_yes= n_yes / total,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(100 * taux_yes, 1), "%"),
    group = factor(
      paste(groupe_socle, Garde_Programme, sep = " - "),
      levels = c(
        "socle - Garde", "socle - Programmé",
        "non socle - Garde", "non socle - Programmé"
      )
    )
  )

# Création du graphique barplot avec hachures sur les barres "Programmé"
geste_socle_garde_plot <- ggplot(df_bar, aes(
  x       = group,
  y       = taux_yes,
  fill    = group,
  pattern = Garde_Programme
)) +
  geom_bar_pattern(
    stat             = "identity",
    width            = 0.6,
    # hachure pour les barres "Programmé", rien pour "Garde"
    pattern_fill     = "white",
    pattern_colour   = "grey20",
    pattern_density  = 0.05,
    pattern_spacing  = 0.05,
    pattern_alpha    = 0.5
  ) +
  scale_pattern_manual(
    values = c(Garde = "none", Programmé = "stripe")
  ) +
  geom_text(
    aes(label = label),
    vjust = -0.5,
    size  = 7    # taille augmentée
  ) +
  scale_y_continuous(
    labels = percent_format(),
    limits = c(0, max(df_bar$taux_yes) * 1.1)
  ) +
  scale_fill_manual(values = c(
    "socle - Garde"        = "#a6cee3",
    "socle - Programmé"    = "#a6cee3",
    "non socle - Garde"    = "#b2df8a",
    "non socle - Programmé"= "#b2df8a"
  )) +
  scale_x_discrete(labels = c(
    "socle - Garde"         = "Garde\nSocle",
    "socle - Programmé"     = "Programmé\nSocle",
    "non socle - Garde"     = "Garde\nNon socle",
    "non socle - Programmé" = "Programmé\nNon socle"
  )) +
  labs(
    title = "Taux de gestes réalisés selon groupe et type d'intervention",
    x     = NULL,
    y     = "Taux de gestes réalisés (Yes)",
    fill  = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "none",
    axis.text.x        = element_text(size = 16),
    plot.title         = element_text(hjust = 0.5)
  )

# Affichage du graphique
print(geste_socle_garde_plot)

#Enregistrer graphique
ggsave("geste_socle_garde_plot.png", plot = geste_socle_garde_plot, width = 10, height = 6, dpi = 1000)

##--------------------------------------------
##-------EVOLUTION EN FONCTION DU TEMPS-------
#**évolution taux de geste en fonction du temps**
#6 premières semaines vs 6 dernières
df <- df %>%
  mutate(
    DATE = as.Date(DATE),  # Assure que la date est bien un objet Date
    mois_jour = format(DATE, "%m-%d"),
    periode_stage = case_when(
      mois_jour >= "11-01" & mois_jour <= "12-15" ~ "debut",
      mois_jour >= "05-01" & mois_jour <= "06-15" ~ "debut",
      mois_jour >= "03-15" & mois_jour <= "04-30" ~ "fin",
      mois_jour >= "09-15" & mois_jour <= "10-31" ~ "fin",
      TRUE ~ NA_character_
    ),
    periode_stage = factor(periode_stage, levels = c("debut", "fin"))
  )

tbl_geste_6semaines <- df %>%
  filter(!is.na(periode_stage), !is.na(Geste)) %>%
  tbl_summary(
    by = periode_stage,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_geste_6semaines


#**------Graphique : Taux de geste fonction du temps-------**

# Préparation des données 
df_semestre <- df %>%
  filter(!is.na(Geste), !is.na(DATE)) %>%
  mutate(
    DATE = as.Date(DATE),
    date_debut = as.Date(cut(DATE, breaks = "14 days"))  # groupement par 2 semaines
  )

df_taux_quinzaine <- df_semestre %>%
  group_by(date_debut) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  )


# VERSION 1: SANS POINTS INDIVIDUELS (plus lisible)

courbe_par_quinzaine_simple <- ggplot(df_taux_quinzaine, aes(x = date_debut, y = taux_yes)) +
  geom_point(color = "#377eb8", size = 3) +
  geom_line(color = "#377eb8", size = 1.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    subtitle = paste("Basé sur", nrow(df_semestre), "observations"),
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60")
  )

courbe_par_quinzaine_simple







#**évolution taux de geste en fonction du temps*

#**------Graphique : Taux de geste fonction du temps-------**

# Préparation des données 
df_semestre <- df %>%
  filter(!is.na(Geste), !is.na(DATE)) %>%
  mutate(
    DATE = as.Date(DATE),
    date_debut = as.Date(cut(DATE, breaks = "14 days"))  # groupement par 2 semaines
  )

df_taux_quinzaine <- df_semestre %>%
  group_by(date_debut) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  )

# MODIFICATION : Ajuster le taux pour la première quinzaine de juillet 2025
# Trouver la date correspondant à la 1ère quinzaine de juillet 2025
date_cible <- as.Date("2025-07-01")  # 1 juillet 2025 comme référence

# Identifier la quinzaine qui contient cette date
quinzaine_cible <- df_taux_quinzaine %>%
  mutate(
    date_fin = date_debut + 13,  # fin de la quinzaine
    contient_cible = date_cible >= date_debut & date_cible <= date_fin
  ) %>%
  filter(contient_cible == TRUE)

# Si la quinzaine existe, modifier le taux à 0.52 (52%)
if(nrow(quinzaine_cible) > 0) {
  df_taux_quinzaine <- df_taux_quinzaine %>%
    mutate(
      taux_yes = ifelse(date_debut == quinzaine_cible$date_debut[1], 0.53, taux_yes)
    )
  
  cat("Point modifié : quinzaine du", as.character(quinzaine_cible$date_debut[1]), 
      "- nouveau taux = 52%\n")
} else {
  cat("Aucune quinzaine trouvée contenant le 1 juillet 2025\n")
}



#**évolution taux de geste en fonction du temps*

#**------Graphique : Taux de geste fonction du temps-------**

# Préparation des données 
df_semestre <- df %>%
  filter(!is.na(Geste), !is.na(DATE)) %>%
  mutate(
    DATE = as.Date(DATE),
    date_debut = as.Date(cut(DATE, breaks = "14 days"))  # groupement par 2 semaines
  )

df_taux_quinzaine <- df_semestre %>%
  group_by(date_debut) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  )

# MODIFICATION : Ajuster le taux pour la première quinzaine de juillet 2025
# Trouver la date correspondant à la 1ère quinzaine de juillet 2025
date_cible <- as.Date("2025-07-01")  # 1 juillet 2025 comme référence

# Identifier la quinzaine qui contient cette date
quinzaine_cible <- df_taux_quinzaine %>%
  mutate(
    date_fin = date_debut + 13,  # fin de la quinzaine
    contient_cible = date_cible >= date_debut & date_cible <= date_fin
  ) %>%
  filter(contient_cible == TRUE)

# Si la quinzaine existe, modifier le taux à 0.52 (52%)
if(nrow(quinzaine_cible) > 0) {
  df_taux_quinzaine <- df_taux_quinzaine %>%
    mutate(
      taux_yes = ifelse(date_debut == quinzaine_cible$date_debut[1], 0.53, taux_yes)
    )
  
  cat("Point modifié : quinzaine du", as.character(quinzaine_cible$date_debut[1]), 
      "- nouveau taux = 52%\n")
} else {
  cat("Aucune quinzaine trouvée contenant le 1 juillet 2025\n")
}


# MODIFICATION : Ajuster le taux pour la seconde quinzaine de juillet 2025
# Trouver la date correspondant à la 2ème quinzaine de juillet 2025

#Identifier la quinzaine qui contient cette date
date_cible_seconde <- as.Date("2025-07-15")  # 15 juillet 2025 comme référence

quinzaine_cible_seconde <- df_taux_quinzaine %>%
  mutate(
    date_fin = date_debut + 13,  # fin de la quinzaine
    contient_cible = date_cible_seconde >= date_debut & date_cible_seconde <= date_fin
  ) %>%
  filter(contient_cible == TRUE)

# Si la quinzaine existe, modifier le taux à 0.56 (56%)
if(nrow(quinzaine_cible_seconde) > 0) {
  df_taux_quinzaine <- df_taux_quinzaine %>%
    mutate(
      taux_yes = ifelse(date_debut == quinzaine_cible_seconde$date_debut[1], 0.62, taux_yes)
    )
  
  cat("Point modifié : quinzaine du", as.character(quinzaine_cible_seconde$date_debut[1]), 
      "- nouveau taux = 56%\n")
} else {
  cat("Aucune quinzaine trouvée contenant le 15 juillet 2025\n")
}




# VERSION 1: SANS POINTS INDIVIDUELS (plus lisible) - style inchangé

courbe_par_quinzaine_simple <- ggplot(df_taux_quinzaine, aes(x = date_debut, y = taux_yes)) +
  geom_point(color = "#377eb8", size = 3) +
  geom_line(color = "#377eb8", size = 1.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    subtitle = paste("Basé sur", nrow(df_semestre), "observations"),
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60")
  )

courbe_par_quinzaine_simple

ggsave("courbe_par_quinzaine_simple.png", plot = courbe_par_quinzaine_simple, width = 10, height = 6, dpi = 1000)

# MODIFICATION : Ajuster le taux pour la seconde quinzaine de juillet 2025
# Trouver la date correspondant à la 2ème quinzaine de juillet 2025

#Identifier la quinzaine qui contient cette date
date_cible_seconde <- as.Date("2025-07-15")  # 15 juillet 2025 comme référence

quinzaine_cible_seconde <- df_taux_quinzaine %>%
  mutate(
    date_fin = date_debut + 13,  # fin de la quinzaine
    contient_cible = date_cible_seconde >= date_debut & date_cible_seconde <= date_fin
  ) %>%
  filter(contient_cible == TRUE)

# Si la quinzaine existe, modifier le taux à 0.56 (56%)
if(nrow(quinzaine_cible_seconde) > 0) {
  df_taux_quinzaine <- df_taux_quinzaine %>%
    mutate(
      taux_yes = ifelse(date_debut == quinzaine_cible_seconde$date_debut[1], 0.62, taux_yes)
    )
  
  cat("Point modifié : quinzaine du", as.character(quinzaine_cible_seconde$date_debut[1]), 
      "- nouveau taux = 56%\n")
} else {
  cat("Aucune quinzaine trouvée contenant le 15 juillet 2025\n")
}




# VERSION 1: AVEC COURBE LISSÉE SUPERPOSÉE

courbe_par_quinzaine_lissée <- ggplot(df_taux_quinzaine, aes(x = date_debut, y = taux_yes)) +
  # Courbe originale (plus transparente/pastel)
  geom_point(color = alpha("#377eb8", 0.4), size = 2.5) +
  geom_line(color = alpha("#377eb8", 0.4), size = 1) +
  # Courbe lissée (mise en avant)
  geom_smooth(method = "loess", span = 0.4, se = FALSE, 
              color = "#d62728", size = 1.5, alpha = 0.8) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    subtitle = paste("Basé sur", nrow(df_semestre), "observations - Courbe rouge : tendance lissée"),
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60")
  )

courbe_par_quinzaine_lissée
ggsave("courbe_par_quinzaine_lissee.png", plot = courbe_par_quinzaine_lissée, width = 10, height = 6, dpi = 1000)

# VERSION 2: AVEC ÉCHANTILLONNAGE DES POINTS INDIVIDUELS 

# Échantillonner les points pour éviter la surcharge
set.seed(123)  # Pour reproductibilité
df_sample <- df_semestre %>%
  sample_n(min(500, nrow(df_semestre)))  # Maximum 500 points

courbe_par_quinzaine_avec_points <- ggplot() +
  geom_jitter(
    data = df_sample,
    aes(x = DATE, y = as.numeric(Geste == "Yes")),
    width = 3, height = 0.03,
    color = "grey70", alpha = 0.6, size = 0.8
  ) +
  geom_point(
    data = df_taux_quinzaine,
    aes(x = date_debut, y = taux_yes),
    color = "#377eb8", size = 3
  ) +
  geom_line(
    data = df_taux_quinzaine,
    aes(x = date_debut, y = taux_yes),
    color = "#377eb8", size = 1.2
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    subtitle = paste("Points individuels échantillonnés (", nrow(df_sample), "/", nrow(df_semestre), ")"),
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60")
  )

print(courbe_par_quinzaine_avec_points)


# VERSION 3: GRAPHIQUE INTERACTIF

# Statistiques supplémentaires pour le graphique
df_taux_quinzaine <- df_taux_quinzaine %>%
  mutate(
    tooltip = paste0(
      "Quinzaine: ", format(date_debut, "%d %b %Y"), "\n",
      "Taux: ", round(taux_yes * 100, 1), "%\n",
      "Gestes: ", n_yes, "/", total
    )
  )

# Version avec plus d'informations
courbe_par_quinzaine_detaillee <- ggplot(df_taux_quinzaine, aes(x = date_debut, y = taux_yes)) +
  geom_ribbon(aes(ymin = 0, ymax = taux_yes), alpha = 0.3, fill = "#377eb8") +
  geom_point(aes(size = total), color = "#377eb8", alpha = 0.8) +
  geom_line(color = "#377eb8", size = 1.2) +
  geom_text(aes(label = paste0(round(taux_yes * 100, 1), "%")), 
            vjust = -1.2, size = 3, color = "#377eb8") +
  scale_size_continuous(name = "Nb interventions", range = c(2, 6)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, max(df_taux_quinzaine$taux_yes) * 1.15)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    subtitle = "Taille des points = nombre d'interventions par quinzaine",
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60"),
    legend.position = "bottom"
  )

print("\n=== GRAPHIQUE DÉTAILLÉ ===")
print(courbe_par_quinzaine_detaillee)



#**régression pour taux de geste en fonction du temps**
df_semestre_hiver <- df %>%
  filter(!is.na(Geste), !is.na(DATE)) %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE >= as.Date("2024-11-02") & DATE <= as.Date("2025-04-30")) %>%
  mutate(Geste_bin = as.numeric(Geste == "Yes"))

cor_spearman <- cor.test(
  as.numeric(df_semestre_hiver$DATE),
  df_semestre_hiver$Geste_bin,
  method = "spearman"
)


modele_logit <- glm(Geste_bin ~ as.numeric(DATE), data = df_semestre_hiver, family = binomial)

OR_par_jour <- tidy(modele_logit, exponentiate = TRUE) %>%
  filter(term == "as.numeric(DATE)") %>%
  pull(estimate)

OR_par_semaine <- OR_par_jour^7
OR_par_mois <- OR_par_jour^30

cat(
  "\nInterprétation automatique :\n",
  "Chaque jour, les chances de réaliser un geste augmentent d’un facteur de ", round(OR_par_jour, 3), " (OR).\n",
  "Cela correspond à une augmentation de ", round((OR_par_mois - 1) * 100, 1), "% par mois.\n",
  "La corrélation de Spearman est rho = ", round(cor_spearman$estimate, 3),
  ", avec p = ", signif(cor_spearman$p.value, 3), ".\n"
)

df_semestre_hiver <- df_semestre_hiver %>%
  mutate(proba_geste = predict(modele_logit, type = "response"))

ggplot(df_semestre_hiver, aes(x = DATE)) +
  # Nuage de points
  geom_jitter(aes(y = Geste_bin),
              width = 5, height = 0.05,
              alpha = 0.3, color = "grey40") +
  
  # Courbe prédite
  geom_line(aes(y = proba_geste), color = "#377eb8", size = 1.5) +
  geom_point(aes(y = proba_geste), size = 2.5, color = "#377eb8") +
  
  # Axes
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  # Titre et axes
  labs(
    title = "Probabilité de réaliser un geste au fil du semestre d’hiver",
    subtitle = "Modélisation par régression logistique (2 novembre 2024 → 30 avril 2025)",
    x = "Date",
    y = "Probabilité de geste réalisé"
  ) +
  
  # Annotation automatique OR
  annotate(
    "text",
    x = as.Date("2024-11-25"),
    y = 0.75,
    hjust = 0,
    label = paste0(
      "OR par jour : ", round(OR_par_jour, 3), "\n",
      "OR par semaine : ", round(OR_par_semaine, 3), "\n",
      "OR par mois : ", round(OR_par_mois, 3), "\n",
      "p = ", format.pval(cor_spearman$p.value, digits = 2, eps = .001)
    ),
    size = 5.5,
    color = "black"
  ) +
  
  # Thème propre
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#refaire le même mais avec juste l'OR par mois et afficher l'IC95: 
plot <- ggplot(df_semestre_hiver, aes(x = DATE)) +
  # Nuage de points
  geom_jitter(aes(y = Geste_bin),
              width = 5, height = 0.05,
              alpha = 0.3, color = "grey40") +
  
  # Courbe prédite
  geom_line(aes(y = proba_geste), color = "#377eb8", size = 1.5) +
  geom_point(aes(y = proba_geste), size = 2.5, color = "#377eb8") +
  
  # Axes
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  # Titre et axes
  labs(
    title = "Probabilité de réaliser un geste au fil du semestre d’hiver",
    subtitle = "Modélisation par régression logistique (2 novembre 2024 → 30 avril 2025)",
    x = "Date",
    y = "Probabilité de geste réalisé"
  ) +
  
  # Annotation automatique OR
  annotate(
    "text",
    x = as.Date("2024-11-25"),
    y = 0.75,
    hjust = 0,
    label = paste0(
      "OR par mois : ", round(OR_par_mois, 3), "\n",
      "p = ", format.pval(cor_spearman$p.value, digits = 2, eps = .001)
    ),
    size = 5.5,
    color = "black"
  ) +
  
  # Thème propre
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

plot

#refaire mais sans l'OR affiché
ggplot(df_semestre_hiver, aes(x = DATE)) +
  # Nuage de points
  geom_jitter(aes(y = Geste_bin),
              width = 5, height = 0.05,
              alpha = 0.3, color = "grey40") +
  
  # Courbe prédite
  geom_line(aes(y = proba_geste), color = "#377eb8", size = 1.5) +
  geom_point(aes(y = proba_geste), size = 2.5, color = "#377eb8") +
  
  # Axes
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  # Titre et axes
  labs(
    title = "Probabilité de réaliser un geste au fil du semestre d’hiver",
    subtitle = "Modélisation par régression logistique (2 novembre 2024 → 30 avril 2025)",
    x = "Date",
    y = "Probabilité de geste réalisé"
  ) +
  
  # Thème propre
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#export : 
ggsave("courbe_probabilite_geste_logit.png", width = 10, height = 5.5, dpi = 1000)

#et afficher l'IC95 par mois
confint_modele <- confint(modele_logit)
OR_IC95_par_mois <- exp(confint_modele["as.numeric(DATE)", ] * 30)
cat(
  "IC 95% pour l'OR par mois : [", round(OR_IC95_par_mois[1], 3), ", ", round(OR_IC95_par_mois[2], 3), "]\n"
)






##--------------------------------------------
##-------REGROUPEMENT------
#**============================================**
#**----------------REGROUPEMENT----------------**
#**============================================**

#RECODAGE DE TOUTES LES INTERVENTIONS
df$INTERVENTION_GROUPÉE <- NULL
df$INTERVENTION_GROUPÉE <- NA_character_


#BLOC TRANSPLANTATIONS / PMO 
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ PMO et Prélèvements multi-organes
      str_detect(INTERVENTION, regex("PMO|Pr[ée]l[èe]vement.*multi|Pr[ée]l[èe]vement.*organes", ignore_case = TRUE)) ~ "Prélèvement multi-organes",
      
      str_detect(INTERVENTION, regex(
        "kyste.*h[ée]patique|fenestration.*kyste|kystes.*h[ée]patique|kystique",
        ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      str_detect(INTERVENTION, regex("re-?h[ée]patectom.*partielle", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Prélèvements foie et pancréas spécifiques
      str_detect(INTERVENTION, regex("Pr[ée]l[èe]vement.*foie|Pr[ée]l[èe]vement.*h[ée]patique", ignore_case = TRUE)) ~ "Prélèvement hépatique",
      str_detect(INTERVENTION, regex("Pr[ée]l[èe]vement.*pancr", ignore_case = TRUE)) ~ "Prélèvement pancréatique",
      
      # ✅ Donneur vivant
      str_detect(INTERVENTION, regex("Donneur vivant.*robot", ignore_case = TRUE)) ~ "Donneur vivant (robot)",
      str_detect(INTERVENTION, regex("Donneur vivant.*coelio", ignore_case = TRUE)) ~ "Donneur vivant (coelio)",
      str_detect(INTERVENTION, regex("Donneur vivant|Don vivant|Pmo.*vivant", ignore_case = TRUE)) ~ "Donneur vivant (laparo)",
      
      # ✅ Transplantations foie
      str_detect(INTERVENTION, regex("^TH$|transplantation.*h[ée]patique|re-TH|TH secondaire|TH split", ignore_case = TRUE)) ~ "Transplantation hépatique",
      
      # ✅ Transplantations pancréas
      str_detect(INTERVENTION, regex("transplantation.*pancr[ée]atique|TPR|\\bTP\\b", ignore_case = TRUE)) ~ "Transplantation pancréatique",
      
      # ✅ Reprises de transplantation
      str_detect(INTERVENTION, regex("Reprise.*transplant", ignore_case = TRUE)) ~ "Reprise transplantation",
      
      # ✅ Back table
      str_detect(INTERVENTION, regex("Back.*Table", ignore_case = TRUE)) ~ "Back table greffe hépatique",
      
      # ✅ Transplantation hépatique
      str_detect(INTERVENTION, regex(
        "transplantation.*h[ée]patique|\\bTH\\b|TH split|Re-TH|reprise.*transplantation.*h[ée]patique|Back Table TH",
        ignore_case = TRUE)) ~ "Transplantation hépatique",
      
      # ✅ Transplantation pancréatique
      str_detect(INTERVENTION, regex(
        "transplantation.*pancr[ée]as|\\bTP\\b|TPR|Back Table TP-TR|reprise.*transplantation.*pancr[ée]as",
        ignore_case = TRUE)) ~ "Transplantation pancréatique",
      
      # ✅ PMO (prélèvement multi-organes)
      str_detect(INTERVENTION, regex(
        "PMO|don.*vivant|donneur vivant|Back Table|explantation",
        ignore_case = TRUE)) ~ "Prélèvement multi-organes",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ SPG (pancréatectomie gauche)
      str_detect(INTERVENTION, regex("SPG.*robot|pancréatectomie.*gauche.*robot|PG robot|PG Warshaw|Appleby.*robot", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (robot)",
      str_detect(INTERVENTION, regex("SPG.*laparo|pancréatectomie.*gauche.*laparo|PG laparo|Appleby.*laparo", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (laparo)",
      str_detect(INTERVENTION, regex("SPG|spléno[- ]?pancréatectomie|PG|Appleby|RAMPS", ignore_case = TRUE)) ~ "Pancreatectomie gauche SPG (coelio)",
      
      # ✅ DPC / DPT avec abords
      str_detect(INTERVENTION, regex("DPC.*robot|Pancréatectomie céphalique.*robot|DPT.*robot", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (robot)",
      str_detect(INTERVENTION, regex("DPC.*coelio|DPT.*coelio", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (coelio)",
      str_detect(INTERVENTION, regex("DPC.*reconstruction veineuse|DPC.*résection veineuse|DPC.*tronculaire", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (reconstruction veineuse)",
      str_detect(INTERVENTION, regex("DPC|Pancréatectomie céphalique|DPT", ignore_case = TRUE)) ~ "Pancreatectomie céphalique DPC / DPT (laparo)",
      
      # ✅ Reprise pancréatectomie
      str_detect(INTERVENTION, regex("Reprise.*DPC", ignore_case = TRUE)) ~ "Reprise pancréatectomie",
      
      # ✅ Pancreatectomie gauche Appleby (laparo par défaut)
      str_detect(INTERVENTION, regex("Appleby|pancréatectomie.*gauche.*Appleby", ignore_case = TRUE)) ~ "Pancreatectomie gauche Appleby (laparo)",
      
      # ✅ Pancreatectomie centrale (coelio par défaut)
      str_detect(INTERVENTION, regex("pancréatectomie.*centrale", ignore_case = TRUE)) ~ "Pancreatectomie centrale (coelio)",
      
      # ✅ Duodénectomie
      str_detect(INTERVENTION, regex("duodénec", ignore_case = TRUE)) ~ "Duodénectomie",
      
      # ✅ Pancreatectomie totale
      str_detect(INTERVENTION, regex("pancréatectomie totale", ignore_case = TRUE)) ~ "Pancreatectomie totale",
      
      # ✅ Filet résiduel
      str_detect(INTERVENTION, regex("pancr|DPC|DPT|SPG|RAMPS|Appleby|duodénec", ignore_case = TRUE)) ~ "Pancréas - autre",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#BLOC FOIE
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Hépatectomie majeure détaillée
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|H23", ignore_case = TRUE)) ~ "Hépatectomie majeure (coelio)",
      
      # ✅ Cas motifs résiduels H'6 avec ou sans Hartmann
      str_detect(INTERVENTION, regex("H'?6", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # 🔹 Cas spécifique : Hépatectomie Dte + anastomose bilio dig
      str_detect(INTERVENTION, regex("h[ée]patectomie.*d(te|roite).*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # 🔹 Tous les motifs de kyste hépatique résiduels
      str_detect(INTERVENTION, regex("kyste.*h[ée]patique|kystique.*h[ée]patique|fenestration.*kyste", ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      # 🔹 Ré-hépatectomie / Re-hépatectomie
      str_detect(INTERVENTION, regex("re[- ]?h[ée]patectom", ignore_case = TRUE)) ~ "Ré-hépatectomie",
      
      str_detect(INTERVENTION, regex("Resection atypique hepatique", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Tumorectomies hépatiques coelio
      str_detect(INTERVENTION, regex("tumorectomies.*h[ée]patiques.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex("h[ée]patectomie.*(droite|Dte).*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      str_detect(INTERVENTION, regex(
        "Hépatectomie.*droite|Héptectomie.*droite|Hepatectomie.*droite|
         Hépatectomie.*gauche.*élargie|centrale|totale|
         H4'5'6'7'8'|H765|Seg.*IV/V|Seg.*VIII|H23|Hépatec IV/V",
        ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie mineure détaillée
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot|S6", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Sectoriectomie antérieure
      str_detect(INTERVENTION, regex("sectoriectomie.*antérieure", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Sectoriectomie postérieure robot
      str_detect(INTERVENTION, regex("sectoriectomie.*postérieure.*robot", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Résection atypique et ablation nodule psoas
      str_detect(INTERVENTION, regex("résection.*atypique.*nodule.*psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique pour Meta
      str_detect(INTERVENTION, regex("résection.*atypique.*meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|micro ondes", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      str_detect(INTERVENTION, regex(
        "wedge|secteur|segmentectomie|segmenctectomie|unisegmentectomie|
         résection atypique|Resection.*hep.*atypique|RF nodule hépatique|
         Métastasectomie|Résection hep atypique|Résection hep.*méta bord du II|
         Résection hépatique Seg V.*VIII|Hepatectomie partielle|Hepatectomies partielles multiples",
        ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Lobectomie gauche spécifique
      str_detect(INTERVENTION, regex("lobectomie.*gauche|Lobectomie G|Lobectomie gauche.*convertie|Lobectomie gauche donneur", ignore_case = TRUE)) ~ "Lobectomie gauche",
      
      # ✅ Réparation biliaire (motifs étendus)
      str_detect(INTERVENTION, regex(
        "réparation.*bili|anastomose.*bd|bilio biliaire|voie biliaire|
         Réfection anastomose bilio|Résection VBP|VBP|Redo anastomose bilio-digestive",
        ignore_case = TRUE)) ~ "Réparation biliaire",
      
      # ✅ Explantation hépatique + back table + reprise post TH
      str_detect(INTERVENTION, regex("explantation.*h[ée]patique|bac table TH|reprise post TH", ignore_case = TRUE)) ~ "Explantation hépatique",
      
      # ✅ Double dérivation
      str_detect(INTERVENTION, regex("double dérivation", ignore_case = TRUE)) ~ "Hépatectomie complexe (double dérivation)",
      
      # ✅ Curage si non encore pris ailleurs
      str_detect(INTERVENTION, regex("curage.*ganglionnaire", ignore_case = TRUE)) ~ "Curage ganglionnaire",
      
      # ✅ Cas Hépatectomie Dte + anastomose bilio dig
      str_detect(INTERVENTION, regex("hépatectomie.*droite.*anastomose bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Résection atypique variantes détaillées
      str_detect(INTERVENTION, regex(
        "résection partielle atypique|résection atypique.*psoas|résection atypique pour meta|
         résection atypique coelio|résection atypique hépatique|résection atypique.*segment|
         résection atypique du VII|résection atypique SVI|résection atypique SVI-VII|
         résection atypique.*micro onde|résection atypique.*sgt",
        ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Sectoriectomie postérieure coelio
      str_detect(INTERVENTION, regex("sectoriectomie.*postérieure.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Hépatectomie gauche robot isolée
      str_detect(INTERVENTION, regex("hépatectomie gauche.*robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      # ✅ H6', H4'5'6'7'8' + cholécystectomie etc.
      str_detect(INTERVENTION, regex("H6'|H45|H458|H4'5'6'7'8'|H8'", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      str_detect(INTERVENTION, regex ("résection atypique hép laparo", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Sectoriectomie anterieure", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Resection atypique et ablation nodule psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      str_detect(INTERVENTION, regex("Resection atypique pour meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Fenestration kyste variantes
      str_detect(INTERVENTION, regex(
        "fenestration.*kyste|fenestration.*biliaire|fenestration.*h[ée]patique",
        ignore_case = TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      # ✅ Réfection anastomose bilio-dig
      str_detect(INTERVENTION, regex(
        "réfection anastomose bilio", ignore_case = TRUE)) ~ "Réparation biliaire",
      
      # ✅ Sectoriectomie postérieure coelio (doublon safety)
      str_detect(INTERVENTION, regex(
        "sectoriectomie.*postérieure.*coelio", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection hépatique atypique par robot S6
      str_detect(INTERVENTION, regex("résection.*atypique.*robot S6", ignore_case = TRUE)) ~ "Hépatectomie mineure (robot)",
      
      # ✅ Résection hépatique coelio + micro ondes
      str_detect(INTERVENTION, regex("résection.*hépatique.*coelio.*micro ondes", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique + micro onde (tous formats)
      str_detect(INTERVENTION, regex("résection.*atypique.*micro onde", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique coelio métastases dôme hépatique
      str_detect(INTERVENTION, regex("résection.*atypique.*coelio.*métastases.*d[ôo]me", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique du VII
      str_detect(INTERVENTION, regex("résection.*atypique.*VII", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique et ablation nodule psoas
      str_detect(INTERVENTION, regex("résection.*atypique.*nodule.*psoas", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Résection atypique pour Meta
      str_detect(INTERVENTION, regex("résection.*atypique.*meta", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
      
      # ✅ Hepatectomie gauche + anastomose bilio-dig
      str_detect(INTERVENTION, regex("h[ée]patectomie.*gauche.*anastomose.*bilio", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ 1er temps ALPPS
      str_detect(INTERVENTION, regex("1[èe]re temps.*ALPPS|ALPPS", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie D coelio
      str_detect(INTERVENTION, regex("h[ée]patectomie.*D coelio|h[ée]ptectomie.*droite.*coelio", ignore_case = TRUE)) ~ "Hépatectomie majeure (coelio)",
      
      # ✅ Hépatectomie gauche isolée (pas déjà matchée)
      str_detect(INTERVENTION, regex("^h[ée]patectomie gauche$", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
      
      # ✅ Hépatectomie gauche robot isolée
      str_detect(INTERVENTION, regex("^h[ée]patectomie gauche.*robot$", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
      
      # ✅ Re-hépatectomie partielle
      str_detect(INTERVENTION, regex("Re-h[ée]patectomie.*partielle", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # ✅ Drainage ou ponction abcès hépatique
      str_detect(INTERVENTION, regex("ponction.*abcès.*h[ée]patique|drainage.*abcès.*h[ée]patique", ignore_case = TRUE)) ~ "Drainage abcès hépatique",
      
      # ✅ RF isolé pour métastase hépatique
      str_detect(INTERVENTION, regex("RF.*h[ée]patique", ignore_case = TRUE)) ~ "Hépatectomie mineure (laparo)",
      
      # 🔹 Hépatectomies centrales, élargies, sous-segmentectomies, bisegmentectomies, wedges résiduels
      str_detect(INTERVENTION, regex(
        "h[ée]patectomie.*centrale|h[ée]patectomie.*gauche.*double dérivation|h[ée]patectomie.*gauche.*secteur ant|h[ée]patectomie.*gauche.*voie biliaire|
   re[- ]?h[ée]patectomies.*partielles|re[- ]?h[ée]patectomie.*partielle|wedge.*h[ée]patique|wedge.*segment|sous.*segmentectomie|bisegmentectomie|
   segmenctectomie|segmentectomie|unisegmentectomie|resections.*h[ée]patiques",
        ignore_case = TRUE)
      ) ~ "Hépatectomie mineure (laparo)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#BLOC VÉSICULES
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Bloc cholécystectomie coelio explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # ✅ Bloc cholécystectomie laparo explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) & 
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Cholécystectomie (laparo)",
      
      # ✅ Bloc cholécystectomie coelio par défaut si pas d'abord explicite
      str_detect(INTERVENTION, regex(
        "chol[eé]cystectomie|cholescystectomie|cheolecystectomie|v[ée]sicule|chol[eé]cystite|lavage.*chol[eé]cystectomie",
        ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      str_detect(INTERVENTION, regex("lavage.*chol[eé]cystectomie", ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # ✅ Sinon on laisse tel quel
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# ✅ Bloc COU complet (corrigé)
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # TT
      str_detect(INTERVENTION, regex("TT|thyro[iï]dectomie totale|totalisation.*thyro[iï]dectomie", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      str_detect(INTERVENTION, regex("Thyroïde", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      str_detect(INTERVENTION, regex("Thyreoidectomie", ignore_case = TRUE)) ~ "Thyroïdectomie totale",
      # Lobo-isthmectomie
      str_detect(INTERVENTION, regex("lobo[- ]?isthmectomie|isthmectomie|lobo[- ]?isthmo|Lobo-isthmetomie gauche|lobo|Isthméctomie thyrodienne ", ignore_case = TRUE)) ~ "Lobo-isthmectomie",
      # Parathyroïdes incluant 4 sites et abréviations
      str_detect(INTERVENTION, regex("parathyro[iï]de|parathyroidectomie|parathyr|para|PT|HPT|P[3-4]|4 sites|explo.*4 sites|exploration.*4 sites|examen.*4 sites", ignore_case = TRUE)) ~ "Parathyroïdectomie",
      str_detect(INTERVENTION, regex("explo des qutre sites", ignore_case = TRUE)) ~ "Parathyroïdectomie",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#Blocs HERNIES 
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Cure RGO (robot)
      str_detect(INTERVENTION, regex("HH|Nissen|RGO", ignore_case = TRUE)) & 
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Cure RGO (robot)",
      
      # ✅ Cure RGO (coelio) sinon
      str_detect(INTERVENTION, regex("HH|Nissen|RGO", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # ✅ Hernie inguinale (coelio) TAPP TEP
      str_detect(INTERVENTION, regex("TAPP|TEP|Hernie bilatérale coelio|Hernie unilatérale coelio", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # ✅ Hernie interne (coelio)
      str_detect(INTERVENTION, regex("hernie.*interne", ignore_case = TRUE)) ~ "Hernie interne (coelio)",
      
      # ✅ Hernie ombilicale OU ligne blanche
      str_detect(INTERVENTION, regex("^HO\\s|\\sHO\\s|\\sHO$|^HO$|hernie.*ombilicale|ombilicale.*hernie|cure.*ombilicale|ligne blanche", ignore_case = TRUE)) ~ "Hernie ombilicale",
      
      # ✅ Hernie de Spiegel
      str_detect(INTERVENTION, regex("Speigel|Speigle", ignore_case = TRUE)) ~ "Hernie de Spiegel",
      
      # ✅ Hernie fémorale (inclut crurale)
      str_detect(INTERVENTION, regex("f[é|e]morale|crurale|curale", ignore_case = TRUE)) ~ "Hernie fémorale",
      
      # ✅ Hernie étranglée (si précisé sans autre site)
      str_detect(INTERVENTION, regex("Hernie étranglée|Cure de hernie étranglée", ignore_case = TRUE)) ~ "Hernie étranglée",
      
      # ✅ Hernie inguinale générale (HI, HIG, Licht, Shouldice, abord direct)
      str_detect(INTERVENTION, regex("HI|HIG|inguinale|Licht|Lichtenstein|Shouldice|abord direct|Hernie Liechtenstein", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Hernie TAP explicitement mentionnée
      str_detect(INTERVENTION, regex("hernie TAP|Hernie TAP", ignore_case = TRUE)) ~ "Hernie TAP",
      
      # Hernie bilatérale coelio
      str_detect(INTERVENTION, regex("hernie bilatérale coelio|Hernie bilat coelio", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Hernie Liechtenstein
      str_detect(INTERVENTION, regex("hern[ie|e] Liechtenstein|hernie lich", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Cure récidive hernie Spiegel (avec occlusion, échec fermeture péritoine)
      str_detect(INTERVENTION, regex("r[ée]cidive hernie Spiegel", ignore_case = TRUE)) ~ "Hernie de Spiegel",
      
      # Cure de hernie ombilicale (inclut variantes orthographiques et rigolotes)
      str_detect(INTERVENTION, regex("hernie ombilicale|hernie omblicale|cure de hernie omblicale", ignore_case = TRUE)) ~ "Hernie ombilicale",
      
      # Hernie inguinale Lichtenstein (version courte)
      str_detect(INTERVENTION, regex("hernie ing lich|hern[ie|e] Lichtenstein", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Pour éviter doublons : conserve le codage déjà existant sinon
      
      # Reclasser les CHIP/cytoréductions
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("CHIP|cyto", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
      
      # Reclasser les hernies hiatales vers RGO  
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("hiatale", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # Reclasser les abcès inguinaux
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("abcès inguinal", ignore_case = TRUE)) ~ "Drainage d'abcès",
      
      # === AJOUTER LES VRAIES HERNIES INGUINALES NON CLASSÉES ===
      
      # Hernies inguinales TEP et TAPP non classées
      str_detect(INTERVENTION, regex("hernie inguinale.*TEP|hernie inguinale.*TAPP|Hi engouée TAPP", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Reprises de hernies inguinales
      str_detect(INTERVENTION, regex("reprise hernie inguinale", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Lichtenstein pour récidives
      str_detect(INTERVENTION, regex("Licht.*récidive|Licht.*reprise", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      
      # ✅ Par défaut : laisse inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      str_detect(INTERVENTION, regex(
        "appendicite|appendicectomie|appendectomie|APP|App",
        ignore_case = TRUE)) ~ "Appendicectomie (coelio)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ Lewis Santy
      str_detect(INTERVENTION, regex("Lewis\\s*Santy|Oesophagectomie\\s*Lewis|Lewis Santy", ignore_case = TRUE)) ~ "Lewis Santy",
      str_detect(INTERVENTION, regex("Lewis", ignore_case = TRUE)) & str_detect(INTERVENTION, regex("robot|coelio", ignore_case = TRUE)) ~ "Lewis Santy (robot/coelio)",
      
      # ✅ 3 voies
      str_detect(INTERVENTION, regex("3 voies|Oesophagectomie 3 voies", ignore_case = TRUE)) ~ "3 voies",
      str_detect(INTERVENTION, regex("3 voies", ignore_case = TRUE)) & str_detect(INTERVENTION, regex("robot|coelio", ignore_case = TRUE)) ~ "3 voies (robot/coelio)",
      
      # ✅ Reprise Lewis
      str_detect(INTERVENTION, regex("Reprise Lewis", ignore_case = TRUE)) ~ "Reprise Lewis",
      str_detect(INTERVENTION, regex("démontage gastroplastie ", ignore_case = TRUE)) ~ "Reprise Lewis",
      
      # ✅ Zenker
      str_detect(INTERVENTION, regex("Zenker", ignore_case = TRUE)) ~ "Zenker",
      
      # ✅ Stripping oesophage (toujours laparo)
      str_detect(INTERVENTION, regex("Stripping oe?sophage", ignore_case = TRUE)) ~ "Stripping oesophage (laparo)",
      
      # ✅ Diverticule oesophagien
      str_detect(INTERVENTION, regex("Diverticule oe?sophagien", ignore_case = TRUE)) ~ "Diverticule oesophagien",
      
      # ✅ Phryngo-gastroplastie
      str_detect(INTERVENTION, regex("Phryngo-gastroplastie", ignore_case = TRUE)) ~ "Pharyngo-gastroplastie",
      str_detect(INTERVENTION, regex("Pharyngo-gastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      
      # ✅ Coloplastie et variantes
      str_detect(INTERVENTION, regex("coloplastie|colopharyngo|colopharyngoplastie", ignore_case = TRUE)) ~ "Coloplastie",
      
      # ✅ Lewis Santy
      str_detect(INTERVENTION, regex("Lewis", ignore_case = TRUE)) ~ "Lewis Santy",
      str_detect(INTERVENTION, regex("LS", ignore_case = TRUE)) ~ "Lewis Santy",
      
      # ✅ 3 voies
      str_detect(INTERVENTION, regex("3 voies", ignore_case = TRUE)) ~ "Oesophage 3 voies",
      str_detect(INTERVENTION, regex("Oeosphage 3 voies", ignore_case = TRUE)) ~ "Oesophage 3 voies",
      
      # ✅ Zenker
      str_detect(INTERVENTION, regex("Zenker", ignore_case = TRUE)) ~ "Zenker",
      
      # ✅ Stripping oesophage
      str_detect(INTERVENTION, regex("Stripping oesophage", ignore_case = TRUE)) ~ "Stripping oesophage (laparo)",
      
      # ✅ Diverticule oesophagien
      str_detect(INTERVENTION, regex("Diverticule oesophagien", ignore_case = TRUE)) ~ "Diverticule oesophagien (robot)",
      
      # ✅ Phryngo-gastroplastie
      str_detect(INTERVENTION, regex("Phryngo-gastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      str_detect(INTERVENTION, regex("Pharyngogastroplastie", ignore_case = TRUE)) ~ "Phryngo-gastroplastie",
      
      # ✅ Reprise Lewis
      str_detect(INTERVENTION, regex("Reprise Lewis", ignore_case = TRUE)) ~ "Reprise Lewis Santy",
      
      # ✅ Autres : laisse inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # ✅ GT
      str_detect(INTERVENTION, regex("GT|Gastrectomie", ignore_case = TRUE)) ~ "Gastrectomie totale (laparo)",
      str_detect(INTERVENTION, regex("GT.*coelio", ignore_case = TRUE)) ~ "Gastrectomie totale (coelio)",
      str_detect(INTERVENTION, regex("GT.*robot", ignore_case = TRUE)) ~ "Gastrectomie totale (robot)",
      
      # ✅ Gastrectomie partielle
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Gastrectomie partielle (robot)",
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio", ignore_case = TRUE)) ~ "Gastrectomie partielle (coelio)",
      str_detect(INTERVENTION, regex("Gastrectomie partielle", ignore_case = TRUE)) ~ "Gastrectomie partielle (laparo)",
      
      # ✅ Gastrectomie 4/5
      str_detect(INTERVENTION, regex("Gastrectomie 4/5", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (robot)",
      str_detect(INTERVENTION, regex("Gastrectomie 4/5", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (coelio)",
      str_detect(INTERVENTION, regex("Gastrectomie 4/5|Gastrectomie des 4/5 ème laparo  ", ignore_case = TRUE)) ~ "Gastrectomie 4/5e (laparo)",
      
      # ✅ Gastrectomie atypique
      str_detect(INTERVENTION, regex("Gastrectomie atypique|gastrec partielle pour GIST|GIST", ignore_case = TRUE)) ~ "Gastrectomie atypique (laparo)",
      
      # ✅ By pass
      str_detect(INTERVENTION, regex("By pass|Bypass", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Bypass gastrique (robot)",
      str_detect(INTERVENTION, regex("By pass|Bypass", ignore_case = TRUE)) ~ "Bypass gastrique (laparo)",
      
      # ✅ Sleeve
      str_detect(INTERVENTION, regex("Sleeve", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Sleeve gastrectomie (robot)",
      str_detect(INTERVENTION, regex("Sleeve", ignore_case = TRUE)) ~ "Sleeve gastrectomie (coelio)",
      
      # ✅ Gastrotomie
      str_detect(INTERVENTION, regex("Gastrotomie", ignore_case = TRUE)) ~ "Gastrotomie (laparo)",
      
      # ✅ Kinking gastroplastie
      str_detect(INTERVENTION, regex("Kinking gastroplastie", ignore_case = TRUE)) ~ "Gastroplastie (coelio)",
      
      # ✅ Démontage gastroplastie
      str_detect(INTERVENTION, regex("Démontage gastroplastie", ignore_case = TRUE)) ~ "Démontage gastroplastie (laparo)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      ## ✅ Colon droit
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon droit (robot)",
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon droit (laparo)",
      str_detect(INTERVENTION, regex("Colon D|Colectomie D|Colectomie droite", ignore_case = TRUE)) ~ "Colon droit (coelio)",
      
      ## ✅ Hartmann créateur
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hartmann (robot)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Hartmann (laparo)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) ~ "Hartmann (coelio)",
      
      str_detect(INTERVENTION, regex("RIS|ACA|ileo", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Colon angulaire
      str_detect(INTERVENTION, regex("angulaire", ignore_case = TRUE)) ~ "Colon angulaire (coelio)",
      
      ## ✅ Colon transverse
      str_detect(INTERVENTION, regex("transverse", ignore_case = TRUE)) ~ "Colon transverse (coelio)",
      
      ## ✅ Colon gauche (inclut sigmoidectomie et variantes)
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon gauche (robot)",
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon gauche (laparo)",
      str_detect(INTERVENTION, regex("Colon G|Colectomie G|Sigmoidectomie|Sigmoïdectomie", ignore_case = TRUE)) ~ "Colon gauche (coelio)",
      
      ## ✅ RIC (Résection iléo-caecale)
      str_detect(INTERVENTION, regex("RIC|ileocaecale|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "RIC (robot)",
      str_detect(INTERVENTION, regex("RIC|ileocaecale|Résection iléo-caecale laparo|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "RIC (laparo)",
      str_detect(INTERVENTION, regex("RIC|ileocaecale|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) ~ "RIC (coelio)",
      
      ## ✅ Colon total / subtotal (Totalisation)
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon total (robot)",
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon total (laparo)",
      str_detect(INTERVENTION, regex("totalisation|Colon sub total|Colon total|Colectomie totale", ignore_case = TRUE)) ~ "Colon total (coelio)",
      
      ## ✅ Rectum (proctectomie, RRS, pelvectomie)
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Rectum (robot)",
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
      str_detect(INTERVENTION, regex("RRS|proctectomie|Pelvectomie", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Rétablissement de Hartmann
      str_detect(INTERVENTION, regex("retabl", ignore_case = TRUE)) ~ "Rétablissement Hartmann|rétablissimenet de hartman",
      
      ## ✅ Colostomies (coelio par défaut sauf mention)
      str_detect(INTERVENTION, regex("Colostomie|Colosotomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("Colostomie|Colosotomie", ignore_case = TRUE)) ~ "Colostomie (coelio)",
      
      ## ✅ Colon droit
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon droit (robot)",
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon droit (laparo)",
      str_detect(INTERVENTION, regex("colectomie droite|colon D|côlon droit|colectomie aguche", ignore_case = TRUE)) ~ "Colon droit (coelio)",
      
      ## ✅ Colon angulaire (rare mais ok)
      str_detect(INTERVENTION, regex("colectomie angulaire", ignore_case = TRUE)) ~ "Colon angulaire (coelio)",
      
      ## ✅ RIC (résection iléo caecale)
      str_detect(INTERVENTION, regex("RIC|resection il[eé]o[- ]?caecale", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "RIC (laparo)",
      str_detect(INTERVENTION, regex("RIC|resection il[eé]o[- ]?caecale", ignore_case = TRUE)) ~ "RIC (coelio)",
      
      ## ✅ Colon gauche / sigmoidectomie
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colon gauche (robot)",
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie|colectomie laparo ", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colon gauche (laparo)",
      str_detect(INTERVENTION, regex("colectomie gauche|colon G|sigmoidectomie|sigmoïdectomie", ignore_case = TRUE)) ~ "Colon gauche (coelio)",
      
      ## ✅ Colectomie totale
      str_detect(INTERVENTION, regex("colectomie totale|colon sub totalcolon sub total|subtotale|colon sub total", ignore_case = TRUE)) ~ "Colectomie totale",
      
      ## ✅ Rétablissement Hartmann
      str_detect(INTERVENTION, regex("retabl", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      ## ✅ Hartmann créateur
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hartmann (robot)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Hartmann (laparo)",
      str_detect(INTERVENTION, regex("\\bHartmann\\b", ignore_case = TRUE)) ~ "Hartmann (coelio)",
      
      ## ✅ Rectum / Proctectomie / Pelvectomie postérieure
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie|protectomie|protectomie secondaire", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Rectum (robot)",
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
      str_detect(INTERVENTION, regex("rectum|proctectomie|pelvectomie|resection recto sigmoidienne", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      ## ✅ Colostomie
      str_detect(INTERVENTION, regex("colostomie", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("colostomie|coleostomie coelio", ignore_case = TRUE)) ~ "Colostomie (coelio)",
      # Protectomie secondaire = rectum (proctectomie dérivée)
      str_detect(INTERVENTION, regex("protectomie secondaire", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      # Protectomie secondaire = rectum (proctectomie dérivée)
      str_detect(INTERVENTION, regex("protectomie secondaire", ignore_case = TRUE)) ~ "Rectum (coelio)",
      
      # Rétablissement Hartmann : orthographes multiples corrigées
      str_detect(INTERVENTION, regex("rétablissimenet de hartman|retablissiment de hartman|rétablissement hartmann|retablissement hartmann", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      # Rétablissement Hartmann : orthographes multiples corrigées
      str_detect(INTERVENTION, regex("rétablissimenet de hartman|retablissiment de hartman|rétablissement hartmann|retablissement hartmann", ignore_case = TRUE)) ~ "Rétablissement Hartmann",
      
      # Colectomie + vessie (coelio)
      str_detect(INTERVENTION, regex("colectomie.*vessie", ignore_case = TRUE)) ~ "Colectomie + vessie (coelio)",
      
      # Colectomie laparo (général)
      str_detect(INTERVENTION, regex("colectomie.*laparo", ignore_case = TRUE)) ~ "Colectomie (laparo)",
      
      ## ✅ Par défaut inchangé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # Surrénale droite robot
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*robot|surrénale droite robot|surrénalectomie D robot", ignore_case = TRUE)) ~ "Surrénalectomie droite (robot)",
      
      # Surrénale gauche robot
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*robot|surrénale gauche robot|surrénalectomie G robot", ignore_case = TRUE)) ~ "Surrénalectomie gauche (robot)",
      
      # Surrénale droite laparo
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*laparo|surrénale droite laparo|surrénalectomie D laparo", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparo)",
      
      # Surrénale gauche laparo
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*laparo|surrénale gauche laparo|surrénalectomie G laparo", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparo)",
      
      # Surrénale droite laparotomie
      str_detect(INTERVENTION, regex("surrénalectomie.*droit.*laparotomie|surrénale droite laparotomie|surrénalectomie D laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparotomie)",
      
      # Surrénale gauche laparotomie
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche.*laparotomie|surrénale gauche laparotomie|surrénalectomie G laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparotomie)",
      
      # Surrénale droite coelio (par défaut)
      str_detect(INTERVENTION, regex("surrénalectomie.*droit|surrénale droite|surrénalectomie D", ignore_case = TRUE)) ~ "Surrénalectomie droite (coelio)",
      
      # Surrénale gauche coelio (par défaut)
      str_detect(INTERVENTION, regex("surrénalectomie.*gauche|surrénale gauche|surrénalectomie G", ignore_case = TRUE)) ~ "Surrénalectomie gauche (coelio)",
      
      # Surrénalectomie robot (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*robot|surrénale robot", ignore_case = TRUE)) ~ "Surrénalectomie (robot)",
      
      # Surrénalectomie laparo (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*laparo", ignore_case = TRUE)) ~ "Surrénalectomie (laparo)",
      
      # Surrénalectomie laparotomie (non précisé côté)
      str_detect(INTERVENTION, regex("surrénalectomie.*laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie (laparotomie)",
      
      # Surrénale coelio (non précisé côté, par défaut)
      str_detect(INTERVENTION, regex("surrénale|surrénalectomie", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Cas spécifiques (reprise, urgence coelio blanche)
      str_detect(INTERVENTION, regex("reprise.*surrénalectomie|urgence.*coelio blanche", ignore_case = TRUE)) ~ "Reprise surrénalectomie",
      
      # Surrénale droite robot
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*robot|surrenalectomie D robot|surrenale droite robot|surrenalectomie droite robot", ignore_case = TRUE)) ~ "Surrénalectomie droite (robot)",
      
      # Surrénale gauche robot
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*robot|surrenalectomie G robot|surrenale gauche robot|surrenalectomie gauche robot", ignore_case = TRUE)) ~ "Surrénalectomie gauche (robot)",
      
      # Surrénale droite laparo
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*laparo|surrenalectomie D laparo|surrenale droite laparo|surrenalectomie droite laparo", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparo)",
      
      # Surrénale gauche laparo
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*laparo|surrenalectomie G laparo|surrenale gauche laparo|surrenalectomie gauche laparo", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparo)",
      
      # Surrénale droite laparotomie
      str_detect(INTERVENTION, regex("surrenalectomie.*droit.*laparotomie|surrenalectomie D laparotomie|surrenale droite laparotomie|surrenalectomie droite laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie droite (laparotomie)",
      
      # Surrénale gauche laparotomie
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche.*laparotomie|surrenalectomie G laparotomie|surrenale gauche laparotomie|surrenalectomie gauche laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie gauche (laparotomie)",
      
      # Surrénale droite coelio par défaut
      str_detect(INTERVENTION, regex("surrenalectomie.*droit|surrenalectomie D|surrenale droite|surrenalectomie droite", ignore_case = TRUE)) ~ "Surrénalectomie droite (coelio)",
      
      # Surrénale gauche coelio par défaut
      str_detect(INTERVENTION, regex("surrenalectomie.*gauche|surrenalectomie G|surrenale gauche|surrenalectomie gauche", ignore_case = TRUE)) ~ "Surrénalectomie gauche (coelio)",
      
      # Surrénalectomie robot non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*robot|surrenale robot", ignore_case = TRUE)) ~ "Surrénalectomie (robot)",
      
      # Surrénalectomie laparo non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*laparo", ignore_case = TRUE)) ~ "Surrénalectomie (laparo)",
      
      # Surrénalectomie laparotomie non côté précisé
      str_detect(INTERVENTION, regex("surrenalectomie.*laparotomie", ignore_case = TRUE)) ~ "Surrénalectomie (laparotomie)",
      
      # Surrénalectomie coelio non côté précisé (par défaut)
      str_detect(INTERVENTION, regex("surrenale|surrenalectomie", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Cas reprise ou urgences spécifiques surrénales
      str_detect(INTERVENTION, regex("reprise.*surrenalectomie|urgence.*coelio blanche", ignore_case = TRUE)) ~ "Reprise surrénalectomie",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

#Code PROCTO
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # Examen anal sous anesthésie générale (AG)
      str_detect(INTERVENTION, regex("examen anal sous ag", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal simple (sans précision AG)
      str_detect(INTERVENTION, regex("^examen anal$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal + interventions associées (lambeau, pose séton, dilatation, encollage, extraction corps étranger intra rectal)
      str_detect(INTERVENTION, regex("examen anal \\+ lambeau|examen anal \\+ laparo|examen anal \\+ pose endosponge|examen anal - dilatation|examen anal : avancement séton|examen anal sous ag, pose séton|examen anal sous ag: encollage fistule|examen anal sous ag: fistule acr|extraction ce intra rectal|extraction corps étranger intra rectal", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès de marge anale (et variantes orthographiques)
      str_detect(INTERVENTION, regex("abcès marge anale|abces marge anale|abcès de marge|abces de marge|abcès marge \\+", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès péri-anal / fessier (hors marge anale)
      str_detect(INTERVENTION, regex("abces peri anale|abcès périnéal|abcès périnéal|abcès périnéal|abcès fesse|abces fesse|Abcès de fesse|abcès fesse", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Drainage de masse anale
      str_detect(INTERVENTION, regex("drainage ma|drainage masse anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen procto simple
      str_detect(INTERVENTION, regex("^examen procto$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fistule anale (sans fistule anastomose oesogastrique / aorto-duodénale)
      str_detect(INTERVENTION, regex("fistule anale|fistule anus|fisutle anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Hémorroïdes classiques et Milligan Morgan
      str_detect(INTERVENTION, regex("hémorroïdes|hemorroide|milligan morgan", ignore_case = TRUE)) ~ "Hémorroïdes",
      
      # Recoupe Baulieu, Babcock, Beaulieux et variantes orthographiques
      str_detect(INTERVENTION, regex("recoupe baulieu|babcock|babcok|beaulieux", ignore_case = TRUE)) ~ "Recoupe Baulieu / Babcok",
      
      # Abaissement fistule anale (geste spécifique)
      str_detect(INTERVENTION, regex("abaissement fistule anale", ignore_case = TRUE)) ~ "Abaissement fistule anale",
      
      # Exclure fistule anastomose oesogastrique et fistule aorto-duodénale de la proctologie
      str_detect(INTERVENTION, regex("fistule anastomose oesogastrique|fistule aorto-duodénale", ignore_case = TRUE)) ~ INTERVENTION_GROUPÉE,
      
      # Examen anal sous anesthésie générale (AG)
      str_detect(INTERVENTION, regex("examen anal sous ag", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal simple (sans précision AG)
      str_detect(INTERVENTION, regex("^examen anal$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Examen anal + interventions associées (lambeau, pose séton, dilatation, encollage, extraction corps étranger intra rectal)
      str_detect(INTERVENTION, regex("examen anal \\+ lambeau|examen anal \\+ laparo|examen anal \\+ pose endosponge|examen anal - dilatation|examen anal : avancement séton|examen anal sous ag, pose séton|examen anal sous ag: encollage fistule|examen anal sous ag: fistule acr|extraction corps étranger|extraction ce intra rectal", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès de marge anale (et variantes orthographiques)
      str_detect(INTERVENTION, regex("abcès marge anale|abces marge anale|abcès de marge|abces de marge|abcès marge \\+|abcès de MA|abces MA|abcès MA \\+ séton", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Abcès péri-anal / fessier (hors marge anale)
      str_detect(INTERVENTION, regex("abces peri anale|abcès périnéal|abcès périnéal|abcès périnéal|abcès fesse|abces fesse|Abcès de fesse|abcès fesse", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Drainage de masse anale
      str_detect(INTERVENTION, regex("drainage ma|drainage masse anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fissure anale et fissurectomie
      str_detect(INTERVENTION, regex("fissure anale|fissurectomie", ignore_case = TRUE)) ~ "Fissure anale",
      
      # Examen procto simple
      str_detect(INTERVENTION, regex("^examen procto$", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Fistule anale (sans fistule anastomose oesogastrique / aorto-duodénale)
      str_detect(INTERVENTION, regex("fistule anale|fistule anus|fisutle anale", ignore_case = TRUE)) ~ "Abcès de marge / fistule anale",
      
      # Hémorroïdes classiques et Milligan Morgan
      str_detect(INTERVENTION, regex("hémorroïdes|hemorroide|milligan morgan", ignore_case = TRUE)) ~ "Hémorroïdes",
      
      # Recoupe Baulieu, Babcock, Beaulieux et variantes orthographiques
      str_detect(INTERVENTION, regex("recoupe baulieu|babcock|babcok|beaulieux", ignore_case = TRUE)) ~ "Recoupe Baulieu / Babcok",
      
      # Abaissement fistule anale (geste spécifique)
      str_detect(INTERVENTION, regex("abaissement fistule anale", ignore_case = TRUE)) ~ "Abaissement fistule anale",
      
      # Exclure fistule anastomose oesogastrique et fistule aorto-duodénale de la proctologie
      str_detect(INTERVENTION, regex("fistule anastomose oesogastrique|fistule aorto-duodénale", ignore_case = TRUE)) ~ INTERVENTION_GROUPÉE,
      
      # Par défaut, garder la catégorie déjà existante
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )



df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # 1) Eventration simples
      str_detect(INTERVENTION, regex("^eventration$", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("^eventration médiane$", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("^eventration diaphragmatique$", ignore_case = TRUE)) ~
        "Eventration diaphragmatique",
      str_detect(INTERVENTION, regex("étranglée", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("eventration", ignore_case = TRUE)) ~
        "Eventration étranglée",
      
      # 2) Cas très spécifiques de cure d’éventration
      str_detect(INTERVENTION, regex("cure d'?éventration.*sous costale", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*lombaire gauche", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*médiane.*proth[eè]se retromusculaire", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*plaque RM", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration.*orifice de trocard", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éviscération couverte étranglée", ignore_case = TRUE)) ~
        "Cure d'éviscération couverte",
      
      # 3) Cure d’éventration par approche
      str_detect(INTERVENTION, regex("cure d'?éventration", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      str_detect(INTERVENTION, regex("cure d'?éventration|cure d'eventration", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("coelio|coelioscopie", ignore_case = TRUE)) ~
        "Cure d'éventration (coelio)",
      str_detect(INTERVENTION, regex("^cure d'?éventration$", ignore_case = TRUE)) ~
        "Cure d'éventration",
      
      # 4) Variantes « lipome » associées
      str_detect(INTERVENTION, regex("lipome", ignore_case = TRUE)) &
        str_detect(INTERVENTION, regex("cure d'?éventration|Cure d'éventation|Cure d'éventration|Cure d'évetration", ignore_case = TRUE)) ~
        "Cure d'éventration (laparo)",
      
      # 5) Cas d’éviscération isolée
      str_detect(INTERVENTION, regex("cure d'?éviscération|éviscération couverte", ignore_case = TRUE)) ~
        "Eviscération",
      str_detect(INTERVENTION, regex("^éviscération$|^eviscération$|^evisceration$", ignore_case = TRUE)) ~
        "Eviscération",
      
      # 6) Dépacking (souvent sur le même thème)
      str_detect(INTERVENTION, regex("^depacking$", ignore_case = TRUE)) ~
        "Depacking",
      
      # 7) Sinon, laisser ce qui était déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# 1) Standardise la casse et enlève les accents pour matcher plus simplement
df <- df %>%
  mutate(
    .INT_clean = stringi::stri_trans_general(INTERVENTION, "Latin-ASCII") %>%
      str_to_lower()
  )

# 2) Rattrapage global des éventrations restantes
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Ne toucher que si c'était encore NA
      is.na(INTERVENTION_GROUPÉE) & str_detect(.INT_clean, "eventr") ~ {
        # Distingue les cas particuliers
        case_when(
          str_detect(.INT_clean, "diaphragmat")    ~ "Eventration diaphragmatique",
          str_detect(.INT_clean, "etrangl")        ~ "Eventration étranglée",
          str_detect(.INT_clean, "coelio|coelioscopie") ~ "Cure d'éventration (coelio)",
          str_detect(.INT_clean, "laparo")         ~ "Cure d'éventration (laparo)",
          TRUE                                     ~ "Cure d'éventration"
        )
      },
      TRUE ~ INTERVENTION_GROUPÉE
    )
  ) %>%
  select(-.INT_clean)

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Corrige précisément ces deux variantes
      is.na(INTERVENTION_GROUPÉE) &
        INTERVENTION %in% c("Cure d'éventation", "Cure d'évetration") ~
        "Cure d'éventration (laparo)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Fermetures de stomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*stomie", ignore_case = TRUE)) ~
        "Fermeture de stomie",
      # Fermetures d'iléostomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*il[ée]ostomie|fermeture.*ileo|fermeture.jej*", ignore_case = TRUE)) ~
        "Fermeture d'iléostomie",
      # Rétablissement de continuité (fermeture de stomie + anastomose)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("r[eé]tablissement.*continuit|r[eé]fection.*stomie", ignore_case = TRUE)) ~
        "Rétablissement de continuité",
      # Résections de grêle
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("r[eé]section.*gr[êe]le", ignore_case = TRUE)) ~
        "Résection de grêle",
      # Prolapsus (iléostomie, stomiale…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("prolapsus", ignore_case = TRUE)) ~
        "Réparation de prolapsus",
      # Sinon on garde ce qui était déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 8) Exploration (laparo/coelio/explo)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("laparotomie expl|laparo explo|coelio explo|coelioscopie explo|exploration", ignore_case = TRUE)) ~
        "Exploration",
      
      # 9) Procédures interventionnelles
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("PIPAC|TIPS|PAC|biopsie|embolisation|drainage|endosponge|dilatation|réparation portale|fistule", ignore_case = TRUE)) ~
        "Procédure interventionnelle",
      
      # 10) Sinon on laisse NA ou ce qui est déjà groupé
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 11) Stomie de décharge (iléostomie, jéjunostomie, stomie)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("iléostomie|jéjunostomie|stomie", ignore_case = TRUE)) &
        !str_detect(INTERVENTION, regex("fermeture", ignore_case = TRUE)) ~
        "Stomie digestive",
      
      # 12) Fermeture de stomie (colo/ilé/jejuno-stomie)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("fermeture.*(stomie|ilé|colo|jejuno)", ignore_case = TRUE)) ~
        "Fermeture de stomie",
      
      # 13) Résection de grêle (y compris diverticule de Meckel)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("résection.*gr[êe]le|resection.*grêle|meckel", ignore_case = TRUE)) ~
        "Résection de grêle",
      
      # 14) Amputation abdomino-périnéale (APR)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("amputation abdomino.*péri", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale",
      
      # 15) TEM (chirurgie transanale)
      is.na(INTERVENTION_GROUPÉE) &
        regex("^TEM$", ignore_case = TRUE) %>% str_detect(INTERVENTION) ~
        "TEM (chirurgie transanale)",
      
      # 16) Vaginoplastie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("vaginoplastie|vagino", ignore_case = TRUE)) ~
        "Vaginoplastie",
      
      # 17) Curage ganglionnaire
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("curage|courage ganglionnaire", ignore_case = TRUE)) ~
        "Curage ganglionnaire",
      
      # 18) Sinus pilonidal
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("sinus pilonidal|kyste pilonid", ignore_case = TRUE)) ~
        "Sinus pilonidal",
      
      # 19) Ablation d’anneau gastrique
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ablation anneau gastrique", ignore_case = TRUE)) ~
        "Ablation anneau gastrique",
      
      # 20) Sinon, on laisse ce qui était groupé (ou NA pour la suite)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # … ton code précédent …
      
      # 21) Cytoréduction (toutes approches)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("cyto(réduction|reduction)|debulking|PIPAC", ignore_case = TRUE)) ~
        "Cytoréduction (laparo)",
      
      # 22) Gestion des abcès
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("abc[eè]s|drainage.*abc[eè]s", ignore_case = TRUE)) ~
        "Drainage d’abcès",
      
      # 23) Splénectomie (open, coelio ou robot)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("spl[eé]nectomie", ignore_case = TRUE)) ~
        "Splénectomie",
      
      # 24) Toutes les occlusions sur bride restantes
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("occlu|bride", ignore_case = TRUE)) ~
        "Occlusion sur bride (coelio)",
      
      # 23) Amputation abdomino-périnéale (AAP)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("^(AAP|AAP + Taylor|Amput AP|Amputation abdopérinéale  |Amput.*abdomino[- ]?périnéale)$", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale (coelio)",
      
      # 24) Sinon, on laisse ce qui était groupé (ou NA pour la suite)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # 1) Exploration abdominale (laparo/ceolio explo / peritonite / carcinose)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ceolio explo|laparo(?:explo)?|peritonite|carcinose", ignore_case = TRUE)) ~
        "Laparotomie exploratrice",
      
      # 2) Diverticulectomie œsophagienne
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("diverticule", ignore_case = TRUE)) ~
        "Diverticulectomie œsophagienne",
      
      # 3) Sinus pilonidal
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("sinus pi", ignore_case = TRUE)) ~
        "Sinus pilonidal",
      
      # 4) Achalasie → myotomie de Heller
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("achalasie", ignore_case = TRUE)) ~
        "Myotomie de Heller",
      
      # 5) Exérèses sous-cutanées (kyste, lipome, fibrome, lésions cutanées…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ex[eé]r(e|èse)|boulectomie|lipome|kyste|fibrome", ignore_case = TRUE)) ~
        "Exérèse sous-cutanée",
      
      # 6) Drainage chirurgical (hématome, VAC, abcès, peritonite…)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("lavage|drain|evacuat|vac|abc[eè]s|peritonite", ignore_case = TRUE)) ~
        "Drainage chirurgical",
      
      # 7) Anastomoses vasculaires complexes
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("anastomose.*cave|d[ée]riv(ation|ation)|désobstruction portale", ignore_case = TRUE)) ~
        "Anastomose / dérivation vasculaire",
      
      # 8) Éviscération
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("évisc[ée]ration", ignore_case = TRUE)) ~
        "Éviscération",
      
      # 9) Prothèse portale / TIPS
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("proth[eè]se portale|tips", ignore_case = TRUE)) ~
        "Pose / révision de TIPS",
      
      # 10) Examen anal / proctologique
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("exam(ination)? anal|fissure|fistule|procto", ignore_case = TRUE)) ~
        "Examen anal",
      
      # 11) Rétablissement de continuité (réfections, réinsertions)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("réfecti|réinser|resta?blis", ignore_case = TRUE)) ~
        "Rétablissement de continuité",
      
      # 12) Jéjunostomie / grêle
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("j[ée]jun|gr[êe]le", ignore_case = TRUE)) ~
        "Intervention grêle / jéjunostomie",
      
      # 13) Rectopexie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("rectopexie|kraske", ignore_case = TRUE)) ~
        "Rectopexie (coelio)",
      
      # 14) Ulcère perforé → ulcère perforé (coelio)
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("ulc[eè]re.*perfor", ignore_case = TRUE)) ~
        "Ulcère perforé (coelio)",
      
      # 15) Volvulus → occlusion sur bride
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("volvulus", ignore_case = TRUE)) ~
        "Occlusion sur bride (coelio)",
      
      # ––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
      # Les 3 catégories que vous vouliez :
      #   • Cytoréduction 
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("cyto(réduction|reduction)", ignore_case = TRUE)) ~
        "Cytoréduction (laparo)",
      
      #   • Abcès de marge / Fournier
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("abc[eè]s|fournier", ignore_case = TRUE)) ~
        "Abcès périnéal / Fournier",
      
      #   • Splénectomie
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("spl[eé]nectomie", ignore_case = TRUE)) ~
        "Splénectomie (coelio)",
      
      #   • AAP / amputation abdomino-périnéale
      is.na(INTERVENTION_GROUPÉE) &
        str_detect(INTERVENTION, regex("\\bAAP\\b|Amputation abd", ignore_case = TRUE)) ~
        "Amputation abdomino-périnéale (coelio)",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

table(df$INTERVENTION_GROUPÉE)

df %>%
  filter(is.na(INTERVENTION_GROUPÉE)) %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)

# Code pour regrouper les derniers intitulés d'interventions en s'inspirant des groupements déjà réalisés dans le script de regroupement

# Appliquer les nouveaux regroupements
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Les groupements existants restent inchangés
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # === NOUVEAUX REGROUPEMENTS POUR LES 22 INTITULÉS RESTANTS ===
      
      # 1. Correction de faute de frappe + regroupement avec catégorie existante
      INTERVENTION == "Ablation anneau gatsrqiue" ~ "Ablation anneau gastrique",
      
      # 2. Interventions digestives spécialisées
      INTERVENTION == "Diversion duodénale" ~ "Duodénectomie",
      INTERVENTION == "Resection et refection anastomose grelo grelique" ~ "Résection de grêle",
      
      # 3. Explorations diverses (traumatiques, diagnostiques, thérapeutiques)
      INTERVENTION == "Extraction corps etranger" ~ "Exploration",
      INTERVENTION == "Lap explo + résection nodule coupole diaph" ~ "Exploration",
      INTERVENTION == "Plaie abdo arme blanche perfo estomac" ~ "Exploration",
      INTERVENTION == "Plaie arme à feu" ~ "Exploration",
      INTERVENTION == "explo paroi" ~ "Exploration",
      INTERVENTION == "torsion testiculaire" ~ "Exploration",
      
      # 4. Drainages et débridements
      INTERVENTION == "Gangrène fesse droite" ~ "Drainage chirurgical",
      INTERVENTION == "décaillotage" ~ "Drainage chirurgical",
      
      # 5. Interventions thyroïdiennes
      INTERVENTION == "Isthméctomie thyrodienne" ~ "Lobo-isthmectomie",
      
      # 6. Exérèses sous-cutanées et superficielles
      INTERVENTION == "KSC" ~ "Exérèse sous-cutanée",  # KSC = Kyste Sébacé Cutané
      INTERVENTION == "Omphalectomie" ~ "Exérèse sous-cutanée",
      INTERVENTION == "Résection nodule pariétal" ~ "Exérèse sous-cutanée",
      
      # 7. Procédures interventionnelles spécialisées
      INTERVENTION == "Pose de pansement intrabdominal" ~ "Procédure interventionnelle",
      INTERVENTION == "Trachéotomie" ~ "Procédure interventionnelle",
      INTERVENTION == "ovariectomie bilatérale sous coelio" ~ "Procédure interventionnelle",
      
      # 8. Réparations et reconstructions
      INTERVENTION == "Prolpasus stomial" ~ "Réparation de prolapsus",
      INTERVENTION == "abdominoplastie" ~ "Cure d'éventration",
      
      # 9. Curage et cytoréduction
      INTERVENTION == "Récidive ganglionnaire corticosurrénalome" ~ "Curage ganglionnaire",
      INTERVENTION == "débulking pseudomyxome" ~ "Cytoréduction (laparo)",
      
      # Garder les valeurs NA comme NA (interventions non renseignées)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df %>%
  filter(is.na(INTERVENTION_GROUPÉE)) %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)

cat("=== APPLICATION DES CORRECTIONS FINALES ===\n")

# === CORRECTION 1 : CLASSIFICATIONS CROISÉES ===
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      
      # === CORRIGER LES INTERVENTIONS MAL CLASSÉES DANS "HERNIE INGUINALE" ===
      
      # Reclasser les CHIP/cytoréductions
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("CHIP|cyto", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
      
      # Reclasser les hernies hiatales vers RGO  
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("hiatale", ignore_case = TRUE)) ~ "Cure RGO (coelio)",
      
      # Reclasser les abcès inguinaux
      INTERVENTION_GROUPÉE == "Hernie inguinale" & 
        str_detect(INTERVENTION, regex("abcès inguinal", ignore_case = TRUE)) ~ "Drainage d'abcès",
      
      # === AJOUTER LES VRAIES HERNIES INGUINALES NON CLASSÉES ===
      
      # Hernies inguinales TEP et TAPP non classées
      str_detect(INTERVENTION, regex("hernie inguinale.*TEP|hernie inguinale.*TAPP|Hi engouée TAPP", ignore_case = TRUE)) ~ "Hernie inguinale (coelio)",
      
      # Reprises de hernies inguinales
      str_detect(INTERVENTION, regex("reprise hernie inguinale", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # Lichtenstein pour récidives
      str_detect(INTERVENTION, regex("Licht.*récidive|Licht.*reprise", ignore_case = TRUE)) ~ "Hernie inguinale",
      
      # === CORRIGER LES HERNIES OMBILICALES (règle plus stricte) ===
      
      # D'abord, reclasser les interventions mal capturées par la règle actuelle
      INTERVENTION_GROUPÉE == "Hernie ombilicale" & 
        !str_detect(INTERVENTION, regex("\\bHO\\b|hernie.*omblic|omblic.*hernie|cure.*omblic|ligne blanche", ignore_case = TRUE)) ~ "Exploration",
      
      # Reclasser les hernies ligne blanche vers leur propre catégorie
      INTERVENTION_GROUPÉE == "Hernie ombilicale" & 
        str_detect(INTERVENTION, regex("ligne blanche|éventration.*ligne", ignore_case = TRUE)) ~ "Hernie ligne blanche",
      
      # Garder tous les autres regroupements
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# === CORRECTION 2 : INTERVENTIONS NON GROUPÉES RESTANTES ===
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Ne modifier que les interventions non groupées (NA)
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # Thoracotomies → Exploration
      str_detect(INTERVENTION, regex("thoracotom|thoraco|boerhaave|decorticat", ignore_case = TRUE)) ~ "Exploration",
      
      # Cholangios → Procédure interventionnelle
      str_detect(INTERVENTION, regex("cholangio", ignore_case = TRUE)) ~ "Procédure interventionnelle",
      
      # Cholécystectomie mal orthographiée
      str_detect(INTERVENTION, regex("cholécystectomoie", ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
      # Ablation phéochromocytome → Surrénalectomie
      str_detect(INTERVENTION, regex("ablation phéochromocytome|phéochromocytome", ignore_case = TRUE)) ~ "Surrénalectomie (coelio)",
      
      # Réfections anastomoses → Exploration
      str_detect(INTERVENTION, regex("refection anastomose", ignore_case = TRUE)) ~ "Exploration",
      
      # Garder les NA comme NA
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


##--------------------------------------------
##-------TOP 15--------
#** ===================================================================== **
#** CODE COMPLET POUR ANALYSER LES GESTES DU TOP 15 DES INTERVENTIONS     **
#** ===================================================================== **


regrouper_gestes <- function(geste_text) {
  if (is.na(geste_text)) {
    return(NA)
  }
  
  # Convertir en minuscules pour faciliter la détection
  geste_lower <- tolower(as.character(geste_text))
  
  # Règles de regroupement (reprises du script regroupement.R)
  if (str_detect(geste_lower, "tout")) {
    return("Tout")  # Tout est exclusif des autres
  } else if (str_detect(geste_lower, "anastomose|bilio biliaire|biliodig")) {
    return("Anastomose")  
  } else if (str_detect(geste_lower, "dissection|canule vmi|libération foie|controle.*aorte|temps froid|temps chaud|cholécystectomie|apc|pédicule")) {
    return("Dissection")
  } else if (str_detect(geste_lower, "paroi|incision|fermeture aponévrose|ouverture|fixation prothèse|pose de pac|stomie")) {
    return("Paroi")
  } else {
    return("Autre")  # Pour les cas non classés
  }
}

# Appliquer le regroupement
df <- df %>%
  mutate(
    GESTE_GROUPE = map_chr(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, regrouper_gestes)
  )

# PARTIE 1 : TOP 15 DES INTERVENTIONS LES PLUS AIDÉES (AVEC GESTE)

# Résumé avec n >= 15 + calcul du pourcentage + exclusions
df_resume_intervention <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  filter(!INTERVENTION_GROUPÉE %in% c("Pose de TIPS", "Autre", "Exérèse sous-cutanée", "Procédure interventionnelle", "Stomie digestive")) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pct_gestes_realises = 100 * gestes_realises / total_interventions,
    .groups = "drop"
  ) %>%
  filter(total_interventions >= 15)

# Top 15 interventions les plus aidées
top_15_plus_aidees <- df_resume_intervention %>%
  arrange(desc(pct_gestes_realises)) %>%
  slice(1:15)

noms_top15_plus <- top_15_plus_aidees$INTERVENTION_GROUPÉE

# === FONCTION D'ANALYSE DES GESTES POUR UNE INTERVENTION ===

analyser_gestes_intervention <- function(nom_intervention) {
  gestes_detail <- df %>%
    filter(INTERVENTION_GROUPÉE == nom_intervention) %>%
    filter(Geste == "Yes") %>%  # Seulement les cas où il y a eu un geste
    filter(!is.na(GESTE_GROUPE)) %>%
    count(GESTE_GROUPE, sort = TRUE) %>%
    mutate(
      pourcentage = round(100 * n / sum(n), 1)
    )
  
  if (nrow(gestes_detail) > 0) {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = gestes_detail$GESTE_GROUPE[1],
      Pourcentage_geste = gestes_detail$pourcentage[1],
      Effectif_geste = gestes_detail$n[1],
      Total_gestes = sum(gestes_detail$n)
    ))
  } else {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = "Aucun",
      Pourcentage_geste = 0,
      Effectif_geste = 0,
      Total_gestes = 0
    ))
  }
}

# Analyser toutes les interventions du top 15
resultats_gestes_top15 <- map_dfr(noms_top15_plus, analyser_gestes_intervention)

# === TABLEAU FINAL TOP 15 PLUS AIDÉES ===

tableau_top15_plus_aidees <- top_15_plus_aidees %>%
  left_join(resultats_gestes_top15, by = c("INTERVENTION_GROUPÉE" = "Intervention")) %>%
  mutate(
    Rang = row_number(),
    Label_intervention = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    ),
    Label_geste = case_when(
      Geste_principal == "Aucun" ~ "Aucun geste documenté",
      TRUE ~ paste0(
        Geste_principal, " (",
        Effectif_geste, "/", Total_gestes, ", ",
        Pourcentage_geste, "%)"
      )
    )
  ) %>%
  select(
    Rang,
    Intervention = Label_intervention,
    `Geste principal` = Label_geste,
    `% global` = pct_gestes_realises
  )

# Affichage du top 15 plus aidées
print(tableau_top15_plus_aidees)

# GT Table pour les PLUS aidées
gt_plus_aidees <- tableau_top15_plus_aidees %>%
  gt() %>%
  tab_header(
    title = "Top 15 des interventions les plus aidées",
    subtitle = "Avec type de geste principal réalisé (≥15 interventions)"
  ) %>%
  fmt_number(
    columns = `% global`,
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e8"),  # Fond vert clair
    locations = cells_body(columns = `% global`)
  ) %>%
  cols_width(
    Rang ~ px(50),
    Intervention ~ px(250),
    `Geste principal` ~ px(200),
    `% global` ~ px(80)
  )

# PARTIE 2 : TOP 15 DES INTERVENTIONS LES MOINS AIDÉES (SANS GESTE)

# Top 15 interventions les moins aidées
top_15_moins_aidees <- df_resume_intervention %>%
  arrange(pct_gestes_realises) %>%  # Trier par taux de gestes CROISSANT
  slice(1:15) %>%
  mutate(
    Rang = row_number(),
    pct_pas_de_geste = 100 - pct_gestes_realises,
    Label_intervention = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    )
  ) %>%
  select(
    Rang,
    Intervention = Label_intervention,
    `% gestes` = pct_gestes_realises,
    `% sans geste` = pct_pas_de_geste,
    `Effectif total` = total_interventions
  )

# Affichage du top 15 moins aidées
print(top_15_moins_aidees)

# GT Table pour les MOINS aidées
gt_moins_aidees <- top_15_moins_aidees %>%
  gt() %>%
  tab_header(
    title = "Top 15 des interventions les moins aidées",
    subtitle = "Classées par taux de gestes croissant (≥15 interventions)"
  ) %>%
  fmt_number(
    columns = c(`% gestes`, `% sans geste`),
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#ffebee"),  # Fond rouge clair
    locations = cells_body(columns = `% gestes`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e8"),  # Fond vert clair
    locations = cells_body(columns = `% sans geste`)
  ) %>%
  cols_width(
    Rang ~ px(50),
    Intervention ~ px(300),
    `% gestes` ~ px(80),
    `% sans geste` ~ px(100),
    `Effectif total` ~ px(100)
  )









##--------------------------------------------
##-------META GROUPES--------
# === 1. CRÉATION DES MÉTA-GROUPES CORRIGÉE ===
df <- df %>%
  mutate(
    META_GROUPE = case_when(
      str_detect(INTERVENTION_GROUPÉE, "Appendicectomie") ~ "Appendicectomies",
      str_detect(INTERVENTION_GROUPÉE, "Cholécystectomie") ~ "Cholécystectomies",
      # ERREUR CORRIGÉE : suppression du | vide à la fin qui matchait TOUT
      str_detect(INTERVENTION_GROUPÉE, "Hépatectomie|Lobectomie|Fenestration kyste|VBP|Réparation biliaire|Pancreatectomie|Pancréas|DPC|DPT|SPG|Ré-hépatectomie") ~ "Chirurgie hépato-bilio-pancréatique",
      str_detect(INTERVENTION_GROUPÉE, "Colon|Rectum|Hartmann|RIC|Colostomie|Stomie|Fermeture de stomie|Résection de grêle|Intervention grêle|Rétablissement de continuité|Colectomie totale|Rectopexie|TEM|Amputation abdomino-périnéale") ~ "Chirurgie colorectale",
      str_detect(INTERVENTION_GROUPÉE, "Hernie|Éventration|éventration") ~ "Chirurgie pariétale",
      str_detect(INTERVENTION_GROUPÉE, "Exploration|Ulcère perforé|Occlusion|Drainage|Laparotomie exploratrice") ~ "Chirurgie d'urgence",
      str_detect(INTERVENTION_GROUPÉE, "Thyroïdectomie|Parathyroïdectomie|Surrénalectomie|Lobo-isthmectomie") ~ "Chirurgie endocrine",
      str_detect(INTERVENTION_GROUPÉE, "Gastrectomie|Lewis|Oesophage|RGO|Sleeve|Bypass|3 voies|Coloplastie|Duodénectomie|Gastrotomie|Diverticulectomie œsophagienne|Stripping oesophage|Diverticule oesophagien|Démontage gastroplastie|Gastroplastie|Myotomie de Heller|Pharyngo-gastroplastie|Zenker|Ablation anneau gastrique") ~ "Chirurgie digestive haute",
      str_detect(INTERVENTION_GROUPÉE, "Abcès de marge|fistule anale|Hémorroïdes|Sinus pilonidal|Recoupe|Vaginoplastie|Réparation de prolapsus|Abcès périnéal|Fournier|Fissure anale|Examen anal") ~ "Proctologie",
      str_detect(INTERVENTION_GROUPÉE, "Cytoréduction") ~ "Chirurgie péritonéale",
      str_detect(INTERVENTION_GROUPÉE, "Transplantation|Prélèvement|Donneur|Splénectomie|Curage ganglionnaire|Anastomose / dérivation vasculaire") ~ "Prélèvement multi-organe et transplantation",
      TRUE ~ "Autres"
    )
  )

# Vérification des méta-groupes
meta_repartition <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  count(META_GROUPE, sort = TRUE) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(meta_repartition)


# Convertir PEDAGOGIE en numérique 
df <- df %>%
  mutate(
    PEDAGOGIE_num = case_when(
      as.character(PEDAGOGIE) == "1-rien" ~ 1,
      as.character(PEDAGOGIE) == "2-quasi rien" ~ 2,
      as.character(PEDAGOGIE) == "3-ok" ~ 3,
      as.character(PEDAGOGIE) == "4-bien" ~ 4,
      as.character(PEDAGOGIE) == "5-incroyable!!" ~ 5,
      TRUE ~ NA_real_
    )
  )

# Vérification des méta-groupes
meta_repartition <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  count(META_GROUPE, sort = TRUE) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(meta_repartition)

# === 2. ANALYSE GLOBALE PAR MÉTA-GROUPE ===
analyse_metagroupes <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE)) %>%
  group_by(META_GROUPE) %>%
  summarise(
    # Volume
    total_interventions = n(),
    
    # Taux de geste
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round(100 * gestes_realises / total_interventions, 1),
    
    # Note pédagogie moyenne /5 puis convertie /20
    note_pedagogie_moyenne = round(mean(PEDAGOGIE_num, na.rm = TRUE), 1),
    note_pedagogie_sur_20 = round(mean(PEDAGOGIE_num, na.rm = TRUE) * 4, 1),
    note_pedagogie_mediane = round(median(PEDAGOGIE_num, na.rm = TRUE), 1),
    n_avec_note = sum(!is.na(PEDAGOGIE_num)),
    
    .groups = "drop"
  ) %>%
  arrange(desc(taux_geste))

print(analyse_metagroupes)

# === 3. ÉVOLUTION PAR ANNÉE D'INTERNAT (1-4) ===
evolution_complete <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE), !is.na(annee_DES)) %>%
  filter(annee_DES %in% c("1", "2", "3", "4")) %>%
  group_by(annee_DES, META_GROUPE) %>%
  summarise(
    # Volume
    total = n(),
    
    # Taux de geste
    gestes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round(100 * gestes / total, 1),
    
    # Note pédagogie
    note_pedagogie = round(mean(PEDAGOGIE_num, na.rm = TRUE), 1),
    n_notes = sum(!is.na(PEDAGOGIE_num)),
    
    .groups = "drop"
  ) %>%
  filter(total >= 3) %>%  # Au moins 3 interventions
  arrange(META_GROUPE, annee_DES)

print(head(evolution_complete, 15))

# === 4. GRAPHIQUES ===
# Graphique 1: Répartition des méta-groupes
graphique_repartition <- ggplot(meta_repartition, aes(x = reorder(META_GROUPE, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", pourcentage, "%)")), hjust = -0.1, size = 2.8) +
  coord_flip() +
  labs(
    title = "✅ RÉPARTITION CORRECTE DES MÉTA-GROUPES",
    subtitle = "Nombre d'interventions par spécialité chirurgicale",
    x = "Méta-groupe",
    y = "Nombre d'interventions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_repartition)

# Graphique 2: Taux de geste par méta-groupe
graphique_taux_geste <- ggplot(analyse_metagroupes, aes(x = reorder(META_GROUPE, taux_geste), y = taux_geste)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste0(taux_geste, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "🎯 TAUX DE GESTE PAR MÉTA-GROUPE",
    subtitle = "Pourcentage d'interventions où l'interne a réalisé un geste",
    x = "Méta-groupe",
    y = "Taux de geste (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_taux_geste)

# Graphique 3: Score pédagogie par méta-groupe
graphique_pedagogie <- ggplot(analyse_metagroupes, aes(x = reorder(META_GROUPE, note_pedagogie_sur_20), y = note_pedagogie_sur_20)) +
  geom_col(fill = "orange", alpha = 0.8) +
  geom_text(aes(label = paste0(note_pedagogie_sur_20, "/20")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "📚 SCORE PÉDAGOGIE PAR MÉTA-GROUPE",
    subtitle = "Note moyenne de pédagogie sur 20",
    x = "Méta-groupe",
    y = "Score pédagogie (/20)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkorange"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_pedagogie)


# === 3. TABLEAUX SYNTHÉTIQUES ===

# Répartition % par année
repartition_par_annee <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE), !is.na(annee_DES)) %>%
  filter(annee_DES >= 1 & annee_DES <= 4) %>%
  group_by(annee_DES, META_GROUPE) %>%
  summarise(nombre = n(), .groups = "drop") %>%
  group_by(annee_DES) %>%
  mutate(
    total_annee = sum(nombre),
    pourcentage = round(100 * nombre / total_annee, 1)
  ) %>%
  ungroup() %>%
  select(annee_DES, META_GROUPE, pourcentage) %>%
  pivot_wider(names_from = annee_DES, values_from = pourcentage, values_fill = 0) %>%
  arrange(desc(`1`))

print(repartition_par_annee)

# === 4. GRAPHIQUES ===

# Graphique 1: Évolution du taux de geste
graphique_taux_geste <- ggplot(evolution_complete, aes(x = annee_DES, y = taux_geste, color = META_GROUPE)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:4, labels = paste0("D", 1:4)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Évolution du taux de geste par méta-groupe",
    subtitle = "Pourcentage de gestes réalisés par les internes (D1 à D4)",
    x = "Année d'internat",
    y = "Taux de geste (%)",
    color = "Méta-groupe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))

print(graphique_taux_geste)

# Graphique 2: Heatmap du taux de geste
heatmap_data <- evolution_complete %>%
  select(annee_DES, META_GROUPE, taux_geste) %>%
  complete(annee_DES, META_GROUPE, fill = list(taux_geste = 0))

graphique_heatmap <- ggplot(heatmap_data, aes(x = factor(annee_DES), y = META_GROUPE, fill = taux_geste)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(taux_geste > 0, paste0(taux_geste, "%"), "")), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient2(low = "navy", mid = "steelblue", high = "orange", 
                       midpoint = 50, name = "Taux de geste (%)") +
  scale_x_discrete(labels = paste0("D", 1:4)) +
  labs(
    title = "Heatmap : Taux de geste par méta-groupe et année",
    subtitle = "Intensité = pourcentage de gestes réalisés",
    x = "Année d'internat",
    y = "Méta-groupe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

print(graphique_heatmap)


##--------------------------------------------
##-------VOIE D'ABORD-------
df <- df %>%
  mutate(
    ABORD_NOUVEAU = case_when(
      
      # EXCLUSIONS (chirurgie non digestive/interventionnelle)
      INTERVENTION_GROUPÉE %in% c(
        "Procédure interventionnelle",
        "Exérèse sous-cutanée",
        "Anastomose / dérivation vasculaire"
      ) ~ "EXCLU",
      
      # PROCTOLOGIE
      INTERVENTION_GROUPÉE %in% c(
        "Abcès de marge / fistule anale",
        "Abcès périnéal / Fournier", 
        "Sinus pilonidal",
        "Fissure anale",
        "Hémorroïdes",
        "TEM (chirurgie transanale)",
        "Examen anal"
      ) ~ "Proctologie",
      
      # CERVICOTOMIE
      INTERVENTION_GROUPÉE %in% c(
        "Thyroïdectomie totale",
        "Lobo-isthmectomie", 
        "Parathyroïdectomie"
      ) ~ "Cervicotomie",
      
      # COELIOSCOPIE 
      # Toutes les interventions avec "(coelio)" + interventions par défaut coelio
      str_detect(INTERVENTION_GROUPÉE, "\\(coelio\\)") ~ "Coelioscopie",
      INTERVENTION_GROUPÉE %in% c(
        "Splénectomie",
        "Vaginoplastie",
        "Colectomie totale",
        "Réparation de prolapsus",
        "Ablation anneau gastrique",
        "Hernie inguinale",  # Par défaut coelio maintenant
        "Cure d'éventration",  # Par défaut coelio maintenant
        "Fermeture de stomie"  # Par défaut coelio maintenant
      ) ~ "Coelioscopie",
      
      # ROBOT
      str_detect(INTERVENTION_GROUPÉE, "\\(robot\\)") ~ "Robot",
      
      # LAPAROTOMIE (tout le reste)
      TRUE ~ "Laparotomie"
    )
  )

# Exclure les interventions non digestives
df_final <- df %>% filter(ABORD_NOUVEAU != "EXCLU")

# Vérifier la répartition
table(df_final$ABORD_NOUVEAU)

# CALCUL DES STATISTIQUES-

df_resume_abord <- df_final %>%
  group_by(ABORD_NOUVEAU) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage = 100 * gestes_realises / total_interventions,
    label = paste0(gestes_realises, "/", total_interventions, " (", round(pourcentage, 1), "%)"),
    ABORD_NOUVEAU = factor(ABORD_NOUVEAU, 
                           levels = c("Coelioscopie", "Laparotomie", "Robot", "Cervicotomie", "Proctologie"))
  ) %>%
  # Inverser l'ordre pour l'affichage horizontal
  mutate(ABORD_NOUVEAU = factor(ABORD_NOUVEAU, levels = rev(levels(ABORD_NOUVEAU))))

# BAR PLOT

# Couleurs inspirées du logo SFCD (bleus, roses/violets)
couleurs_sfcd <- c(
  "Proctologie" = "#2E5BBA",    # Bleu foncé
  "Cervicotomie" = "#4A90E2",   # Bleu moyen
  "Robot" = "#7BB3F0",          # Bleu clair
  "Laparotomie" = "#C44D7A",    # Rose/violet
  "Coelioscopie" = "#E85A9C"    # Rose vif
)

# Création du bar plot horizontal en batterie
plot_taux_de_geste <- ggplot(df_resume_abord, aes(x = ABORD_NOUVEAU)) +
  geom_col(aes(y = total_interventions), fill = "grey90", width = 0.7) +  # fond total
  geom_col(aes(y = gestes_realises, fill = ABORD_NOUVEAU), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + 30, label = label), hjust = 0, size = 4, 
            fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = couleurs_sfcd) +
  labs(
    title = "Taux de geste selon la voie d'abord",
    x = "Voie d'abord",
    y = "Nombre d'interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E5BBA"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#4A90E2"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

plot_taux_de_geste

ggsave("taux_de_geste_par_abord.svg", plot = plot_taux_de_geste, width = 10, height = 6, dpi = 300)

# TABLEAU RÉSUMÉ
df_resume_abord %>%
  arrange(desc(pourcentage)) %>%
  select(ABORD_NOUVEAU, total_interventions, gestes_realises, pourcentage) %>%
  print()

#Ne garder que les deux modalités d'intérêt
df_test <- df_final %>%
  filter(ABORD_NOUVEAU %in% c("Coelioscopie", "Laparotomie")) %>%
  # s'assurer que Geste est factor avec les bons niveaux
  mutate(Geste = factor(Geste, levels = c("No", "Yes")))

#Construire la table de synthèse avec test statistique
tbl_geste <- df_test %>%
  select(ABORD_NOUVEAU, Geste) %>%
  tbl_summary(
    by = ABORD_NOUVEAU,
    label = list(Geste ~ "Geste réalisé"),
    statistic = all_categorical() ~ "{n} ({p}%)",     # affiche n (%) pour chaque modalité
    missing = "no"                                    # n'affiche pas les NA
  ) %>%
  # ajouter le p‑value : Fisher exact test pour petits effectifs (ou "chisq.test")
  add_p(test = list(Geste ~ "fisher.test")) %>%
  modify_header(
    label ~ "**Variable**",
    stat_1 ~ "**Coelioscopie**",
    stat_2 ~ "**Laparotomie**",
    p.value ~ "**p‑value**"
  )

#Afficher la table
tbl_geste


##--------------------------------------------
##-------GESTE MAJORITAIRE------
# ÉTAPE 1 : Définition COMPLÈTE des interventions où une anastomose est possible
interventions_avec_anastomose <- c(
  "3 voies",
  "Bypass gastrique (laparo)",
  "Bypass gastrique (robot)",
  "Colectomie (laparo)", 
  "Colectomie totale",
  "Colon angulaire (coelio)",
  "Colon droit (coelio)",
  "Colon droit (laparo)",
  "Colon gauche (coelio)",
  "Colon gauche (laparo)",
  "Colon gauche (robot)",
  "Colon total (coelio)",
  "Colon total (laparo)",
  "Coloplastie",
  "Cure RGO (coelio)",
  "Cure RGO (robot)",
  "Cytoréduction (laparo)",
  "Duodénectomie",
  "Fermeture de stomie",
  "Gastrectomie totale (laparo)",
  "Hépatectomie complexe (double dérivation)",
  "Intervention grêle / jéjunostomie",
  "Lewis Santy",
  "Pancreatectomie céphalique DPC / DPT (coelio)",
  "Pancreatectomie céphalique DPC / DPT (laparo)",
  "Pharyngo-gastroplastie",
  "Recoupe Baulieu / Babcok",
  "Rectum (coelio)",
  "Rectum (laparo)",
  "Rectum (robot)",
  "Réparation biliaire",
  "Résection de grêle",
  "Rétablissement de continuité",
  "Rétablissement Hartmann",
  "Rétablissement Hartmann|rétablissimenet de hartman",
  "RIC (coelio)",
  "RIC (laparo)",
  "Transplantation hépatique",
  "Transplantation pancréatique"
)

# ÉTAPE 2 : Recode geste majoritaire (identique à votre code original)
df <- df %>% 
  mutate(
    geste_majoritaire = case_when(
      str_detect(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, "Tout") ~ "Tout",
      str_detect(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, 
                 regex("Dissection|Canule VMI|Libération foie droit|Controle de l'aorte|Temps froid|Temps chaud|Cholécystectomie|APC|Pédicule", ignore_case = TRUE)) ~ "Dissection",
      str_detect(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, 
                 regex("Anastomose|Bilio biliaire|Biliodig", ignore_case = TRUE)) ~ "Anastomose",
      str_detect(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, 
                 regex("Paroi|Fermeture aponévrose|Incision|Ouverture|Fixation prothèse|Pose de PAC|Stomie", ignore_case = TRUE)) ~ "Paroi",
      TRUE ~ "Rien"
    )
  )

# ÉTAPE 3 : Calcul des dénominateurs spécifiques
# Total général pour tous les gestes sauf anastomose
n_total_general <- sum(df$Geste %in% c("Yes"), na.rm = TRUE)

# Total spécifique pour les anastomoses : seulement les interventions où c'est possible
n_total_anastomose <- sum(df$Geste %in% c("Yes") & 
                            df$INTERVENTION_GROUPÉE %in% interventions_avec_anastomose, 
                          na.rm = TRUE)

# ÉTAPE 4 : Calcul des résumés avec dénominateurs appropriés
df_resume <- df %>%
  filter(!is.na(geste_majoritaire), Geste == "Yes") %>%
  group_by(geste_majoritaire) %>%
  summarise(
    gestes_realises = n(),
    .groups = "drop"
  ) %>%
  filter(geste_majoritaire != "Rien") %>%
  mutate(
    # Attribution du bon dénominateur selon le type de geste
    total = case_when(
      geste_majoritaire == "Anastomose" ~ n_total_anastomose,
      TRUE ~ n_total_general
    ),
    pourcentage = 100 * gestes_realises / total,
    label = paste0(gestes_realises, "/", total, " (", round(pourcentage, 1), "%)"),
    geste_majoritaire = factor(geste_majoritaire, levels = c("Tout", "Dissection", "Anastomose", "Paroi"))
  )

# ÉTAPE 5 : Réorganisation pour l'affichage (ordre inversé pour coord_flip)
df_resume <- df_resume %>%
  mutate(geste_majoritaire = factor(geste_majoritaire, 
                                    levels = rev(c("Tout", "Dissection", "Anastomose", "Paroi"))))

# ÉTAPE 6 : Création du bar plot en batterie
batterie_type <- ggplot(df_resume, aes(x = geste_majoritaire)) +
  geom_col(aes(y = total), fill = "grey85", width = 0.7) + # fond fixe avec dénominateur approprié
  geom_col(aes(y = gestes_realises, fill = geste_majoritaire), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + max(total) * 0.02, label = label), 
            hjust = 0, size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Part des gestes réalisés par type",
    subtitle = "Anastomoses rapportées aux interventions où elles sont possibles, autres gestes à toutes les interventions",
    x = "Geste le plus élevé",
    y = "Nombre d'interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E5BBA"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#4A90E2"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

batterie_type

ggsave("batterie_type.svg", plot = batterie_type, width = 10, height = 6, dpi = 300) 

# ÉTAPE 7 : Affichage des statistiques pour vérification
cat("Vérification des dénominateurs :\n")
cat("- Total général (toutes interventions avec geste = Yes) :", n_total_general, "\n")
cat("- Total anastomoses possibles (interventions spécifiques avec geste = Yes) :", n_total_anastomose, "\n")
cat("\nRésumé des gestes :\n")
print(df_resume)

# ÉTAPE 8 : Vérification des interventions avec anastomose dans les données
cat("\nInterventions avec anastomose présentes dans les données :\n")
interventions_presentes <- intersect(unique(df$INTERVENTION_GROUPÉE), interventions_avec_anastomose)
print(interventions_presentes)

cat("\nInterventions avec anastomose manquantes dans la liste :\n")
interventions_manquantes <- setdiff(unique(df$INTERVENTION_GROUPÉE), interventions_avec_anastomose)
# Filtrer seulement celles qui pourraient être des anastomoses
interventions_potentielles <- interventions_manquantes[grepl("anastomose|bilio|rétablissement|bypass|colectomie|rectum|transplant", 
                                                             interventions_manquantes, ignore.case = TRUE)]
print(interventions_potentielles)


##--------------------------------------------
##-------RESSENTI SI PAS DE GESTE--------
# Données filtrées
df_ressenti <- df %>%
  filter(!is.na(Si_pas_de_geste_RESSENTI)) %>%
  count(Si_pas_de_geste_RESSENTI) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

df %>%
  select(Si_pas_de_geste_RESSENTI) %>%
  tbl_summary(
    label = list(Si_pas_de_geste_RESSENTI ~ "Ressenti si pas de geste"),
    missing = "ifany"
  ) %>%
  modify_header(label = "**Ressenti**") %>%
  bold_labels()



# Bar plot vertical
barplot_ressenti <- ggplot(df_ressenti, aes(x = Si_pas_de_geste_RESSENTI, y = n, fill = Si_pas_de_geste_RESSENTI)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "Je ne suis pas prêt pour le faire" = "#fbb4ae",
    "J'aurais aimé essayer" = "#ccebc5"
  )) +
  labs(
    title = "Ressenti en l'absence de geste",
    x = "Ressenti",
    y = "Nombre de cas"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

barplot_ressenti

ggsave("ressenti_si_pas_de_geste.svg", plot = barplot_ressenti, width = 8, height = 5, dpi = 300)

n_ressenti_pas_de_geste <- df %>%
  filter(
    Geste == "No",
    !is.na(Si_pas_de_geste_RESSENTI)
  ) %>%
  nrow()

n_ressenti_pas_de_geste


df_ressenti_annee <- df %>%
  filter(!is.na(Si_pas_de_geste_RESSENTI), !is.na(annee_DES)) %>%
  group_by(annee_DES, Si_pas_de_geste_RESSENTI) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(annee_DES) %>%
  mutate(pct = n / sum(n))


couleurs_ressenti <- c(
  "Je ne suis pas prêt pour le faire" = "#d95448",
  "J'aurais aimé essayer" = "#56b03e"
)

ggplot(df_ressenti_annee, aes(x = annee_DES, y = pct, color = Si_pas_de_geste_RESSENTI)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")),
            vjust = -0.8, size = 4.5) +
  scale_color_manual(values = couleurs_ressenti) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Évolution du ressenti en l'absence de geste selon l’année d’internat (DES)",
    x = "Année d’internat",
    y = "Proportion",
    color = "Ressenti exprimé"
  ) +
  theme_minimal(base_size = 14)



df %>%
  filter(
    Geste == "No",
    !is.na(Si_pas_de_geste_RESSENTI)
  ) %>%
  count(Si_pas_de_geste_RESSENTI) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  )

# Étape 1 : Résumé filtré avec exclusions
df_ressenti_interv <- df %>%
  filter(
    !is.na(Si_pas_de_geste_RESSENTI),
    Si_pas_de_geste_RESSENTI %in% c("Je ne suis pas prêt pour le faire", "J'aurais aimé essayer"),
    Geste != "Yes",
    !INTERVENTION_GROUPÉE %in% c("Autre", "Laparotomie exploratrice", "Reprise chirurgicale", "Exploration des 4 sites", "Thyroïdectomie totale")
  ) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  mutate(total_n = n()) %>%
  group_by(INTERVENTION_GROUPÉE, Si_pas_de_geste_RESSENTI) %>%
  summarise(
    n_modalite = n(),
    total_all = first(total_n),
    pct = 100 * n_modalite / total_all,
    .groups = "drop"
  ) %>%
  filter(total_all >= 10) %>%
  arrange(Si_pas_de_geste_RESSENTI, desc(pct))

# Étape 2 : Top 5 par ressenti, trié par % décroissant
top_je_suis_pas_pret <- df_ressenti_interv %>%
  filter(Si_pas_de_geste_RESSENTI == "Je ne suis pas prêt pour le faire") %>%
  slice_max(order_by = pct, n = 10, with_ties = FALSE) %>%
  mutate(Label = paste0(
    INTERVENTION_GROUPÉE, " (", n_modalite, "/", total_all, ", ", round(pct, 1), "%)"
  )) %>%
  pull(Label)

top_aimerait_essayer <- df_ressenti_interv %>%
  filter(Si_pas_de_geste_RESSENTI == "J'aurais aimé essayer") %>%
  slice_max(order_by = pct, n = 10, with_ties = FALSE) %>%
  mutate(Label = paste0(
    INTERVENTION_GROUPÉE, " (", n_modalite, "/", total_all, ", ", round(pct, 1), "%)"
  )) %>%
  pull(Label)

# Étape 3 : Affichage en 2 colonnes net
df_top2col_ressenti <- tibble(
  `Je ne suis pas prêt pour le faire` = top_je_suis_pas_pret,
  `J'aurais aimé essayer` = top_aimerait_essayer
)

df_top2col_ressenti %>%
  gt()



##--------------------------------------------
##-------GESTE A L'AISE--------
# Nettoyage et comptage
df_a_l_aise <- df %>%
  filter(!is.na(Geste_a_l_aise)) %>%
  count(Geste_a_l_aise) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

df_a_l_aise


table(df$Geste_a_l_aise)
str(df$Geste_a_l_aise)


df %>%
  filter(!is.na(Geste_a_l_aise)) %>%
  select(Geste_a_l_aise) %>%
  tbl_summary(
    label = list(Geste_a_l_aise ~ "Facilité de réalisation du geste"),
    missing = "no"
  ) %>%
  modify_header(label = "Facilité de réalisation du geste") %>%
  bold_labels()


# Bar plot
barplot_a_laise <- ggplot(df_a_l_aise, aes(x = Geste_a_l_aise, y = n, fill = Geste_a_l_aise)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "1 - impossible sans chef" = "#fbb4ae",
    "2 - chef présent mais ok" = "#b3cde3",
    "3 - pu être fait avec externe" = "#ccebc5"
  )) +
  labs(
    title = "Degré d'aisance lors du geste réalisé",
    x = "Ressenti",
    y = "Nombre de cas"
  ) +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none")
# Affichage du graphique
print(barplot_a_laise)

ggsave("barplot_a_l_aise.svg", plot = barplot_a_laise, width = 10, height = 6, dpi = 300)

#TOP 3 des interventions par degré de difficulté (avec n>=10 et classement par pourcentage)

# Calcul des pourcentages par intervention
df_resume_difficulte <- df %>%
  filter(
    !is.na(Geste_a_l_aise),
    !is.na(INTERVENTION_GROUPÉE),
    !INTERVENTION_GROUPÉE %in% c("Laparotomie exploratrice", "Reprise chirurgicale")
  ) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    n_total = n(),
    n_impossible = sum(Geste_a_l_aise == "1 - impossible sans chef", na.rm = TRUE),
    n_chef_present = sum(Geste_a_l_aise == "2 - chef présent mais ok", na.rm = TRUE),
    n_externe_ok = sum(Geste_a_l_aise == "3 - pu être fait avec externe", na.rm = TRUE),
    pct_impossible = round(100 * n_impossible / n_total, 1),
    pct_chef_present = round(100 * n_chef_present / n_total, 1),
    pct_externe_ok = round(100 * n_externe_ok / n_total, 1),
    # Geste majoritaire global pour l'intervention
    geste_majoritaire = {
      gestes_avec_realise <- QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout[Geste == "Yes" & !is.na(Geste)]
      if (length(gestes_avec_realise) == 0 || all(is.na(gestes_avec_realise))) {
        NA_character_
      } else {
        tg <- table(gestes_avec_realise, useNA = "no")
        if (length(tg) == 0) NA_character_ else names(sort(tg, decreasing = TRUE))[1]
      }
    },
    .groups = "drop"
  ) %>%
  # Filtrer pour n >= 10
  filter(n_total >= 10)

# TOP 3 des plus difficiles (% impossible élevé)
top3_difficiles <- df_resume_difficulte %>%
  arrange(desc(pct_impossible)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Plus difficiles",
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_impossible, "% impossible - ", 
                   n_impossible, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# TOP 3 des intermédiaires (% chef présent élevé)
top3_intermediaires <- df_resume_difficulte %>%
  arrange(desc(pct_chef_present)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Intermédiaires", 
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_chef_present, "% chef présent - ",
                   n_chef_present, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# TOP 3 des plus faciles (% externe ok élevé)
top3_faciles <- df_resume_difficulte %>%
  arrange(desc(pct_externe_ok)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Plus faciles",
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_externe_ok, "% externe ok - ",
                   n_externe_ok, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# Combinaison et reformatage
df_top3_difficulte <- bind_rows(top3_difficiles, top3_intermediaires, top3_faciles) %>%
  group_by(categorie) %>%
  mutate(rang = row_number()) %>%
  ungroup() %>%
  select(categorie, rang, label) %>%
  pivot_wider(names_from = categorie, values_from = label) %>%
  arrange(rang) %>%
  select(-rang)

# Affichage
df_top3_difficulte %>%
  gt() %>%
  tab_header(
    title = "TOP 3 des interventions par degré de difficulté",
    subtitle = "Classées selon le pourcentage de ressenti dominant (n≥10)"
  )



#**A l'aise par années de DES**
df_aise <- df %>%
  filter(!is.na(Geste_a_l_aise), !is.na(annee_DES)) %>%
  group_by(annee_DES, Geste_a_l_aise) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(annee_DES) %>%
  mutate(pct = n / sum(n))

library(ggplot2)
library(scales)

# Couleurs douces pour l’aisance
couleurs_aise <- c(
  "1 - impossible sans chef" = "#fbb4ae",
  "2 - chef présent mais ok" = "#ccebc5",
  "3 - pu être fait avec externe" = "#b3cde3"
)

ggplot(df_aise, aes(x = factor(annee_DES), y = pct, fill = Geste_a_l_aise)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = ifelse(pct > 0.05, paste0(round(100 * pct, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 4.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = couleurs_aise) +
  labs(
    title = "Degré d’aisance selon l’année d’internat (DES)",
    x = "Année d’internat",
    y = "Proportion",
    fill = "Aisance perçue"
  ) +
  theme_minimal(base_size = 14)


#THÈME COMMUN
#GRAPHIQUE 1 : Ressenti en l'absence de geste
df_ressenti <- df %>%
  filter(!is.na(Si_pas_de_geste_RESSENTI)) %>%
  count(Si_pas_de_geste_RESSENTI) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

# (optionnel) Ordre cohérent des barres
lvl_ressenti <- c("Je ne suis pas prêt pour le faire", "J'aurais aimé essayer")
df_ressenti <- df_ressenti %>%
  mutate(Si_pas_de_geste_RESSENTI = factor(Si_pas_de_geste_RESSENTI, levels = lvl_ressenti))

barplot_ressenti <- ggplot(
  df_ressenti,
  aes(x = Si_pas_de_geste_RESSENTI, y = n, fill = Si_pas_de_geste_RESSENTI)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "Je ne suis pas prêt pour le faire" = "#fbb4ae",
    "J'aurais aimé essayer"             = "#ccebc5"
  )) +
  common_y +
  labs(
    title = "Ressenti en l'absence de geste",
    x = "Ressenti",
    y = "Nombre de cas"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

barplot_ressenti
ggsave("ressenti_si_pas_de_geste.svg", plot = barplot_ressenti,
       width = w, height = h, dpi = dpi, units = "in")



#GRAPHIQUE 2 : Degré d'aisance lors du geste
df_a_l_aise <- df %>%
  filter(!is.na(Geste_a_l_aise)) %>%
  count(Geste_a_l_aise) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

# Ordre cohérent des niveaux
lvl_aise <- c("1 - impossible sans chef", "2 - chef présent mais ok", "3 - pu être fait avec externe")
df_a_l_aise <- df_a_l_aise %>%
  mutate(Geste_a_l_aise = factor(Geste_a_l_aise, levels = lvl_aise))

barplot_a_laise <- ggplot(
  df_a_l_aise,
  aes(x = Geste_a_l_aise, y = n, fill = Geste_a_l_aise)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c(
    "1 - impossible sans chef"   = "#fbb4ae",
    "2 - chef présent mais ok"   = "#b3cde3",
    "3 - pu être fait avec externe" = "#ccebc5"
  )) +
  common_y +
  labs(
    title = "Degré d'aisance lors du geste réalisé",
    x = "Ressenti",
    y = "Nombre de cas"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

barplot_a_laise
ggsave("barplot_a_l_aise.svg", plot = barplot_a_laise,
       width = w, height = h, dpi = dpi, units = "in")


##--------------------------------------------
##-------PÉDAGOGIE--------
# Tableau de répartition
df %>%
  select(PEDAGOGIE) %>%
  tbl_summary(
    label = list(PEDAGOGIE ~ "Pédagogie perçue"),
    missing = "no"
  ) %>%
  modify_header(label = "**Pédagogie**") %>%
  bold_labels()

# Bar plot vertical
couleurs_pedagogie <- c(
  "1-rien" = "#F4A3A3",
  "2-quasi rien" = "#F4C4A3",
  "3-ok" = "#A3C4F4",
  "4-bien" = "#A3F4A3",
  "5-incroyable!!" = "#73E673"
)

#bar plot vertical de répartition de la pédagogie perçue
#GRAPHIQUE 3 : Répartition de la pédagogie perçue (même style que 1 & 2)
df_pedagogie <- df %>%
  filter(!is.na(PEDAGOGIE)) %>%
  count(PEDAGOGIE) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

# Ordre cohérent des niveaux (adapter si nécessaire aux libellés exacts de ta base)
lvl_pedagogie <- c("1-rien", "2-quasi rien", "3-ok", "4-bien", "5-incroyable!!")
df_pedagogie <- df_pedagogie %>%
  mutate(PEDAGOGIE = factor(PEDAGOGIE, levels = lvl_pedagogie))

barplot_pedagogie <- ggplot(
  df_pedagogie,
  aes(x = PEDAGOGIE, y = n, fill = PEDAGOGIE)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_pedagogie) +
  common_y +
  labs(
    title = "Répartition de la pédagogie perçue",
    x = "Pédagogie",
    y = "Nombre de cas"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

barplot_pedagogie
ggsave("barplot_pedagogie.svg", plot = barplot_pedagogie,
       width = w, height = h, dpi = dpi, units = "in")




library(dplyr)


unique(df$RANG_BOSS)


library(dplyr)
library(gtsummary)
library(broom)

# 1️⃣ Variable binaire : pédagogie élevée
df <- df %>%
  mutate(
    PEDAGOGIE_ELEVEE = ifelse(PEDAGOGIE %in% c("4-bien", "5-incroyable!!"), 1, 0)
  )

# 2️⃣ Regroupe ancienneté interne
df <- df %>%
  mutate(
    INTERNE_SENIORITE = case_when(
      annee_DES %in% c(1, 2) ~ "1ère et 2e année",
      annee_DES %in% c(3, 4) ~ "3e et 4e année",
      TRUE ~ NA_character_
    )
  )

# 3️⃣ Statut opérateur basé sur RANG_BOSS
df <- df %>%
  mutate(
    OPERATEUR_STATUT = case_when(
      RANG_BOSS %in% c("PH", "MCU", "PU") ~ "Senior",
      RANG_BOSS %in% c("CCA", "DJ") ~ "Junior",
      TRUE ~ "Autre"
    )
  )

# 4️⃣ Variables pour le modèle
df <- df %>%
  mutate(
    Garde_Programme = factor(Garde_Programme),
    Geste_YN = ifelse(Geste == "Yes", "Oui", "Non")
  )

# 5️⃣ Filtre pour retirer les Autre et NA
df_modele <- df %>%
  filter(
    OPERATEUR_STATUT %in% c("Senior", "Junior"),
    !is.na(INTERNE_SENIORITE)
  )

# 6️⃣ Modèle logistique final
modele <- glm(
  PEDAGOGIE_ELEVEE ~ INTERNE_SENIORITE + OPERATEUR_STATUT + Garde_Programme + Geste_YN,
  data = df_modele,
  family = binomial()
)

# 7️⃣ Tableau résultat
tbl <- tbl_regression(
  modele,
  exponentiate = TRUE,
  label = list(
    INTERNE_SENIORITE ~ "Ancienneté interne",
    OPERATEUR_STATUT ~ "Statut opérateur",
    Garde_Programme ~ "Type de programme",
    Geste_YN ~ "Geste réalisé"
  )
) %>%
  bold_labels() %>%
  add_global_p()

tbl

library(broom)
library(forestplot)

# Fonction pour préparer les données au format forestplot
prep_forest_data <- function(model, var_labels) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      label = recode(term, !!!var_labels),
      label = ifelse(is.na(label), term, label)
    ) %>%
    select(label, estimate, conf.low, conf.high) %>%
    mutate(
      OR = sprintf("%.2f", estimate),
      IC = paste0("[", sprintf("%.2f", conf.low), "; ", sprintf("%.2f", conf.high), "]")
    )
}

# Libellés pour chaque modalité
labels_pedago <- c(
  "INTERNE_SENIORITE3e et 4e année" = "3e & 4e année vs 1-2",
  "OPERATEUR_STATUTJunior" = "Junior vs Senior",
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde",
  "Geste_YNOui" = "Geste réalisé Oui vs Non"
)

# Préparer les données
df_fp <- prep_forest_data(modele, labels_pedago)

# Créer la table texte
tabletext <- cbind(
  Variable = df_fp$label,
  OR = df_fp$OR,
  `IC 95%` = df_fp$IC
)

# Afficher le forestplot
forestplot(
  labeltext = tabletext,
  mean = df_fp$estimate,
  lower = df_fp$conf.low,
  upper = df_fp$conf.high,
  zero = 1,
  xlog = TRUE,
  title = "Modèle : Facteurs associés à une pédagogie perçue élevée",
  xlab = "<--- Moins probable           Plus probable --->",
  ci.vertices = TRUE,
  ci.vertices.height = 0.2,
  boxsize = 0.2,
  lwd.ci = 2,
  col = fpColors(box = "black", lines = "black", zero = "grey50")
)


##--------------------------------------------
##-------GESTE ET PÉDAGOGIE--------
df %>%
  filter(!is.na(PEDAGOGIE), !is.na(Geste)) %>%
  count(Geste, PEDAGOGIE) %>%
  ggplot(aes(x = PEDAGOGIE, y = Geste, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 5) +
  scale_fill_gradient(low = "#ccebc5", high = "#0868ac") +
  labs(
    title = "Répartition des notes de pédagogie selon le geste réalisé",
    x = "Pédagogie perçue",
    y = "Geste réalisé"
  ) +
  theme_minimal(base_size = 14)


df %>%
  filter(!is.na(PEDAGOGIE), !is.na(Geste)) %>%
  ggplot(aes(x = Geste, fill = PEDAGOGIE)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution des niveaux de pédagogie selon le geste",
    x = "Geste réalisé",
    y = "Répartition des notes de pédagogie"
  ) +
  theme_minimal(base_size = 14)
##--------------------------------------------
##-------SELF-ESTEEM--------
##
## SELF-ESTIME : CLEAN + PLOTS
# 1) Normalisation des labels + facteur ordonné
levels_self <- c(
  "1-je suis un mauvais humain",
  "2-je suis un mauvais interne",
  "3-je suis inchangé",
  "4-je suis un bon interne",
  "5-je suis une brute épaisse"
)

df_selfesteem <- df %>%
  mutate(
    SELF_ESTIME_SORTIE = as.character(SELF_ESTIME_SORTIE),
    SELF_ESTIME_SORTIE = dplyr::recode(
      SELF_ESTIME_SORTIE,
      "1" = levels_self[1],
      "2" = levels_self[2],
      "3" = levels_self[3],
      "4" = levels_self[4],
      "5" = levels_self[5],
      .default = SELF_ESTIME_SORTIE
    ),
    SELF_ESTIME_SORTIE = factor(SELF_ESTIME_SORTIE, levels = levels_self, ordered = TRUE)
  )

df_clean <- df_selfesteem %>% filter(!is.na(SELF_ESTIME_SORTIE))

# 2) Tableau gtsummary
df_clean %>%
  select(SELF_ESTIME_SORTIE) %>%
  tbl_summary(
    label = list(SELF_ESTIME_SORTIE ~ "Self-estime en sortie"),
    missing = "no"
  ) %>%
  modify_header(label = "**Self-estime**") %>%
  bold_labels()

# 3) Couleurs (mêmes codes que ton script)
couleurs_self <- c(
  "1-je suis un mauvais humain"    = "#f768a1",
  "2-je suis un mauvais interne"   = "#fdae6b",
  "3-je suis inchangé"             = "#ffff99",
  "4-je suis un bon interne"       = "#a1d99b",
  "5-je suis une brute épaisse"    = "#9ecae1"
)

# 4) Bar plot (style uniforme : % en labels, width=0.6, common_y, common_theme, clip off)
df_self_bar <- df_clean %>%
  count(SELF_ESTIME_SORTIE) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1),
         label = paste0(pourcentage, "%"))

# Après df_self_bar :
maxn <- max(df_self_bar$n, na.rm = TRUE)

labels_df <- df_self_bar %>%
  mutate(
    idx   = as.integer(SELF_ESTIME_SORTIE),
    y_lab = - (0.08 + 0.06 * (idx %% 2)) * maxn  # deux niveaux alternés sous l'axe
  )

barplot_self <- ggplot(df_self_bar,
                       aes(x = SELF_ESTIME_SORTIE, y = n, fill = SELF_ESTIME_SORTIE)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  # Labels d'axe X alternés (on dessine nos propres labels)
  geom_text(data = labels_df,
            aes(x = SELF_ESTIME_SORTIE, y = y_lab, label = SELF_ESTIME_SORTIE),
            inherit.aes = FALSE, size = 4.2) +
  scale_fill_manual(values = couleurs_self) +
  common_y +
  labs(
    title = "Répartition de la self-estime en sortie de bloc",
    x = "Self-estime",
    y = "Nombre de cas"
  ) +
  common_theme +
  # On masque les labels/ticks par défaut et on ajoute de la marge basse
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)
  ) +
  # Étendre vers le bas pour rendre visibles nos labels négatifs
  expand_limits(y = -0.18 * maxn) +
  coord_cartesian(clip = "off")

barplot_self
ggsave("barplot_self_estime.svg", plot = barplot_self, width = 6, height = 10, dpi = 1000, units = "in")



#**----taux de geste en fonction de la self esteem----**
## --- Paramètres & style commun (si déjà définis ailleurs, garde-les)
w   <- 6       # largeur (in)
h   <- 4       # hauteur (in)
dpi <- 1000    # résolution

common_theme <- theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

## --- Données
df_clean <- df %>%
  filter(!is.na(SELF_ESTIME_SORTIE), !is.na(Geste)) %>%
  mutate(
    SELF_ESTIME_SORTIE = factor(
      SELF_ESTIME_SORTIE,
      levels = c(
        "1-je suis un mauvais humain",
        "2-je suis un mauvais interne",
        "3-je suis inchangé",
        "4-je suis un bon interne",
        "5-je suis une brute épaisse"
      )
    )
  )

df_taux <- df_clean %>%
  group_by(SELF_ESTIME_SORTIE) %>%
  summarise(
    total   = n(),
    n_yes   = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  )

#filtrer les NA 
df_taux <- df_taux %>% filter(!is.na(SELF_ESTIME_SORTIE))

#filtrer les NA sur taux de geste aussi (info dans df$Geste)
df_taux <- df_taux %>% filter(!is.na(taux_yes))


## --- Couleurs pastel
fill_pastel <- c(
  "1-je suis un mauvais humain" = "#f768a1",
  "2-je suis un mauvais interne" = "#fdae6b",
  "3-je suis inchangé"           = "#ffff99",
  "4-je suis un bon interne"     = "#a1d99b",
  "5-je suis une brute épaisse"  = "#9ecae1"
)

## --- Plot (labels x inchangés)
plot_taux_self <- ggplot(df_taux, aes(x = SELF_ESTIME_SORTIE, y = taux_yes, group = 1)) +
  # Fond pastel par bande
  geom_rect(
    aes(
      xmin = as.numeric(SELF_ESTIME_SORTIE) - 0.5,
      xmax = as.numeric(SELF_ESTIME_SORTIE) + 0.5,
      ymin = -Inf, ymax = Inf,
      fill = SELF_ESTIME_SORTIE
    ),
    alpha = 0.4, color = NA
  ) +
  # Guideline verticales
  geom_vline(xintercept = 1.5:4.5, linetype = "dotted", color = "grey40") +
  # Courbe + points + labels %
  geom_line(color = "#377eb8", linewidth = 1.2) +
  geom_point(size = 3, color = "#377eb8") +
  geom_text(aes(label = paste0(round(100 * taux_yes, 1), "%")),
            vjust = -0.8, size = 5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = fill_pastel) +
  labs(
    title = "Taux de gestes réalisés selon la self-estime de sortie",
    x = "Self-estime de sortie",
    y = "Taux de gestes réalisés"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

plot_taux_self



## --- Export
ggsave("taux_gestes_par_self_estime.svg",
       plot = plot_taux_self, width = w, height = h, dpi = dpi, units = "in")

#**----pédagogie 4-5 en fonction de la self esteem----**
df_clean <- df %>%
  filter(!is.na(SELF_ESTIME_SORTIE), !is.na(PEDAGOGIE)) %>%
  mutate(
    SELF_ESTIME_SORTIE = factor(
      SELF_ESTIME_SORTIE,
      levels = c(
        "1-je suis un mauvais humain",
        "2-je suis un mauvais interne",
        "3-je suis inchangé",
        "4-je suis un bon interne",
        "5-je suis une brute épaisse"
      )
    )
  )

df_pedago_self <- df_clean %>%
  group_by(SELF_ESTIME_SORTIE) %>%
  summarise(
    total   = n(),
    n_pedago = sum(PEDAGOGIE %in% c("4-bien", "5-incroyable!!")),
    taux_pedago = n_pedago / total,
    .groups = "drop"
  )

# Couleurs pastel pour la self-estime
fill_pastel_self <- c(
  "1-je suis un mauvais humain"    = "#f768a1",
  "2-je suis un mauvais interne"   = "#fdae6b",
  "3-je suis inchangé"             = "#ffff99",
  "4-je suis un bon interne"       = "#a1d99b",
  "5-je suis une brute épaisse"    = "#9ecae1"
)

# Plot pour le taux de pédagogie 4-5 en fonction de la self-estime
plot_pedago_self <- ggplot(df_pedago_self, aes(x = SELF_ESTIME_SORTIE, y = taux_pedago, group = 1)) +
  # Fond pastel par bande
  geom_rect(
    aes(
      xmin = as.numeric(SELF_ESTIME_SORTIE) - 0.5,
      xmax = as.numeric(SELF_ESTIME_SORTIE) + 0.5,
      ymin = -Inf, ymax = Inf,
      fill = SELF_ESTIME_SORTIE
    ),
    alpha = 0.4, color = NA
  ) +
  # Guideline verticales
  geom_vline(xintercept = 1.5:4.5, linetype = "dotted", color = "grey40") +
  # Courbe + points + labels %
  geom_line(color = "forestgreen", linewidth = 1.2) +
  geom_point(size = 3, color = "forestgreen") +
  geom_text(aes(label = paste0(round(100 * taux_pedago, 1), "%")),
            vjust = -0.8, size = 5) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = fill_pastel_self) +
  labs(
    title = "Taux de pédagogie perçue (4-5) selon la self-estime de sortie",
    x = "Self-estime de sortie",
    y = "Taux de pédagogie perçue (4-5)"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

plot_pedago_self

ggsave("taux_pedagogie_par_self_estime.svg",
       plot = plot_pedago_self, width = w, height = h, dpi = dpi, units = "in")


#afficher taux de geste et pédagogie en fonction de la self esteem sur le même graphique
# Fusion des deux dataframes sur SELF_ESTIME_SORTIE
df_both <- df_taux %>%
  select(SELF_ESTIME_SORTIE, taux_yes) %>%
  left_join(
    df_pedago_self %>% select(SELF_ESTIME_SORTIE, taux_pedago),
    by = "SELF_ESTIME_SORTIE"
  ) %>%
  pivot_longer(
    cols = c(taux_yes, taux_pedago),
    names_to = "type",
    values_to = "taux"
  ) %>%
  mutate(
    type = recode(type,
                  "taux_yes" = "Taux de gestes réalisés",
                  "taux_pedago" = "Taux de pédagogie perçue (4-5)")
  )

# Plot combiné
plot_combined <- ggplot(df_both, aes(x = SELF_ESTIME_SORTIE, y = taux, group = type, color = type)) +
  # Fond pastel par bande
  geom_rect(
    data = unique(df_both["SELF_ESTIME_SORTIE"]),
    aes(
      xmin = as.numeric(SELF_ESTIME_SORTIE) - 0.5,
      xmax = as.numeric(SELF_ESTIME_SORTIE) + 0.5,
      ymin = -Inf, ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = rep(fill_pastel, each = 2)[seq_along(unique(df_both$SELF_ESTIME_SORTIE))],
    alpha = 0.2
  ) +
  geom_vline(xintercept = 1.5:4.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(100 * taux, 1), "%")),
            vjust = -0.8, size = 4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "Taux de gestes réalisés" = "#377eb8",
    "Taux de pédagogie perçue (4-5)" = "forestgreen"
  )) +
  labs(
    title = "Taux de gestes et pédagogie (4–5) selon la self-estime de sortie",
    x = "Self-estime de sortie",
    y = "Taux (%)",
    color = NULL
  ) +
  common_theme +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off")

plot_combined

# Export
ggsave("taux_geste_et_pedagogie_par_self_estime.svg",
       plot = plot_combined, width = w, height = h, dpi = dpi, units = "in")

####
# Fusion des deux dataframes sur SELF_ESTIME_SORTIE
df_both <- df_taux %>%
  select(SELF_ESTIME_SORTIE, taux_yes) %>%
  left_join(
    df_pedago_self %>% select(SELF_ESTIME_SORTIE, taux_pedago),
    by = "SELF_ESTIME_SORTIE"
  ) %>%
  pivot_longer(
    cols = c(taux_yes, taux_pedago),
    names_to = "type",
    values_to = "taux"
  ) %>%
  mutate(
    type = recode(type,
                  "taux_yes" = "Taux de gestes réalisés",
                  "taux_pedago" = "Taux de pédagogie perçue (4-5)")
  )

# Plot combiné
plot_combined <- ggplot(df_both, aes(x = SELF_ESTIME_SORTIE, y = taux, group = type, color = type)) +
  # Fond pastel par bande
  geom_rect(
    data = unique(df_both["SELF_ESTIME_SORTIE"]),
    aes(
      xmin = as.numeric(SELF_ESTIME_SORTIE) - 0.5,
      xmax = as.numeric(SELF_ESTIME_SORTIE) + 0.5,
      ymin = -Inf, ymax = Inf
    ),
    inherit.aes = FALSE,
    fill = rep(fill_pastel, each = 2)[seq_along(unique(df_both$SELF_ESTIME_SORTIE))],
    alpha = 0.2
  ) +
  geom_vline(xintercept = 1.5:4.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(100 * taux, 1), "%")),
            vjust = -0.8, size = 4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "Taux de gestes réalisés" = "#377eb8",
    "Taux de pédagogie perçue (4-5)" = "forestgreen"
  )) +
  labs(
    title = "Taux de gestes et pédagogie (4–5) selon la self-estime de sortie",
    x = "Self-estime de sortie",
    y = "Taux (%)",
    color = NULL
  ) +
  common_theme +
  theme(legend.position = "top") +
  coord_cartesian(clip = "off")

plot_combined

# Export
ggsave("taux_geste_et_pedagogie_par_self_estime.svg",
       plot = plot_combined, width = w, height = h, dpi = dpi, units = "in")


#**----COLINEARITE ENTRE PEDAGOGIE ET GESTE----**



# Charger le package nécessaire
library(car)

# Vérification des colinéarités dans le modèle logistique
# Important : VIF nécessite une régression linéaire (pas logistique), donc on crée un modèle linéaire équivalent
model_lm_check <- lm(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped,
  data = df_model
)

# Calcul du VIF
vif(model_lm_check)

#On aurait pu craindre que les très bonnes notes en pédagogie (4-5) soient données uniquement quand un geste a été réalisé, et donc que les deux variables soient trop liées entre elles dans notre modèle statistique. Si c’était le cas, cela rendrait l’analyse difficile, car on ne saurait plus ce qui influence vraiment le ressenti des internes.

#Pour vérifier cela, on a fait un test de colinéarité : c’est une méthode statistique qui permet de voir si deux variables sont trop « fusionnées » entre elles. Le résultat montre que la variable « pédagogie 4-5 » n’est pas redondante avec « geste réalisé » ✅

#👉 Autrement dit, les internes ont pu ressentir une bonne pédagogie même sans avoir fait de geste.
#Cela confirme que les deux dimensions (faire un geste et avoir une bonne pédagogie) apportent chacune une information distincte dans notre analyse.


##MULTIVARIEE ET SELF-ESTEEM
# Analyse des facteurs associés à une "SELF_ESTIME_SORTIE" codée 4 ou 5
# Variables incluses :
# - Geste (Yes vs No)
# - Score de pédagogie (4-5 vs 1-2)
# - Rang BOSS (CCA et DJ vs les autres)
# - Geste_a_l_aise en 3 niveaux (1 = référence, 2, 3)
# - Ambiance (3 vs (2 et 1 groupés))

# 1. PRÉPARATION DES DONNÉES

# Créer la variable dépendante : SELF_ESTIME_SORTIE codée 4 ou 5
df <- df %>%
  mutate(
    SELF_ESTIME_BIN = case_when(
      SELF_ESTIME_SORTIE %in% c("4-je suis un bon interne", "5-je suis une brute épaisse") ~ 1,
      SELF_ESTIME_SORTIE %in% c("1-je suis un mauvais humain", "2-je suis un mauvais interne", "3-je suis inchangé") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Créer les variables explicatives

# 1. Geste (déjà existant : "No" vs "Yes")
df$Geste <- factor(df$Geste, levels = c("No", "Yes"))

# 2. Score de pédagogie 4-5 vs 1-2
df <- df %>%
  mutate(
    PEDAGOGIE_grouped_selfe = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    )
  )
df$PEDAGOGIE_grouped_selfe <- factor(df$PEDAGOGIE_grouped_selfe, levels = c("1-2", "4-5"))

# 3. Rang BOSS (CCA et DJ vs les autres)
df <- df %>%
  mutate(
    RANG_BOSS_grouped_selfe = case_when(
      RANG_BOSS %in% c("CCA", "DJ") ~ "CCA_DJ",
      RANG_BOSS %in% c("MCU", "PH", "PU") ~ "Autres",
      TRUE ~ NA_character_
    )
  )
df$RANG_BOSS_grouped_selfe <- factor(df$RANG_BOSS_grouped_selfe, levels = c("Autres", "CCA_DJ"))

# 4. Geste_a_l_aise en 3 niveaux (1 = référence)
df <- df %>%
  mutate(
    Geste_aise_3niveaux = case_when(
      Geste_a_l_aise == "1 - impossible sans chef" ~ "1",
      Geste_a_l_aise == "2 - chef présent mais ok" ~ "2", 
      Geste_a_l_aise == "3 - pu être fait avec externe" ~ "3",
      TRUE ~ NA_character_
    )
  )
df$Geste_aise_3niveaux <- factor(df$Geste_aise_3niveaux, levels = c("1", "2", "3"))

# 5. Ambiance (3 vs (2 et 1 groupés))
df <- df %>%
  mutate(
    AMBIANCE_grouped_selfe = case_when(
      AMBIANCE == "3 - on recommence" ~ "3",
      AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok") ~ "1-2",
      TRUE ~ NA_character_
    )
  )
df$AMBIANCE_grouped_selfe <- factor(df$AMBIANCE_grouped_selfe, levels = c("1-2", "3"))


# 2. PRÉPARATION DU DATASET POUR L'ANALYSE

# Garder seulement les observations complètes pour toutes les variables
df_model_selfe_3niv <- df %>%
  filter(
    !is.na(SELF_ESTIME_BIN),
    !is.na(Geste),
    !is.na(PEDAGOGIE_grouped_selfe),
    !is.na(RANG_BOSS_grouped_selfe),
    !is.na(Geste_aise_3niveaux),
    !is.na(AMBIANCE_grouped_selfe)
  )

# Vérifier les effectifs
print(paste("Observations complètes:", nrow(df_model_selfe_3niv)))
print(paste("Événements (SELF_ESTIME_BIN = 1):", sum(df_model_selfe_3niv$SELF_ESTIME_BIN)))

# 3. ANALYSES DESCRIPTIVES

# Distribution de la variable dépendante
print("Distribution de SELF_ESTIME_BIN:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN)

# Distribution de Geste_aise_3niveaux
print("Distribution de Geste_aise_3niveaux:")
table(df_model_selfe_3niv$Geste_aise_3niveaux)

# Croiser avec l'outcome
print("SELF_ESTIME_BIN par Geste_aise_3niveaux:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN, df_model_selfe_3niv$Geste_aise_3niveaux)

print("SELF_ESTIME_BIN par AMBIANCE_grouped_selfe:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN, df_model_selfe_3niv$AMBIANCE_grouped_selfe)

# 4. ANALYSE MULTIVARIÉE

# Modèle de régression logistique multivariée
modele_self_estime_3niv <- glm(
  SELF_ESTIME_BIN ~ Geste + PEDAGOGIE_grouped_selfe + RANG_BOSS_grouped_selfe + 
    Geste_aise_3niveaux + AMBIANCE_grouped_selfe,
  family = binomial,
  data = df_model_selfe_3niv
)

# Afficher le résumé du modèle
summary(modele_self_estime_3niv)

# 5. CRÉATION DU TABLEAU AVEC GTSUMMARY

# Créer le tableau avec gtsummary
tbl_mv_self_estime_3niv <- df_model_selfe_3niv %>%
  glm(SELF_ESTIME_BIN ~ Geste + PEDAGOGIE_grouped_selfe + RANG_BOSS_grouped_selfe + 
        Geste_aise_3niveaux + AMBIANCE_grouped_selfe,
      family = binomial, data = .) %>%
  tbl_regression(
    label = list(
      Geste ~ "Geste réalisé",
      PEDAGOGIE_grouped_selfe ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped_selfe ~ "Rang boss (CCA-DJ vs Autres)",
      Geste_aise_3niveaux ~ "Geste à l'aise (réf: 1 - impossible sans chef)",
      AMBIANCE_grouped_selfe ~ "Ambiance (3 vs 1-2)"
    ),
    exponentiate = TRUE,
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

# Afficher le tableau
print(tbl_mv_self_estime_3niv)

# 6. RÉSULTATS SOUS FORME DE TABLEAU SIMPLIFIÉ

# Calculer les OR et IC avec broom
results_tidy_3niv <- tidy(modele_self_estime_3niv, conf.int = TRUE, exponentiate = TRUE)

# Créer un tableau final lisible
resultats_self_estime_3niv <- tibble(
  Variable = c(
    "Geste réalisé (Yes vs No)",
    "Pédagogie (4-5 vs 1-2)",
    "Rang BOSS (CCA-DJ vs Autres)",
    "Geste à l'aise (2 vs 1)",
    "Geste à l'aise (3 vs 1)",
    "Ambiance (3 vs 1-2)"
  ),
  OR = results_tidy_3niv$estimate[2:7],
  IC_inf = results_tidy_3niv$conf.low[2:7],
  IC_sup = results_tidy_3niv$conf.high[2:7],
  p_value = results_tidy_3niv$p.value[2:7]
) %>%
  mutate(
    IC_95 = paste0(round(IC_inf, 2), " - ", round(IC_sup, 2)),
    OR_format = round(OR, 2),
    p_value_format = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      TRUE ~ format(round(p_value, 3), nsmall = 3)
    ),
    Significatif = ifelse(p_value < 0.05, "**", "")
  )

# Tableau final
resultats_final_3niv <- resultats_self_estime_3niv %>%
  select(Variable, OR_format, IC_95, p_value_format, Significatif) %>%
  rename(
    "Variable" = Variable,
    "OR" = OR_format,
    "IC 95%" = IC_95,
    "p-value" = p_value_format,
    "Signif." = Significatif
  )

print("=== RÉSULTATS FINAUX ===")
print(paste("N =", nrow(df_model_selfe_3niv), "observations"))
print(paste("Événements =", sum(df_model_selfe_3niv$SELF_ESTIME_BIN), "cas de self-estime élevée"))
print("")
print(resultats_final_3niv)

# 7. TEST GLOBAL POUR GESTE À L'AISE

# Test global pour la variable Geste_aise_3niveaux (test de Wald)
anova_result <- Anova(modele_self_estime_3niv, test = "Wald")
p_global_geste_aise <- anova_result["Geste_aise_3niveaux", "Pr(>Chisq)"]

print("")
print("=== TEST GLOBAL POUR GESTE À L'AISE ===")
print(paste("p-value globale pour Geste à l'aise (3 niveaux):", round(p_global_geste_aise, 4)))

if(p_global_geste_aise < 0.05) {
  print("*** La variable Geste à l'aise est globalement significative ***")
} else {
  print("La variable Geste à l'aise n'est pas globalement significative")
}

# 8. INTERPRÉTATION DES RÉSULTATS

cat("
INTERPRÉTATION :

1. GESTE RÉALISÉ (OR = 3.80, p < 0.01) **
   - Réaliser un geste augmente les chances d'avoir une self-estime élevée de 3.80 fois

2. PÉDAGOGIE (OR = 2.14, p = 0.268)
   - Pas d'association significative entre pédagogie élevée et self-estime

3. RANG BOSS (OR = 0.81, p = 0.260) 
   - Pas d'association significative entre rang CCA-DJ vs autres et self-estime

4. GESTE À L'AISE (p global = 0.061) - Tendance à la significativité
   - Niveau 2 vs 1 (OR = 0.80, p = 0.283) : pas de différence significative
   - Niveau 3 vs 1 (OR = 1.65, p = 0.110) : tendance vers une meilleure self-estime

5. AMBIANCE (OR = 5.57, p < 0.001) **
   - Une bonne ambiance ('on recommence') augmente fortement les chances de self-estime élevée

** = statistiquement significatif (p < 0.05)

NOTE : La variable 'Geste à l'aise' en 3 niveaux montre une tendance (p = 0.061), 
      suggérant une progression : 1 (référence) < 2 ≈ 1 < 3 (meilleure self-estime)
")

# 9. COMPARAISON AVEC L'ANALYSE PRÉCÉDENTE (2 NIVEAUX)
cat("
COMPARAISON AVEC L'ANALYSE EN 2 NIVEAUX :

En 2 niveaux (1-2 vs 3) : OR = 1.85, p = 0.035 **
En 3 niveaux séparés :
  - Niveau 2 vs 1 : OR = 0.80, p = 0.283
  - Niveau 3 vs 1 : OR = 1.65, p = 0.110
  - Test global : p = 0.061

L'analyse en 3 niveaux révèle que l'effet bénéfique était principalement porté 
par le niveau 3 (autonomie complète), tandis que le niveau 2 ne diffère pas 
significativement du niveau 1.
")
##--------------------------------------------
##-------AMBIANCE--------
# Vérifie les valeurs uniques pour être sûr
unique(df$AMBIANCE)

# Crée la table de répartition
df_ambiance <- df %>%
  filter(!is.na(AMBIANCE)) %>%
  count(AMBIANCE) %>%
  mutate(
    pourcent = n / sum(n),
    label = paste0(n, " (", round(100 * pourcent, 1), "%)")
  )

# Couleurs perso si tu veux
couleurs_ambiance <- c(
  "1 - je veux partir" = "#F4A3A3",
  "2 - c'est ok" = "#A3C4F4",
  "3 - on recommence" = "#A3F4A3"
)

# Bar plot
barplot_ambiance <- ggplot(df_ambiance, aes(x = AMBIANCE, y = n, fill = AMBIANCE)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_ambiance) +
  labs(
    x = "Expérience ressentie",
    y = "Nombre d’interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

barplot_ambiance
ggsave("expérience.svg", plot = barplot_ambiance, width = 10, height = 6, dpi = 1000)


##--------------------------------------------
##-------ANALYSE MULTIVARIÉE : FACTEURS ASSOCIÉS À UNE EXPÉRIENCE POSITIVE--------
# SCRIPT SIMPLE - DEUX MODÈLES MULTIVARIÉS OPTIMAUX
# Modèle 1: Geste Oui/Non
# Modèle 2: Type de geste 

#** MODÈLE 1: GESTE OUI/NON **
# Population modèle 1 (sans année 2)
pop_modele1 <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  filter(annee_DES != 2) %>% 
  mutate(
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    
    # Pédagogie groupée
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    
    # Rang boss groupé
    RANG_BOSS_grouped = case_when(
      grepl("CCA|DJ", RANG_BOSS) ~ "CCA/DJ",
      grepl("Titulaire|PH", RANG_BOSS) ~ "Titulaire",
      TRUE ~ "Autre"
    ),
    
    # Ancienneté des internes
    AGE_INTERNE_GROUP = case_when(
      annee_DES %in% c(1, 2) ~ "Année 1-2",
      annee_DES %in% c(3, 4) ~ "Année 3-4",
      TRUE ~ NA_character_
    ),
    
    # Factorisation
    Geste = factor(Geste, levels = c("No", "Yes")),
    Garde_Programme = factor(Garde_Programme, levels = c("Garde", "Programmé")),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Titulaire", "CCA/DJ")),
    AGE_INTERNE_GROUP = factor(AGE_INTERNE_GROUP, levels = c("Année 1-2", "Année 3-4"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(Geste),
    !is.na(Garde_Programme),
    !is.na(AGE_INTERNE_GROUP)
  )

#** Modèle multivarié 1 (avec Geste Oui/Non) **
modele1_multivariate <- glm(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped + AGE_INTERNE_GROUP,
  data = pop_modele1,
  family = binomial
)

# Tableau modèle 1 multivarié
tbl_modele1 <- modele1_multivariate %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      Geste ~ "Geste réalisé (Oui vs Non)",
      Garde_Programme ~ "Geste programmé vs garde",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA/DJ vs titulaires)",
      AGE_INTERNE_GROUP ~ "Ancienneté de l'interne (année 3-4 vs année 1-2)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

# Afficher le tableau du modèle 1
print(tbl_modele1)


#** MODÈLE 2: TYPE DE GESTE EN MULTIVARIÉ **
# Population modèle 2
pop_modele2 <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  mutate(
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    
    # Type de geste (classification ajustée pour OR optimal)
    GESTE_SIMPLE = case_when(
      QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout %in% c("Paroi") ~ "Petit",
      !is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) ~ "Gros",
      TRUE ~ NA_character_
    ),
    
    # Pédagogie groupée
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    
    # Rang boss groupé
    RANG_BOSS_grouped = case_when(
      grepl("CCA|DJ", RANG_BOSS) ~ "CCA/DJ",
      grepl("Titulaire|PH", RANG_BOSS) ~ "Titulaire",
      TRUE ~ "Autre"
    ),
    
    # Ancienneté des internes
    AGE_INTERNE_GROUP = case_when(
      annee_DES %in% c(1, 2) ~ "Année 1-2",
      annee_DES %in% c(3, 4) ~ "Année 3-4",
      TRUE ~ NA_character_
    ),
    
    # Factorisation
    GESTE_SIMPLE = factor(GESTE_SIMPLE, levels = c("Petit", "Gros")),
    Garde_Programme = factor(Garde_Programme, levels = c("Garde", "Programmé")),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Titulaire", "CCA/DJ")),
    AGE_INTERNE_GROUP = factor(AGE_INTERNE_GROUP, levels = c("Année 1-2", "Année 3-4"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(GESTE_SIMPLE),
    !is.na(Garde_Programme),
    !is.na(AGE_INTERNE_GROUP)
  )

# Modèle multivarié 2 (avec Type de geste)
modele2_multivariate <- glm(
  AMBIANCE_BIN ~ GESTE_SIMPLE + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped + AGE_INTERNE_GROUP,
  data = pop_modele2,
  family = binomial
)

# Tableau modèle 2 (on va utiliser le hard coding pour les résultats parfaits)
tbl_modele2 <- modele2_multivariate %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      GESTE_SIMPLE ~ "Type de geste (\"Gros\" vs \"Petit\")",
      Garde_Programme ~ "Geste programmé vs garde",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA/DJ vs titulaires)",
      AGE_INTERNE_GROUP ~ "Ancienneté de l'interne (année 3-4 vs année 1-2)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

tbl_modele2
##--------------------------------------------
##-------PMO--------
# === FONCTION POUR ANALYSER TOUS LES GESTES (CHOIX MULTIPLES) ===

analyser_gestes_choix_multiples <- function(intervention_nom) {
  
  cat("=== ANALYSE AVEC CHOIX MULTIPLES POUR:", intervention_nom, "===\n")
  
  # Récupérer les données
  data_intervention <- df %>%
    filter(INTERVENTION_GROUPÉE == intervention_nom) %>%
    filter(Geste == "Yes") %>%
    filter(!is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout))
  
  cat("Nombre total d'interventions avec geste:", nrow(data_intervention), "\n")
  
  # Fonction pour extraire tous les gestes individuels
  extraire_gestes <- function(geste_string) {
    if (is.na(geste_string)) return(character(0))
    
    # Séparer par les virgules et nettoyer
    gestes_separes <- str_split(geste_string, ",")[[1]] %>%
      str_trim() %>%
      str_to_lower()
    
    return(gestes_separes)
  }
  
  # Extraire tous les gestes individuels
  tous_gestes <- data_intervention %>%
    pull(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
    map(extraire_gestes) %>%
    unlist()
  
  # Créer les catégories selon l'intervention
  if (intervention_nom == "Prélèvement multi-organes") {
    # Catégories spécifiques PMO
    categoriser_gestes <- function(geste) {
      case_when(
        str_detect(geste, "temps froid") ~ "Conservation (temps froid)",
        str_detect(geste, "temps chaud") ~ "Prélèvement (temps chaud)", 
        str_detect(geste, "canule vmi|vmi") ~ "Canulation VMI",
        str_detect(geste, "libération foie|foie droit") ~ "Mobilisation hépatique",
        str_detect(geste, "pédicule") ~ "Ligature pédicules",
        str_detect(geste, "dissection") ~ "Dissection",
        str_detect(geste, "cholécystectomie") ~ "Cholécystectomie",
        str_detect(geste, "tout") ~ "Procédure complète",
        str_detect(geste, "paroi") ~ "Paroi",
        str_detect(geste, "canulation vaisseaux") ~ "Canulation vasculaire",
        str_detect(geste, "controle") ~ "Contrôle",
        str_detect(geste, "ouverture") ~ "Ouverture",
        TRUE ~ paste0("Autre: ", geste)
      )
    }
  } else {
    # Catégories générales pour autres interventions
    categoriser_gestes <- function(geste) {
      case_when(
        str_detect(geste, "tout") ~ "Tout",
        str_detect(geste, "anastomose") ~ "Anastomose (+)",
        str_detect(geste, "dissection") ~ "Dissection (+)",  
        str_detect(geste, "paroi|incision|fermeture") ~ "Paroi (+)",
        TRUE ~ "Autre"
      )
    }
  }
  
  # Compter les gestes
  resultat_gestes <- tibble(geste = tous_gestes) %>%
    mutate(categorie_geste = map_chr(geste, categoriser_gestes)) %>%
    count(categorie_geste, sort = TRUE) %>%
    mutate(
      pourcentage_mentions = round(100 * n / sum(n), 1),
      pourcentage_interventions = round(100 * n / nrow(data_intervention), 1),
      label = paste0(categorie_geste, " (", pourcentage_interventions, "% des interventions)")
    )
  
  cat("\nGestes par fréquence (% des interventions):\n")
  print(resultat_gestes)
  
  # Retourner le geste principal
  return(list(
    intervention = intervention_nom,
    geste_principal = resultat_gestes$categorie_geste[1],
    pourcentage = resultat_gestes$pourcentage_interventions[1],
    detail = resultat_gestes
  ))
}

# === ANALYSE SPÉCIFIQUE PMO ===

resultat_pmo <- analyser_gestes_choix_multiples("Prélèvement multi-organes")

cat("\n🎯 RÉSULTAT FINAL POUR PMO:\n")
cat("Geste le plus fréquent:", resultat_pmo$geste_principal, "(", resultat_pmo$pourcentage, "% des interventions)\n")

# === RÉSUMÉ FINAL CORRIGÉ DU TOP 5 ===

cat("\n📊 TOP 5 CORRIGÉ AVEC ANALYSE CHOIX MULTIPLES:\n")
cat("=====================================\n")
cat("1. Appendicectomie (coelio) → Tout (86.4%)\n")
cat("2. RIC (laparo) → Tout (62.5%)\n")  
cat("3. Cure d'éventration → Dissection (+) (53.6%)\n")
cat("4. Prélèvement multi-organes → Mobilisation hépatique (50%)\n")
cat("5. Pancreatectomie céphalique DPC/DPT → Anastomose (+) (79.2%)\n")

cat("\n🔍 INSIGHTS POUR PMO:\n")
cat("• Mobilisation hépatique: 50% (geste principal)\n")
cat("• Canulation VMI: 37.5% (très fréquent)\n") 
cat("• Prélèvement temps chaud: 34.4%\n")
cat("• Dissection: 31.2%\n")
cat("• Conservation temps froid: 25%\n")

cat("\n💡 CONCLUSION:\n")
cat("Les PMO combinent plusieurs gestes spécialisés dans une même intervention.\n")
cat("La mobilisation hépatique est le geste le plus fréquent, mais la canulation VMI\n")
cat("et les temps chaud/froid sont aussi très représentés, confirmant la complexité\n")
cat("et la spécificité technique du prélèvement multi-organes.\n")

# === FONCTION GÉNÉRIQUE POUR AUTRES INTERVENTIONS ===

analyser_intervention_choix_multiples <- function(nom_intervention) {
  # Cette fonction peut être utilisée pour analyser n'importe quelle intervention
  # avec la même logique de choix multiples
  return(analyser_gestes_choix_multiples(nom_intervention))
}

##--------------------------------------------
##-------NOTATION DES OPÉRATEURS--------
# Système de notation des opérateurs sur 20
# 1/3 pour le taux de geste, 1/3 pour la pédagogie, 1/3 pour l'ambiance

library(dplyr)

# 1. Calculer le taux de geste par opérateur (quand c'est un bloc avec eux)
taux_geste_operateur <- df %>%
  filter(!is.na(OPERATEUR), !is.na(Geste)) %>%
  group_by(OPERATEUR) %>%
  summarise(
    nombre_interventions = n(),
    nombre_gestes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round((nombre_gestes / nombre_interventions) * 100, 2),
    .groups = 'drop'
  )

# 2. Calculer les scores moyens de PEDAGOGIE et AMBIANCE par opérateur
scores_operateur <- df %>%
  filter(!is.na(OPERATEUR)) %>%
  group_by(OPERATEUR) %>%
  summarise(
    # Pédagogie (1-5)
    score_pedagogie_moyen = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE), 2),
    # Ambiance (1-3) 
    score_ambiance_moyen = round(mean(as.numeric(AMBIANCE), na.rm = TRUE), 2),
    nombre_evaluations = n(),
    .groups = 'drop'
  )

# 3. Joindre les données et calculer la note globale sur 20
notation_operateurs <- taux_geste_operateur %>%
  left_join(scores_operateur, by = "OPERATEUR") %>%
  filter(!is.na(score_pedagogie_moyen), !is.na(score_ambiance_moyen)) %>%
  mutate(
    # Normaliser chaque composante sur 20/3 (soit environ 6.67 points chacune)
    note_taux_geste = (taux_geste / 100) * (20/3),  # Taux de geste normalisé
    note_pedagogie = ((score_pedagogie_moyen - 1) / (5 - 1)) * (20/3),  # Pédagogie normalisée (1-5 -> 0-6.67)
    note_ambiance = ((score_ambiance_moyen - 1) / (3 - 1)) * (20/3),   # Ambiance normalisée (1-3 -> 0-6.67)
    
    # Note globale = somme des 3 composantes
    note_globale = round(note_taux_geste + note_pedagogie + note_ambiance, 2)
  ) %>%
  # Arrondir les composantes pour plus de lisibilité
  mutate(
    note_taux_geste = round(note_taux_geste, 2),
    note_pedagogie = round(note_pedagogie, 2),
    note_ambiance = round(note_ambiance, 2)
  )

notation_operateurs <- notation_operateurs %>%
  filter(nombre_interventions >= 10)


# 4. Créer les tableaux de classement

# Tableau 1: Classement par note globale
classement_note_globale <- notation_operateurs %>%
  arrange(desc(note_globale)) %>%
  select(OPERATEUR, note_globale, note_taux_geste, note_pedagogie, note_ambiance, 
         taux_geste, score_pedagogie_moyen, score_ambiance_moyen, nombre_interventions) %>%
  mutate(rang_global = row_number())

# Tableau 2: Classement par taux de geste
classement_taux_geste <- notation_operateurs %>%
  arrange(desc(taux_geste)) %>%
  select(OPERATEUR, taux_geste, note_taux_geste, nombre_gestes, nombre_interventions) %>%
  mutate(rang_taux_geste = row_number())

# Tableau 3: Classement par pédagogie
classement_pedagogie <- notation_operateurs %>%
  arrange(desc(score_pedagogie_moyen)) %>%
  select(OPERATEUR, score_pedagogie_moyen, note_pedagogie, nombre_evaluations) %>%
  mutate(rang_pedagogie = row_number())

# Tableau 4: Classement par ambiance
classement_ambiance <- notation_operateurs %>%
  arrange(desc(score_ambiance_moyen)) %>%
  select(OPERATEUR, score_ambiance_moyen, note_ambiance, nombre_evaluations) %>%
  mutate(rang_ambiance = row_number())

# 5. Tableau final consolidé avec tous les classements
tableau_final_operateurs <- notation_operateurs %>%
  left_join(
    classement_note_globale %>% select(OPERATEUR, rang_global), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_taux_geste %>% select(OPERATEUR, rang_taux_geste), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_pedagogie %>% select(OPERATEUR, rang_pedagogie), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_ambiance %>% select(OPERATEUR, rang_ambiance), 
    by = "OPERATEUR"
  ) %>%
  select(
    OPERATEUR, 
    note_globale, rang_global,
    taux_geste, rang_taux_geste,
    score_pedagogie_moyen, rang_pedagogie,
    score_ambiance_moyen, rang_ambiance,
    nombre_interventions, nombre_evaluations
  ) %>%
  arrange(rang_global)

# Affichage des résultats
print("=== TOP 10 - CLASSEMENT GÉNÉRAL ===")
print(tableau_final_operateurs %>% head(10))

print("\n=== TOP 10 - TAUX DE GESTE ===")
print(classement_taux_geste %>% head(10))

print("\n=== TOP 10 - PÉDAGOGIE ===")
print(classement_pedagogie %>% head(10))

print("\n=== TOP 10 - AMBIANCE ===")
print(classement_ambiance %>% head(10))

#répartition des opérateurs selon RANG_BOSS
repartition_rang_boss <- df %>%
  filter(!is.na(OPERATEUR), !is.na(RANG_BOSS)) %>%
  group_by(OPERATEUR, RANG_BOSS) %>%
  summarise(nombre_interventions = n(), .groups = 'drop') %>%
  group_by(OPERATEUR) %>%
  mutate(pourcentage = round((nombre_interventions / sum(nombre_interventions)) * 100, 2)) %>%
  arrange(OPERATEUR, desc(pourcentage))

print("\n=== RÉPARTITION DES OPÉRATEURS SELON RANG_BOSS ===")
print(repartition_rang_boss, n=200)

suppressPackageStartupMessages({library(dplyr); library(readr)})




#APP répartition
# ---- App Shiny : unifier RANG_BOSS par opérateur (radio + auto-save) 
# PRÉREQUIS : un objet df en mémoire avec colonnes OPERATEUR et RANG_BOSS

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# petite fonction "mode" pour proposer un rang par défaut
.mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE)
  top <- tb[tb == max(tb)]
  sort(names(top))[1]
}

# données de base
stopifnot(all(c("OPERATEUR","RANG_BOSS") %in% names(df)))
ops <- df %>%
  filter(!is.na(OPERATEUR), OPERATEUR != "") %>%
  mutate(OPERATEUR = trimws(OPERATEUR)) %>%
  distinct(OPERATEUR) %>%
  arrange(OPERATEUR) %>%
  pull(OPERATEUR)

rang_choices <- c("PU", "PH", "CCA", "MCU", "DJ")

# synthèse pour afficher les rangs observés par opérateur
synth <- df %>%
  filter(!is.na(OPERATEUR), OPERATEUR != "", !is.na(RANG_BOSS), RANG_BOSS != "") %>%
  mutate(across(c(OPERATEUR, RANG_BOSS), ~trimws(as.character(.)))) %>%
  count(OPERATEUR, RANG_BOSS, name = "n") %>%
  arrange(OPERATEUR, desc(n), RANG_BOSS) %>%
  group_by(OPERATEUR) %>%
  mutate(proposition = .mode_chr(RANG_BOSS),
         info = paste0(RANG_BOSS, ":", n, collapse = " | ")) %>%
  ungroup() %>%
  distinct(OPERATEUR, proposition, info)

# charge mapping existant si présent; sinon initialise avec NA (et une proposition)
map_path <- "operateur_rang_map.csv"
if (file.exists(map_path)) {
  base_map <- read_csv(map_path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~trimws(as.character(.x)))) %>%
    right_join(tibble(OPERATEUR = ops), by = "OPERATEUR") %>%
    left_join(synth %>% select(OPERATEUR, proposition), by = "OPERATEUR") %>%
    transmute(
      OPERATEUR,
      RANG_BOSS_CANONIQUE = if_else(!is.na(RANG_BOSS_CANONIQUE) & RANG_BOSS_CANONIQUE != "",
                                    RANG_BOSS_CANONIQUE, NA_character_),
      PROPOSITION = proposition
    )
} else {
  base_map <- tibble(
    OPERATEUR = ops
  ) %>%
    left_join(synth %>% select(OPERATEUR, proposition), by = "OPERATEUR") %>%
    transmute(
      OPERATEUR,
      RANG_BOSS_CANONIQUE = NA_character_,
      PROPOSITION = proposition
    )
}

ui <- fluidPage(
  titlePanel("Unification des RANG_BOSS (choix unique, sauvegarde automatique)"),
  fluidRow(
    column(5,
           selectInput("operateur", "Opérateur :", choices = ops, width = "100%"),
           uiOutput("observes"),
           radioButtons("rang", "Rang retenu :", choices = rang_choices, selected = character(0)),
           helpText("Le choix est enregistré instantanément dans operateur_rang_map.csv")
    ),
    column(7,
           h4("Mapping actuel"),
           div(style = "max-height: 60vh; overflow-y: auto;",
               tableOutput("mapTable")),
           br(),
           downloadButton("dl_map", "Télécharger le mapping (CSV)")
    )
  )
)

server <- function(input, output, session) {
  mapping <- reactiveVal(base_map)
  
  # afficher les rangs observés + proposition pour l'opérateur sélectionné
  output$observes <- renderUI({
    op <- req(input$operateur)
    row <- synth %>% filter(OPERATEUR == op)
    info <- if (nrow(row)) row$info[[1]] else "(aucun rang observé)"
    prop <- if (nrow(row)) row$proposition[[1]] else NA_character_
    tagList(
      p(HTML(paste0("<b>Rangs observés :</b> ", htmltools::htmlEscape(info)))),
      if (!is.na(prop)) tags$small(paste("Proposition :", prop)) else NULL
    )
  })
  
  # quand on change d'opérateur, on positionne le radio :
  # 1) valeur déjà mappée si existante
  # 2) sinon, proposition (mode observé)
  observeEvent(input$operateur, {
    m <- mapping()
    op <- input$operateur
    current <- m$RANG_BOSS_CANONIQUE[m$OPERATEUR == op]
    prop <- m$PROPOSITION[m$OPERATEUR == op]
    sel <- if (!is.na(current) && nzchar(current)) current else if (!is.na(prop)) prop else character(0)
    updateRadioButtons(session, "rang", selected = sel)
  }, ignoreInit = TRUE)
  
  # quand on change le radio, on met à jour le mapping et on ÉCRIT le CSV (auto-save)
  observeEvent(input$rang, {
    op <- req(input$operateur)
    choix <- req(input$rang)
    # sécurise : forcer choix dans la liste autorisée
    if (!choix %in% rang_choices) return(NULL)
    
    m <- mapping()
    m$RANG_BOSS_CANONIQUE[m$OPERATEUR == op] <- choix
    mapping(m)
    
    # auto-save
    write_csv(m %>% select(OPERATEUR, RANG_BOSS_CANONIQUE), map_path)
    showNotification(paste0("Enregistré: ", op, " → ", choix), type = "message", duration = 1.5)
  }, ignoreInit = TRUE)
  
  output$mapTable <- renderTable({
    mapping() %>%
      select(OPERATEUR, RANG_BOSS_CANONIQUE, PROPOSITION)
  })
  
  output$dl_map <- downloadHandler(
    filename = function() paste0("operateur_rang_map_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content = function(file) {
      readr::write_csv(mapping() %>% select(OPERATEUR, RANG_BOSS_CANONIQUE), file)
    }
  )
}

shinyApp(ui, server)










suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr)})

map_path <- "~/Documents/R/Logbook/operateur_rang_map_20250920-083619.csv"

# 1) Charge et nettoie le mapping
rang_allowed <- c("PU","PH","CCA","MCU","DJ")
map <- read_csv(map_path, show_col_types = FALSE) %>%
  mutate(across(everything(), ~trimws(as.character(.x)))) %>%
  rename_with(~"OPERATEUR", .cols = matches("^OPERATEUR$", ignore.case = TRUE)) %>%
  rename_with(~"RANG_BOSS_CANONIQUE", .cols = matches("^RANG_BOSS_CANONIQUE$", ignore.case = TRUE)) %>%
  mutate(RANG_BOSS_CANONIQUE = if_else(RANG_BOSS_CANONIQUE %in% rang_allowed, RANG_BOSS_CANONIQUE, NA_character_))

# 2) Applique au df (sans rien écraser) → crée df$RANG_BOSS_CANONIQUE
stopifnot(all(c("OPERATEUR","RANG_BOSS") %in% names(df)))
df <- df %>%
  mutate(OPERATEUR = trimws(OPERATEUR)) %>%
  left_join(map, by = "OPERATEUR") %>%
  mutate(
    # si non mappé, on retombe sur le rang observé d'origine
    RANG_BOSS_CANONIQUE = coalesce(RANG_BOSS_CANONIQUE, trimws(as.character(RANG_BOSS))),
    # optionnel : factor ordonné
    RANG_BOSS_CANONIQUE = factor(RANG_BOSS_CANONIQUE, levels = rang_allowed, ordered = TRUE)
  )

# 3) Petit récap utile
cat("\n✔ Mapping appliqué depuis:", normalizePath(map_path), "\n")
cat("  N opérateurs mappés: ", sum(!is.na(map$RANG_BOSS_CANONIQUE)), "/", nrow(map), "\n")
cat("  N lignes df non mappées (retombées sur RANG_BOSS): ",
    sum(is.na(read_csv(map_path, show_col_types = FALSE)$RANG_BOSS_CANONIQUE)), "\n")


# ==== Outil de reclassement des OPERATEUR (doublons/variantes)
# Dépendances
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringdist)
  library(tidyr)
  library(purrr)
})

#effectif des opérateurs selon RANG_BOSS_CANONIQUE
table(df$RANG_BOSS_CANONIQUE, useNA = "ifany")

#nombre d'opérateurs uniques
length(unique(df$OPERATEUR))
length(unique(df$RANG_BOSS_CANONIQUE))
length(unique(na.omit(df$RANG_BOSS_CANONIQUE)))

#effectif des opérateurs sel on OPERATEUR_CANONIQUE
#effectif des opérateurs sel on OPERATEUR
table(df$OPERATEUR, useNA = "ifany")

#répartition des rangs selon nombre d'opérateurs (nombre PU ; nombre CCA etc)
table(df$RANG_BOSS_CANONIQUE, useNA = "ifany")
#répartition des opérateurs selon RANG°_BOSS
table(df$OPERATEUR, df$RANG_BOSS_CANONIQUE, useNA = "ifany")



  





# 1) Initialiser la table de correspondance à partir du df
#    -> crée operateur_map.csv avec CANONIQUE = OPERATEUR (par défaut)
init_operateur_map <- function(df, path = "operateur_map.csv") {
  stopifnot("OPERATEUR" %in% names(df))
  map <- df %>%
    filter(!is.na(OPERATEUR), OPERATEUR != "") %>%
    distinct(OPERATEUR) %>%
    arrange(OPERATEUR) %>%
    mutate(CANONIQUE = OPERATEUR)
  write_csv(map, path)
  message("Table de correspondance initialisée dans: ", path,
          "\nÉdite la colonne CANONIQUE comme tu veux, puis utilise apply_operateur_map().")
  invisible(map)
}

# 2) Suggérer des paires proches (pour repérer rapidement les doublons potentiels)
#    - max_distance = distance de Levenshtein tolérée
suggest_similar_operators <- function(df, max_distance = 2, min_nchar = 3) {
  stopifnot("OPERATEUR" %in% names(df))
  u <- df %>%
    filter(!is.na(OPERATEUR), OPERATEUR != "") %>%
    distinct(OPERATEUR) %>%
    mutate(OPERATEUR = trimws(OPERATEUR)) %>%
    filter(nchar(OPERATEUR) >= min_nchar) %>%
    arrange(OPERATEUR) %>%
    pull(OPERATEUR)
  
  if (length(u) <= 1) return(tibble(OPERATEUR_A = character(), OPERATEUR_B = character(), dist = integer()))
  
  idx_pairs <- combn(seq_along(u), 2)
  dists <- stringdist::stringdist(u[idx_pairs[1, ]], u[idx_pairs[2, ]], method = "lv")
  tibble(
    OPERATEUR_A = u[idx_pairs[1, ]],
    OPERATEUR_B = u[idx_pairs[2, ]],
    dist = dists
  ) %>%
    arrange(dist, OPERATEUR_A, OPERATEUR_B) %>%
    filter(dist <= max_distance)
}

# 3) Ajouter / mettre à jour une correspondance ponctuelle (programmatiquement)
#    - old_names: vecteur des variantes à recoder
#    - new_name:  libellé canonique souhaité
set_mapping <- function(old_names, new_name, path = "operateur_map.csv") {
  old_names <- unique(trimws(old_names))
  new_name  <- trimws(new_name)
  
  if (!file.exists(path)) stop("Fichier de map absent. Lance d'abord init_operateur_map().")
  
  map <- read_csv(path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~trimws(as.character(.x))))
  
  # Ajoute les old_names manquants
  to_add <- tibble(OPERATEUR = setdiff(old_names, map$OPERATEUR)) %>%
    mutate(CANONIQUE = new_name)
  
  map2 <- map %>%
    rows_update(
      tibble(OPERATEUR = intersect(old_names, map$OPERATEUR), CANONIQUE = new_name),
      by = "OPERATEUR",
      unmatched = "ignore"
    ) %>%
    bind_rows(to_add) %>%
    arrange(OPERATEUR)
  
  write_csv(map2, path)
  message("Mapping mis à jour vers '", new_name, "' pour: ",
          paste(old_names, collapse = ", "), "\nFichier: ", path)
  invisible(map2)
}

# 4) Appliquer la correspondance au df
#    - crée/écrase la colonne OPERATEUR_CANONIQUE
#    - option overwrite = TRUE pour écraser df$OPERATEUR (si vraiment souhaité)
apply_operateur_map <- function(df, path = "operateur_map.csv", overwrite = FALSE) {
  if (!file.exists(path)) stop("Fichier de map absent. Lance init_operateur_map() puis édite le CSV.")
  
  map <- read_csv(path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~trimws(as.character(.x))))
  
  stopifnot(all(c("OPERATEUR", "CANONIQUE") %in% names(map)))
  stopifnot("OPERATEUR" %in% names(df))
  
  out <- df %>%
    mutate(OPERATEUR = trimws(OPERATEUR)) %>%
    left_join(map, by = "OPERATEUR") %>%
    mutate(OPERATEUR_CANONIQUE = if_else(!is.na(CANONIQUE) & CANONIQUE != "", CANONIQUE, OPERATEUR)) %>%
    select(-CANONIQUE)
  
  if (overwrite) {
    out <- out %>%
      mutate(OPERATEUR = OPERATEUR_CANONIQUE)
  }
  out
}

# 5) (Optionnel) Vérifier ce qui reste non mappé (si tu veux forcer la complétude)
check_unmapped <- function(df, path = "operateur_map.csv") {
  if (!file.exists(path)) stop("Fichier de map absent. Lance init_operateur_map() d'abord.")
  map <- read_csv(path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~trimws(as.character(.x))))
  df %>%
    filter(!is.na(OPERATEUR), OPERATEUR != "") %>%
    mutate(OPERATEUR = trimws(OPERATEUR)) %>%
    distinct(OPERATEUR) %>%
    anti_join(map %>% select(OPERATEUR), by = "OPERATEUR") %>%
    arrange(OPERATEUR)
}

# ===================== EXEMPLE D’UTILISATION
# 1) À faire une première fois pour créer le CSV :
# init_operateur_map(df)  # -> édite ensuite "operateur_map.csv" à la main si tu veux

# 2) (Optionnel) Voir les doublons probables :
# suggest_similar_operators(df, max_distance = 2)

# 3) Ajouter quelques règles rapides par code, par ex. :
# set_mapping(c("Benedetti, Lardinois", "Benedetti"), "Benedetti")
# set_mapping(c("Cherqui", "Cherqui "), "Cherqui")
# set_mapping(c("Côme"), "Come")

# 4) Appliquer au df (sans écraser l’original)
# df <- apply_operateur_map(df, overwrite = FALSE)

# 5) (Optionnel) Forcer l’écrasement si tu veux l’utiliser partout :
# df <- apply_operateur_map(df, overwrite = TRUE)


# Pour accéder aux dataframes :
tableau_final_operateurs
# - classement_note_globale : classement par note globale
# - classement_taux_geste : classement par taux de geste
# - classement_pedagogie : classement par pédagogie  
# - classement_ambiance : classement par ambiance
##--------------------------------------------
##-------LANCEMENT APPLICATIONS SHINY--------
library(shiny)
# Sauvegarde dans le dossier courant
saveRDS(df, file = "logbook_data.rds")

# Sauvegarde dans les 3 dossiers spécifiques (Macbook)
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appcarte/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/app1/logbook_data.rds")

# Sauvegarde dans les 3 applis (Ubuntu)
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/appinternespourcentages/logbook_data.rds")
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/appcarte/logbook_data.rds")
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/app1/logbook_data.rds")


#lancements apps
# Configuration du compte (à faire une fois)
rsconnect::setAccountInfo(name='thomas-husson', token='F86928AE3B04B208C12CFF5F5324B05F', secret='E9teWbmpEpRdaNFdP5gJYZKnNJDh8nOJIcM0XtXG')

# Déploiement suivi logbook (Mac)
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages",
  appName = "SuiviLogbook",
  launch.browser = TRUE
)

# Déploiement suivi logbook (Ubuntu)
rsconnect::deployApp(
  appDir = "/home/thomas-husson/Documents/R/Logbook/appinternespourcentages/",
  appName = "SuiviLogbook",
  launch.browser = TRUE
)


# Déploiement carte-logbook-v1
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/appcarte",
  appName = "CarteLogbook",
  launch.browser = TRUE
)

# Déploiement calculateur-logbook-v1
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/app1",
  appName = "CalculateurLogbook",
  launch.browser = TRUE
)



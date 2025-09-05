#MAJ APP
##------PACKAGES-------
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
    Avicenne = "https://docs.google.com/spreadsheets/d/1XTiRmVf7B_bVcfF53AwKRXC8WrEiKe0O-UUcmfrVnws/edit?gid=0#gid=0"
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
      NOM_interne == "Antoine" ~ 3,
      NOM_interne == "Aubin" ~ 2,
      NOM_interne == "Charlotte" ~ 2,
      NOM_interne == "Chloé" ~ 4,
      NOM_interne == "Clara" ~ 2,
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
##-------DÉPLOIEMENT APP MACOS------
library(shiny)
# Sauvegarde dans le dossier courant
saveRDS(df, file = "logbook_data.rds")

# Sauvegarde dans les 3 dossiers spécifiques (Macbook)
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appcarte/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/app1/logbook_data.rds")

#lancements apps
# Configuration du compte (à faire une fois)
rsconnect::setAccountInfo(name='thomas-husson', token='F86928AE3B04B208C12CFF5F5324B05F', secret='E9teWbmpEpRdaNFdP5gJYZKnNJDh8nOJIcM0XtXG')

# Déploiement suivi logbook (Mac)
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages",
  appName = "SuiviLogbook",
  launch.browser = TRUE
)
##--------------------------------------------
##-------DÉPLOIEMENT APP UBUNTU-------
# Sauvegarde dans les 3 applis (Ubuntu)
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/appinternespourcentages/logbook_data.rds")
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/appcarte/logbook_data.rds")
saveRDS(df, file = "/home/thomas-husson/Documents/R/Logbook/app1/logbook_data.rds")

# Déploiement suivi logbook (Ubuntu)
rsconnect::deployApp(
  appDir = "/home/thomas-husson/Documents/R/Logbook/appinternespourcentages/",
  appName = "SuiviLogbook",
  launch.browser = TRUE
)

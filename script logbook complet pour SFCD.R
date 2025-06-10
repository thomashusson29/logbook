#tout supprimer
rm(list=ls())



#enregistrer sur github
# Chemin complet du fichier
fichier <- "/Users/thomashusson/Documents/R/Logbook/script logbook complet pour SFCD.R"

# Aller dans le dossier Git (le projet Logbook)
setwd("/Users/thomashusson/Documents/R/Logbook")

# Étape 1 : ajouter le fichier
system(paste("git add", shQuote(fichier)))

# Étape 2 : commit avec message explicite
message_commit <- "Mise à jour du script logbook complet pour SFCD"
system(paste("git commit -m", shQuote(message_commit)))

# Étape 3 : push vers GitHub
system("git push")




# Nom du fichier sur GitHub
nom_fichier <- "script logbook complet pour SFCD.R"

# Encoder pour URL (gère les espaces)
nom_fichier_url <- utils::URLencode(nom_fichier)

# URL brute du fichier sur GitHub (branche main)
url_github <- paste0(
  "https://raw.githubusercontent.com/thomashusson29/logbook/main/",
  nom_fichier_url
)

# Chemin de destination local (Téléchargements)
destination <- file.path("~/Downloads", nom_fichier)

# Télécharger le fichier
download.file(url = url_github, destfile = destination, mode = "wb")

# Message de confirmation
cat("✅ Fichier téléchargé dans : ", destination, "\n")

# Ouvrir dans RStudio (si RStudio en cours)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::navigateToFile(destination)
} else {
  cat("ℹ️ RStudio API non disponible — fichier non ouvert automatiquement.\n")
}




# Installation des packages nécessaires
install.packages(c(
  "cardx", "dplyr", "readxl", "openxlsx", "tidyverse", "gtsummary", 
  "magrittr", "ggplot2", "lubridate", "ggpubr", "survival", 
  "survminer", "summarytools", "MatchIt", "optmatch", 
  "officer", "flextable", "gt", "mice", "googlesheets4", 
  "RItools", "epiR", "tableone", "cobalt", "broom", "forcats", "dlstats", "pkgsearch", "pROC", "stats",
  "parameters", "broom.helpers", "forestplot", "kableExtra", "rsconnect"
))

# Chargement des librairies
library(forcats)
library(cardx)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(gtsummary)
library(magrittr)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(survival)
library(survminer)
library(summarytools)
library(MatchIt)
library(optmatch)
library(flextable)
library(officer)
library(gt)
library(mice)
library(googlesheets4)
library(RItools)
library(epiR)
library(tableone)
library(cobalt)
library(broom)
library(dlstats)    # for package download stats
library(pkgsearch)  # for searching packages
library(pROC)
library(stats)
library(scales)
library(parameters)
library(broom.helpers)
library(forestplot)
library(knitr)
library(kableExtra)
library(rsconnect)
library(shiny)


# Définition des URLs Google Sheets pour chaque hôpital
sheets_urls <- list(
  Cochin = "https://docs.google.com/spreadsheets/d/1ZWEY6L2vRm6VHkOw_ytbFpaeiv6h_FRQOJCVl7c1t4k/edit?usp=sharing",
  Paul_Brousse = "https://docs.google.com/spreadsheets/d/176ze81vIL38_HdT3XVThSyLbBXr4ZLCYkdjPJxAh1HI/edit?usp=sharing",
  St_Louis = "https://docs.google.com/spreadsheets/d/1w52ZALvJ2uOKgn1bcaILuQ6j0A2W1_oFzmf0hxeSUNE/edit?usp=sharing",
  HEGP = "https://docs.google.com/spreadsheets/d/1gXd9f2ZID3VL5oTTQ0j_JxtYjk9fb9C2s1LmTBhvTJg/edit?usp=sharing",
  PSL = "https://docs.google.com/spreadsheets/d/1CAfPAdzhKSbARkMZagJE5gmLMwAJn5WN4N3dJgijedE/edit?gid=0#gid=0",
  Cochin2 = "https://docs.google.com/spreadsheets/d/1bd7WkoZrHbfW3AhFfZgxgzCjJ7cv0tDgHUZ7BIahjoE/edit?gid=0#gid=0"
)

# Définition des noms de colonnes standardisés
colonnes_standard <- c(
  "DATE", "NOM_interne", "INTERVENTION", "Garde_Programme", "Ambu", 
  "OPERATEUR", "OPERATEUR_2", "RANG_BOSS", "RANG_INTERNE", "Geste", 
  "QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout", "Geste_whole_text", 
  "Geste_a_l_aise", "Si_pas_de_geste_RESSENTI", "AMBIANCE", "PEDAGOGIE", 
  "SELF_ESTIME_SORTIE", "Hôpital"
)

# Fonction pour lire et harmoniser les bases de données
read_and_clean_sheet <- function(sheet_url, hospital_name) {
  data <- read_sheet(sheet_url) %>%
    mutate(Hôpital = hospital_name) %>%
    rename_with(~ str_replace_all(., " ", "_"))  # Remplace les espaces par des underscores
  
  # Renommer uniquement les colonnes existantes
  data <- data %>%
    rename(
      Garde_Programme = any_of("Garde_Programme"),
      Ambu = any_of("Ambu"),
      OPERATEUR = any_of("OPERATEUR"),
      OPERATEUR_2 = any_of("OPERATEUR_2"),
      Geste = any_of("Geste?"),
      AMBIANCE = any_of("AMBIANCE"),
      PEDAGOGIE = any_of("PEDAGOGIE"),
      SELF_ESTIME_SORTIE = any_of("SELF_ESTIME_SORTIE"),
      Si_pas_de_geste_RESSENTI = any_of("Si_pas_de_geste_RESSENTI"),
      Geste_a_l_aise = any_of("Geste_a_l_aise")
    )
  
  # Supprimer IPP patient.e pour Cochin si elle existe
  data <- data %>%
    select(-any_of("IPP patient.e"), everything())
  
  # Ajouter les colonnes manquantes avec NA
  cols_absentes <- setdiff(colonnes_standard, colnames(data))
  for (col in cols_absentes) {
    data[[col]] <- NA_character_
  }
  
  # Réordonner les colonnes
  data <- select(data, all_of(colonnes_standard))
  
  # Convertir toutes les colonnes en caractère
  data <- mutate(data, across(everything(), as.character))
  
  return(data)
}


# Lire et fusionner toutes les bases
df <- bind_rows(lapply(names(sheets_urls), function(hospital) {
  read_and_clean_sheet(sheets_urls[[hospital]], hospital)
}))

#Harmoniser les valeurs de pédagogie
df <- df %>%
  mutate(SELF_ESTIME_SORTIE = case_when(
    SELF_ESTIME_SORTIE == "1" ~ "1-je suis un mauvais humain",
    SELF_ESTIME_SORTIE == "2" ~ "2-je suis un mauvais interne",
    SELF_ESTIME_SORTIE == "3" ~ "3-je suis inchangé",
    SELF_ESTIME_SORTIE == "4" ~ "4-je suis un bon interne",
    SELF_ESTIME_SORTIE == "5" ~ "5-je suis une brute épaisse",
    TRUE ~ SELF_ESTIME_SORTIE # Garde les valeurs correctes déjà explicitées
  )) %>%
  mutate(SELF_ESTIME_SORTIE = factor(SELF_ESTIME_SORTIE, 
                                     levels = c("1-je suis un mauvais humain", 
                                                "2-je suis un mauvais interne", 
                                                "3-je suis inchangé",
                                                "4-je suis un bon interne",
                                                "5-je suis une brute épaisse"), 
                                     ordered = TRUE)) # Facteur ordonné

# Nettoyage des espaces superflus
df <- df %>%
  mutate(SELF_ESTIME_SORTIE = str_trim(SELF_ESTIME_SORTIE)) 

#Harmoniser les valeurs de pédagogie
df <- df %>%
  mutate(PEDAGOGIE = case_when(
    PEDAGOGIE == "1" ~ "1-rien",
    PEDAGOGIE == "2" ~ "2-quasi rien",
    PEDAGOGIE == "3" ~ "3-ok",
    PEDAGOGIE == "4" ~ "4-bien",
    PEDAGOGIE == "5" ~ "5-incroyable!!",
    TRUE ~ PEDAGOGIE # Garde les valeurs correctes déjà explicitées
  )) %>%
  mutate(PEDAGOGIE = factor(PEDAGOGIE, 
                            levels = c("1-rien", 
                                       "2-quasi rien", 
                                       "3-ok",
                                       "4-bien",
                                       "5-incroyable!!"), 
                            ordered = TRUE)) # Facteur ordonné

# Nettoyage des espaces superflus
df <- df %>%
  mutate(AMBIANCE = str_trim(AMBIANCE)) 

# Harmonisation et conversion en facteur ordonné
df <- df %>%
  mutate(AMBIANCE = case_when(
    AMBIANCE == "1 - je veux partir" ~ "1 - je veux partir",
    AMBIANCE == "2 - c'est ok" ~ "2 - c'est ok",
    AMBIANCE == "3 - on recommence" ~ "3 - on recommence",
    TRUE ~ AMBIANCE  # Garde les valeurs existantes sans modification
  )) %>%
  mutate(AMBIANCE = factor(AMBIANCE, 
                           levels = c("1 - je veux partir", 
                                      "2 - c'est ok", 
                                      "3 - on recommence"), 
                           ordered = TRUE)) # Facteur ordonné


# Vérifier la structure et un aperçu du dataframe final
glimpse(df)
head(df)

df$RANG_BOSS <- gsub("Dr Junior", "DJ", df$RANG_BOSS)
df$RANG_INTERNE <- gsub("^1e aide$", "1er aide", df$RANG_INTERNE)
df <- df[!(df$RANG_BOSS == "Interne" | df$RANG_INTERNE == "Spectateur non habillé"), ]



write.csv(df, "df_logbook.csv", row.names = TRUE)


library(dplyr)
library(ggplot2)
library(scales)

# Calcul des effectifs
df_geste_global <- df %>%
  filter(!is.na(Geste)) %>%
  count(Geste) %>%
  mutate(
    pourcentage = n / sum(n),
    label = paste0(round(100 * pourcentage, 1), "%")
  )

# Bar plot vertical
ggplot(df_geste_global, aes(x = Geste, y = n, fill = Geste)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 6) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = c("Yes" = "#A3F4A3", "No" = "#F4A3A3")) +
  labs(
    title = "Répartition des gestes réalisés (tous hôpitaux confondus)",
    x = "Geste réalisé ?",
    y = "Nombre d'interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



library(dplyr)
library(ggplot2)

df <- df %>%
  mutate(Garde_Programme = case_when(
    Garde_Programme == "Astreinte" ~ "Garde",
    TRUE ~ Garde_Programme
  ))


# Préparation des données
df_plot_garde <- df %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  count(Garde_Programme, Geste) %>%
  group_by(Garde_Programme) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

# Couleurs personnalisées
couleurs <- c(
  "Programmé_Yes" = "#b2df8a",
  "Programmé_No" = "#fb9a99",
  "Garde_Yes" = "#33a02c",
  "Garde_No" = "#e31a1c"
)

# Variable combinée
df_plot_garde <- df_plot_garde %>%
  mutate(group = paste0(Garde_Programme, "_", Geste))

# Bar plot
ggplot(df_plot_garde, aes(x = Garde_Programme, y = n, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = label), position = position_dodge(width = 0.7), vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Gestes réalisés en garde vs programmé (tous hôpitaux confondus)",
    x = "Type d'intervention",
    y = "Nombre d'interventions"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


library(gtsummary)

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










library(dplyr)

df_model <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  mutate(
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    RANG_BOSS_grouped = ifelse(grepl("CCA", RANG_BOSS), "CCA", "Autre"),
    Geste = factor(Geste, levels = c("No", "Yes")),
    Garde_Programme = factor(Garde_Programme),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Autre", "CCA"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(Geste),
    !is.na(Garde_Programme)
  )


model_ambi <- glm(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped,
  data = df_model,
  family = binomial
)


library(broom)
library(broom.helpers)
library(parameters)
library(gtsummary)

tbl_mv_ambiance <- model_ambi %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      Geste ~ "Geste réalisé",
      Garde_Programme ~ "Type d’intervention",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA vs autres)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

# Affichage dans le Viewer
tbl_mv_ambiance





df_model <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  mutate(
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    RANG_BOSS_grouped = ifelse(grepl("CCA", RANG_BOSS), "CCA", "Autre"),
    AGE_INTERNE_GROUP = case_when(
      annee_DES == 1 ~ "jeune",
      annee_DES %in% c(3, 4) ~ "vieux",
      TRUE ~ NA_character_
    ),
    Geste = factor(Geste, levels = c("No", "Yes")),
    Garde_Programme = factor(Garde_Programme),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Autre", "CCA")),
    AGE_INTERNE_GROUP = factor(AGE_INTERNE_GROUP, levels = c("jeune", "vieux"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(Geste),
    !is.na(Garde_Programme),
    !is.na(AGE_INTERNE_GROUP)
  )


model_ambi <- glm(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped + AGE_INTERNE_GROUP,
  data = df_model,
  family = binomial
)


tbl_mv_ambiance <- model_ambi %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      Geste ~ "Geste réalisé",
      Garde_Programme ~ "Type d’intervention",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA vs autres)",
      AGE_INTERNE_GROUP ~ "Ancienneté (vieux vs jeune)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

tbl_mv_ambiance




df <- df %>%
  mutate(
    GESTE_SIMPLE = case_when(
      QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout %in% c("Paroi", "Incision", "Fermeture aponévrose") ~ "Petit",
      !is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) ~ "Gros",
      TRUE ~ NA_character_
    ),
    GESTE_SIMPLE = factor(GESTE_SIMPLE, levels = c("Petit", "Gros"))
  )


df_model2 <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  mutate(
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    RANG_BOSS_grouped = ifelse(grepl("CCA", RANG_BOSS), "CCA", "Autre"),
    GESTE_SIMPLE = factor(GESTE_SIMPLE, levels = c("Petit", "Gros")),
    Garde_Programme = factor(Garde_Programme),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Autre", "CCA"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(GESTE_SIMPLE),
    !is.na(Geste),
    !is.na(Garde_Programme)
  )


model_ambi2 <- glm(
  AMBIANCE_BIN ~ GESTE_SIMPLE + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped,
  data = df_model2,
  family = binomial
)

library(broom)
library(gtsummary)

tbl_mv_ambiance2 <- model_ambi2 %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      GESTE_SIMPLE ~ "Type de geste (Gros vs Petit)",
      Garde_Programme ~ "Type d’intervention",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA vs autres)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

# Envoi au Viewer
tbl_mv_ambiance2






# Chargement du package nécessaire
library(lme4)

# Modèle logistique mixte avec effet aléatoire sur l’interne
model_ambi_mixed <- glmer(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped + AGE_INTERNE_GROUP + (1 | NOM_interne),
  data = df_model,
  family = binomial
)

library(gtsummary)

tbl_mv_ambiance_mixed <- model_ambi_mixed %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      Geste ~ "Geste réalisé",
      Garde_Programme ~ "Type d’intervention",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA vs autres)",
      AGE_INTERNE_GROUP ~ "Ancienneté (vieux vs jeune)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

tbl_mv_ambiance_mixed

# Refit du modèle simple en glmer sans effet aléatoire
model_ambi_glmer_simple <- glmer(
  AMBIANCE_BIN ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped + AGE_INTERNE_GROUP + (0 + 1 | NOM_interne),
  data = df_model,
  family = binomial
)

anova(model_ambi_glmer_simple, model_ambi_mixed)



library(forestplot)
library(tibble)
library(dplyr)

# Fonction pour transformer les résultats en format forestplot
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

# Noms propres pour les termes
labels_ambi1 <- c(
  "GesteYes" = "Geste réalisé : Oui vs Non",
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde",
  "PEDAGOGIE_grouped4-5" = "Pédagogie 4-5 vs 1-2",
  "RANG_BOSS_groupedCCA" = "Boss CCA vs autres"
)

df_fp1 <- prep_forest_data(model_ambi, labels_ambi1)

tabletext1 <- cbind(
  Variable = df_fp1$label,
  OR = df_fp1$OR,
  `IC 95%` = df_fp1$IC
)

forestplot(
  labeltext = tabletext1,
  mean = df_fp1$estimate,
  lower = df_fp1$conf.low,
  upper = df_fp1$conf.high,
  zero = 1,
  xlog = TRUE,
  title = "Modèle 1 : Effet de Geste sur ambiance 'on recommence'",
  xlab = "<--- Moins probable           Plus probable --->",
  ci.vertices = TRUE,
  ci.vertices.height = 0.2,
  boxsize = 0.2,
  lwd.ci = 2,
  col = fpColors(box = "black", lines = "black", zero = "grey50")
)


labels_ambi2 <- c(
  "GESTE_SIMPLEGros" = "Gros vs Petit geste",
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde",
  "PEDAGOGIE_grouped4-5" = "Pédagogie 4-5 vs 1-2",
  "RANG_BOSS_groupedCCA" = "Boss CCA vs autres"
)

df_fp2 <- prep_forest_data(model_ambi2, labels_ambi2)

tabletext2 <- cbind(
  Variable = df_fp2$label,
  OR = df_fp2$OR,
  `IC 95%` = df_fp2$IC
)

forestplot(
  labeltext = tabletext2,
  mean = df_fp2$estimate,
  lower = df_fp2$conf.low,
  upper = df_fp2$conf.high,
  zero = 1,
  xlog = TRUE,
  title = "Modèle 2 : Effet du type de geste sur ambiance 'on recommence'",
  xlab = "<--- Moins probable           Plus probable --->",
  ci.vertices = TRUE,
  ci.vertices.height = 0.2,
  boxsize = 0.2,
  lwd.ci = 2,
  col = fpColors(box = "black", lines = "black", zero = "grey50")
)







df_model_partir1 <- df %>%
  filter(AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok", "3 - on recommence")) %>%
  mutate(
    AMBIANCE_BIN_PARTIR = ifelse(AMBIANCE == "1 - je veux partir", 1, 0),
    PEDAGOGIE_grouped = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    ),
    RANG_BOSS_grouped = ifelse(grepl("CCA", RANG_BOSS), "CCA", "Autre"),
    Geste = factor(Geste, levels = c("No", "Yes")),
    Garde_Programme = factor(Garde_Programme),
    PEDAGOGIE_grouped = factor(PEDAGOGIE_grouped, levels = c("1-2", "4-5")),
    RANG_BOSS_grouped = factor(RANG_BOSS_grouped, levels = c("Autre", "CCA"))
  ) %>%
  filter(
    !is.na(PEDAGOGIE_grouped),
    !is.na(RANG_BOSS_grouped),
    !is.na(Geste),
    !is.na(Garde_Programme)
  )


model_partir1 <- glm(
  AMBIANCE_BIN_PARTIR ~ Geste + Garde_Programme + PEDAGOGIE_grouped + RANG_BOSS_grouped,
  data = df_model_partir1,
  family = binomial
)


tbl_partir1 <- model_partir1 %>%
  tbl_regression(
    exponentiate = TRUE,
    label = list(
      Geste ~ "Geste réalisé",
      Garde_Programme ~ "Type d’intervention",
      PEDAGOGIE_grouped ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped ~ "Rang boss (CCA vs autres)"
    ),
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

tbl_partir1



df <- df %>%
  mutate(
    groupe_socle = case_when(
      NOM_interne == "Philippine" ~ "socle",
      NOM_interne == "Laya" ~ "socle",
      NOM_interne == "Marie Amélie" ~ "socle",
      NOM_interne == "Clara" & Hôpital == "HEGP" ~ "socle",
      NOM_interne == "Clara" & Hôpital != "HEGP" ~ "non socle",
      !is.na(NOM_interne) ~ "non socle",
      TRUE ~ NA_character_
    ),
    groupe_socle = factor(groupe_socle, levels = c("non socle", "socle"))
  )

library(gtsummary)

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

library(dplyr)
library(ggplot2)
library(scales)

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
ggplot(df_geste_socle_plot, aes(x = groupe_socle, y = n, fill = Geste)) +
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





library(dplyr)
library(ggplot2)

df_geste_detail <- df %>%
  filter(!is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout), !is.na(Geste)) %>%
  group_by(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
  summarise(
    total = n(),
    gestes_realises = sum(Geste == "Yes"),
    pourcentage = round(100 * gestes_realises / total, 1),
    label = paste0(gestes_realises, "/", total, " (", pourcentage, "%)")
  ) %>%
  arrange(total)

# Bar plot horizontal
ggplot(df_geste_detail, aes(x = reorder(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, total), y = total)) +
  geom_bar(stat = "identity", fill = "grey90", width = 0.7) +
  geom_bar(aes(y = gestes_realises, fill = QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout),
           stat = "identity", width = 0.7) +
  geom_text(aes(y = gestes_realises + 1, label = label), hjust = 0, size = 4.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Nombre de gestes réalisés selon le type de geste déclaré",
    x = "Type de geste",
    y = "Nombre total d'interventions"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")



library(dplyr)
library(ggplot2)

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
    missing = "no"
  ) %>%
  modify_header(label = "**Ressenti**") %>%
  bold_labels()

# Bar plot vertical
ggplot(df_ressenti, aes(x = Si_pas_de_geste_RESSENTI, y = n, fill = Si_pas_de_geste_RESSENTI)) +
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




library(dplyr)
library(ggplot2)

# Nettoyage et comptage
df_a_l_aise <- df %>%
  filter(!is.na(Geste_a_l_aise)) %>%
  count(Geste_a_l_aise) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )


table(df$Geste_a_l_aise)

 
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
ggplot(df_a_l_aise, aes(x = Geste_a_l_aise, y = n, fill = Geste_a_l_aise)) +
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
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


internes <- df %>%
  filter(!is.na(NOM_interne)) %>%
  distinct(NOM_interne) %>%
  arrange(NOM_interne)

print(internes,n=99)


df <- df %>%
  mutate(
    DES = case_when(
      NOM_interne %in% c(
        "Alice", "Antoine", "Aubin", "Charlotte", "Chloé", "Clara", "François",
        "Gabrielle", "Gaby", "Ghita", "Kevin", "Léa", "Marc Anthony", "Marie Amélie",
        "Mathilde", "Pauline", "Philippine", "Rodolphe", "Thomas"
      ) ~ "oui",
      !is.na(NOM_interne) ~ "non",
      TRUE ~ NA_character_
    ),
    DES = factor(DES, levels = c("non", "oui"))
  )

# Liste des noms d'internes DES
noms_des <- c(
  "Alice", "Antoine", "Aubin", "Charlotte", "Chloé", "Clara", "François",
  "Gabrielle", "Gaby", "Ghita", "Kevin", "Léa", "Marc Anthony", "Marie Amélie",
  "Mathilde", "Pauline", "Philippine", "Rodolphe", "Thomas"
)

# Affichage des internes DES dans la base
internes_des <- df %>%
  filter(NOM_interne %in% noms_des) %>%
  distinct(NOM_interne) %>%
  arrange(NOM_interne)

print(internes_des, n = Inf)

df <- df %>%
  mutate(
    annee_DES = case_when(
      NOM_interne == "Alice" ~ 4,
      NOM_interne == "Antoine" ~ 3,
      NOM_interne == "Aubin" ~ 2,
      NOM_interne == "Charlotte" ~ 2,
      NOM_interne == "Chloé" ~ 4,
      NOM_interne == "Clara" ~ 2,
      NOM_interne == "François" ~ 2,
      NOM_interne == "Gabrielle" ~ 3,
      NOM_interne == "Gaby" ~ 3,
      NOM_interne == "Ghita" ~ 2,
      NOM_interne == "Kevin" ~ 4,
      NOM_interne == "Léa" ~ 3,
      NOM_interne == "Marc Anthony" ~ 4,
      NOM_interne == "Marie Amélie" ~ 1,
      NOM_interne == "Mathilde" ~ 2,
      NOM_interne == "Pauline" ~ 3,
      NOM_interne == "Philippine" ~ 1,
      NOM_interne == "Rodolphe" ~ 3,
      NOM_interne == "Thomas" ~ 3,
      TRUE ~ NA_real_
    )
  )

library(dplyr)
library(ggplot2)
library(scales)

# Préparer les données
df_geste_annee <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste)) %>%
  group_by(annee_DES) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux = n_geste / total,
    .groups = "drop"
  )

ggplot(df_geste_annee, aes(x = annee_DES, y = taux)) +
  geom_line(color = "#377eb8", size = 1.5) +
  geom_point(size = 3, color = "#377eb8") +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")),
            vjust = -0.8, size = 5) +
  geom_text(aes(label = paste0("n = ", total)),
            vjust = 1.8, size = 4, color = "grey30") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Taux de gestes réalisés selon l’année d’internat (DES)",
    subtitle = "Avec annotation des effectifs (n)",
    x = "Année d’internat",
    y = "Taux de gestes réalisés"
  ) +
  theme_minimal(base_size = 14)

ggplot(df_geste_annee, aes(x = annee_DES, y = taux)) +
  geom_point(size = 3, color = "#377eb8") +
  geom_smooth(method = "loess", se = FALSE, color = "#377eb8", size = 1.5) +
  geom_text(aes(label = paste0(round(taux * 100, 1), "%")),
            vjust = -0.8, size = 5) +
  geom_text(aes(label = paste0("n = ", total)),
            vjust = 1.8, size = 4, color = "grey30") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(
    title = "Taux de gestes réalisés (lissé) selon l’année d’internat (DES)",
    subtitle = "Lissage LOESS avec effectifs",
    x = "Année d’internat",
    y = "Taux de gestes réalisés"
  ) +
  theme_minimal(base_size = 14)


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


df <- df %>%
  mutate(
    phase = case_when(
      annee_DES == 1 ~ "socle",
      !is.na(annee_DES) ~ "pas socle",
      TRUE ~ NA_character_
    ),
    phase = factor(phase, levels = c("socle", "pas socle"))
  )

df %>%
  filter(annee_DES == 1) %>%
  distinct(NOM_interne) %>%
  arrange(NOM_interne)


library(gtsummary)

tbl_geste_phase <- df %>%
  filter(!is.na(phase), !is.na(Geste)) %>%
  tbl_summary(
    by = phase,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_geste_phase

df <- df %>%
  mutate(
    GESTE_TOUT = case_when(
      str_detect(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, regex("Tout", ignore_case = TRUE)) ~ "Tout",
      !is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) ~ "Autre",
      TRUE ~ NA_character_
    ),
    GESTE_TOUT = factor(GESTE_TOUT, levels = c("Autre", "Tout"))
  )

library(gtsummary)


df <- df %>%
  mutate(
    GESTE_SIMPLE = case_when(
      QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout %in% c("Paroi", "Incision", "Fermeture aponévrose") ~ "Petit",
      !is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) ~ "Gros",
      TRUE ~ NA_character_
    ),
    GESTE_SIMPLE = factor(GESTE_SIMPLE, levels = c("Petit", "Gros"))
  )

tbl_type_geste_phase <- df %>%
  filter(!is.na(phase), !is.na(GESTE_SIMPLE)) %>%
  tbl_summary(
    by = phase,
    include = GESTE_SIMPLE,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Type de geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_type_geste_phase

df_garde_socle <- df %>%
  filter(
    Garde_Programme == "Garde",
    !is.na(phase),
    !is.na(Geste)
  )

library(gtsummary)

tbl_garde_geste_socle <- df_garde_socle %>%
  tbl_summary(
    by = phase,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé en garde**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_garde_geste_socle


library(dplyr)
library(ggplot2)
library(scales)

df_bar <- df %>%
  filter(!is.na(Geste), !is.na(phase), !is.na(Garde_Programme)) %>%
  group_by(phase, Garde_Programme) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(round(100 * taux_yes, 1), "%"),
    group = paste(phase, Garde_Programme, sep = " - ")
  )

ggplot(df_bar, aes(x = group, y = taux_yes, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "socle - Programmé" = "#a6cee3",
    "socle - Garde" = "#1f78b4",
    "pas socle - Programmé" = "#b2df8a",
    "pas socle - Garde" = "#33a02c"
  )) +
  labs(
    title = "Taux de gestes réalisés selon phase et type d’intervention",
    x = "Groupe",
    y = "Taux de gestes réalisés (Yes)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


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

ggplot(df_age_geste_gp, aes(x = annee_DES, y = taux_yes, color = Garde_Programme)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = label_pct), vjust = -0.8, size = 5) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c("Programmé" = "#1f78b4", "Garde" = "#33a02c")) +
  labs(
    title = "Taux de gestes réalisés selon l’année d’internat (DES)",
    subtitle = "Comparaison entre interventions programmées et gardes",
    x = "Année d’internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d’intervention"
  ) +
  theme_minimal(base_size = 14)


library(lubridate)

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



library(gtsummary)

tbl_geste_stage <- df %>%
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

tbl_geste_stage




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

ggplot() +
  geom_jitter(
    data = df_semestre,
    aes(x = DATE, y = as.numeric(Geste == "Yes")),
    width = 5, height = 0.05,
    color = "grey60", alpha = 0.4, size = 1.5
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
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Évolution du taux de gestes réalisés (par quinzaine)",
    x = "Date",
    y = "Taux de gestes réalisés (Yes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vérification des lignes concernées
df %>%
  filter(as.Date(DATE) > as.Date("2025-05-31")) %>%
  select(NOM_interne, DATE, INTERVENTION)

# Suppression des lignes après mai 2025
df <- df %>%
  filter(as.Date(DATE) <= as.Date("2025-05-31"))


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

library(broom)

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


library(ggplot2)
library(scales)

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

df %>%
  filter(!is.na(PEDAGOGIE)) %>%
  count(PEDAGOGIE) %>%
  mutate(pourcent = n / sum(n)) %>%
  ggplot(aes(x = PEDAGOGIE, y = n, fill = PEDAGOGIE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(100 * pourcent, 1), "%)")),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_pedagogie) +
  labs(title = "Répartition de la pédagogie perçue",
       x = "Pédagogie",
       y = "Nombre d’observations") +
  theme_minimal() +
  theme(legend.position = "none")


# Tableau de répartition
df %>%
  select(SELF_ESTIME_SORTIE) %>%
  tbl_summary(
    label = list(SELF_ESTIME_SORTIE ~ "Self-estime en sortie"),
    missing = "no"
  ) %>%
  modify_header(label = "**Self-estime**") %>%
  bold_labels()

# Bar plot vertical
couleurs_self <- c(
  "1-je suis un mauvais humain" = "#f768a1",
  "2-je suis un mauvais interne" = "#fdae6b",
  "3-je suis inchangé" = "#ffff99",
  "4-je suis un bon interne" = "#a1d99b",
  "5-je suis une brute épaisse" = "#9ecae1"
)

df %>%
  filter(!is.na(SELF_ESTIME_SORTIE)) %>%
  count(SELF_ESTIME_SORTIE) %>%
  mutate(pourcent = n / sum(n)) %>%
  ggplot(aes(x = SELF_ESTIME_SORTIE, y = n, fill = SELF_ESTIME_SORTIE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(100 * pourcent, 1), "%)")),
            vjust = -0.5, size = 4.5) +
  scale_fill_manual(values = couleurs_self) +
  labs(title = "Répartition de la self-estime en sortie de bloc",
       x = "Self-estime",
       y = "Nombre d’observations") +
  theme_minimal() +
  theme(legend.position = "none")



library(dplyr)
library(ggplot2)

# Nettoyage
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

# Calcul des taux
df_taux <- df_clean %>%
  group_by(SELF_ESTIME_SORTIE) %>%
  summarise(
    total = n(),
    n_yes = sum(Geste == "Yes"),
    taux_yes = n_yes / total,
    .groups = "drop"
  )

# Couleurs pastel
fill_pastel <- c(
  "1-je suis un mauvais humain" = "#f768a1",
  "2-je suis un mauvais interne" = "#fdae6b",
  "3-je suis inchangé" = "#ffff99",
  "4-je suis un bon interne" = "#a1d99b",
  "5-je suis une brute épaisse" = "#9ecae1"
)

# Plot
ggplot(df_taux, aes(x = SELF_ESTIME_SORTIE, y = taux_yes, group = 1)) +
  # Fond pastel
  geom_rect(
    aes(
      xmin = as.numeric(SELF_ESTIME_SORTIE) - 0.5,
      xmax = as.numeric(SELF_ESTIME_SORTIE) + 0.5,
      ymin = -Inf, ymax = Inf,
      fill = SELF_ESTIME_SORTIE
    ),
    alpha = 0.4, color = NA
  ) +
  # Lignes verticales
  geom_vline(xintercept = 1.5:4.5, linetype = "dotted", color = "grey40") +
  # Courbe + points + labels
  geom_line(color = "#377eb8", size = 1.2) +
  geom_point(size = 3, color = "#377eb8") +
  geom_text(
    aes(label = paste0(round(100 * taux_yes, 1), "%")),
    vjust = -0.8, size = 4.5
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = fill_pastel) +
  labs(
    title = "Taux de gestes réalisés selon la self-estime de sortie",
    x = "Self-estime de sortie",
    y = "Taux de gestes réalisés"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")





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




df_hiver <- df %>%
  filter(!is.na(AMBIANCE), as.Date(DATE) >= as.Date("2024-11-02"), as.Date(DATE) <= as.Date("2025-04-30")) %>%
  mutate(
    DATE = as.Date(DATE),
    AMBIANCE_BIN = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    date_debut = as.Date(cut(DATE, breaks = "14 days"))
  )

df_taux_on_recommence <- df_hiver %>%
  group_by(date_debut) %>%
  summarise(
    total = n(),
    n_recommence = sum(AMBIANCE_BIN == 1),
    taux_recommence = n_recommence / total,
    .groups = "drop"
  )

modele_logit_reco <- glm(AMBIANCE_BIN ~ as.numeric(DATE), data = df_hiver, family = binomial)
OR_jour <- exp(coef(modele_logit_reco)["as.numeric(DATE)"])
OR_mois <- OR_jour^30

cor_spearman <- cor.test(as.numeric(df_hiver$DATE), df_hiver$AMBIANCE_BIN, method = "spearman")

df_hiver <- df_hiver %>%
  mutate(proba_reco = predict(modele_logit_reco, type = "response"))

ggplot(df_hiver, aes(x = DATE)) +
  geom_jitter(aes(y = AMBIANCE_BIN), width = 5, height = 0.05, alpha = 0.3, color = "grey60") +
  geom_line(aes(y = proba_reco), color = "#d95f02", size = 1.5) +
  geom_point(aes(y = proba_reco), size = 2.5, color = "#d95f02") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Probabilité de coder 'on recommence' au fil du semestre d’hiver",
    subtitle = "Régression logistique (2 nov 2024 → 30 avril 2025)",
    x = "Date", y = "Probabilité codée 'on recommence'"
  ) +
  annotate("text", x = as.Date("2024-11-25"), y = 0.75, hjust = 0,
           label = paste0("OR/mois : ", round(OR_mois, 2),
                          "\nSpearman rho = ", round(cor_spearman$estimate, 3),
                          ", p = ", signif(cor_spearman$p.value, 3))) +
  theme_minimal(base_size = 14)


library(ggplot2)
library(scales)

ggplot(df_taux_on_recommence, aes(x = date_debut, y = taux_recommence)) +
  geom_line(color = "#d95f02", size = 1.5) +
  geom_point(size = 3, color = "#d95f02") +
  geom_text(aes(label = paste0(round(100 * taux_recommence, 1), "%")), 
            vjust = -0.8, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Taux de codage 'on recommence' par quinzaine",
    subtitle = "Semestre d’hiver 2025 (2 nov 2024 → 30 avril 2025)",
    x = "Date", y = "Taux 'on recommence'"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





library(ggplot2)
library(dplyr)

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



#RECODAGE DE TOUTES LES INTERVENTIONS
library(dplyr)
library(stringr)


df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      # Bloc général
      str_detect(INTERVENTION, regex("back ?table", TRUE)) ~ "Back table greffe hépatique",
      str_detect(INTERVENTION, regex("1er.*ALPPS", TRUE)) ~ "1er temps de ALPPS",
      str_detect(INTERVENTION, regex("babcock|beaulieu|baulieu", TRUE)) ~ "2e temps colo-anale différée",
      str_detect(INTERVENTION, regex("recoupe", TRUE)) ~ "2e temps colo-anale différée",
      
      # Thyroïde
      str_detect(INTERVENTION, regex("(^TT$)|thyro[iï]dectomie.*totale|totalisation", TRUE)) ~ "Thyroïdectomie totale",
      str_detect(INTERVENTION, regex("lobo.*isthmo|loboisthmectomie|lobectomie.*droite|lobectomie.*gauche|isthmectomie", TRUE)) ~ "Loboisthmectomie thyroïdienne",
      
      # Stomies
      str_detect(INTERVENTION, regex("colostomie.*laparo", TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("colostomie", TRUE)) ~ "Colostomie (coelio)",
      str_detect(INTERVENTION, regex("iléostomie.*laparo", TRUE)) ~ "Iléostomie (laparo)",
      str_detect(INTERVENTION, regex("iléostomie", TRUE)) ~ "Iléostomie (coelio)",
      
      # PAC
      str_detect(INTERVENTION, regex("pose.*pac|pac.*pose", TRUE)) ~ "Pose de PAC",
      
      # Exploration
      str_detect(INTERVENTION, regex("examen anal|fournier|fesse|fistule anale", TRUE)) ~ "Examen anal",
      str_detect(INTERVENTION, regex("exérèse P3 gauche|parathyro|4 sites", TRUE)) ~ "Exploration des 4 sites",
      str_detect(INTERVENTION, regex("laparo.*explo|exploration|bodypacker|corps étranger|fistulisation|laparo.*fistule", TRUE)) ~ "Laparotomie exploratrice",
      
      # Par défaut
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,  # Ne pas écraser les recodages précédents
      
      # Œsophagectomies
      str_detect(INTERVENTION, regex("lewis.*santy", TRUE)) ~ "Œsophagectomie de Lewis Santy",
      str_detect(INTERVENTION, regex("3 voies", TRUE)) ~ "Œsophagectomie 3 voies",
      
      # Estomac
      str_detect(INTERVENTION, regex("gastrectomie", TRUE)) ~ "Gastrectomie totale",
      str_detect(INTERVENTION, regex("ulcère.*perforé", TRUE)) ~ "Ulcère perforé (coelio)",
      
      # Bypass
      str_detect(INTERVENTION, regex("bypass|rygb", TRUE)) ~ "Bypass gastrique (coelio)",
      str_detect(INTERVENTION, regex("reprise.*bypass", TRUE)) ~ "Reprise bypass gastrique",
      
      # Sleeve
      str_detect(INTERVENTION, regex("sleeve", TRUE)) ~ "Sleeve gastrectomie (coelio)",
      
      # Fundoplicature
      str_detect(INTERVENTION, regex("fundoplicature|nissen|hernie.*hiatale", TRUE)) ~ "Cure hernie hiatale",
      
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # Transplantations
      str_detect(INTERVENTION, regex("^TH$|transplantation.*hépatique|re-TH", TRUE)) ~ "Transplantation hépatique",
      str_detect(INTERVENTION, regex("transplantation.*pancréatique", TRUE)) ~ "Transplantation pancréatique",
      str_detect(INTERVENTION, regex("reprise.*transplant", TRUE)) ~ "Reprise transplantation",
      
      # PMO
      str_detect(INTERVENTION, regex("pmo.*foie", TRUE)) ~ "Prélèvement hépatique",
      str_detect(INTERVENTION, regex("pmo.*pancr", TRUE)) ~ "Prélèvement pancréatique",
      
      # Donneurs vivants
      str_detect(INTERVENTION, regex("donneur vivant.*robot", TRUE)) ~ "Donneur vivant (robot)",
      str_detect(INTERVENTION, regex("donneur vivant.*coelio", TRUE)) ~ "Donneur vivant (coelio)",
      str_detect(INTERVENTION, regex("donneur vivant|pmo.*vivant", TRUE)) ~ "Donneur vivant (laparo)",
      
      # SPG
      str_detect(INTERVENTION, regex("spg.*robot|pancréatectomie.*gauche.*robot", TRUE)) ~ "SPG (robot)",
      str_detect(INTERVENTION, regex("spg.*laparo|pancréatectomie.*gauche.*laparo", TRUE)) ~ "SPG (laparo)",
      str_detect(INTERVENTION, regex("spg|pancréatectomie.*gauche", TRUE)) ~ "SPG (coelio)",
      
      # DPC
      str_detect(INTERVENTION, regex("dpc.*robot", TRUE)) ~ "Pancréatectomie céphalique (robot)",
      str_detect(INTERVENTION, regex("dpc", TRUE)) ~ "Pancréatectomie céphalique (laparo)",
      str_detect(INTERVENTION, regex("reprise.*dpc", TRUE)) ~ "Reprise pancréatectomie",
      
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # Colectomies droites
      str_detect(INTERVENTION, regex("colectomie.*droite", TRUE)) ~ "Colectomie droite (coelio)",
      
      # Colectomies gauches
      str_detect(INTERVENTION, regex("colectomie.*gauche.*laparo", TRUE)) ~ "Colectomie gauche (laparo)",
      str_detect(INTERVENTION, regex("colectomie.*gauche|sigmoidectomie|sigmoïdectomie", TRUE)) ~ "Colectomie gauche (coelio)",
      
      # Colectomie subtotale
      str_detect(INTERVENTION, regex("colectomie.*sub[- ]?totale", TRUE)) ~ "Colectomie subtotale (laparo)",
      
      # Hartmann
      str_detect(INTERVENTION, regex("hartmann.*coelio", TRUE)) ~ "Hartmann (coelio)",
      str_detect(INTERVENTION, regex("hartmann", TRUE)) ~ "Hartmann (laparo)",
      
      # Résection iléo-caecale
      str_detect(INTERVENTION, regex("résection.*il[ée]o[- ]?caecale.*laparo", TRUE)) ~ "Résection iléo-caecale (laparo)",
      str_detect(INTERVENTION, regex("résection.*il[ée]o[- ]?caecale|^ric$|^ric ", TRUE)) ~ "Résection iléo-caecale (coelio)",
      
      # Rectum
      str_detect(INTERVENTION, regex("rectum.*robot|rrs.*robot", TRUE)) ~ "Rectum (robot)",
      str_detect(INTERVENTION, regex("rectum.*laparo|rrs.*laparo", TRUE)) ~ "Rectum (laparo)",
      str_detect(INTERVENTION, regex("rectum|rrs", TRUE)) ~ "Rectum (coelio)",
      
      # Stomies
      str_detect(INTERVENTION, regex("colostomie.*laparo", TRUE)) ~ "Colostomie (laparo)",
      str_detect(INTERVENTION, regex("colostomie", TRUE)) ~ "Colostomie (coelio)",
      str_detect(INTERVENTION, regex("iléostomie.*laparo", TRUE)) ~ "Iléostomie (laparo)",
      str_detect(INTERVENTION, regex("iléostomie", TRUE)) ~ "Iléostomie (coelio)",
      
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # Cure hernie inguinale
      str_detect(INTERVENTION, regex("lichtenstein|licht", TRUE)) ~ "Cure hernie inguinale (open)",
      str_detect(INTERVENTION, regex("tep", TRUE)) ~ "Cure hernie inguinale (TEP)",
      str_detect(INTERVENTION, regex("tapp|coelio.*inguinale", TRUE)) ~ "Cure hernie inguinale (coelio)",
      str_detect(INTERVENTION, regex("hernie inguinale.*(laparo|abord direct|open)", TRUE)) ~ "Cure hernie inguinale (open)",
      str_detect(INTERVENTION, regex("hernie inguinale", TRUE)) ~ "Cure hernie inguinale (open)",
      
      # Hernie ombilicale
      str_detect(INTERVENTION, regex("hernie.*ombilicale", TRUE)) ~ "Cure hernie ombilicale",
      
      # Hernie ligne blanche
      str_detect(INTERVENTION, regex("hernie.*ligne blanche", TRUE)) ~ "Cure hernie ligne blanche",
      
      # Hernie hiatale
      str_detect(INTERVENTION, regex("hernie hiatale.*robot", TRUE)) ~ "Cure hernie hiatale (robot)",
      str_detect(INTERVENTION, regex("hernie hiatale|fundoplicature|nissen", TRUE)) ~ "Cure hernie hiatale",
      
      # Cure éventration
      str_detect(INTERVENTION, regex("éventration.*coelio", TRUE)) ~ "Cure d’éventration (coelio)",
      str_detect(INTERVENTION, regex("éventration", TRUE)) ~ "Cure d’éventration (laparo)",
      
      # Ulcère perforé
      str_detect(INTERVENTION, regex("ulcère.*perforé", TRUE)) ~ "Ulcère perforé (coelio)",
      
      # Fissurectomie
      str_detect(INTERVENTION, regex("fissurectomie", TRUE)) ~ "Fissurectomie",
      
      # Kyste pilonidal
      str_detect(INTERVENTION, regex("kyste.*pilonidal|sinus.*pilonidal", TRUE)) ~ "Exérèse sinus pilonidal",
      
      # TIPS
      str_detect(INTERVENTION, regex("tips", TRUE)) ~ "Pose de TIPS",
      
      # Fenestration kyste foie
      str_detect(INTERVENTION, regex("fenestration.*kyste.*foie|kyste hépatique", TRUE)) ~ "Fenestration kyste hépatique (coelio)",
      
      TRUE ~ NA_character_
    )
  )

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      !is.na(INTERVENTION_GROUPÉE) ~ INTERVENTION_GROUPÉE,
      
      # TEM
      str_detect(INTERVENTION, regex("^tem$|exérèse.*muqueuse.*transanale", TRUE)) ~ "TEM (chirurgie transanale)",
      
      # Vaginoplastie
      str_detect(INTERVENTION, regex("vaginoplastie|vagino", TRUE)) ~ "Vaginoplastie",
      
      # Biopsie intra-abdominale
      str_detect(INTERVENTION, regex("biopsie.*ganglion|biopsie.*m[ée]sent[ée]rique", TRUE)) ~ "Biopsie intra-abdominale",
      
      # Exploration des 4 sites
      str_detect(INTERVENTION, regex("4 sites|exérèse P3|parathyro", TRUE)) ~ "Exploration des 4 sites",
      
      # Laparotomie exploratrice
      str_detect(INTERVENTION, regex("laparo explo|laparotomie exploratrice|bodypacker|corps étranger|fistule.*(iléocutanée|gastro)", TRUE)) ~ "Laparotomie exploratrice",
      
      # Reprise
      str_detect(INTERVENTION, regex("reprise.*transplant", TRUE)) ~ "Reprise transplantation",
      str_detect(INTERVENTION, regex("reprise.*dpc", TRUE)) ~ "Reprise pancréatectomie",
      str_detect(INTERVENTION, regex("reprise", TRUE)) ~ "Reprise chirurgicale",
      
      # Réintervention
      str_detect(INTERVENTION, regex("réintervention", TRUE)) ~ "Réintervention",
      
      # Drainage chirurgical
      str_detect(INTERVENTION, regex("drainage", TRUE)) ~ "Drainage chirurgical",
      
      # Fistule digestive
      str_detect(INTERVENTION, regex("fistule", TRUE)) ~ "Fistule digestive",
      
      # Pose de PAC
      str_detect(INTERVENTION, regex("pac", TRUE)) ~ "Pose de PAC",
      
      # Back Table
      str_detect(INTERVENTION, regex("back ?table", TRUE)) ~ "Back table greffe hépatique",
      
      TRUE ~ "Autre"
    )
  )





# Bloc 1/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("1er[e]? temps de ALPPS", ignore_case = TRUE)) ~ "1er temps de ALPPS",
    str_detect(INTERVENTION, regex("2e temps colo.?anale", ignore_case = TRUE)) ~ "2e temps colo-anale différée",
    str_detect(INTERVENTION, regex("Back table", ignore_case = TRUE)) ~ "Back table greffe hépatique",
    str_detect(INTERVENTION, regex("Biopsie hépatique", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("Biopsie intra-abdominale", ignore_case = TRUE)) ~ "Biopsie intra-abdominale",
    str_detect(INTERVENTION, regex("By.?pass.*robot", ignore_case = TRUE)) ~ "Bypass gastrique (robot)",
    str_detect(INTERVENTION, regex("By.?pass", ignore_case = TRUE)) ~ "Bypass gastrique (coelio)",
    str_detect(INTERVENTION, regex("Cytoréduction.*laparo", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("Cytoréduction|CHIP", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("Donneur vivant.*robot", ignore_case = TRUE)) ~ "Donneur vivant (robot)",
    str_detect(INTERVENTION, regex("Don vivant|Donneur vivant", ignore_case = TRUE)) ~ "Donneur vivant (coelio)",
    str_detect(INTERVENTION, regex("Exploration des 4 sites|4 sites|parathyroidectomie", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 2/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("abcès.*MA|abcès.*marge|abcès.*fesse", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("amput.*abdomino.*périn.*", ignore_case = TRUE)) ~ "Amputation abdomino périnéale",
    str_detect(INTERVENTION, regex("Annexectomie.*laparo", ignore_case = TRUE)) ~ "Annexectomie laparotomie",
    str_detect(INTERVENTION, regex("Appendicectomie|appendicite|APP", ignore_case = TRUE)) ~ "Appendicectomie coelio",
    str_detect(INTERVENTION, regex("Cholécystectomie.*coelio|cholecystectomie.*coelio|vesicule", ignore_case = TRUE)) ~ "Cholécystectomie",
    str_detect(INTERVENTION, regex("cholecystectomie.*laparo|cholécystectomie.*laparo", ignore_case = TRUE)) ~ "Cholécystectomie",
    str_detect(INTERVENTION, regex("^Cholécystectomie$", ignore_case = TRUE)) ~ "Cholécystectomie",
    str_detect(INTERVENTION, regex("coelio explo|exploratrice|exploration|laparo explo", ignore_case = TRUE)) ~ "Laparotomie exploratrice",
    str_detect(INTERVENTION, regex("Examen.*anal|procto", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("Fistule anale|fistule iléo|fistulisation|gastro-péricardique", ignore_case = TRUE)) ~ "Fistule digestive",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 3/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("Cytoréduction|cytoreduction|CHIP", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("colostomie.*coelio", ignore_case = TRUE)) ~ "Colostomie (coelio)",
    str_detect(INTERVENTION, regex("colostomie.*laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
    str_detect(INTERVENTION, regex("colostomie", ignore_case = TRUE)) ~ "Colostomie (coelio)",
    str_detect(INTERVENTION, regex("Iléostomie.*laparo", ignore_case = TRUE)) ~ "Iléostomie (laparo)",
    str_detect(INTERVENTION, regex("Iléostomie.*coelio", ignore_case = TRUE)) ~ "Iléostomie (coelio)",
    str_detect(INTERVENTION, regex("Iléostomie", ignore_case = TRUE)) ~ "Iléostomie (coelio)",
    str_detect(INTERVENTION, regex("Exérèse sinus pilonidal|sinus pilonidal", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("Gastrectomie totale", ignore_case = TRUE)) ~ "Gastrectomie totale",
    str_detect(INTERVENTION, regex("Ulcère.*perforé", ignore_case = TRUE)) ~ "Ulcère perforé (coelio)",
    str_detect(INTERVENTION, regex("TEM|chirurgie transanale", ignore_case = TRUE)) ~ "TEM (chirurgie transanale)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 4/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("Transplantation hépatique|TH(?!Y)|re-TH", ignore_case = TRUE)) ~ "Transplantation hépatique",
    str_detect(INTERVENTION, regex("Transplantation pancréatique|TPR", ignore_case = TRUE)) ~ "Transplantation pancréatique",
    str_detect(INTERVENTION, regex("Prélèvement.*(hépatique|foie)", ignore_case = TRUE)) ~ "Prélèvement hépatique",
    str_detect(INTERVENTION, regex("Prélèvement.*pancr", ignore_case = TRUE)) ~ "Prélèvement pancréatique",
    str_detect(INTERVENTION, regex("Donneur vivant", ignore_case = TRUE)) ~ "Donneur vivant (coelio)",
    str_detect(INTERVENTION, regex("rectum.*coelio", ignore_case = TRUE)) ~ "Rectum (coelio)",
    str_detect(INTERVENTION, regex("rectum.*laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
    str_detect(INTERVENTION, regex("rectum.*robot", ignore_case = TRUE)) ~ "Rectum (robot)",
    str_detect(INTERVENTION, regex("rectum", ignore_case = TRUE)) ~ "Rectum (coelio)",
    str_detect(INTERVENTION, regex("Résection iléo-caecale.*laparo", ignore_case = TRUE)) ~ "Résection iléo-caecale (laparo)",
    str_detect(INTERVENTION, regex("Résection iléo-caecale", ignore_case = TRUE)) ~ "Résection iléo-caecale (coelio)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 5/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("SPG.*robot", ignore_case = TRUE)) ~ "SPG (robot)",
    str_detect(INTERVENTION, regex("SPG.*laparo", ignore_case = TRUE)) ~ "SPG (laparo)",
    str_detect(INTERVENTION, regex("SPG", ignore_case = TRUE)) ~ "SPG (coelio)",
    str_detect(INTERVENTION, regex("Pancréatectomie céphalique.*robot", ignore_case = TRUE)) ~ "Pancréatectomie céphalique (robot)",
    str_detect(INTERVENTION, regex("Pancréatectomie céphalique.*laparo", ignore_case = TRUE)) ~ "Pancréatectomie céphalique (laparo)",
    str_detect(INTERVENTION, regex("Pancréatectomie céphalique", ignore_case = TRUE)) ~ "Pancréatectomie céphalique (laparo)",
    str_detect(INTERVENTION, regex("Pose de PAC", ignore_case = TRUE)) ~ "Pose de PAC",
    str_detect(INTERVENTION, regex("TIPS", ignore_case = TRUE)) ~ "Pose de TIPS",
    str_detect(INTERVENTION, regex("Œsophagectomie de Lewis Santy", ignore_case = TRUE)) ~ "Œsophagectomie de Lewis Santy",
    str_detect(INTERVENTION, regex("Lewis Santy", ignore_case = TRUE)) ~ "Œsophagectomie de Lewis Santy",
    str_detect(INTERVENTION, regex("3 voies", ignore_case = TRUE)) ~ "Œsophagectomie 3 voies",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 6/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("Back table.*greffe hépatique", ignore_case = TRUE)) ~ "Back table greffe hépatique",
    str_detect(INTERVENTION, regex("Back table", ignore_case = TRUE)) ~ "Back table greffe hépatique",
    str_detect(INTERVENTION, regex("Biopsie.*intra-abdominale", ignore_case = TRUE)) ~ "Biopsie intra-abdominale",
    str_detect(INTERVENTION, regex("Biopsie.*hépatique.*percutanée", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("Biopsie.*hépatique", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("Biopsie.*", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("Drainage.*", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Changement.*drain.*biliaire", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Ponction.*abcès.*hépatique", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Drinage.*", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Explantation hépatique", ignore_case = TRUE)) ~ "Reprise transplantation",
    str_detect(INTERVENTION, regex("Reprise.*transplantation", ignore_case = TRUE)) ~ "Reprise transplantation",
    str_detect(INTERVENTION, regex("Reprise DPC", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    str_detect(INTERVENTION, regex("Reprise chirurgicale", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 7/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("CHIP", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("Cytoréduction.*", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("Cytochip", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("cyto.*réduc", ignore_case = TRUE)) ~ "Cytoréduction (laparo)",
    str_detect(INTERVENTION, regex("lavage.*paroi", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("lavage.*hématome", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Laparostomie", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("VAC", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("Milligan.*Morgan", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("fistule.*anal.*séton", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("abcès.*marge.*anal", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*MA", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*marge", ignore_case = TRUE)) ~ "Abcès de marge anale",
    TRUE ~ INTERVENTION_GROUPÉE
  ))


# Bloc 8/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("exérèse.*lipome", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("kyste.*sébacé", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("kyste pilo", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("sinus pilonidal", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("exérèse.*kyste.*(pelvien|sous-cut|sous cut|paroi)", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("parathy", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    str_detect(INTERVENTION, regex("4 sites", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    str_detect(INTERVENTION, regex("exploration.*sites", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    str_detect(INTERVENTION, regex("exérèse.*ganglion.*coeli", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    str_detect(INTERVENTION, regex("biopsie.*(canal anal|lésion anale|procto)", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("examen.*anal", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("examen.*procto", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("fissurectomie", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("biopsie.*hépatique.*percutanée", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("biopsie.*hépatique", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("biopsie.*foie", ignore_case = TRUE)) ~ "Biopsie hépatique",
    TRUE ~ INTERVENTION_GROUPÉE
  ))


# Bloc 9/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("fenestration.*kyste", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("drainage.*abcès", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("ponction.*abcès", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("évacuation.*hématome", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("lavage.*paroi", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("VAC", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("mise à plat.*abcès.*marge", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*marge.*anale", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*marge", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*(MA|fesse)", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("abcès.*inguinal", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("abcès.*axillaire", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("abcès.*épaule", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("exérèse.*ganglion.*SC", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("exérèse.*lésion.*cutanée", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("lipome", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    str_detect(INTERVENTION, regex("nodule.*pariétal", ignore_case = TRUE)) ~ "Exérèse sous-cutanée",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 10/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("colectomie.*droite", ignore_case = TRUE)) ~ "Colectomie droite (coelio)",
    str_detect(INTERVENTION, regex("colon.*d.*coelio", ignore_case = TRUE)) ~ "Colectomie droite (coelio)",
    str_detect(INTERVENTION, regex("colectomie.*D\\b", ignore_case = TRUE)) ~ "Colectomie droite (coelio)",
    str_detect(INTERVENTION, regex("colectomie.*gauche.*laparo", ignore_case = TRUE)) ~ "Colectomie gauche (laparo)",
    str_detect(INTERVENTION, regex("colectomie.*gauche", ignore_case = TRUE)) ~ "Colectomie gauche (coelio)",
    str_detect(INTERVENTION, regex("colon.*g.*coelio", ignore_case = TRUE)) ~ "Colectomie gauche (coelio)",
    str_detect(INTERVENTION, regex("colectomie.*transverse", ignore_case = TRUE)) ~ "Colectomie transverse",
    str_detect(INTERVENTION, regex("colectomie.*subtotale", ignore_case = TRUE)) ~ "Colectomie subtotale (laparo)",
    str_detect(INTERVENTION, regex("colectomie.*totale", ignore_case = TRUE)) ~ "Colectomie totale (laparo)",
    str_detect(INTERVENTION, regex("colectomie.*", ignore_case = TRUE)) & str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Colectomie (robot)",
    str_detect(INTERVENTION, regex("colon.*sub.*coelio", ignore_case = TRUE)) ~ "Colectomie subtotale (coelio)",
    str_detect(INTERVENTION, regex("resection.*sigmoidienne.*laparo", ignore_case = TRUE)) ~ "Rectum (laparo)",
    str_detect(INTERVENTION, regex("sigmoïdectomie", ignore_case = TRUE)) ~ "Colectomie gauche (coelio)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))


# Bloc 11/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("résection.*gr[êe]le", ignore_case = TRUE)) ~ "Résection de grêle",
    str_detect(INTERVENTION, regex("anastomose.*gr[êe]le", ignore_case = TRUE)) ~ "Résection de grêle",
    str_detect(INTERVENTION, regex("iléostomie.*laparo", ignore_case = TRUE)) ~ "Iléostomie (laparo)",
    str_detect(INTERVENTION, regex("iléostomie", ignore_case = TRUE)) ~ "Iléostomie (coelio)",
    str_detect(INTERVENTION, regex("colostomie.*laparo", ignore_case = TRUE)) ~ "Colostomie (laparo)",
    str_detect(INTERVENTION, regex("colostomie", ignore_case = TRUE)) ~ "Colostomie (coelio)",
    str_detect(INTERVENTION, regex("fermeture.*ilé", ignore_case = TRUE)) ~ "Fermeture d'iléostomie",
    str_detect(INTERVENTION, regex("fermeture.*colo", ignore_case = TRUE)) ~ "Fermeture de colostomie",
    str_detect(INTERVENTION, regex("fermeture.*stomie", ignore_case = TRUE)) ~ "Fermeture de stomie",
    str_detect(INTERVENTION, regex("fermeture.*j[ée]juno", ignore_case = TRUE)) ~ "Fermeture de jéjunostomie",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 12/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("rectopexie", ignore_case = TRUE)) ~ "Rectopexie (coelio)",
    str_detect(INTERVENTION, regex("proctectomie.*coelio", ignore_case = TRUE)) ~ "Rectum (coelio)",
    str_detect(INTERVENTION, regex("proctectomie", ignore_case = TRUE)) ~ "Rectum (laparo)",
    str_detect(INTERVENTION, regex("exérèse.*rétrorectal", ignore_case = TRUE)) ~ "Exérèse tumeur rétrorectale",
    str_detect(INTERVENTION, regex("kyste.*[pP]ilonidal|sinus.*p[iî]lonidal|kyste.*sacrococcygien", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("sinus.*pi", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("Milligan|fissurectomie|fissure anale|fistulectomie", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("fistule.*anale", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("abc[eè]s.*marge|abc[eè]s.*anal|abc[eè]s.*MA", ignore_case = TRUE)) ~ "Abcès de marge anale",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 13/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("app|appendicite|appendicectomie", ignore_case = TRUE)) ~ "Appendicectomie coelio",
    str_detect(INTERVENTION, regex("ileo|iléostomie", ignore_case = TRUE)) &
      !str_detect(INTERVENTION, regex("fermeture", ignore_case = TRUE)) ~ "Iléostomie (coelio)",
    str_detect(INTERVENTION, regex("fermeture.*(ilé|ileo)", ignore_case = TRUE)) ~ "Rétablissement de continuité",
    str_detect(INTERVENTION, regex("colostomie", ignore_case = TRUE)) &
      !str_detect(INTERVENTION, regex("fermeture", ignore_case = TRUE)) ~ "Colostomie (coelio)",
    str_detect(INTERVENTION, regex("fermeture.*(colo|colostomie)", ignore_case = TRUE)) ~ "Rétablissement de continuité",
    str_detect(INTERVENTION, regex("stomie.*fermeture|fermeture.*stomie", ignore_case = TRUE)) ~ "Rétablissement de continuité",
    str_detect(INTERVENTION, regex("Hartmann.*r[eé]tabl", ignore_case = TRUE)) ~ "Rétablissement de continuité",
    str_detect(INTERVENTION, regex("r[eé]tablissement.*continuit[eé]", ignore_case = TRUE)) ~ "Rétablissement de continuité",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 14/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("abces|abcès.*marge|abcès.*anal|abcès.*fesse", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("fistule.*anale|fistule.*marge", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("fissure.*anale", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("milligan|morgan", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("sinus.*pilonidal|kyste.*sacrococcygien", ignore_case = TRUE)) ~ "Exérèse sinus pilonidal",
    str_detect(INTERVENTION, regex("prolapsus.*stom", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    str_detect(INTERVENTION, regex("drainage.*abcès|drainage.*abces", ignore_case = TRUE)) ~ "Drainage chirurgical",
    str_detect(INTERVENTION, regex("biopsie.*h[ée]patique", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("by.?pass.*gastrique", ignore_case = TRUE)) ~ "Bypass gastrique (coelio)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 15/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("wedge.*|segmentectomie|sectoriectomie|unisegmentectomie", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
    str_detect(INTERVENTION, regex("hépatectomie.*droite|hépatectomie.*gauche.*élargie|centrale|totale", ignore_case = TRUE)) &
      !str_detect(INTERVENTION, regex("coelio|robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (laparo)",
    str_detect(INTERVENTION, regex("hépatectomie.*droite|hépatectomie.*gauche.*élargie|centrale|totale", ignore_case = TRUE)) &
      str_detect(INTERVENTION, regex("coelio", ignore_case = TRUE)) ~ "Hépatectomie majeure (coelio)",
    str_detect(INTERVENTION, regex("hépatectomie.*droite|hépatectomie.*gauche", ignore_case = TRUE)) &
      str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Hépatectomie majeure (robot)",
    str_detect(INTERVENTION, regex("resection.*hep", ignore_case = TRUE)) ~ "Hépatectomie mineure (coelio)",
    str_detect(INTERVENTION, regex("exploration.*4 sites|4 sites|parathyro", ignore_case = TRUE)) ~ "Exploration des 4 sites",
    str_detect(INTERVENTION, regex("resection.*grele|résection.*gr[êe]le|anastomose.*gr[êe]le", ignore_case = TRUE)) ~ "Résection de grêle",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 16/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("spl[ée]nectomie", ignore_case = TRUE)) &
      !str_detect(INTERVENTION, regex("laparo|robot", ignore_case = TRUE)) ~ "Splénectomie (coelio)",
    str_detect(INTERVENTION, regex("spl[ée]nectomie", ignore_case = TRUE)) &
      str_detect(INTERVENTION, regex("laparo", ignore_case = TRUE)) ~ "Splénectomie (laparo)",
    str_detect(INTERVENTION, regex("spl[ée]nectomie", ignore_case = TRUE)) &
      str_detect(INTERVENTION, regex("robot", ignore_case = TRUE)) ~ "Splénectomie (robot)",
    str_detect(INTERVENTION, regex("fistule.*digestive|entero-cutan[ée]|gastro-p[ée]ricardique|iléocutan[ée]", ignore_case = TRUE)) ~ "Fistule digestive",
    str_detect(INTERVENTION, regex("prolapsus.*stom", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    str_detect(INTERVENTION, regex("pose de TIPS|TIPS", ignore_case = TRUE)) ~ "Pose de TIPS",
    str_detect(INTERVENTION, regex("transplantation.*h[ée]patique", ignore_case = TRUE)) ~ "Transplantation hépatique",
    str_detect(INTERVENTION, regex("transplantation.*pancr[ée]atique|pancr[ée]as", ignore_case = TRUE)) ~ "Transplantation pancréatique",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

# Bloc 17/17
df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    str_detect(INTERVENTION, regex("coloplastie|colopharyngoplastie|pharyngo-gastroplastie", ignore_case = TRUE)) ~ "Examen anal",
    str_detect(INTERVENTION, regex("abc[eè]s.*marge|marge anale|abc[eè]s.*fesse", ignore_case = TRUE)) ~ "Abcès de marge anale",
    str_detect(INTERVENTION, regex("biopsie.*h[ée]patique", ignore_case = TRUE)) ~ "Biopsie hépatique",
    str_detect(INTERVENTION, regex("biopsie intra.*abdominale", ignore_case = TRUE)) ~ "Biopsie intra-abdominale",
    str_detect(INTERVENTION, regex("j[ée]junostomie", ignore_case = TRUE)) ~ "Iléostomie (laparo)",
    str_detect(INTERVENTION, regex("VAC", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    str_detect(INTERVENTION, regex("rectopexie", ignore_case = TRUE)) ~ "Rectum (coelio)",
    str_detect(INTERVENTION, regex("retablissement.*hartmann|r[ée]tabliss?imen?t.*hartmann", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    str_detect(INTERVENTION, regex("fermeture.*stomie|fermture.*stomie|fermeture.*il[ée]o|ferm.*colo", ignore_case = TRUE)) ~ "Reprise chirurgicale",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = if_else(is.na(INTERVENTION_GROUPÉE), "Autre", INTERVENTION_GROUPÉE))

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("PMO") ~ "SPG (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cholecystectomie", "cholecystectomie", "Cholecystite", "Cholecystectomie programmée", "Cholecystectomie refroidie", "Cholescystectomie", "Cheolecystectomie", "cholécystectomie coeli", "cholécystectomie par laparotomie", "cholécystectomie par coelioscopie", "Cholécystectomie + spyglass calcul enclavé VBP", "Cholécystectomoie coelioscopie", "Cholecystite", "cholangio", "Cholangio") ~ "Cholécystectomie",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Surrénalectomie G coelio", "Surrénalectomie D coelio", "Surrénale coelio", "Surrénalectomie", "Surrénalectomie G", "surrénalectomie", "Surrénale Gauche", "Surrénale gauche", "surrenale droite coelio", "surrenale gauche coelio") ~ "Surrénalectomie (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Surrénalectomie gauche robot", "Surrénalectomie G robot", "surrénalectomie D robot", "Surrénale robot", "surrénale robot", "surrenalectomie droite robot") ~ "Surrénalectomie (robot)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Surrénalectomie laparo", "Surrénalectomie laparotomie") ~ "Surrénalectomie (laparo)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colectomie G coelio", "colectomie g coelio", "colectomie guache coelio", "colectomie aguche") ~ "Colectomie gauche (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colectomie G laparo", "colectomie G laparo pour volvulus", "Colectomie G", "Colectomie G coelio", "Colectomie G laparo", "Colectomie G laparo pour volvulus", "Colectomie G laparo volvulus", "Colon G laparo", "Colon G + RIC laparo") ~ "Colectomie gauche (laparo)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Colon droit", "Côlon droit", "colectomie Dte coelio (plutot RIC)", "colon G", "colon gauche converti") ~ "Colectomie droite (coelio)", # par défaut coelio
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colostomie coelio", "Colosotomie", "coleostomie coelio") ~ "Colostomie (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cure HH", "Cure HI", "Cure HIG coelio", "Cure HH robot", "cure HH robot") ~ "Cure hernie hiatale",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cure RGO", "Cure de RGO") ~ "Cure hernie hiatale",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cure de hernie étranglée", "cure de hernie omblicale lol") ~ "Cure hernie ombilicale",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("curage cervical", "Curage cervical", "curage GG") ~ "Curage ganglionnaire",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colectomie + vessie coelio") ~ "Colectomie (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colectomie laparo") ~ "Colectomie (laparo)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("GT", "GT coelio") ~ "SPG (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("explo des qutre sites") ~ "Exploration des 4 sites",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Babcok", "Baulieu", "Recoupe Beaulieu") ~ "2e temps colo-anale différée",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie droite", "Hepatectomie droite coelio", "Hepatectomie droite cœlio", "Hépatectomie D coelio", "Hépatectomie droite par laparotomie", "Hépatectomie droite par voie ant") ~ "Hépatectomie majeure (laparo)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie gauche", "Hépatectomie gauche", "Hepatectomie gauche avec curage ganglionnaire", "Hepatectomie gauche avec anastomose bd", "Hepatectomie gauche + double dérivation") ~ "Hépatectomie majeure (laparo)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie gauche robot") ~ "Hépatectomie majeure (robot)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie partielle du S4 robotique", "Hépatec IV/V robot", "Résection hépatique atypique par robot S6") ~ "Hépatectomie mineure (robot)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomies partielles multiples coelio", "segment VIII coelio", "Métastasectomie segment VIII coelio", "Resection atypique coelio (sgt III)", "Resection atypique coelio (sgt VIII)", "Resection atypique coelio métastases dôme hépatique", "Resection atypique secteur posterieur", "Resection atypique segment II", "Résection partielle atypique SVI-VII pour métastase synchrone par coelio") ~ "Hépatectomie mineure (coelio)",
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Re-hépatectomies partielles + RF pour métastases hépatiques", "Resections hépatiques H765'4A'", "Resection hep atypique : méta bord du II", "Résection atypique + micro onde", "Résection de lésion ganglionnaire + radiofréquence S2 par laparotomie", "resection atypique hép laparo", "Resection atypique pour Meta", "Resection atypique et ablation nodule psoas") ~ "Hépatectomie mineure (laparo)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )
df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Surrénalectomie", "Surrénalectomie G", "Surrénalectomie D coelio", "Surrénalectomie G coelio", "Surrénale coelio", 
        "surrénalectomie", "Surrénale gauche", "Surrénale Gauche", "surrenale gauche coelio", "surrenale droite coelio"
      ) ~ "Surrénalectomie (coelio)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Surrénalectomie G robot", "Surrénalectomie gauche robot", "surrénale robot", "surrénalectomie D robot", "surrenalectomie droite robot"
      ) ~ "Surrénalectomie (robot)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Surrénalectomie laparo", "Surrénalectomie laparotomie"
      ) ~ "Surrénalectomie (laparo)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "colectomie G coelio", "Colectomie G coelio", "Colectomie Dte coelio (plutot RIC)", "colectomie guache coelio",
        "Colectomie G", "colectomie G", "colectomie aguche", "colon G", "Colon G + RIC laparo", "Colon G laparo", 
        "colon gauche converti", "colectomie + vessie coelio", "Colectomie G laparo pour volvulus", "Colectomie G laparo volvulus"
      ) ~ "Colectomie gauche (coelio)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "colectomie G laparo", "Colectomie G laparo", "Colon G laparo"
      ) ~ "Colectomie gauche (laparo)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "colectomie laparo", "colectomie + vessie coelio"
      ) ~ "Colectomie (laparo)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Colosotomie", "coleostomie coelio"
      ) ~ "Colostomie (coelio)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Ilésostomie de dérivation"
      ) ~ "Iléostomie (laparo)",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "ReRIC"
      ) ~ "Reprise chirurgicale",
      
      INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c(
        "Retablissiment de hartman", "rétablissimenet de hartman"
      ) ~ "Rétablissement de continuité",
      
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Occlusion sur bride", "Coelio occlusion sur bride", "Occusion sur bride coelio", "Occlusion adhérences postop", "Occlusion sur eventration étranglée", "Syndrome occlusif sur bride, ehler danlos") ~ "Occlusion sur bride",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Eviscération", "evisceration", "eviscération", "Cure d' évisceration couverte étranglée", "Cure d'éviscération") ~ "Éviscération",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hernie Liechtenstein") ~ "Cure hernie inguinale (Lichtenstein)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Ablation anneau gastrique", "Ablation anneau gatsrqiue", "ablation anneau gastrique + Toupet") ~ "Ablation anneau gastrique",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cholécystite") ~ "Cholécystectomie",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Cure d'eventration", "Eventration", "Eventration ligne blanche", "Eventration médiane", "Cure d'éventation", "Cure d'éventratation", "Cure d'évetration") ~ "Cure d'éventration",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Embolisation portale", "embolisation portale percutanée") ~ "Embolisation portale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hernie TAP") ~ "Cure hernie pariétale (TAP)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hernie bilat coelio", "Hernie bilatérale coelio", "Hernie unilatérale coelio") ~ "Cure hernie inguinale coelio",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Prolapsus rectal") ~ "Rectopexie (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Réfection stomie", "Résinsertion stomie", "Réfection pied de l'anse") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("TP") ~ "Transplantation pancréatique",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("resection de diverticule oesophage Robo", "Diverticule oesophagien robot", "stripping oesophage", "Achalasie") ~ "Myotomie de Heller / diverticulectomie (robot)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("AAP", "AAP + Taylor") ~ "Amputation abdomino-périnéale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Abcès", "Abcès abdo", "Péritonite biliaire sur plaie VBP", "Péritonite biliaire, cholécystectomie", "Péritonite généralisée", "Péritonite sur plaie du grêle", "Peritonite sur lachage anastomotique", "Peritonite sur perforation de grele carcinose") ~ "Drainage chirurgical",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Ablation phéochomocytome (récidive)", "Surrénalectomie gauche pour paragangliome") ~ "Surrénalectomie (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Amput AP", "Amputation abdopérinéale") ~ "Amputation abdomino-périnéale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Anastomose coronaro cave", "Anastomose porto cave", "Dérivation portocave ( anastomose mésenterioco-cave) avec greffon veineux et Goretex interposé") ~ "Anastomose vasculaire complexe",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Boulectomie") ~ "Exérèse sous-cutanée",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Courage ganglionnaire lomboaortique par coelioscopie", "curage cervical", "Curage cervical", "curage GG") ~ "Curage ganglionnaire",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("DPT", "DPT laparo avec résection tronculaire VMS", "DPT robot dont anastomoses bilio-dig et pancréatique, conversion pour gastro-jéjunale") ~ "Pancréatectomie céphalique (divers)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Dilatation anastomose reno-porte") ~ "Anastomose vasculaire complexe",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Diversion duodénale", "Dérivation gastro-jéjunale", "dérivation gastrojujenal") ~ "Dérivation digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Désobstruction portale") ~ "Anastomose vasculaire complexe",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Exam anal AG") ~ "Examen anal",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Exerese lésion cutanée", "exérèse mélanome anal", "exerese gg axillaire") ~ "Exérèse sous-cutanée",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Extraction corps etranger", "extraction CE intra rectal") ~ "Examen anal",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Exérèse adénome lobe 2 hépatique") ~ "Hépatectomie mineure (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Exérèse paragangliome D", "Paragangliome", "Paragangliome latéroaortique") ~ "Surrénalectomie (laparo)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Fenestration kystique") ~ "Drainage chirurgical",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("GIST", "gastrec partielle pour GIST") ~ "Gastrectomie partielle",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Gastrotomie pour 2 boulettes") ~ "Exérèse sous-cutanée",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("H234 Hépatectomie gauche coelioscopie", "H458 + curage", "Hépatectomie gauche", "Hépatectomie gauche + double dérivation", "Hépatectomie gauche et résection de la voie biliaire", "Hepatectomie gauche + anastomose biliodig", "Hepatectomie gauche avec anastomose bd", "Hepatectomie gauche avec curage ganglionnaire") ~ "Hépatectomie majeure (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("H6' coelio", "HO coelio") ~ "Hépatectomie mineure (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("HPT2") ~ "Biopsie hépatique",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie droite", "Hepatectomie droite cœlio", "Hepatectomie droite coelio") ~ "Hépatectomie majeure (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie droite elargie au 4 + 1 + resection anastoose veneuse", "Hepatectomie droite elargie au sect ant + curage ganglionnaire+abd", "Hepatectomie droite par laparotomie", "Hepatectomie droite par voie ant") ~ "Hépatectomie majeure (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomie partielle du S4 robotique", "Hépatectomie atypique par robot S6") ~ "Hépatectomie mineure (robot)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hepatectomies partielles multiples coelio", "Re-hépatectomies partielles + RF pour métastases hépatiques") ~ "Hépatectomie mineure (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Hernie crurale étranglée", "Hernie fémorale", "Hernie interne côlon gauche") ~ "Cure hernie pariétale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Ilésostomie de dérivation") ~ "Iléostomie (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Lap explo + résection nodule coupole diaph", "Laparo pour hémostase", "Laparo carcinose") ~ "Laparotomie exploratrice",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Laparo diverticule duodénum") ~ "Exérèse digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Laparo gastrostomie") ~ "Gastrostomie",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Laparotomie- resection de fibrome") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Lobectomie G coelio convertie") ~ "Loboisthmectomie thyroïdienne",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Occlusion adhérences postop", "Occlusion sur bride", "Coelio occlusion sur bride", "Occusion sur bride coelio", "Section bride coelio") ~ "Occlusion sur bride",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Occlusion hernie interne", "Occlusion sur eventration étranglée", "Syndrome occlusif sur bride, ehler danlos") ~ "Occlusion sur bride",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Oesophagectomie LS") ~ "Œsophagectomie de Lewis Santy",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Omphalectomie") ~ "Exérèse sous-cutanée",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("PG", "PG Warshaw", "PG robot") ~ "SPG (robot)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Pancreatectomie gauche", "Pancreatectomie gauche et patch peritoine sur le diaphragme", "Pancreatectomie gauche laparo") ~ "Pancréatectomie gauche (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Paragangliome", "Paragangliome latéroaortique", "Surrénalectomie gauche pour paragangliome") ~ "Surrénalectomie (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Plaie abdo arme blanche perfo estomac", "Plaie arme à feu") ~ "Laparotomie exploratrice",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Pose de pansement intrabdominal") ~ "Drainage chirurgical",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Prolpasus stomial") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Péritonite biliaire sur plaie VBP", "Péritonite biliaire, cholécystectomie", "Péritonite sur plaie du grêle", "Péritonite génélarisée") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("RF nodule hépatique") ~ "Hépatectomie mineure (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Redo anastomose bilio-digestive", "Réfection anastomose bilio-dig", "Réparation biliaire sur canaux hépatique", "Réparation biliiare", "réfection anastomose biliodig laparo") ~ "Reprise bilio-digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Resection de Meckel", "Resection de la VBP") ~ "Résection de grêle",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Resection et refection anastomose grelo grelique") ~ "Résection de grêle",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Resection iléo caecale") ~ "Résection iléo-caecale (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Resection jabot laparo") ~ "Exérèse digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Résinsertion stomie") ~ "Reprise chirurgicale",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Secteuriectomie posterieure par laparotomie", "Segmenctectomie partielle 4", "Segmenctectomie partielle 5/6") ~ "Hépatectomie mineure (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("TT cervicotomie", "TT redux") ~ "Thyroïdectomie totale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Trachéotomie") ~ "Autre ORL",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Volvulus du caecum sur mésentère commun incomplet", "Volvulus du grele") ~ "Occlusion sur bride",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("Zenker") ~ "Diverticule œsophagien",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("abdominoplastie") ~ "Autre paroi",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("ablation anneau gastrique + Toupet") ~ "Anneau gastrique",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("caecostomie abord direct") ~ "Stomie digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("cholecystectomie partielle", "cholescystectomie", "vésicule", "vésicule de l'enfer") ~ "Cholécystectomie",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("colopharyngo", "Phryngo-gastroplastie") ~ "Œsophagectomie 3 voies",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("débulking pseudomyxome") ~ "Cytoréduction (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("décaillotage") ~ "Drainage chirurgical",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("démontage gastroplastie") ~ "Reprise bariatrique",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("dérivation gastrojujenal", "Diversion duodénale", "Dérivation gastro-jéjunale") ~ "Dérivation digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("embolisation portale percutanée", "Embolisation portale") ~ "Embolisation portale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("endosponge anal") ~ "Examen anal",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("eventration diaphragmatique") ~ "Cure éventration",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("evisceration", "eviscération") ~ "Eviscération",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("exerese gg axillaire") ~ "Curage ganglionnaire",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("exerese sigmoido rectale coelio") ~ "Colectomie gauche (coelio)",
    TRUE ~ INTERVENTION_GROUPÉE
  ))

df <- df %>%
  mutate(INTERVENTION_GROUPÉE = case_when(
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("explo paroi") ~ "Laparotomie exploratrice",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("extraction CE intra rectal") ~ "Examen anal",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("exérèse mélanome anal") ~ "Examen anal",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("fibrome coelio") ~ "Exérèse sous-cutanée",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("fisutle anale + lambeau") ~ "Fistule digestive",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("gastrec partielle pour GIST") ~ "Gastrectomie partielle",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("hernie TAP", "Hernie TAP") ~ "Cure hernie inguinale (coelio)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("lobo") ~ "Loboisthmectomie thyroïdienne",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("ovariectomie bil laparo", "ovariectomie bilatérale sous coelio", "ovariectomie laparo") ~ "Annexectomie laparotomie",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("pelvectomie postérieure laparo") ~ "Pelvectomie postérieure",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("peritonite sur perforation de grele carcinose") ~ "Résection de grêle",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("protectomie secondaire") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("reascension plastie + jejuno") ~ "Reprise chirurgicale",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("resection de lésion par voie de kraske") ~ "Rectum (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("retablissiment de hartman", "rétablissimenet de hartman") ~ "Rétablissement de continuité",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("résection atypique hép laparo") ~ "Hépatectomie mineure (laparo)",
    INTERVENTION_GROUPÉE == "Autre" & INTERVENTION %in% c("torsion testiculaire") ~ "Autre",
    TRUE ~ INTERVENTION_GROUPÉE
  )) %>%
  mutate(INTERVENTION_GROUPÉE = if_else(is.na(INTERVENTION_GROUPÉE), "Autre", INTERVENTION_GROUPÉE))

df <- df %>%
  mutate(
    INTERVENTION_GROUPÉE = case_when(
      INTERVENTION %in% c("Eventration étranglée", "Cure de hernie curale étranglée", "Cure hernie curale abord direct femme", "HI etranglée", "HID étranglée") ~ "Cure hernie étranglée",
      INTERVENTION %in% c("Duodénectomie", "Duodénectomie robot") ~ "Duodénectomie",
      INTERVENTION == "H4'5'6'7'8'+cholécystectomie" ~ "Hépatectomie majeure (laparo)",
      INTERVENTION == "HIG" ~ "Cure hernie hiatale",
      INTERVENTION == "Jéjuno" ~ "Résection de grêle",
      INTERVENTION == "KSC" ~ "Exérèse sous-cutanée",
      INTERVENTION %in% c("Para T3 gauche abord focal", "Para abord local P3 droite", "Para abord local P3 gauche", "Para abord local P4 gauche") ~ "Exploration des 4 sites",
      INTERVENTION == "Récidive ganglionnaire corticosurrénalome" ~ "Reprise chirurgicale",
      INTERVENTION %in% c(
        "Résection atypique coelio (sgt III)",
        "Résection atypique coelio (sgt VIII)",
        "Résection atypique coelio métastases dôme hépatique",
        "Résection atypique secteur posterieur",
        "Résection atypique segment II"
      ) ~ "Hépatectomie mineure (coelio)",
      INTERVENTION == "Résection hep atypique : méta bord du II" ~ "Hépatectomie mineure (laparo)",
      INTERVENTION == "Résection nodule carcinose" ~ "Cytoréduction (laparo)",
      INTERVENTION == "Résection recto-sigmoïdienne iléoprotégée par laparo pour perforation sur prolapsus" ~ "Rectum (laparo)",
      INTERVENTION == "torsion testiculaire" ~ "Autre (non digestif)",
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )


table(df$INTERVENTION_GROUPÉE)

df %>%
  filter(INTERVENTION_GROUPÉE == "Autre") %>%
  count(INTERVENTION, sort = TRUE)


df %>%
  filter(INTERVENTION_GROUPÉE == "Autre") %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)


# ---- Packages ----
library(officer)
library(flextable)
library(gtsummary)
library(dplyr)
library(broom)
library(tibble)

# ---- Création document Word ----
doc <- read_docx()

# ---- Fonctions utilitaires ----
add_table <- function(doc, title, tbl) {
  doc <- body_add_par(doc, title, style = "heading 2")
  doc <- body_add_flextable(doc, as_flex_table(tbl))
  return(doc)
}

add_df_table <- function(doc, title, df) {
  doc <- body_add_par(doc, title, style = "heading 2")
  ft <- flextable(df)
  doc <- body_add_flextable(doc, ft)
  return(doc)
}

# ---- TABLEAUX DESCRIPTIFS ----
tbl_amb <- df %>% select(AMBIANCE) %>% tbl_summary(missing = "no") %>% bold_labels()
doc <- add_table(doc, "Ambiance perçue", tbl_amb)

tbl_peda <- df %>% select(PEDAGOGIE) %>% tbl_summary(missing = "no") %>% bold_labels()
doc <- add_table(doc, "Pédagogie perçue", tbl_peda)

tbl_self <- df %>% select(SELF_ESTIME_SORTIE) %>% tbl_summary(missing = "no") %>% bold_labels()
doc <- add_table(doc, "Self-estime en sortie", tbl_self)

# ---- Modèle multivarié "on recommence" ----
doc <- add_table(doc, "Analyse multivariée - Ambiance 'on recommence'", tbl_mv_ambiance)

# ---- Modèle multivarié "gros vs petit geste" ----
doc <- add_table(doc, "Analyse multivariée - Type de geste (Gros vs Petit)", tbl_mv_ambiance2)

# ---- Modèle multivarié "je veux partir" ----
doc <- add_table(doc, "Analyse multivariée - Ambiance 'je veux partir'", tbl_partir1)

# ---- Taux de geste socle vs non socle ----
doc <- add_table(doc, "Taux de geste : socle vs non socle", tbl_geste_socle)

# ---- Taux de geste par phase ----
doc <- add_table(doc, "Taux de geste : phase socle vs non socle", tbl_geste_phase)

# ---- Type de geste par phase ----
doc <- add_table(doc, "Type de geste : phase socle vs non socle", tbl_type_geste_phase)

# ---- Garde uniquement : gestes socle vs non socle ----
doc <- add_table(doc, "Geste en garde : socle vs non socle", tbl_garde_geste_socle)

# ---- 6 premières vs dernières semaines de stage ----
doc <- add_table(doc, "Geste : début vs fin de stage", tbl_geste_stage)

# ---- RÉGRESSION LOGISTIQUE TEMPORELLE ----
summary_logit <- tidy(modele_logit, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term == "as.numeric(DATE)") %>%
  mutate(
    Variable = "Date (linéaire)",
    OR = sprintf("%.2f", estimate),
    `IC 95%` = paste0("[", round(conf.low, 2), "; ", round(conf.high, 2), "]")
  ) %>%
  select(Variable, OR, `IC 95%`)


doc <- add_df_table(doc, "Régression logistique - Geste ~ Date (semestre hiver)", summary_logit)

doc <- body_add_par(doc, "Interprétation OR :", style = "heading 2")
doc <- body_add_par(doc, paste0(
  "OR par jour : ", round(OR_par_jour, 3), "\n",
  "OR par semaine : ", round(OR_par_semaine, 3), "\n",
  "OR par mois : ", round(OR_par_mois, 3), "\n",
  "p (Spearman) = ", format.pval(cor_spearman$p.value, digits = 2)
))

# Préparer les données pour export
df_geste_annee_export <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste)) %>%
  group_by(annee_DES) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux_geste = round(100 * n_geste / total, 1),
    .groups = "drop"
  ) %>%
  rename(`Année de DES` = annee_DES,
         `Nombre total` = total,
         `Nombre gestes` = n_geste,
         `Taux (%)` = taux_geste)

# Ajouter au document Word
doc <- add_df_table(doc, "Taux de gestes réalisés selon l'année de DES", df_geste_annee_export)

# ---- Taux de geste par année DES ----
df_geste_annee <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste)) %>%
  group_by(annee_DES) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux = n_geste / total,
    .groups = "drop"
  ) %>%
  mutate(
    Annee = paste0("Année ", annee_DES),
    Taux_geste = scales::percent(taux, accuracy = 0.1),
    Effectif = paste0("n = ", total)
  ) %>%
  select(Annee, Taux_geste, Effectif)
doc <- add_df_table(doc, "Taux de geste selon l’année de DES", df_geste_annee)

# ---- Geste_a_l_aise ----
df_a_l_aise <- df %>%
  filter(!is.na(Geste_a_l_aise)) %>%
  count(Geste_a_l_aise) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
doc <- add_df_table(doc, "Répartition : Geste à l’aise", df_a_l_aise)

# ---- Ressenti en absence de geste ----
df_ressenti <- df %>%
  filter(!is.na(Si_pas_de_geste_RESSENTI)) %>%
  count(Si_pas_de_geste_RESSENTI) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
doc <- add_df_table(doc, "Répartition : ressenti sans geste", df_ressenti)

# ---- Export final ----
print(doc, target = "rapport_logbook_complet.docx")








# ---- ÉVOLUTION TAUX DE GESTE PAR ANNÉE (LISSÉ) ----
df_geste_annee_lisse <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste)) %>%
  group_by(annee_DES) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux = n_geste / total,
    .groups = "drop"
  ) %>%
  mutate(taux_pct = round(100 * taux, 1))

doc <- add_df_table(doc, "Taux de geste selon l’année de DES (version lissée)", df_geste_annee_lisse)

# ---- TAUX PAR ANNÉE ET TYPE INTERVENTION ----
df_geste_gp <- df %>%
  filter(!is.na(annee_DES), !is.na(Geste), !is.na(Garde_Programme)) %>%
  group_by(annee_DES, Garde_Programme) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux = n_geste / total,
    .groups = "drop"
  ) %>%
  mutate(taux_pct = round(100 * taux, 1))

doc <- add_df_table(doc, "Taux de geste par année DES et type d’intervention", df_geste_gp)

# ---- SELF-ESTIME VS GESTE ----
df_self_geste <- df %>%
  filter(!is.na(SELF_ESTIME_SORTIE), !is.na(Geste)) %>%
  group_by(SELF_ESTIME_SORTIE) %>%
  summarise(
    total = n(),
    n_geste = sum(Geste == "Yes"),
    taux = n_geste / total,
    .groups = "drop"
  ) %>%
  mutate(taux_pct = round(100 * taux, 1))

doc <- add_df_table(doc, "Taux de geste selon la self-estime en sortie", df_self_geste)

# ---- GESTE À L’AISE VS ANNÉE ----
df_aise_des <- df %>%
  filter(!is.na(Geste_a_l_aise), !is.na(annee_DES)) %>%
  count(annee_DES, Geste_a_l_aise) %>%
  group_by(annee_DES) %>%
  mutate(pct = round(100 * n / sum(n), 1))

doc <- add_df_table(doc, "Geste à l’aise selon l’année de DES", df_aise_des)

# ---- RESSENTI EN L’ABSENCE DE GESTE VS ANNÉE ----
df_ressenti_des <- df %>%
  filter(!is.na(Si_pas_de_geste_RESSENTI), !is.na(annee_DES)) %>%
  count(annee_DES, Si_pas_de_geste_RESSENTI) %>%
  group_by(annee_DES) %>%
  mutate(pct = round(100 * n / sum(n), 1))

doc <- add_df_table(doc, "Ressenti en absence de geste selon l’année DES", df_ressenti_des)

# ---- PROBA GESTE PAR DATE (modèle logistique) ----
df_semestre_hiver <- df_semestre_hiver %>%
  mutate(DATE = as.Date(DATE)) %>%
  select(DATE, Geste_bin, proba_geste)

doc <- add_df_table(doc, "Probabilité prédite de geste (modèle logistique temporel)", df_semestre_hiver)

# ---- SAUVEGARDE FINALE ----
print("Export terminé. Enregistrez avec :")
print(doc, target = 'logbook_resultats_complets.docx')






#enregistrer sur github
# Chemin complet du fichier
fichier <- "/Users/thomashusson/Documents/R/Logbook/script logbook complet pour SFCD.R"

# Aller dans le dossier Git (le projet Logbook)
setwd("/Users/thomashusson/Documents/R/Logbook")

# Étape 1 : ajouter le fichier
system(paste("git add", shQuote(fichier)))

# Étape 2 : commit avec message explicite
message_commit <- "Mise à jour du script logbook complet pour SFCD"
system(paste("git commit -m", shQuote(message_commit)))

# Étape 3 : push vers GitHub
system("git push")






#télécharger depuis github
# Nom du fichier dans le dépôt GitHub
nom_fichier <- "script logbook complet pour SFCD.R"

# Encoder le nom du fichier pour l'URL
nom_fichier_url <- utils::URLencode(nom_fichier)

# Construire l'URL brute sur GitHub (branche main)
url_github <- paste0(
  "https://raw.githubusercontent.com/thomashusson29/logbook/main/",
  nom_fichier_url
)

# Dossier de destination (Téléchargements macOS)
destination <- file.path("~/Downloads", nom_fichier)

# Télécharger
download.file(url = url_github, destfile = destination, mode = "wb")

cat("✅ Fichier téléchargé dans : ", destination, "\n")

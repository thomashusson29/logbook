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







#tout supprimer
rm(list=ls())



#enregistrer sur github depuis macbook claire
system("git remote add origin https://github.com/thomashusson29/logbook.git")

# Chemin vers ton fichier
fichier <- "/Users/thomashusson/Documents/R/Logbook/script logbook complet pour SFCD.R"

# Aller dans le dossier Git (le projet Logbook)
setwd("/Users/thomashusson/Documents/R/Logbook")

# Étape 1 : Ajouter le fichier
system(paste("git add", shQuote(fichier)))

# Étape 2 : Commit avec message explicite
message_commit <- "Mise à jour du script logbook complet pour SFCD"
system(paste("git commit -m", shQuote(message_commit)))

# Étape 3 : Push avec définition de la branche de suivi si nécessaire
system("git push --set-upstream origin main")



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

ggsave("gestes_par_annee_DES.svg", plot = gestes_par_année, width = 10, height = 6)



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






library(ggplot2)
library(dplyr)

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
ggplot(df_ambiance, aes(x = AMBIANCE, y = n, fill = AMBIANCE)) +
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

library(dplyr)
library(ggplot2)
library(stringr)

# Étape 1 : Recode geste majoritaire
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

# Étape 2 : Résumé
n_total <- sum(df$Geste %in% c("Yes"), na.rm = TRUE)

df_resume <- df %>%
  filter(!is.na(geste_majoritaire), Geste == "Yes") %>%
  group_by(geste_majoritaire) %>%
  summarise(
    gestes_realises = n(),
    .groups = "drop"
  ) %>%
  filter(geste_majoritaire != "Rien") %>%
  mutate(
    total = n_total,
    pourcentage = 100 * gestes_realises / total,
    label = paste0(gestes_realises, "/", total, " (", round(pourcentage, 1), "%)"),
    geste_majoritaire = factor(geste_majoritaire, levels = c("Tout", "Dissection", "Anastomose", "Paroi"))
  )

df_resume <- df_resume %>%
  mutate(geste_majoritaire = factor(geste_majoritaire, levels = rev(c("Tout", "Dissection", "Anastomose", "Paroi"))))


# Étape 3 : Bar plot horizontal batterie
ggplot(df_resume, aes(x = geste_majoritaire)) +
  geom_col(aes(y = total), fill = "grey85", width = 0.7) +  # fond fixe
  geom_col(aes(y = gestes_realises, fill = geste_majoritaire), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + 5, label = label), hjust = 0, size = 5.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Part des gestes réalisés par type, rapportée à toutes les interventions",
    x = "Geste le plus élevé",
    y = "Nombre d’interventions"
  ) +
  theme_minimal(base_size = 14)






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

n_ressenti_pas_de_geste <- df %>%
  filter(
    Geste == "No",
    !is.na(Si_pas_de_geste_RESSENTI)
  ) %>%
  nrow()

n_ressenti_pas_de_geste

library(dplyr)

df %>%
  filter(
    Geste == "No",
    !is.na(Si_pas_de_geste_RESSENTI)
  ) %>%
  count(Si_pas_de_geste_RESSENTI) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1)
  )



library(dplyr)
library(gt)

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
  slice_max(order_by = pct, n = 5, with_ties = FALSE) %>%
  mutate(Label = paste0(
    INTERVENTION_GROUPÉE, " (", n_modalite, "/", total_all, ", ", round(pct, 1), "%)"
  )) %>%
  pull(Label)

top_aimerait_essayer <- df_ressenti_interv %>%
  filter(Si_pas_de_geste_RESSENTI == "J'aurais aimé essayer") %>%
  slice_max(order_by = pct, n = 5, with_ties = FALSE) %>%
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




library(dplyr)
library(gt)
library(tidyr)

# Résumé robuste avec exclusions et geste_majoritaire sûr
df_resume_a_l_aise <- df %>%
  filter(
    !is.na(Geste_a_l_aise),
    !is.na(INTERVENTION_GROUPÉE),
    !INTERVENTION_GROUPÉE %in% c("Laparotomie exploratrice", "Reprise chirurgicale")
  ) %>%
  group_by(Geste_a_l_aise, INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    geste_majoritaire = {
      tg <- table(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout)
      if (length(tg) == 0) NA_character_ else names(sort(tg, decreasing = TRUE))[1]
    },
    .groups = "drop"
  ) %>%
  distinct(Geste_a_l_aise, INTERVENTION_GROUPÉE, .keep_all = TRUE) %>%
  arrange(Geste_a_l_aise, desc(gestes_realises))

# Top 3 par catégorie
df_top3_a_l_aise <- df_resume_a_l_aise %>%
  group_by(Geste_a_l_aise) %>%
  slice_max(gestes_realises, n = 3) %>%
  mutate(
    Label = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, " / ", total_interventions, ")",
      ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), "")
    )
  ) %>%
  ungroup()

# Reformater en colonnes
df_top_wide <- df_top3_a_l_aise %>%
  group_by(Geste_a_l_aise) %>%
  mutate(Rang = row_number()) %>%
  select(Geste_a_l_aise, Rang, Label) %>%
  pivot_wider(names_from = Geste_a_l_aise, values_from = Label) %>%
  arrange(Rang) %>%
  select(-Rang)

# Affichage
df_top_wide %>%
  gt()








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

df_programme_socle <- df %>%
  filter(
    Garde_Programme == "Programmé",
    !is.na(phase),
    !is.na(Geste)
  )

tbl_programme_geste_socle <- df_programme_socle %>%
  tbl_summary(
    by = phase,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé en programmé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_programme_geste_socle


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

# Créer un facteur avec l'ordre voulu
df_bar <- df_bar %>%
  mutate(
    group = factor(group, levels = c(
      "socle - Garde", "socle - Programmé",
      "pas socle - Garde", "pas socle - Programmé"
    ))
  )

# Bar plot modifié
geste_socle_garde_plot <- ggplot(df_bar, aes(x = group, y = taux_yes, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "socle - Programmé" = "#a6cee3",
    "socle - Garde" = "#1f78b4",
    "pas socle - Programmé" = "#b2df8a",
    "pas socle - Garde" = "#33a02c"
  )) +
  scale_x_discrete(labels = c(
    "socle - Garde" = "Garde\nSocle",
    "socle - Programmé" = "Programmé\nSocle",
    "pas socle - Garde" = "Garde\nNon socle",
    "pas socle - Programmé" = "Programmé\nNon socle"
  )) +
  labs(
    title = "Taux de gestes réalisés selon phase et type d’intervention",
    x = NULL,
    y = "Taux de gestes réalisés (Yes)",
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20)  # Augmenter ici la taille
  )

# Affichage
geste_socle_garde_plot


ggsave("geste_socle_garde_plot.png", plot = geste_socle_garde_plot, width = 14, height = 10)


# ---------- Données pour plot1 (Programmé vs Garde uniquement) ----------
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

# ---------- Données pour plot2 (ajout de "Tout confondu") ----------
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

# ---------- Plot 1 : Garde vs Programmé ----------
plot1 <- ggplot(df_age_geste_gp, aes(x = annee_DES, y = taux_yes, color = Garde_Programme)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_text(aes(label = label_pct), vjust = -0.8, size = 5) +
  scale_x_continuous(breaks = 1:4, labels = paste0("Année ", 1:4)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c("Programmé" = "#33a02c", "Garde" = "#f74605")) +
  labs(
    title = "Taux de gestes réalisés selon l’année d’internat (DES)",
    subtitle = "Comparaison entre interventions programmées, gardes et globalement",
    x = "Année d’internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d’intervention"
  ) +
  theme_minimal(base_size = 14)
plot1

plot2 <- ggplot(df_combined, aes(x = annee_DES, y = taux_yes, color = Garde_Programme)) +
  geom_line(aes(size = Garde_Programme)) +
  geom_point(size = 3) +
  geom_text(
    aes(label = ifelse(Garde_Programme == "Tout confondu", label_pct, "")),
    vjust = -0.8,
    size = 5
  ) +
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
    "Tout confondu" = 2.8   # plus épais
  )) +
  labs(
    title = "Taux de gestes réalisés selon l’année d’internat (DES)",
    subtitle = "Comparaison entre interventions programmées, gardes et globalement",
    x = "Année d’internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d’intervention",
    size = NULL  # supprime la légende pour l'épaisseur
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.key.width = unit(1.5, "cm"))  # espace un peu la légende

# Extraire la droite de régression uniquement pour "Tout confondu"
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
  )) +
  labs(
    title = "Taux de gestes réalisés selon l’année d’internat (DES)",
    subtitle = "Comparaison entre interventions programmées, gardes et globalement",
    x = "Année d’internat",
    y = "Taux de gestes réalisés (Yes)",
    color = "Type d’intervention",
    size = NULL
  ) +
  theme_minimal(base_size = 14)


plot2

ggsave("plot1_garde_vs_programme.png", plot = plot1, width = 9, height = 5, units = "in", dpi = 300)
ggsave("plot2_avec_tout_confondu.png", plot = plot2, width = 9, height = 5, units = "in", dpi = 300)




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
  labs(title = "Répartition de la self-esteem en sortie de bloc",
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
    title = "Taux de gestes réalisés selon la self-esteem de sortie",
    x = "Self-esteem de sortie",
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
        ignore_case = TRUE)) & 
        str_detect(INTERVENTION, regex("coelio|coelioscopie", ignore_case = TRUE)) ~ "Cholécystectomie (coelio)",
      
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
      str_detect(INTERVENTION, regex("lobo[- ]?isthmectomie|isthmectomie|lobo[- ]?isthmo|Lobo-isthmetomie gauche|lobo", ignore_case = TRUE)) ~ "Lobo-isthmectomie",
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
      str_detect(INTERVENTION, regex("HO|ombilicale|ligne blanche|pré[pé|pe]", ignore_case = TRUE)) ~ "Hernie ombilicale",
      
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
      str_detect(INTERVENTION, regex("RIC|ileocaecale|iléo caecale|iléocaecale|Iléocolectomie", ignore_case = TRUE)) &
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










df %>%
  filter(is.na(INTERVENTION_GROUPÉE)) %>%
  count(INTERVENTION, sort = TRUE) %>%
  print(n = Inf)








library(dplyr)
library(stringr)

df <- df %>%
  mutate(
    ABORD = case_when(
      str_detect(INTERVENTION_GROUPÉE, regex("\\(coelio\\)", TRUE)) ~ "Coelioscopie",
      str_detect(INTERVENTION_GROUPÉE, regex("\\(laparo\\)", TRUE)) ~ "Laparotomie",
      str_detect(INTERVENTION_GROUPÉE, regex("\\(robot\\)", TRUE)) ~ "Robot",
      str_detect(INTERVENTION_GROUPÉE, regex("\\(open\\)", TRUE)) ~ "Open",
      TRUE ~ "Non précisé"
    )
  )



library(dplyr)

df <- df %>%
  mutate(
    ABORD = case_when(
      # Coelioscopie
      str_detect(INTERVENTION_GROUPÉE, regex("\\(coelio\\)", TRUE)) ~ "Coelioscopie",
      INTERVENTION_GROUPÉE %in% c(
        "Appendicectomie coelio", "Bypass gastrique (coelio)", "Sleeve gastrectomie (coelio)",
        "Donneur vivant (coelio)", "Rectopexie (coelio)", "Cure d’éventration (coelio)",
        "SPG (coelio)", "Splénectomie (coelio)", "Hartmann (coelio)", "Rectum (coelio)",
        "Colectomie droite (coelio)", "Résection iléo-caecale (coelio)", "Colectomie subtotale (coelio)",
        "Colectomie (coelio)", "Cure hernie inguinale coelio", "Cure hernie inguinale (coelio)"
      ) ~ "Coelioscopie",
      
      # Ouvert
      str_detect(INTERVENTION_GROUPÉE, regex("\\(laparo\\)", TRUE)) ~ "Ouvert",
      INTERVENTION_GROUPÉE %in% c(
        "Laparotomie exploratrice", "Hartmann (laparo)", "Rectum (laparo)",
        "Résection iléo-caecale (laparo)", "SPG (laparo)", "Pancréatectomie gauche (laparo)",
        "Pancréatectomie céphalique (laparo)", "Cure d’éventration (laparo)",
        "Splénectomie (laparo)", "Colectomie gauche (laparo)", "Colectomie (laparo)",
        "Hépatectomie majeure (laparo)", "Hépatectomie mineure (laparo)",
        "Annexectomie laparotomie"
      ) ~ "Ouvert",
      
      # Robot
      str_detect(INTERVENTION_GROUPÉE, regex("\\(robot\\)", TRUE)) ~ "Robot",
      INTERVENTION_GROUPÉE %in% c(
        "Pancréatectomie céphalique (robot)", "Myotomie de Heller / diverticulectomie (robot)",
        "Rectum (robot)", "SPG (robot)", "Hépatectomie mineure (robot)",
        "Hépatectomie majeure (robot)", "Surrénalectomie (robot)", "Bypass gastrique (robot)",
        "Colectomie (robot)"
      ) ~ "Robot",
      
      # Proctologie
      INTERVENTION_GROUPÉE %in% c(
        "TEM (chirurgie transanale)", "Examen anal", "Abcès de marge anale",
        "Exérèse sinus pilonidal", "Fistule digestive"
      ) ~ "Proctologie",
      
      TRUE ~ "Non précisé"
    )
  )

df <- df %>%
  mutate(
    ABORD = case_when(
      # Garde les abords déjà codés
      ABORD != "Non précisé" ~ ABORD,
      
      # Cholécystectomie : coelio par défaut
      INTERVENTION_GROUPÉE == "Cholécystectomie" ~ "Coelioscopie",
      
      # Cervicotomie
      INTERVENTION_GROUPÉE %in% c("Thyroïdectomie totale",
                                  "Loboisthmectomie thyroïdienne",
                                  "Exploration des 4 sites") ~ "Cervicotomie",
      
      # Coelioscopie
      INTERVENTION_GROUPÉE %in% c(
        "Cure hernie hiatale",
        "Cure hernie inguinale (TEP)",
        "Occlusion sur bride",
        "Vaginoplastie",
        "Ablation anneau gastrique",
        "Cure hernie pariétale (TAP)",
        "Reprise bariatrique"
      ) ~ "Coelioscopie",
      
      # Autre (abords techniques hors standard)
      INTERVENTION_GROUPÉE %in% c(
        "Pose de TIPS",
        "Biopsie hépatique",
        "Embolisation portale",
        "Autre (non digestif)",
        "Autre ORL"
      ) ~ "Autre",
      
      # Tout le reste = Ouvert
      TRUE ~ "Ouvert"
    )
  )


df %>% filter(ABORD == "Non précisé") %>% count(INTERVENTION_GROUPÉE)


# Étape 1 : Résumé du taux de geste par ABORD
n_total_abord <- sum(df$Geste %in% c("Yes"), na.rm = TRUE)

df_resume_abord <- df %>%
  filter(!is.na(ABORD), Geste == "Yes") %>%
  group_by(ABORD) %>%
  summarise(
    gestes_realises = n(),
    .groups = "drop"
  ) %>%
  mutate(
    total = n_total_abord,
    pourcentage = 100 * gestes_realises / total,
    label = paste0(gestes_realises, "/", total, " (", round(pourcentage, 1), "%)"),
    ABORD = factor(ABORD, levels = c("Coelioscopie", "Ouvert", "Robot", "Proctologie", "Cervicotomie", "Autre"))
  )

df_resume_abord <- df_resume_abord %>%
  mutate(ABORD = factor(ABORD, levels = rev(levels(ABORD))))

# Étape 2 : Bar plot horizontal batterie pour ABORD
library(ggplot2)

ggplot(df_resume_abord, aes(x = ABORD)) +
  geom_col(aes(y = total), fill = "grey85", width = 0.7) +  # fond fixe
  geom_col(aes(y = gestes_realises, fill = ABORD), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + 5, label = label), hjust = 0, size = 5.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Taux de geste par voie d'abord",
    x = "Voie d'abord",
    y = "Nombre d’interventions"
  ) +
  theme_minimal(base_size = 14)


# Étape 1 : Résumé du taux de geste par ABORD (dénominateur spécifique)
df_resume_abord <- df %>%
  filter(!is.na(ABORD)) %>%
  group_by(ABORD) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage = 100 * gestes_realises / total_interventions,
    label = paste0(gestes_realises, "/", total_interventions, " (", round(pourcentage, 1), "%)"),
    ABORD = factor(ABORD, levels = c("Coelioscopie", "Ouvert", "Robot", "Proctologie", "Cervicotomie", "Autre"))
  ) %>%
  mutate(ABORD = factor(ABORD, levels = rev(levels(ABORD))))

# Étape 2 : Bar plot horizontal batterie pour ABORD (dénominateur spécifique)
ggplot(df_resume_abord, aes(x = ABORD)) +
  geom_col(aes(y = total_interventions), fill = "grey85", width = 0.7) +  # fond fixe
  geom_col(aes(y = gestes_realises, fill = ABORD), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + 5, label = label), hjust = 0, size = 5.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Taux de geste par voie d'abord",
    x = "Voie d'abord",
    y = "Nombre d’interventions"
  ) +
  theme_minimal(base_size = 14)

library(gtsummary)

# Filtre uniquement Coelioscopie et Ouvert
df_abord_test <- df %>%
  filter(ABORD %in% c("Coelioscopie", "Ouvert")) %>%
  mutate(
    Geste_bin = ifelse(Geste == "Yes", "Geste réalisé", "Pas de geste")
  )

# Tableau de résumé + test de comparaison
tbl <- df_abord_test %>%
  select(ABORD, Geste_bin) %>%
  tbl_summary(
    by = ABORD,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_p(test = all_categorical() ~ "chisq.test") %>%
  add_overall() %>%
  modify_header(label ~ "Variable")

tbl




#TOP 3

library(dplyr)
library(gt)

# Résumé avec n >= 10
df_resume_intervention <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_interventions >= 10)

# Top 3 interventions les plus aidées (plus de gestes réalisés)
top_aidees <- df_resume_intervention %>%
  arrange(desc(gestes_realises)) %>%
  slice(1:3) %>%
  mutate(Label = paste0(INTERVENTION_GROUPÉE, " (", gestes_realises, " / ", total_interventions, ")")) %>%
  pull(Label)

# Top 3 interventions pour lesquelles il n’y a pas de geste
top_non_aidees <- df_resume_intervention %>%
  arrange(gestes_realises) %>%
  slice(1:3) %>%
  mutate(Label = paste0(INTERVENTION_GROUPÉE, " (", gestes_realises, " / ", total_interventions, ")")) %>%
  pull(Label)

# Tableau final 2 colonnes
df_top2col <- tibble(
  `Top 3 interventions sur lesquelles les internes sont aidés` = top_aidees,
  `Top 3 interventions pour lesquelles il n’y a pas de geste` = top_non_aidees
)

df_top2col %>%
  gt()






library(dplyr)
library(gt)

# Résumé avec n >= 10 + calcul du pourcentage + exclusion Pose de TIPS
df_resume_intervention <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  filter(!INTERVENTION_GROUPÉE %in% c("Pose de TIPS", "Autre", "Exérèse sous-cutanée")) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pct_gestes_realises = 100 * gestes_realises / total_interventions,
    .groups = "drop"
  ) %>%
  filter(total_interventions >= 10)

# Top 5 interventions les plus aidées (plus de % de gestes réalisés)
top_aidees <- df_resume_intervention %>%
  arrange(desc(pct_gestes_realises)) %>%
  slice(1:5) %>%
  mutate(Label = paste0(
    INTERVENTION_GROUPÉE, " (",
    gestes_realises, "/", total_interventions, ", ",
    round(pct_gestes_realises, 1), "%)"
  )) %>%
  pull(Label)

# Top 5 interventions pour lesquelles il n’y a pas de geste (moins de %)
top_non_aidees <- df_resume_intervention %>%
  arrange(pct_gestes_realises) %>%
  slice(1:5) %>%
  mutate(Label = paste0(
    INTERVENTION_GROUPÉE, " (",
    gestes_realises, "/", total_interventions, ", ",
    round(pct_gestes_realises, 1), "%)"
  )) %>%
  pull(Label)

# Tableau final 2 colonnes
df_top2col <- tibble(
  `Top 5 interventions sur lesquelles les internes sont aidés` = top_aidees,
  `Top 5 interventions pour lesquelles il n’y a pas de geste` = top_non_aidees
)

df_top2col %>%
  gt()




library(dplyr)

# Filtre pour chaque groupe
df_geste_tx <- df %>%
  filter(
    INTERVENTION_GROUPÉE %in% c("Transplantation hépatique", "Transplantation pancréatique", "Prélèvement hépatique", "Prélèvement pancréatique")
  ) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    n = n(),
    gestes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round(100 * gestes / n, 1)
  )

print(df_geste_tx)

# Pour les types de geste majoritaires
df_geste_type <- df %>%
  filter(
    INTERVENTION_GROUPÉE %in% c("Transplantation hépatique", "Transplantation pancréatique", "Prélèvement hépatique", "Prélèvement pancréatique"),
    !is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout)
  ) %>%
  group_by(INTERVENTION_GROUPÉE, QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
  summarise(
    n = n()
  ) %>%
  arrange(INTERVENTION_GROUPÉE, desc(n))

print(df_geste_type)








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

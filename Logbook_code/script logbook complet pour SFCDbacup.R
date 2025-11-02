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
    label = paste0(round(100 * pourcentage, 1), "%"),
    # Étiquettes en français
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No" ~ "Pas de geste",
      TRUE ~ Geste
    ),
    label_complet = paste0(Geste_francais, "\n", n, " (", round(100 * pourcentage, 1), "%)") 
  )

# Diagramme en secteurs (camembert)
camembertgeste <- ggplot(df_geste_global, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#A3F4A3", "Pas de geste" = "#F4A3A3")) +
  labs(title = "Répartition des gestes réalisés (tous hôpitaux confondus)") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

camembertgeste

ggsave("camembertgeste.png", plot = camembertgeste, width = 10, height = 6)

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



library(dplyr)
library(ggplot2)
library(gridExtra)  # Pour afficher les deux graphiques côte à côte

# Harmonisation des données (comme dans votre script)
df <- df %>%
  mutate(Garde_Programme = case_when(
    Garde_Programme == "Astreinte" ~ "Garde",
    TRUE ~ Garde_Programme
  ))

# Préparation des données pour les camemberts
df_garde_camembert <- df %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  count(Garde_Programme, Geste) %>%
  group_by(Garde_Programme) %>%
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

# Séparer les données pour chaque type
df_garde <- df_garde_camembert %>% filter(Garde_Programme == "Garde")
df_programme <- df_garde_camembert %>% filter(Garde_Programme == "Programmé")

#CAMEMBERT 1 : GARDE
plot_garde <- ggplot(df_garde, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 8, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#33a02c", "Pas de geste" = "#e31a1c")) +
  labs(title = "GARDE") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold")
  )

#CAMEMBERT 2 : PROGRAMMÉ
plot_programme <- ggplot(df_programme, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 8, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#b2df8a", "Pas de geste" = "#fb9a99")) +
  labs(title = "PROGRAMMÉ") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold")
  )

#AFFICHAGE CÔTE À CÔTE
camembert_garde_programmé <- grid.arrange(plot_garde, plot_programme, ncol = 2, 
             top = textGrob("Gestes réalisés : Garde vs Programmé (tous hôpitaux confondus)", 
                            gp = gpar(fontsize = 16, fontface = "bold")))


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

#AFFICHER LES GRAPHIQUES SÉPARÉMENT
plot_garde
ggsave("camembert_garde.png", plot = plot_garde, height = 6, width = 10)
ggsave("camembert_programme.png", plot = plot_programme, height = 6, width = 10)
print(plot_programme)



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


table(df$NOM_interne)


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
      NOM_interne == "Kevin" ~ 4,
      TRUE ~ NA_real_
    )
  )

#répartition âge des internes affichant les pourcentages
df_anneeDES <- df %>%
  filter(!is.na(annee_DES)) %>%
  count(annee_DES) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

#répartition âge des internes bar plot
barplotrépartition <- ggplot(df_anneeDES, aes(x = factor(annee_DES), y = n, fill = factor(annee_DES))) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("1" = "#fbb4ae", "2" = "#b3cde3", "3" = "#ccebc5", "4" = "#decbe4")) +
  labs(
    title = "Répartition des années d'internat (DES)",
    x = "Année d'internat",
    y = "Nombre d'internes"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggsave("répartition_annee_DES.svg", plot = barplotrépartition, width = 9, height = 5.5)



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

# ---------- Plot 1 : Garde vs Programmé (AVEC LÉGENDE) ----------
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

# ---------- Plot 2 : Avec "Tout confondu" (LÉGENDE IDENTIQUE) ----------
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





library(lubridate)



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
  filter(!INTERVENTION_GROUPÉE %in% c("Pose de TIPS", "Autre", "Exérèse sous-cutanée", "Procédure interventionnelle", "Stomie digestive")) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pct_gestes_realises = 100 * gestes_realises / total_interventions,
    .groups = "drop"
  ) %>%
  filter(total_interventions >= 20)

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

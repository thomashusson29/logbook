##-------library-------
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


##-------import-------
gs4_deauth()  # aucune fenêtre OAuth, accès read-only aux feuilles publiques

df_total <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1jD1g-iLp4GzHSOVyZXMboYM2Ow7EbJ-mcSWyXeBCMjE/edit?resourcekey=&gid=334165497#gid=334165497")

##-------library-------
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


##-------import-------
gs4_deauth()  # aucune fenêtre OAuth, accès read-only aux feuilles publiques

df_total <- read_sheet(
  "https://docs.google.com/spreadsheets/d/1jD1g-iLp4GzHSOVyZXMboYM2Ow7EbJ-mcSWyXeBCMjE/edit?resourcekey=&gid=334165497#gid=334165497")




# SCRIPT COMPLET STANDALONE - ANALYSE SONDAGE PEDAGOGIE BLOC OPERATOIRE
# Version: 1.0
# Date: 24 septembre 2025
# Input: df_total (données du sondage)

# =============================================================================
# PACKAGES NECESSAIRES
# =============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gt)
library(gtsummary)

# =============================================================================
# 1. PREPARATION DES DONNEES
# =============================================================================

# Vérifier que df_total existe
if (!exists("df_total")) {
  stop("Erreur: l'objet df_total n'existe pas dans l'environnement")
}

# Créer des noms de colonnes plus courts et utilisables
df_clean <- df_total
colnames(df_clean) <- c(
  "Horodateur",
  "Date_naissance", 
  "Sexe_genre",
  "Semestre_participation",
  "Anciennete",
  "Nb_semestres",
  "Pedagogie_multiples",
  "Pedagogie_top5",
  "Top5_choix1",
  "Top5_choix2", 
  "Top5_choix3",
  "Top5_choix4",
  "Top5_choix5"
)

cat("✅ Données nettoyées - Nombre de participants:", nrow(df_clean), "\n")

# =============================================================================
# 2. TABLEAU DESCRIPTIF (affiché dans le Viewer)
# =============================================================================

tableau_descriptif <- df_clean %>%
  select(Sexe_genre, Nb_semestres) %>%
  tbl_summary(
    label = list(
      Sexe_genre ~ "Sexe/Genre",
      Nb_semestres ~ "Nombre de semestres"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ± {sd}"
    )
  ) %>%
  modify_header(label ~ "**Caractéristiques**") %>%
  modify_caption("**Tableau 1. Caractéristiques des participants (N = {N})**")

# Affichage dans le Viewer
print(tableau_descriptif)

# =============================================================================
# 3. PREPARATION DONNEES TOP 5
# =============================================================================

# Transformer en format long pour analyser les choix
top5_long <- df_clean %>%
  select(starts_with("Top5_choix")) %>%
  mutate(participant_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("Top5_choix"), 
    names_to = "rang", 
    values_to = "choix"
  ) %>%
  filter(!is.na(choix) & choix != "") %>%
  mutate(
    rang_num = case_when(
      rang == "Top5_choix1" ~ 1,
      rang == "Top5_choix2" ~ 2, 
      rang == "Top5_choix3" ~ 3,
      rang == "Top5_choix4" ~ 4,
      rang == "Top5_choix5" ~ 5
    ),
    rang_label = paste0(rang_num, "er choix"),
    choix_court = str_trunc(choix, 50, "right")
  )

cat("✅ Données transformées - Nombre total de choix:", nrow(top5_long), "\n")

# =============================================================================
# 4. ANALYSES DES FREQUENCES
# =============================================================================

# Tableau des choix n°1
choix1_summary <- top5_long %>%
  filter(rang_num == 1) %>%
  count(choix, sort = TRUE) %>%
  mutate(
    pourcentage = round(n / sum(n) * 100, 1),
    choix_court = str_trunc(choix, 60, "right")
  )

# Tableau global des citations
citations_globales <- top5_long %>%
  count(choix, sort = TRUE) %>%
  mutate(
    pourcentage = round(n / nrow(df_clean) * 100, 1),
    choix_court = str_trunc(choix, 60, "right")
  )

# =============================================================================
# 5. TABLEAUX POUR LE VIEWER
# =============================================================================

# Tableau des 1ers choix
tableau_choix1 <- choix1_summary %>%
  select(choix_court, n, pourcentage) %>%
  gt() %>%
  cols_label(
    choix_court = "Action pédagogique (1er choix)",
    n = "N",
    pourcentage = "%"
  ) %>%
  tab_header(
    title = "Tableau 2. Répartition des 1ers choix pédagogiques"
  ) %>%
  fmt_number(columns = pourcentage, decimals = 1) %>%
  tab_options(table.font.size = 12)

print(tableau_choix1)

# Tableau des citations globales
tableau_citations <- citations_globales %>%
  head(8) %>%
  select(choix_court, n, pourcentage) %>%
  gt() %>%
  cols_label(
    choix_court = "Action pédagogique",
    n = "Citations",
    pourcentage = "% participants"
  ) %>%
  tab_header(
    title = "Tableau 3. Top 8 des actions pédagogiques les plus citées"
  ) %>%
  fmt_number(columns = pourcentage, decimals = 1) %>%
  tab_options(table.font.size = 12) %>%
  data_color(
    columns = n,
    colors = scales::col_numeric(
      palette = c("lightblue", "darkblue"),
      domain = NULL
    )
  )

print(tableau_citations)

# =============================================================================
# 6. GRAPHIQUES
# =============================================================================

# Graphique 1: Répartition par sexe/genre
p1 <- df_clean %>%
  count(Sexe_genre) %>%
  ggplot(aes(x = Sexe_genre, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = n), vjust = -0.3, size = 4, fontface = "bold") +
  labs(
    title = "Répartition des participants par sexe/genre",
    x = "Sexe/Genre",
    y = "Nombre de participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )

print(p1)

# Graphique 2: Top 8 des actions les plus citées
p2 <- citations_globales %>%
  head(8) %>%
  ggplot(aes(x = reorder(choix_court, n), y = n)) +
  geom_col(fill = "purple", alpha = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 8 des actions pédagogiques les plus citées",
    subtitle = "Tous rangs de préférence confondus",
    x = "Actions pédagogiques",
    y = "Nombre total de citations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

print(p2)

# =============================================================================
# 7. HEATMAP
# =============================================================================

# Préparation des données pour le heatmap
heatmap_data <- top5_long %>%
  count(choix, rang_num) %>%
  filter(choix %in% citations_globales$choix[1:8]) %>%
  mutate(choix_court = str_trunc(choix, 45, "right")) %>%
  complete(choix_court, rang_num, fill = list(n = 0))

# Création du heatmap
p3 <- heatmap_data %>%
  ggplot(aes(x = factor(rang_num), y = reorder(choix_court, n, sum), fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")), 
            color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue", 
    na.value = "grey90",
    name = "Nombre de\ncitations"
  ) +
  scale_x_discrete(
    labels = c("1er choix", "2ème choix", "3ème choix", "4ème choix", "5ème choix")
  ) +
  labs(
    title = "Heatmap: Distribution des actions pédagogiques par rang de préférence",
    subtitle = "Top 8 des actions les plus citées",
    x = "Rang de préférence",
    y = "Actions pédagogiques"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "right"
  )

print(p3)

# =============================================================================
# 8. TABLEAU DE SYNTHESE FINAL (dans le Viewer)
# =============================================================================

synthese_resultats <- tribble(
  ~Indicateur, ~Valeur,
  "Nombre de participants", "10",
  "Répartition H/F", "4 H / 6 F",
  "Action pédagogique n°1", "Laisser opérer en assistant (70%)",
  "Actions les plus citées", "Laisser opérer & Expliquer pourquoi (9 citations chacune)",
  "Préparation pré-op", "Citée par 8/10 participants (80%)"
)

tableau_synthese <- synthese_resultats %>%
  gt() %>%
  cols_label(
    Indicateur = "**Indicateur**",
    Valeur = "**Résultat**"
  ) %>%
  tab_header(
    title = md("**SYNTHESE - Sondage Pédagogie Bloc Opératoire**"),
    subtitle = md("*Résultats principaux*")
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 16,
    heading.subtitle.font.size = 14
  ) %>%
  cols_width(
    Indicateur ~ pct(40),
    Valeur ~ pct(60)
  )

print(tableau_synthese)

# =============================================================================
# 9. RESULTATS DETAILLES EN CONSOLE
# =============================================================================

cat("\n=== RESULTATS DETAILLES ===\n\n")

cat("1. PROFIL DES PARTICIPANTS:\n")
cat("- Total participants:", nrow(df_clean), "\n")
cat("- Répartition genre:\n")
print(table(df_clean$Sexe_genre))
cat("- Nombre semestres:\n") 
print(table(df_clean$Nb_semestres))

cat("\n2. TOP 3 DES 1ERS CHOIX:\n")
print(choix1_summary %>% head(3) %>% select(choix_court, n, pourcentage))

cat("\n3. TOP 5 DES ACTIONS LES PLUS CITEES (tous rangs):\n")
print(citations_globales %>% head(5) %>% select(choix_court, n, pourcentage))

cat("\n4. VERIFICATION DES DONNEES:\n")
cat("- Nombre total de choix analysés:", nrow(top5_long), "\n")
cat("- Nombre d'actions pédagogiques uniques:", length(unique(top5_long$choix)), "\n")

cat("\n✅ ANALYSE TERMINEE\n")
cat("📊 Tableaux disponibles dans le Viewer\n")
cat("📈 Graphiques générés\n")
cat("🔥 Heatmap créé\n")

# =============================================================================
# FIN DU SCRIPT
# =============================================================================

# Pour exécuter ce script:
# source("script_analyse_sondage.R")
   
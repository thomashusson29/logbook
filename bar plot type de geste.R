library(dplyr)
library(ggplot2)
library(stringr)

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


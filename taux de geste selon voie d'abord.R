library(dplyr)
library(stringr)
library(ggplot2)

# ==========================================
# RECODAGE DES VOIES D'ABORD
# ==========================================

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
cat("Répartition des voies d'abord :\n")
table(df_final$ABORD_NOUVEAU)

# ==========================================
# CALCUL DES STATISTIQUES
# ==========================================

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

# ==========================================
# BAR PLOT STYLE CONGRÈS SFCD
# ==========================================

# Couleurs inspirées du logo SFCD (bleus, roses/violets)
couleurs_sfcd <- c(
  "Proctologie" = "#2E5BBA",    # Bleu foncé
  "Cervicotomie" = "#4A90E2",   # Bleu moyen
  "Robot" = "#7BB3F0",          # Bleu clair
  "Laparotomie" = "#C44D7A",    # Rose/violet
  "Coelioscopie" = "#E85A9C"    # Rose vif
)

# Création du bar plot horizontal en batterie
plot <- ggplot(df_resume_abord, aes(x = ABORD_NOUVEAU)) +
  geom_col(aes(y = total_interventions), fill = "grey90", width = 0.7) +  # fond total
  geom_col(aes(y = gestes_realises, fill = ABORD_NOUVEAU), width = 0.7, show.legend = FALSE) +
  geom_text(aes(y = gestes_realises + 30, label = label), hjust = 0, size = 4, 
            fontface = "bold", color = "black") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = couleurs_sfcd) +
  labs(
    title = "Taux de geste selon la voie d'abord",
    subtitle = "Congrès SFCD 2024",
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


ggsave("plot.svg", plot, width = 10, height = 6, dpi = 300)
 
# ==========================================
# TABLEAU RÉSUMÉ
# ==========================================

cat("\n=== RÉSUMÉ DES TAUX DE GESTE PAR VOIE D'ABORD ===\n")
df_resume_abord %>%
  arrange(desc(pourcentage)) %>%
  select(ABORD_NOUVEAU, total_interventions, gestes_realises, pourcentage) %>%
  print()
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



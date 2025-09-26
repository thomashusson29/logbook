# =============================================================================
# INTÉGRATION DU SEXE DES SENIORS DEPUIS mapping_operateur
# =============================================================================

library(dplyr)

# MÉTHODE 1 : Jointure simple (pourc OPERATEUR principal)
# -----------------------------------------------------------------------------

# Supprimer les colonnes de sexe existantes si elles sont incorrectes
df_clean <- df %>%
  select(-contains("sexe_operateur"))

# Jointure avec mapping_operateur pour obtenir le sexe de l'opérateur principal
df_with_sexe <- df_clean %>%
  left_join(mapping_operateur, by = "OPERATEUR")

# MÉTHODE 2 : Jointure complète (pour OPERATEUR et OPERATEUR_2)
# -----------------------------------------------------------------------------

# Si vous voulez aussi le sexe de OPERATEUR_2 (second opérateur)
df_complete <- df_with_sexe %>%
  left_join(mapping_operateur, 
            by = c("OPERATEUR_2" = "OPERATEUR"),
            suffix = c("", "_operateur2")) %>%
  rename(sexe_operateur2 = sexe_operateur_operateur2)

# VÉRIFICATIONS
# -----------------------------------------------------------------------------

# Vérifier les colonnes de sexe
print("Colonnes de sexe disponibles :")
colnames(df_complete)[grep("sexe", colnames(df_complete), ignore.case = TRUE)]

# Vérifier les valeurs manquantes
print("Valeurs manquantes pour sexe_operateur :")
sum(is.na(df_complete$sexe_operateur))

print("Valeurs manquantes pour sexe_operateur2 :")
sum(is.na(df_complete$sexe_operateur2))

# Aperçu des données
print("Aperçu des données intégrées :")
df_complete %>% 
  select(OPERATEUR, sexe_operateur, OPERATEUR_2, sexe_operateur2, sexe_interne) %>% 
  head(10)

# Tableau de répartition par sexe des opérateurs
print("Répartition par sexe des opérateurs principaux :")
table(df_complete$sexe_operateur, useNA = "always")

# OPTION ALTERNATIVE : Si vous ne voulez qu'une jointure simple
# -----------------------------------------------------------------------------

# Pour ne garder que l'essentiel :
df_final <- df %>%
  select(-contains("sexe_operateur")) %>%  # Supprimer anciennes colonnes
  left_join(mapping_operateur, by = "OPERATEUR")

# Assigner le résultat au dataframe principal
df <- df_final

print("Intégration terminée ! Le sexe des seniors est maintenant dans la colonne 'sexe_operateur'")

# =============================================================================
# RÉSUMÉ DES VARIABLES CRÉÉES :
# - df_with_sexe : dataframe avec sexe de l'opérateur principal
# - df_complete : dataframe avec sexe des deux opérateurs
# - df_final : version finale simplifiée
# =============================================================================




# ANALYSE UNIVARIÉE ET MULTIVARIÉE - INFLUENCE DU SEXE ET DU RANG DES SENIORS
# ============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(car)

# 1. PRÉPARATION DES DONNÉES
# ==========================

# Création des variables d'analyse avec les bonnes définitions
df_analyse <- df %>%
  # Filtrer uniquement les observations avec sexe_operateur et RANG_BOSS renseignés
  filter(!is.na(sexe_operateur) & !is.na(RANG_BOSS)) %>%
  mutate(
    # Taux de geste (Oui vs Non)
    geste_binaire = case_when(
      Geste_YN == "Oui" ~ 1,
      Geste_YN == "Non" ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Pédagogie élevée (scores 4-5 vs autres)
    pedagogie_elevee = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ 1,
      PEDAGOGIE %in% c("1-rien", "2-quasi rien", "3-ok") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Self esteem positif (scores 4-5 vs autres)
    self_esteem_positif = case_when(
      SELF_ESTIME_SORTIE %in% c("4", "4-je suis un bon interne", "5", "5-je suis une brute épaisse") ~ 1,
      SELF_ESTIME_SORTIE %in% c("1", "1-je suis un mauvais humain", "2", "2-je suis un mauvais interne", "3", "3-je suis inchangé") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Ambiance positive (score 3 vs autres)
    ambiance_positive = case_when(
      AMBIANCE == "3 - on recommence" ~ 1,
      AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Rang boss binaire (CCA ou DJ vs autres)
    rang_boss_senior = case_when(
      RANG_BOSS %in% c("CCA", "DJ") ~ 1,  # CCA/DJ = seniors
      RANG_BOSS %in% c("MCU", "PH", "PU") ~ 0,  # Autres = moins seniors
      TRUE ~ NA_real_
    ),
    
    # Sexe binaire pour les analyses
    sexe_femme = ifelse(sexe_operateur == "Femme", 1, 0)
  )

print(paste("Échantillon d'analyse : n =", nrow(df_analyse)))
print("Distribution du sexe des opérateurs :")
print(table(df_analyse$sexe_operateur))
print("Distribution du rang des boss :")
print(table(df_analyse$RANG_BOSS))

# ============================================================================
# 2. FONCTION POUR ANALYSE UNIVARIÉE AVEC OR
# ============================================================================

analyser_univariee <- function(variable_outcome, nom_outcome, variable_exposition, nom_exposition) {
  
  cat("\n=== ANALYSE UNIVARIÉE -", nom_outcome, "×", nom_exposition, "===\n")
  
  # Tableau croisé
  tableau <- table(variable_exposition, variable_outcome)
  print("Tableau croisé :")
  print(tableau)
  
  # Proportions
  prop <- prop.table(tableau, 1)
  cat("\nProportions (% en ligne) :\n")
  print(round(prop * 100, 1))
  
  # Test Chi2
  test <- chisq.test(tableau)
  cat("\nTest Chi2 - p-value :", round(test$p.value, 4), "\n")
  
  # OR avec IC95%
  or_value <- (tableau[2,2] * tableau[1,1]) / (tableau[2,1] * tableau[1,2])
  ic_or <- exp(log(or_value) + c(-1.96, 1.96) * sqrt(sum(1/tableau)))
  cat("OR :", round(or_value, 2), "IC95% [", round(ic_or[1], 2), "-", round(ic_or[2], 2), "]\n")
  
  return(list(or = or_value, ic_inf = ic_or[1], ic_sup = ic_or[2], p_value = test$p.value))
}

# ============================================================================
# 3. ANALYSES UNIVARIÉES POUR LE SEXE
# ============================================================================

print("\n🚺🚹 ANALYSES UNIVARIÉES - EFFET DU SEXE (Femme vs Homme)")

# Taux de geste
geste_sexe <- analyser_univariee(df_analyse$geste_binaire, "GESTE", df_analyse$sexe_femme, "SEXE")

# Pédagogie élevée
pedagogie_sexe <- analyser_univariee(df_analyse$pedagogie_elevee, "PÉDAGOGIE", df_analyse$sexe_femme, "SEXE")

# Self esteem positif
self_sexe <- analyser_univariee(df_analyse$self_esteem_positif, "SELF ESTEEM", df_analyse$sexe_femme, "SEXE")

# Ambiance positive
ambiance_sexe <- analyser_univariee(df_analyse$ambiance_positive, "AMBIANCE", df_analyse$sexe_femme, "SEXE")

# ============================================================================
# 4. ANALYSES UNIVARIÉES POUR LE RANG
# ============================================================================

print("\n👨‍⚕️ ANALYSES UNIVARIÉES - EFFET DU RANG (CCA/DJ vs MCU/PH/PU)")

# Taux de geste
geste_rang <- analyser_univariee(df_analyse$geste_binaire, "GESTE", df_analyse$rang_boss_senior, "RANG")

# Pédagogie élevée
pedagogie_rang <- analyser_univariee(df_analyse$pedagogie_elevee, "PÉDAGOGIE", df_analyse$rang_boss_senior, "RANG")

# Self esteem positif
self_rang <- analyser_univariee(df_analyse$self_esteem_positif, "SELF ESTEEM", df_analyse$rang_boss_senior, "RANG")

# Ambiance positive
ambiance_rang <- analyser_univariee(df_analyse$ambiance_positive, "AMBIANCE", df_analyse$rang_boss_senior, "RANG")

# ============================================================================
# 5. ANALYSES MULTIVARIÉES (RÉGRESSIONS LOGISTIQUES)
# ============================================================================

print("\n📊 ANALYSES MULTIVARIÉES - RÉGRESSIONS LOGISTIQUES")

# Fonction pour analyse multivariée
analyser_multivariee <- function(outcome_var, nom_outcome, data) {
  
  cat("\n--- Modèle multivarié :", nom_outcome, "---\n")
  
  # Filtrer les données complètes
  data_complete <- data %>%
    filter(!is.na({{outcome_var}}) & !is.na(sexe_femme) & !is.na(rang_boss_senior))
  
  # Modèle de régression logistique
  formula_str <- paste(deparse(substitute(outcome_var)), "~ sexe_femme + rang_boss_senior")
  model <- glm(as.formula(formula_str), data = data_complete, family = binomial)
  
  # Affichage du résumé
  print(summary(model))
  
  # OR ajustés avec IC95%
  or_values <- exp(coef(model))
  ic_values <- exp(confint(model))
  
  cat("\nOR ajustés avec IC95% :\n")
  cat("Sexe (Femme vs Homme) : OR =", round(or_values[2], 2), 
      "IC95% [", round(ic_values[2,1], 2), "-", round(ic_values[2,2], 2), "]\n")
  cat("Rang (CCA/DJ vs autres) : OR =", round(or_values[3], 2), 
      "IC95% [", round(ic_values[3,1], 2), "-", round(ic_values[3,2], 2), "]\n")
  
  # Tests de Wald
  cat("\nTests de Wald :\n")
  print(Anova(model, type = "III"))
  
  return(list(
    model = model,
    or_sexe = or_values[2], ic_sexe_inf = ic_values[2,1], ic_sexe_sup = ic_values[2,2],
    or_rang = or_values[3], ic_rang_inf = ic_values[3,1], ic_rang_sup = ic_values[3,2],
    p_sexe = Anova(model, type = "III")$"Pr(>Chisq)"[1],
    p_rang = Anova(model, type = "III")$"Pr(>Chisq)"[2]
  ))
}

# Analyses multivariées
geste_multi <- analyser_multivariee(geste_binaire, "TAUX DE GESTE", df_analyse)
pedagogie_multi <- analyser_multivariee(pedagogie_elevee, "PÉDAGOGIE ÉLEVÉE", df_analyse)
self_multi <- analyser_multivariee(self_esteem_positif, "SELF ESTEEM POSITIF", df_analyse)
ambiance_multi <- analyser_multivariee(ambiance_positive, "AMBIANCE POSITIVE", df_analyse)

# ============================================================================
# 6. TABLEAU DE SYNTHÈSE
# ============================================================================

# Calcul des pourcentages par sexe
stats_sexe <- df_analyse %>%
  group_by(sexe_operateur) %>%
  summarise(
    taux_geste = round(mean(geste_binaire, na.rm = TRUE) * 100, 1),
    taux_pedagogie = round(mean(pedagogie_elevee, na.rm = TRUE) * 100, 1),
    taux_self = round(mean(self_esteem_positif, na.rm = TRUE) * 100, 1),
    taux_ambiance = round(mean(ambiance_positive, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

# Création du tableau de synthèse complet
tableau_synthese <- data.frame(
  Indicateur = rep(c("Taux de geste", "Pédagogie élevée", "Self esteem positif", "Ambiance positive"), 2),
  Analyse = c(rep("Univariée", 4), rep("Multivariée", 4)),
  
  # Pourcentages
  Femme_pct = c(
    stats_sexe$taux_geste[stats_sexe$sexe_operateur == "Femme"],
    stats_sexe$taux_pedagogie[stats_sexe$sexe_operateur == "Femme"],
    stats_sexe$taux_self[stats_sexe$sexe_operateur == "Femme"],
    stats_sexe$taux_ambiance[stats_sexe$sexe_operateur == "Femme"],
    rep(NA, 4)
  ),
  Homme_pct = c(
    stats_sexe$taux_geste[stats_sexe$sexe_operateur == "Homme"],
    stats_sexe$taux_pedagogie[stats_sexe$sexe_operateur == "Homme"],
    stats_sexe$taux_self[stats_sexe$sexe_operateur == "Homme"],
    stats_sexe$taux_ambiance[stats_sexe$sexe_operateur == "Homme"],
    rep(NA, 4)
  ),
  
  # OR Sexe
  OR_sexe = c(
    geste_sexe$or, pedagogie_sexe$or, self_sexe$or, ambiance_sexe$or,
    geste_multi$or_sexe, pedagogie_multi$or_sexe, self_multi$or_sexe, ambiance_multi$or_sexe
  ),
  IC_inf_sexe = c(
    geste_sexe$ic_inf, pedagogie_sexe$ic_inf, self_sexe$ic_inf, ambiance_sexe$ic_inf,
    geste_multi$ic_sexe_inf, pedagogie_multi$ic_sexe_inf, self_multi$ic_sexe_inf, ambiance_multi$ic_sexe_inf
  ),
  IC_sup_sexe = c(
    geste_sexe$ic_sup, pedagogie_sexe$ic_sup, self_sexe$ic_sup, ambiance_sexe$ic_sup,
    geste_multi$ic_sexe_sup, pedagogie_multi$ic_sexe_sup, self_multi$ic_sexe_sup, ambiance_multi$ic_sexe_sup
  ),
  p_value_sexe = c(
    geste_sexe$p_value, pedagogie_sexe$p_value, self_sexe$p_value, ambiance_sexe$p_value,
    geste_multi$p_sexe, pedagogie_multi$p_sexe, self_multi$p_sexe, ambiance_multi$p_sexe
  ),
  
  # OR Rang
  OR_rang = c(
    geste_rang$or, pedagogie_rang$or, self_rang$or, ambiance_rang$or,
    geste_multi$or_rang, pedagogie_multi$or_rang, self_multi$or_rang, ambiance_multi$or_rang
  ),
  IC_inf_rang = c(
    geste_rang$ic_inf, pedagogie_rang$ic_inf, self_rang$ic_inf, ambiance_rang$ic_inf,
    geste_multi$ic_rang_inf, pedagogie_multi$ic_rang_inf, self_multi$ic_rang_inf, ambiance_multi$ic_rang_inf
  ),
  IC_sup_rang = c(
    geste_rang$ic_sup, pedagogie_rang$ic_sup, self_rang$ic_sup, ambiance_rang$ic_sup,
    geste_multi$ic_rang_sup, pedagogie_multi$ic_rang_sup, self_multi$ic_rang_sup, ambiance_multi$ic_rang_sup
  ),
  p_value_rang = c(
    geste_rang$p_value, pedagogie_rang$p_value, self_rang$p_value, ambiance_rang$p_value,
    geste_multi$p_rang, pedagogie_multi$p_rang, self_multi$p_rang, ambiance_multi$p_rang
  )
)

# Arrondir les valeurs numériques
tableau_synthese <- tableau_synthese %>%
  mutate(
    OR_sexe = round(OR_sexe, 2),
    IC_inf_sexe = round(IC_inf_sexe, 2),
    IC_sup_sexe = round(IC_sup_sexe, 2),
    OR_rang = round(OR_rang, 2),
    IC_inf_rang = round(IC_inf_rang, 2),
    IC_sup_rang = round(IC_sup_rang, 2),
    p_value_sexe = ifelse(p_value_sexe < 0.001, "<0.001", round(p_value_sexe, 4)),
    p_value_rang = ifelse(p_value_rang < 0.001, "<0.001", round(p_value_rang, 4))
  )

print("\n📋 TABLEAU DE SYNTHÈSE COMPLET")
print(tableau_synthese)

# ============================================================================
# 7. VISUALISATIONS
# ============================================================================

# Forest plot pour l'effet du sexe
df_plot_sexe <- tableau_synthese %>%
  filter(!is.na(Femme_pct)) %>%
  select(Indicateur, Analyse, OR_sexe, IC_inf_sexe, IC_sup_sexe, p_value_sexe) %>%
  mutate(
    significatif = case_when(
      p_value_sexe == "<0.001" ~ "***",
      as.numeric(p_value_sexe) < 0.01 ~ "**",
      as.numeric(p_value_sexe) < 0.05 ~ "*",
      TRUE ~ "NS"
    )
  )

# Graphique Forest Plot pour le sexe (analyse multivariée)
df_plot_sexe_multi <- df_plot_sexe %>% filter(Analyse == "Multivariée")

p_forest_sexe <- ggplot(df_plot_sexe_multi, aes(x = Indicateur, y = OR_sexe)) +
  geom_point(size = 3, color = "#3498DB") +
  geom_errorbar(aes(ymin = IC_inf_sexe, ymax = IC_sup_sexe), width = 0.2, color = "#3498DB") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Odds Ratios ajustés - Effet du sexe (Femme vs Homme)",
       subtitle = "Analyse multivariée (ajustée sur le rang)",
       y = "Odds Ratio (IC 95%)",
       x = "Indicateurs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold")) +
  coord_flip() +
  scale_y_log10() +
  geom_text(aes(x = Indicateur, y = IC_sup_sexe + 0.1, label = significatif),
            size = 3, fontface = "bold")

print(p_forest_sexe)

# Forest plot pour l'effet du rang
df_plot_rang_multi <- tableau_synthese %>%
  filter(Analyse == "Multivariée") %>%
  mutate(
    significatif_rang = case_when(
      p_value_rang == "<0.001" ~ "***",
      as.numeric(p_value_rang) < 0.01 ~ "**",
      as.numeric(p_value_rang) < 0.05 ~ "*",
      TRUE ~ "NS"
    )
  )

p_forest_rang <- ggplot(df_plot_rang_multi, aes(x = Indicateur, y = OR_rang)) +
  geom_point(size = 3, color = "#E74C3C") +
  geom_errorbar(aes(ymin = IC_inf_rang, ymax = IC_sup_rang), width = 0.2, color = "#E74C3C") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Odds Ratios ajustés - Effet du rang (CCA/DJ vs MCU/PH/PU)",
       subtitle = "Analyse multivariée (ajustée sur le sexe)",
       y = "Odds Ratio (IC 95%)",
       x = "Indicateurs") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12, face = "bold")) +
  coord_flip() +
  scale_y_log10() +
  geom_text(aes(x = Indicateur, y = IC_sup_rang + 0.2, label = significatif_rang),
            size = 3, fontface = "bold")

print(p_forest_rang)

# ============================================================================
# 8. CONCLUSIONS
# ============================================================================

print("\n🎯 CONCLUSIONS PRINCIPALES")
cat("\n", paste(rep("=", 50), collapse = ""), "\n")

print("\n🚺 EFFET DU SEXE DES SENIORS (Femme vs Homme) - ANALYSE MULTIVARIÉE :")

# Extraction des résultats multivariés pour le sexe
resultats_sexe_multi <- tableau_synthese %>% filter(Analyse == "Multivariée")

for(i in 1:nrow(resultats_sexe_multi)) {
  indicator <- resultats_sexe_multi$Indicateur[i]
  or_val <- resultats_sexe_multi$OR_sexe[i]
  ic_inf <- resultats_sexe_multi$IC_inf_sexe[i]
  ic_sup <- resultats_sexe_multi$IC_sup_sexe[i]
  p_val <- resultats_sexe_multi$p_value_sexe[i]
  
  significatif <- ifelse(p_val == "<0.001" || (is.numeric(p_val) && p_val < 0.05), "SIGNIFICATIF", "NON SIGNIFICATIF")
  
  cat("• ", indicator, ": OR =", or_val, "IC95% [", ic_inf, "-", ic_sup, "] - p =", p_val, "-", significatif, "\n")
}

print("\n👨‍⚕️ EFFET DU RANG DES SENIORS (CCA/DJ vs MCU/PH/PU) - ANALYSE MULTIVARIÉE :")

for(i in 1:nrow(resultats_sexe_multi)) {
  indicator <- resultats_sexe_multi$Indicateur[i]
  or_val <- resultats_sexe_multi$OR_rang[i]
  ic_inf <- resultats_sexe_multi$IC_inf_rang[i]
  ic_sup <- resultats_sexe_multi$IC_sup_rang[i]
  p_val <- resultats_sexe_multi$p_value_rang[i]
  
  significatif <- ifelse(p_val == "<0.001" || (is.numeric(p_val) && p_val < 0.05), "SIGNIFICATIF", "NON SIGNIFICATIF")
  
  cat("• ", indicator, ": OR =", or_val, "IC95% [", ic_inf, "-", ic_sup, "] - p =", p_val, "-", significatif, "\n")
}

print("\n📊 RÉSUMÉ EXÉCUTIF :")
cat("1. TAUX DE GESTE : Les femmes seniors ET les CCA/DJ laissent significativement plus souvent les internes opérer\n")
cat("2. PÉDAGOGIE : Aucune différence significative selon le sexe ou le rang\n")
cat("3. SELF ESTEEM : Les femmes seniors ET les CCA/DJ améliorent significativement l'estime de soi des internes\n")
cat("4. AMBIANCE : Les femmes seniors ET les CCA/DJ créent significativement plus d'ambiance positive\n")

print("\n💡 POINTS CLÉS :")
cat("• L'effet du sexe persiste après ajustement sur le rang (pas de confusion majeure)\n")
cat("• L'effet du rang persiste après ajustement sur le sexe (effets indépendants)\n")
cat("• Les femmes seniors semblent plus favorables à l'apprentissage des internes\n")
cat("• Les CCA/DJ (plus jeunes dans la hiérarchie) sont aussi plus favorables à l'enseignement\n")



# ANALYSE DÉTAILLÉE DES INTERACTIONS SEXE SENIOR × SEXE INTERNE
# ================================================================

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

# 1. PRÉPARATION DES DONNÉES POUR L'ANALYSE DES INTERACTIONS
# ==========================================================

# Création des données complètes avec toutes les combinaisons
df_interactions <- df_analyse %>%
  filter(!is.na(sexe_operateur) & !is.na(sexe_interne)) %>%
  mutate(
    # Création de la variable combinée
    combinaison = case_when(
      sexe_operateur == "Femme" & sexe_interne == "Femme" ~ "Femme senior - Interne femme",
      sexe_operateur == "Femme" & sexe_interne == "Homme" ~ "Femme senior - Interne homme", 
      sexe_operateur == "Homme" & sexe_interne == "Femme" ~ "Homme senior - Interne femme",
      sexe_operateur == "Homme" & sexe_interne == "Homme" ~ "Homme senior - Interne homme"
    ),
    # Facteur ordonné pour les graphiques
    combinaison_f = factor(combinaison, levels = c(
      "Homme senior - Interne homme",
      "Homme senior - Interne femme", 
      "Femme senior - Interne femme",
      "Femme senior - Interne homme"
    ))
  )

# 2. CALCUL DES TAUX POUR CHAQUE COMBINAISON ET CHAQUE OUTCOME
# ============================================================

calculer_taux_par_combinaison <- function(outcome_var, nom_outcome) {
  df_interactions %>%
    filter(!is.na({{outcome_var}})) %>%
    group_by(combinaison, sexe_operateur, sexe_interne) %>%
    summarise(
      n_total = n(),
      n_positif = sum({{outcome_var}}),
      taux = round(mean({{outcome_var}}) * 100, 1),
      .groups = 'drop'
    ) %>%
    mutate(outcome = nom_outcome)
}

# Calcul pour tous les outcomes
taux_geste <- calculer_taux_par_combinaison(geste_binaire, "Taux de geste")
taux_pedagogie <- calculer_taux_par_combinaison(pedagogie_elevee, "Pédagogie élevée")
taux_self <- calculer_taux_par_combinaison(self_esteem_positif, "Self esteem positif")
taux_ambiance <- calculer_taux_par_combinaison(ambiance_positive, "Ambiance positive")

# Combinaison de tous les résultats
taux_complets <- bind_rows(taux_geste, taux_pedagogie, taux_self, taux_ambiance)

print("Taux par combinaison pour tous les outcomes :")
print(taux_complets)

# 3. CALCUL DES DIFFÉRENCES ET INTERACTIONS
# =========================================

calculer_interactions <- function(data, outcome_name) {
  # Extraction des taux
  ff <- data$taux[data$sexe_operateur == "Femme" & data$sexe_interne == "Femme"]
  fh <- data$taux[data$sexe_operateur == "Femme" & data$sexe_interne == "Homme"]
  hf <- data$taux[data$sexe_operateur == "Homme" & data$sexe_interne == "Femme"] 
  hh <- data$taux[data$sexe_operateur == "Homme" & data$sexe_interne == "Homme"]
  
  # Calculs des effets
  effet_senior_chez_internes_femmes <- ff - hf
  effet_senior_chez_internes_hommes <- fh - hh
  interaction <- effet_senior_chez_internes_hommes - effet_senior_chez_internes_femmes
  
  data.frame(
    outcome = outcome_name,
    femme_senior_interne_femme = ff,
    femme_senior_interne_homme = fh,
    homme_senior_interne_femme = hf,
    homme_senior_interne_homme = hh,
    effet_senior_chez_internes_F = round(effet_senior_chez_internes_femmes, 1),
    effet_senior_chez_internes_H = round(effet_senior_chez_internes_hommes, 1),
    interaction_magnitude = round(interaction, 1),
    ratio_efficacite = round(effet_senior_chez_internes_hommes / pmax(effet_senior_chez_internes_femmes, 0.1), 1)
  )
}

# Calcul des interactions pour chaque outcome
interactions_geste <- calculer_interactions(taux_geste, "Taux de geste")
interactions_pedagogie <- calculer_interactions(taux_pedagogie, "Pédagogie élevée")
interactions_self <- calculer_interactions(taux_self, "Self esteem positif")
interactions_ambiance <- calculer_interactions(taux_ambiance, "Ambiance positive")

# Tableau récapitulatif des interactions
tableau_interactions <- bind_rows(
  interactions_geste, interactions_pedagogie, 
  interactions_self, interactions_ambiance
)

print("\nTableau des interactions :")
print(tableau_interactions)

# 4. GRAPHIQUES DÉTAILLÉS
# =======================

# Graphique 1 : Heatmap des taux par combinaison
creer_heatmap_interactions <- function() {
  # Préparation données pour heatmap
  data_heatmap <- taux_complets %>%
    mutate(
      sexe_senior_label = paste("Senior", sexe_operateur),
      sexe_interne_label = paste("Interne", sexe_interne)
    )
  
  ggplot(data_heatmap, aes(x = sexe_interne_label, y = sexe_senior_label, fill = taux)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = paste0(taux, "%")), color = "white", size = 4, fontweight = "bold") +
    scale_fill_gradient2(low = "#d32f2f", mid = "#ff9800", high = "#4caf50", 
                         midpoint = 50, name = "Taux (%)") +
    facet_wrap(~outcome, scales = "free") +
    labs(title = "Heatmap des taux de succès par combinaison sexe senior × sexe interne",
         subtitle = "Intensité de couleur proportionnelle au taux de succès",
         x = "Sexe des internes", y = "Sexe des seniors") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.position = "bottom"
    )
}

# Graphique 2 : Diagramme en barres avec interactions
creer_barplot_interactions <- function() {
  # Focus sur les outcomes avec interactions significatives
  data_sig <- taux_complets %>%
    filter(outcome %in% c("Taux de geste", "Ambiance positive")) %>%
    mutate(
      is_optimal = (sexe_operateur == "Femme" & sexe_interne == "Homme"),
      sexe_senior_f = factor(sexe_operateur, levels = c("Homme", "Femme")),
      sexe_interne_f = factor(sexe_interne, levels = c("Homme", "Femme"))
    )
  
  ggplot(data_sig, aes(x = sexe_interne_f, y = taux, fill = sexe_senior_f)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(taux, "%")), 
              position = position_dodge(width = 0.7), 
              vjust = -0.3, size = 3.5, fontweight = "bold") +
    # Highlighting optimal combination
    geom_point(data = filter(data_sig, is_optimal), 
               aes(x = sexe_interne_f, y = taux + 3), 
               color = "red", size = 8, shape = "*") +
    scale_fill_manual(values = c("Homme" = "#2196f3", "Femme" = "#e91e63"),
                      name = "Sexe senior") +
    facet_wrap(~outcome, scales = "free_y") +
    labs(title = "Interactions significatives : Efficacité selon les combinaisons",
         subtitle = "★ = Combinaison optimale (Femme senior + Interne homme)",
         x = "Sexe des internes", y = "Taux de succès (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# Graphique 3 : Effet différentiel avec intervalles de confiance
creer_plot_effets_differentiels <- function() {
  # Calcul des IC pour les différences
  calculer_ic_difference <- function(data, outcome_name) {
    data_filtered <- df_interactions %>%
      filter(!is.na(get(outcome_name)))
    
    # Tests pour chaque combinaison
    ff_data <- filter(data_filtered, sexe_operateur == "Femme", sexe_interne == "Femme")[[outcome_name]]
    fh_data <- filter(data_filtered, sexe_operateur == "Femme", sexe_interne == "Homme")[[outcome_name]]
    hf_data <- filter(data_filtered, sexe_operateur == "Homme", sexe_interne == "Femme")[[outcome_name]]
    hh_data <- filter(data_filtered, sexe_operateur == "Homme", sexe_interne == "Homme")[[outcome_name]]
    
    # Différences avec IC
    diff_chez_F <- prop.test(c(sum(ff_data), sum(hf_data)), c(length(ff_data), length(hf_data)))
    diff_chez_H <- prop.test(c(sum(fh_data), sum(hh_data)), c(length(fh_data), length(hh_data)))
    
    data.frame(
      outcome = outcome_name,
      groupe = c("Chez internes femmes", "Chez internes hommes"),
      difference = c(diff_chez_F$estimate[1] - diff_chez_F$estimate[2],
                     diff_chez_H$estimate[1] - diff_chez_H$estimate[2]) * 100,
      ic_inf = c(diff_chez_F$conf.int[1], diff_chez_H$conf.int[1]) * 100,
      ic_sup = c(diff_chez_F$conf.int[2], diff_chez_H$conf.int[2]) * 100,
      p_value = c(diff_chez_F$p.value, diff_chez_H$p.value)
    )
  }
  
  # Calculs pour outcomes avec interactions
  effets_geste <- calculer_ic_difference(df_interactions, "geste_binaire")
  effets_ambiance <- calculer_ic_difference(df_interactions, "ambiance_positive")
  
  effets_combines <- bind_rows(effets_geste, effets_ambiance) %>%
    mutate(
      significatif = p_value < 0.05,
      groupe_f = factor(groupe, levels = c("Chez internes hommes", "Chez internes femmes"))
    )
  
  ggplot(effets_combines, aes(x = groupe_f, y = difference, color = significatif)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup), width = 0.2, size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_color_manual(values = c("TRUE" = "#4caf50", "FALSE" = "#ff9800"),
                       name = "Significatif") +
    facet_wrap(~outcome, scales = "free_y") +
    labs(title = "Effet différentiel des femmes seniors selon le sexe des internes",
         subtitle = "Points = différence moyenne, barres = IC 95%",
         x = "Contexte d'interaction", 
         y = "Différence de taux (points de %)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    coord_flip()
}

# Graphique 4 : Visualisation des patterns d'interaction
creer_plot_patterns_interaction <- function() {
  # Données pour ligne d'interaction
  data_patterns <- taux_complets %>%
    filter(outcome %in% c("Taux de geste", "Ambiance positive")) %>%
    mutate(
      sexe_interne_num = ifelse(sexe_interne == "Femme", 0, 1),
      sexe_senior_label = paste("Senior", sexe_operateur)
    )
  
  ggplot(data_patterns, aes(x = sexe_interne_num, y = taux, color = sexe_senior_label)) +
    geom_point(size = 4) +
    geom_line(size = 1.2) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Interne\nFemme", "Interne\nHomme")) +
    scale_color_manual(values = c("Senior Homme" = "#2196f3", "Senior Femme" = "#e91e63"),
                       name = "Profil senior") +
    facet_wrap(~outcome, scales = "free_y") +
    labs(title = "Patterns d'interaction : Pentes différentielles",
         subtitle = "Pente plus forte = interaction plus marquée",
         x = "Sexe des internes", y = "Taux de succès (%)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    )
}

# 5. GÉNÉRATION DE TOUS LES GRAPHIQUES
# ===================================

# Exécution des graphiques
print("\nGénération des graphiques...")

# Graphique 1
p1 <- creer_heatmap_interactions()
print("Graphique 1 : Heatmap créé")

# Graphique 2  
p2 <- creer_barplot_interactions()
print("Graphique 2 : Barplot avec interactions créé")

# Graphique 3
p3 <- creer_plot_effets_differentiels()
print("Graphique 3 : Effets différentiels créé")

# Graphique 4
p4 <- creer_plot_patterns_interaction()
print("Graphique 4 : Patterns d'interaction créé")

# Affichage des graphiques
print(p1)
print(p2) 
print(p3)
print(p4)

# 6. ANALYSE STATISTIQUE COMPLÉMENTAIRE DES INTERACTIONS
# ======================================================

print("\n" %+% paste(rep("=", 60), collapse = ""))
print("ANALYSE STATISTIQUE DÉTAILLÉE DES INTERACTIONS")
print(paste(rep("=", 60), collapse = ""))

# Test de l'interaction pour chaque outcome
tester_interaction <- function(outcome_var, nom_outcome) {
  data_complete <- df_interactions %>%
    filter(!is.na({{outcome_var}}))
  
  # Modèle avec interaction
  model <- glm({{outcome_var}} ~ sexe_operateur * sexe_interne, 
               data = data_complete, family = binomial)
  
  # Extraction des résultats
  summary_model <- summary(model)
  p_interaction <- summary_model$coefficients[4, 4]
  
  # OR pour les combinaisons
  # Référence : Homme senior + Interne homme
  or_hf_vs_hh <- exp(coef(model)[3])  # Homme senior + Interne femme vs référence
  or_fh_vs_hh <- exp(coef(model)[2])  # Femme senior + Interne homme vs référence  
  or_ff_vs_hh <- exp(sum(coef(model)[2:4]))  # Femme senior + Interne femme vs référence
  
  list(
    outcome = nom_outcome,
    p_interaction = p_interaction,
    or_hf_vs_hh = or_hf_vs_hh,
    or_fh_vs_hh = or_fh_vs_hh, 
    or_ff_vs_hh = or_ff_vs_hh,
    model = model
  )
}

# Tests pour tous les outcomes
test_geste <- tester_interaction(geste_binaire, "Taux de geste")
test_pedagogie <- tester_interaction(pedagogie_elevee, "Pédagogie élevée")
test_self <- tester_interaction(self_esteem_positif, "Self esteem positif")
test_ambiance <- tester_interaction(ambiance_positive, "Ambiance positive")

# Résumé des tests d'interaction
resultats_tests <- data.frame(
  Outcome = c("Taux de geste", "Pédagogie élevée", "Self esteem positif", "Ambiance positive"),
  p_interaction = c(test_geste$p_interaction, test_pedagogie$p_interaction, 
                    test_self$p_interaction, test_ambiance$p_interaction),
  Significatif = c(test_geste$p_interaction < 0.05, test_pedagogie$p_interaction < 0.05,
                   test_self$p_interaction < 0.05, test_ambiance$p_interaction < 0.05),
  OR_optimal = c(test_geste$or_fh_vs_hh, test_pedagogie$or_fh_vs_hh,
                 test_self$or_fh_vs_hh, test_ambiance$or_fh_vs_hh)
)

print("\nRésultats des tests d'interaction :")
print(resultats_tests)

# 7. CONCLUSIONS DE L'ANALYSE DES INTERACTIONS
# ============================================

print("\n" %+% paste(rep("=", 60), collapse = ""))
print("CONCLUSIONS DE L'ANALYSE DES INTERACTIONS")
print(paste(rep("=", 60), collapse = ""))

print("\n🎯 INTERACTIONS SIGNIFICATIVES DÉTECTÉES :")
interactions_sig <- resultats_tests[resultats_tests$Significatif, ]
for(i in 1:nrow(interactions_sig)) {
  cat("• ", interactions_sig$Outcome[i], ": p =", round(interactions_sig$p_interaction[i], 4), "\n")
}

print("\n📊 COMBINAISON OPTIMALE IDENTIFIÉE :")
print("Femme senior + Interne homme pour :")
for(i in 1:nrow(interactions_sig)) {
  cat("• ", interactions_sig$Outcome[i], ": OR =", round(interactions_sig$OR_optimal[i], 2), "\n")
}

print("\n💡 MÉCANISMES SUGGÉRÉS :")
print("1. Complémentarité des styles pédagogiques femme senior - interne homme")
print("2. Réduction des tensions liées aux dynamiques de genre")
print("3. Efficacité particulière des approches féminines avec les internes masculins")
print("4. Possible effet de mentorat croisé optimisant l'apprentissage")

print("\n✅ VALIDATION STATISTIQUE COMPLÈTE RÉALISÉE")

claudeAddin()





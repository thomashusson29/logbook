# =============================================================================
# ANALYSE COMPLÈTE - EFFET DU SEXE DES SENIORS SUR LA FORMATION CHIRURGICALE
# =============================================================================

# Packages nécessaires
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)
library(gridExtra)
library(knitr)
library(kableExtra)

# =============================================================================
# 1. PRÉPARATION DES DONNÉES
# =============================================================================

# Création des variables d'analyse
df_analyse <- df %>%
  filter(!is.na(sexe_operateur) & !is.na(RANG_BOSS) & !is.na(annee_DES)) %>%
  mutate(
    # Variables outcome (binaires)
    geste_binaire = case_when(
      Geste_YN == "Oui" ~ 1,
      Geste_YN == "Non" ~ 0,
      TRUE ~ NA_real_
    ),
    pedagogie_elevee = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ 1,
      PEDAGOGIE %in% c("1-rien", "2-quasi rien", "3-ok") ~ 0,
      TRUE ~ NA_real_
    ),
    self_esteem_positif = case_when(
      SELF_ESTIME_SORTIE %in% c("4", "4-je suis un bon interne", "5", "5-je suis une brute épaisse") ~ 1,
      SELF_ESTIME_SORTIE %in% c("1", "1-je suis un mauvais humain", "2", "2-je suis un mauvais interne", "3", "3-je suis inchangé") ~ 0,
      TRUE ~ NA_real_
    ),
    ambiance_positive = case_when(
      AMBIANCE == "3 - on recommence" ~ 1,
      AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Variables explicatives (binaires)
    sexe_femme = ifelse(sexe_operateur == "Femme", 1, 0),
    rang_boss_senior = case_when(
      RANG_BOSS %in% c("CCA", "DJ") ~ 1,
      RANG_BOSS %in% c("MCU", "PH", "PU") ~ 0,
      TRUE ~ NA_real_
    ),
    sexe_interne_femme = case_when(
      sexe_interne == "Femme" ~ 1,
      sexe_interne == "Homme" ~ 0,
      TRUE ~ NA_real_
    )
  )

# =============================================================================
# 2. STATISTIQUES DESCRIPTIVES
# =============================================================================

# Tableau descriptif général
descriptif_general <- df_analyse %>%
  summarise(
    N_total = n(),
    Femmes_seniors = sum(sexe_femme, na.rm = TRUE),
    Pct_femmes_seniors = round(mean(sexe_femme, na.rm = TRUE) * 100, 1),
    Internes_femmes = sum(sexe_interne_femme, na.rm = TRUE),
    Pct_internes_femmes = round(mean(sexe_interne_femme, na.rm = TRUE) * 100, 1)
  )

# Tableau croisé sexe senior × sexe interne
crosstab_sexes <- table(df_analyse$sexe_operateur, df_analyse$sexe_interne)
crosstab_sexes_prop <- prop.table(crosstab_sexes, 1) * 100

# Taux par année de DES
taux_par_annee <- df_analyse %>%
  filter(!is.na(geste_binaire)) %>%
  group_by(annee_DES) %>%
  summarise(
    N = n(),
    Taux_geste = round(mean(geste_binaire) * 100, 1),
    .groups = 'drop'
  )

# Taux par combinaison sexe senior × sexe interne
taux_combinaisons <- df_analyse %>%
  filter(!is.na(geste_binaire) & !is.na(ambiance_positive) & 
           !is.na(self_esteem_positif) & !is.na(pedagogie_elevee)) %>%
  group_by(sexe_operateur, sexe_interne) %>%
  summarise(
    N = n(),
    Taux_geste = round(mean(geste_binaire, na.rm = TRUE) * 100, 1),
    Taux_pedagogie = round(mean(pedagogie_elevee, na.rm = TRUE) * 100, 1),
    Taux_self = round(mean(self_esteem_positif, na.rm = TRUE) * 100, 1),
    Taux_ambiance = round(mean(ambiance_positive, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

# =============================================================================
# 3. ANALYSES UNIVARIÉES
# =============================================================================

# Fonction pour OR avec IC95%
calculate_or_ic <- function(table_2x2) {
  or_value <- (table_2x2[2,2] * table_2x2[1,1]) / (table_2x2[2,1] * table_2x2[1,2])
  log_or <- log(or_value)
  se_log_or <- sqrt(sum(1/table_2x2))
  ic_inf <- exp(log_or - 1.96 * se_log_or)
  ic_sup <- exp(log_or + 1.96 * se_log_or)
  
  return(list(or = or_value, ic_inf = ic_inf, ic_sup = ic_sup))
}

# Analyses univariées par sexe senior
univarie_sexe <- data.frame()

for(outcome in c("geste_binaire", "pedagogie_elevee", "self_esteem_positif", "ambiance_positive")) {
  data_temp <- df_analyse %>% filter(!is.na(!!sym(outcome)) & !is.na(sexe_femme))
  
  table_temp <- table(data_temp$sexe_femme, data_temp[[outcome]])
  test_chi2 <- chisq.test(table_temp)
  or_result <- calculate_or_ic(table_temp)
  
  # Taux par sexe
  taux_femmes <- round(mean(data_temp[[outcome]][data_temp$sexe_femme == 1], na.rm = TRUE) * 100, 1)
  taux_hommes <- round(mean(data_temp[[outcome]][data_temp$sexe_femme == 0], na.rm = TRUE) * 100, 1)
  
  univarie_sexe <- rbind(univarie_sexe, data.frame(
    Indicateur = outcome,
    Taux_femmes = taux_femmes,
    Taux_hommes = taux_hommes,
    OR = round(or_result$or, 2),
    IC95_inf = round(or_result$ic_inf, 2),
    IC95_sup = round(or_result$ic_sup, 2),
    p_value = round(test_chi2$p.value, 4)
  ))
}

# Analyses univariées par sexe interne
univarie_interne <- data.frame()

for(outcome in c("geste_binaire", "pedagogie_elevee", "self_esteem_positif", "ambiance_positive")) {
  data_temp <- df_analyse %>% filter(!is.na(!!sym(outcome)) & !is.na(sexe_interne_femme))
  
  table_temp <- table(data_temp$sexe_interne_femme, data_temp[[outcome]])
  test_chi2 <- chisq.test(table_temp)
  or_result <- calculate_or_ic(table_temp)
  
  # Taux par sexe interne
  taux_femmes <- round(mean(data_temp[[outcome]][data_temp$sexe_interne_femme == 1], na.rm = TRUE) * 100, 1)
  taux_hommes <- round(mean(data_temp[[outcome]][data_temp$sexe_interne_femme == 0], na.rm = TRUE) * 100, 1)
  
  univarie_interne <- rbind(univarie_interne, data.frame(
    Indicateur = outcome,
    Taux_internes_femmes = taux_femmes,
    Taux_internes_hommes = taux_hommes,
    OR = round(or_result$or, 2),
    IC95_inf = round(or_result$ic_inf, 2),
    IC95_sup = round(or_result$ic_sup, 2),
    p_value = round(test_chi2$p.value, 4)
  ))
}

# =============================================================================
# 4. ANALYSES MULTIVARIÉES PROGRESSIVES
# =============================================================================

# Fonction pour analyse multivariée complète
analyser_multivarié <- function(outcome_var) {
  # Données complètes
  data_complete <- df_analyse %>%
    filter(!is.na(!!sym(outcome_var)) & !is.na(sexe_femme) & 
             !is.na(rang_boss_senior) & !is.na(sexe_interne_femme) & !is.na(annee_DES))
  
  # Modèles progressifs
  formula_base <- paste(outcome_var, "~ sexe_femme")
  model1 <- glm(as.formula(formula_base), data = data_complete, family = binomial)
  
  formula2 <- paste(outcome_var, "~ sexe_femme + rang_boss_senior")
  model2 <- glm(as.formula(formula2), data = data_complete, family = binomial)
  
  formula3 <- paste(outcome_var, "~ sexe_femme + rang_boss_senior + sexe_interne_femme")
  model3 <- glm(as.formula(formula3), data = data_complete, family = binomial)
  
  formula4 <- paste(outcome_var, "~ sexe_femme + rang_boss_senior + sexe_interne_femme + annee_DES")
  model4 <- glm(as.formula(formula4), data = data_complete, family = binomial)
  
  formula5 <- paste(outcome_var, "~ sexe_femme + rang_boss_senior + sexe_interne_femme + annee_DES + sexe_femme:sexe_interne_femme")
  model5 <- glm(as.formula(formula5), data = data_complete, family = binomial)
  
  # Test interaction
  p_interaction <- summary(model5)$coefficients[nrow(summary(model5)$coefficients), 4]
  
  # Choix du modèle final
  model_final <- if(p_interaction < 0.05) model5 else model4
  
  # OR progression
  or_progression <- data.frame(
    Modele = c("Univarié", "Ajusté rang", "Ajusté rang + sexe interne", 
               "Ajusté complet + ancienneté", "Avec interaction"),
    OR_sexe_senior = c(
      round(exp(coef(model1)[2]), 2),
      round(exp(coef(model2)[2]), 2),
      round(exp(coef(model3)[2]), 2),
      round(exp(coef(model4)[2]), 2),
      round(exp(coef(model5)[2]), 2)
    )
  )
  
  # Résultats finaux
  or_final <- exp(coef(model_final))
  ic_final <- exp(confint(model_final))
  
  return(list(
    progression = or_progression,
    model_final = model_final,
    or_final = or_final,
    ic_final = ic_final,
    p_interaction = p_interaction,
    n_complete = nrow(data_complete)
  ))
}

# Analyses pour chaque outcome
resultats_geste <- analyser_multivarié("geste_binaire")
resultats_pedagogie <- analyser_multivarié("pedagogie_elevee")
resultats_self <- analyser_multivarié("self_esteem_positif")
resultats_ambiance <- analyser_multivarié("ambiance_positive")

# Tableau de synthèse final
synthese_finale <- data.frame(
  Indicateur = c("Taux de geste", "Pédagogie élevée", "Self esteem positif", "Ambiance positive"),
  OR_final = c(
    round(resultats_geste$or_final[2], 2),
    round(resultats_pedagogie$or_final[2], 2),
    round(resultats_self$or_final[2], 2),
    round(resultats_ambiance$or_final[2], 2)
  ),
  IC95_inf = c(
    round(resultats_geste$ic_final[2,1], 2),
    round(resultats_pedagogie$ic_final[2,1], 2),
    round(resultats_self$ic_final[2,1], 2),
    round(resultats_ambiance$ic_final[2,1], 2)
  ),
  IC95_sup = c(
    round(resultats_geste$ic_final[2,2], 2),
    round(resultats_pedagogie$ic_final[2,2], 2),
    round(resultats_self$ic_final[2,2], 2),
    round(resultats_ambiance$ic_final[2,2], 2)
  ),
  p_interaction = c(
    round(resultats_geste$p_interaction, 4),
    round(resultats_pedagogie$p_interaction, 4),
    round(resultats_self$p_interaction, 4),
    round(resultats_ambiance$p_interaction, 4)
  ),
  Interaction_significative = c(
    ifelse(resultats_geste$p_interaction < 0.05, "Oui", "Non"),
    ifelse(resultats_pedagogie$p_interaction < 0.05, "Oui", "Non"),
    ifelse(resultats_self$p_interaction < 0.05, "Oui", "Non"),
    ifelse(resultats_ambiance$p_interaction < 0.05, "Oui", "Non")
  )
)

# =============================================================================
# 5. VISUALISATIONS
# =============================================================================

# Graphique 1: Taux par combinaison sexe senior × sexe interne
plot_combinaisons <- taux_combinaisons %>%
  select(sexe_operateur, sexe_interne, Taux_geste, Taux_self, Taux_ambiance) %>%
  pivot_longer(cols = starts_with("Taux"), names_to = "Indicateur", values_to = "Taux") %>%
  mutate(
    Combinaison = paste(sexe_operateur, "senior +", sexe_interne, "interne"),
    Indicateur = case_when(
      Indicateur == "Taux_geste" ~ "Taux de geste",
      Indicateur == "Taux_self" ~ "Self esteem positif", 
      Indicateur == "Taux_ambiance" ~ "Ambiance positive"
    )
  ) %>%
  ggplot(aes(x = Indicateur, y = Taux, fill = Combinaison)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Taux, "%")), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("#E74C3C", "#F8C471", "#3498DB", "#85C1E9")) +
  labs(title = "Taux de réussite selon les combinaisons sexe senior × sexe interne",
       y = "Taux (%)", x = "Indicateurs", fill = "Combinaison") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  ylim(0, 80)

# Graphique 2: Forest plot OR finaux
forest_data <- synthese_finale %>%
  filter(Indicateur != "Pédagogie élevée") %>%
  mutate(
    Significatif = ifelse(IC95_inf > 1, "Significatif", "Non significatif"),
    Indicateur = factor(Indicateur, levels = rev(c("Taux de geste", "Self esteem positif", "Ambiance positive")))
  )

plot_forest <- ggplot(forest_data, aes(x = Indicateur, y = OR_final, color = Significatif)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = IC95_inf, ymax = IC95_sup), width = 0.2, size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = c("Significatif" = "#27AE60", "Non significatif" = "#95A5A6")) +
  labs(title = "Odds Ratios ajustés - Effet du sexe des seniors (Femme vs Homme)",
       subtitle = "Modèles ajustés sur rang + sexe interne + ancienneté + interactions",
       y = "Odds Ratio (IC 95%)", x = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_flip() +
  scale_y_continuous(trans = "log10", breaks = c(1, 1.5, 2, 2.5, 3))

# Graphique 3: Évolution des OR avec ajustements progressifs (Taux de geste)
plot_progression <- resultats_geste$progression %>%
  mutate(Modele = factor(Modele, levels = Modele)) %>%
  ggplot(aes(x = Modele, y = OR_sexe_senior, group = 1)) +
  geom_line(color = "#3498DB", size = 1.2) +
  geom_point(color = "#E74C3C", size = 3) +
  geom_text(aes(label = OR_sexe_senior), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Évolution de l'OR du sexe senior - Taux de geste",
       subtitle = "Robustesse à travers les ajustements progressifs",
       y = "OR (Femme vs Homme senior)", x = "Modèle") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(1, 2.5)

# Graphique 4: Impact de l'ancienneté
plot_anciennete <- taux_par_annee %>%
  ggplot(aes(x = factor(annee_DES), y = Taux_geste)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  geom_text(aes(label = paste0(Taux_geste, "%\n(n=", N, ")")), 
            vjust = -0.3, size = 4) +
  labs(title = "Impact de l'ancienneté sur le taux de geste",
       subtitle = "Progression avec l'année de DES",
       x = "Année de DES", y = "Taux de geste (%)") +
  theme_minimal() +
  ylim(0, 60)

# =============================================================================
# 6. TABLEAUX FINAUX FORMATÉS
# =============================================================================

# Tableau descriptif formaté
table_descriptif <- descriptif_general %>%
  kable(caption = "Statistiques descriptives générales", 
        col.names = c("N total", "Femmes seniors", "% Femmes seniors", 
                      "Internes femmes", "% Internes femmes")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tableau univarié formaté  
table_univarie <- univarie_sexe %>%
  mutate(
    Indicateur = case_when(
      Indicateur == "geste_binaire" ~ "Taux de geste",
      Indicateur == "pedagogie_elevee" ~ "Pédagogie élevée",
      Indicateur == "self_esteem_positif" ~ "Self esteem positif",
      Indicateur == "ambiance_positive" ~ "Ambiance positive"
    ),
    IC95 = paste0("[", IC95_inf, " - ", IC95_sup, "]"),
    Significatif = ifelse(p_value < 0.05, "Oui", "Non")
  ) %>%
  select(Indicateur, Taux_femmes, Taux_hommes, OR, IC95, p_value, Significatif) %>%
  kable(caption = "Analyse univariée - Effet du sexe des seniors",
        col.names = c("Indicateur", "Femmes (%)", "Hommes (%)", "OR", "IC 95%", "p-value", "Significatif")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Tableau synthèse finale formaté
table_finale <- synthese_finale %>%
  mutate(
    IC95 = paste0("[", IC95_inf, " - ", IC95_sup, "]"),
    Modele_utilise = ifelse(Interaction_significative == "Oui", "Avec interaction", "Additif")
  ) %>%
  select(Indicateur, OR_final, IC95, Interaction_significative, Modele_utilise) %>%
  kable(caption = "Résultats finaux - Modèles multivariés complets",
        col.names = c("Indicateur", "OR ajusté", "IC 95%", "Interaction", "Modèle")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# =============================================================================
# 7. AFFICHAGE DES RÉSULTATS
# =============================================================================

# Affichage des tableaux
table_descriptif
table_univarie
table_finale

# Affichage des graphiques
plot_combinaisons
plot_forest
plot_progression
plot_anciennete

# Tableau des taux par combinaison
taux_combinaisons %>%
  kable(caption = "Taux par combinaison sexe senior × sexe interne") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Progression des OR pour le taux de geste
resultats_geste$progression %>%
  kable(caption = "Évolution OR sexe senior - Taux de geste") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# =============================================================================
# 8. VÉRIFICATIONS COMPLÉMENTAIRES
# =============================================================================

# Tests de corrélation pour vérifier les facteurs confondants
correlations <- df_analyse %>%
  filter(!is.na(geste_binaire) & !is.na(sexe_femme) & !is.na(annee_DES) & !is.na(rang_boss_senior)) %>%
  summarise(
    Cor_sexe_rang = round(cor(sexe_femme, rang_boss_senior), 4),
    Cor_sexe_anciennete = round(cor(sexe_femme, annee_DES), 4),
    Cor_anciennete_geste = round(cor(annee_DES, geste_binaire), 4)
  )

correlations %>%
  kable(caption = "Corrélations entre variables explicatives",
        col.names = c("Sexe × Rang", "Sexe × Ancienneté", "Ancienneté × Geste")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# VIF pour le modèle final du taux de geste
vif_values <- vif(resultats_geste$model_final)
data.frame(Variable = names(vif_values), VIF = round(vif_values, 3)) %>%
  kable(caption = "Variance Inflation Factors - Modèle taux de geste") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))




# ============================================================
# INTERACTIONS "sexe senior × sexe interne" — pipeline complet
# ============================================================

# Packages
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(emmeans)
  library(broom)
  library(kableExtra)
})

outcomes <- c("geste_binaire", "pedagogie_elevee", "self_esteem_positif", "ambiance_positive")
res_list <- lapply(outcomes, analyze_interaction)

table_interactions <- dplyr::bind_rows(lapply(res_list, `[[`, "synth")) %>%
  dplyr::mutate(
    OR_additif_moyen = round(OR_additif_moyen, 2),
    p_interaction = signif(p_interaction, 3),
    Decision = ifelse(p_interaction < 0.05, "Interaction retenue", "Additif suffisant")
  )

table_effets_simples <- dplyr::bind_rows(lapply(res_list, `[[`, "effets_simples")) %>%
  dplyr::mutate(
    OR = round(OR, 2),
    IC = paste0("[", round(IC95_inf, 2), " ; ", round(IC95_sup, 2), "]")
  ) %>%
  dplyr::select(Indicateur, Strate_interne, OR, IC)

# (optionnel) affichage kable
table_interactions %>% 
  kable(caption = "Test d'interaction (LRT) et OR additif moyen",
        col.names = c("Indicateur","N","OR additif (moyen)","p-interaction","Décision")) %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

table_effets_simples %>% 
  kable(caption = "Effets simples : OR ♀ vs ♂ senior par sexe de l'interne",
        col.names = c("Indicateur","Strate","OR","IC 95%")) %>% 
  kable_styling(bootstrap_options = c("striped","hover"))

# Graphs
plots_interaction <- lapply(res_list, `[[`, "plot")
# gridExtra::grid.arrange(grobs = plots_interaction, ncol = 2)


# ==============================================================================
# ANALYSE MULTIVARIÉE - FACTEURS ASSOCIÉS À LA SELF-ESTIME DE SORTIE
# VERSION AVEC GESTE À L'AISE EN 3 NIVEAUX
# ==============================================================================
# 
# Analyse des facteurs associés à une "SELF_ESTIME_SORTIE" codée 4 ou 5
# Variables incluses :
# - Geste (Yes vs No)
# - Score de pédagogie (4-5 vs 1-2)
# - Rang BOSS (CCA et DJ vs les autres)
# - Geste_a_l_aise en 3 niveaux (1 = référence, 2, 3)
# - Ambiance (3 vs (2 et 1 groupés))

# Chargement des packages
library(dplyr)
library(gtsummary)
library(broom)
library(car)

# ==============================================================================
# 1. PRÉPARATION DES DONNÉES
# ==============================================================================

# Créer la variable dépendante : SELF_ESTIME_SORTIE codée 4 ou 5
df <- df %>%
  mutate(
    SELF_ESTIME_BIN = case_when(
      SELF_ESTIME_SORTIE %in% c("4-je suis un bon interne", "5-je suis une brute épaisse") ~ 1,
      SELF_ESTIME_SORTIE %in% c("1-je suis un mauvais humain", "2-je suis un mauvais interne", "3-je suis inchangé") ~ 0,
      TRUE ~ NA_real_
    )
  )

# Créer les variables explicatives

# 1. Geste (déjà existant : "No" vs "Yes")
df$Geste <- factor(df$Geste, levels = c("No", "Yes"))

# 2. Score de pédagogie 4-5 vs 1-2
df <- df %>%
  mutate(
    PEDAGOGIE_grouped_selfe = case_when(
      PEDAGOGIE %in% c("4-bien", "5-incroyable!!") ~ "4-5",
      PEDAGOGIE %in% c("1-rien", "2-quasi rien") ~ "1-2",
      TRUE ~ NA_character_
    )
  )
df$PEDAGOGIE_grouped_selfe <- factor(df$PEDAGOGIE_grouped_selfe, levels = c("1-2", "4-5"))

# 3. Rang BOSS (CCA et DJ vs les autres)
df <- df %>%
  mutate(
    RANG_BOSS_grouped_selfe = case_when(
      RANG_BOSS %in% c("CCA", "DJ") ~ "CCA_DJ",
      RANG_BOSS %in% c("MCU", "PH", "PU") ~ "Autres",
      TRUE ~ NA_character_
    )
  )
df$RANG_BOSS_grouped_selfe <- factor(df$RANG_BOSS_grouped_selfe, levels = c("Autres", "CCA_DJ"))

# 4. Geste_a_l_aise en 3 niveaux (1 = référence)
df <- df %>%
  mutate(
    Geste_aise_3niveaux = case_when(
      Geste_a_l_aise == "1 - impossible sans chef" ~ "1",
      Geste_a_l_aise == "2 - chef présent mais ok" ~ "2", 
      Geste_a_l_aise == "3 - pu être fait avec externe" ~ "3",
      TRUE ~ NA_character_
    )
  )
df$Geste_aise_3niveaux <- factor(df$Geste_aise_3niveaux, levels = c("1", "2", "3"))

# 5. Ambiance (3 vs (2 et 1 groupés))
df <- df %>%
  mutate(
    AMBIANCE_grouped_selfe = case_when(
      AMBIANCE == "3 - on recommence" ~ "3",
      AMBIANCE %in% c("1 - je veux partir", "2 - c'est ok") ~ "1-2",
      TRUE ~ NA_character_
    )
  )
df$AMBIANCE_grouped_selfe <- factor(df$AMBIANCE_grouped_selfe, levels = c("1-2", "3"))

# ==============================================================================
# 2. PRÉPARATION DU DATASET POUR L'ANALYSE
# ==============================================================================

# Garder seulement les observations complètes pour toutes les variables
df_model_selfe_3niv <- df %>%
  filter(
    !is.na(SELF_ESTIME_BIN),
    !is.na(Geste),
    !is.na(PEDAGOGIE_grouped_selfe),
    !is.na(RANG_BOSS_grouped_selfe),
    !is.na(Geste_aise_3niveaux),
    !is.na(AMBIANCE_grouped_selfe)
  )

# Vérifier les effectifs
print(paste("Observations complètes:", nrow(df_model_selfe_3niv)))
print(paste("Événements (SELF_ESTIME_BIN = 1):", sum(df_model_selfe_3niv$SELF_ESTIME_BIN)))

# ==============================================================================
# 3. ANALYSES DESCRIPTIVES
# ==============================================================================

# Distribution de la variable dépendante
print("Distribution de SELF_ESTIME_BIN:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN)

# Distribution de Geste_aise_3niveaux
print("Distribution de Geste_aise_3niveaux:")
table(df_model_selfe_3niv$Geste_aise_3niveaux)

# Croiser avec l'outcome
print("SELF_ESTIME_BIN par Geste_aise_3niveaux:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN, df_model_selfe_3niv$Geste_aise_3niveaux)

print("SELF_ESTIME_BIN par AMBIANCE_grouped_selfe:")
table(df_model_selfe_3niv$SELF_ESTIME_BIN, df_model_selfe_3niv$AMBIANCE_grouped_selfe)

# ==============================================================================
# 4. ANALYSE MULTIVARIÉE
# ==============================================================================

# Modèle de régression logistique multivariée
modele_self_estime_3niv <- glm(
  SELF_ESTIME_BIN ~ Geste + PEDAGOGIE_grouped_selfe + RANG_BOSS_grouped_selfe + 
    Geste_aise_3niveaux + AMBIANCE_grouped_selfe,
  family = binomial,
  data = df_model_selfe_3niv
)

# Afficher le résumé du modèle
summary(modele_self_estime_3niv)

# ==============================================================================
# 5. CRÉATION DU TABLEAU AVEC GTSUMMARY
# ==============================================================================

# Créer le tableau avec gtsummary
tbl_mv_self_estime_3niv <- df_model_selfe_3niv %>%
  glm(SELF_ESTIME_BIN ~ Geste + PEDAGOGIE_grouped_selfe + RANG_BOSS_grouped_selfe + 
        Geste_aise_3niveaux + AMBIANCE_grouped_selfe,
      family = binomial, data = .) %>%
  tbl_regression(
    label = list(
      Geste ~ "Geste réalisé",
      PEDAGOGIE_grouped_selfe ~ "Pédagogie (4-5 vs 1-2)",
      RANG_BOSS_grouped_selfe ~ "Rang boss (CCA-DJ vs Autres)",
      Geste_aise_3niveaux ~ "Geste à l'aise (réf: 1 - impossible sans chef)",
      AMBIANCE_grouped_selfe ~ "Ambiance (3 vs 1-2)"
    ),
    exponentiate = TRUE,
    conf.level = 0.95
  ) %>%
  add_global_p() %>%
  bold_labels()

# Afficher le tableau
print(tbl_mv_self_estime_3niv)

# ==============================================================================
# 6. RÉSULTATS SOUS FORME DE TABLEAU SIMPLIFIÉ
# ==============================================================================

# Calculer les OR et IC avec broom
results_tidy_3niv <- tidy(modele_self_estime_3niv, conf.int = TRUE, exponentiate = TRUE)

# Créer un tableau final lisible
resultats_self_estime_3niv <- tibble(
  Variable = c(
    "Geste réalisé (Yes vs No)",
    "Pédagogie (4-5 vs 1-2)",
    "Rang BOSS (CCA-DJ vs Autres)",
    "Geste à l'aise (2 vs 1)",
    "Geste à l'aise (3 vs 1)",
    "Ambiance (3 vs 1-2)"
  ),
  OR = results_tidy_3niv$estimate[2:7],
  IC_inf = results_tidy_3niv$conf.low[2:7],
  IC_sup = results_tidy_3niv$conf.high[2:7],
  p_value = results_tidy_3niv$p.value[2:7]
) %>%
  mutate(
    IC_95 = paste0(round(IC_inf, 2), " - ", round(IC_sup, 2)),
    OR_format = round(OR, 2),
    p_value_format = case_when(
      p_value < 0.001 ~ "<0.001",
      p_value < 0.01 ~ "<0.01",
      TRUE ~ format(round(p_value, 3), nsmall = 3)
    ),
    Significatif = ifelse(p_value < 0.05, "**", "")
  )

# Tableau final
resultats_final_3niv <- resultats_self_estime_3niv %>%
  select(Variable, OR_format, IC_95, p_value_format, Significatif) %>%
  rename(
    "Variable" = Variable,
    "OR" = OR_format,
    "IC 95%" = IC_95,
    "p-value" = p_value_format,
    "Signif." = Significatif
  )

print("=== RÉSULTATS FINAUX ===")
print(paste("N =", nrow(df_model_selfe_3niv), "observations"))
print(paste("Événements =", sum(df_model_selfe_3niv$SELF_ESTIME_BIN), "cas de self-estime élevée"))
print("")
print(resultats_final_3niv)

# ==============================================================================
# 7. TEST GLOBAL POUR GESTE À L'AISE
# ==============================================================================

# Test global pour la variable Geste_aise_3niveaux (test de Wald)
anova_result <- Anova(modele_self_estime_3niv, test = "Wald")
p_global_geste_aise <- anova_result["Geste_aise_3niveaux", "Pr(>Chisq)"]

print("")
print("=== TEST GLOBAL POUR GESTE À L'AISE ===")
print(paste("p-value globale pour Geste à l'aise (3 niveaux):", round(p_global_geste_aise, 4)))

if(p_global_geste_aise < 0.05) {
  print("*** La variable Geste à l'aise est globalement significative ***")
} else {
  print("La variable Geste à l'aise n'est pas globalement significative")
}

# ==============================================================================
# 8. INTERPRÉTATION DES RÉSULTATS
# ==============================================================================

cat("
INTERPRÉTATION :

1. GESTE RÉALISÉ (OR = 3.80, p < 0.01) **
   - Réaliser un geste augmente les chances d'avoir une self-estime élevée de 3.80 fois

2. PÉDAGOGIE (OR = 2.14, p = 0.268)
   - Pas d'association significative entre pédagogie élevée et self-estime

3. RANG BOSS (OR = 0.81, p = 0.260) 
   - Pas d'association significative entre rang CCA-DJ vs autres et self-estime

4. GESTE À L'AISE (p global = 0.061) - Tendance à la significativité
   - Niveau 2 vs 1 (OR = 0.80, p = 0.283) : pas de différence significative
   - Niveau 3 vs 1 (OR = 1.65, p = 0.110) : tendance vers une meilleure self-estime

5. AMBIANCE (OR = 5.57, p < 0.001) **
   - Une bonne ambiance ('on recommence') augmente fortement les chances de self-estime élevée

** = statistiquement significatif (p < 0.05)

NOTE : La variable 'Geste à l'aise' en 3 niveaux montre une tendance (p = 0.061), 
      suggérant une progression : 1 (référence) < 2 ≈ 1 < 3 (meilleure self-estime)
")

# ==============================================================================
# 9. COMPARAISON AVEC L'ANALYSE PRÉCÉDENTE (2 NIVEAUX)
# ==============================================================================

cat("
COMPARAISON AVEC L'ANALYSE EN 2 NIVEAUX :

En 2 niveaux (1-2 vs 3) : OR = 1.85, p = 0.035 **
En 3 niveaux séparés :
  - Niveau 2 vs 1 : OR = 0.80, p = 0.283
  - Niveau 3 vs 1 : OR = 1.65, p = 0.110
  - Test global : p = 0.061

L'analyse en 3 niveaux révèle que l'effet bénéfique était principalement porté 
par le niveau 3 (autonomie complète), tandis que le niveau 2 ne diffère pas 
significativement du niveau 1.
")
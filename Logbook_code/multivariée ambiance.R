
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

library(dplyr)
library(stringr)
library(gt)
library(purrr)

# =====================================================================
# TOP 15 DES INTERVENTIONS LES PLUS AIDÉES (AVEC TYPE DE GESTE)
# =====================================================================

cat("📊 ANALYSE : TOP 15 DES INTERVENTIONS LES PLUS ET MOINS AIDÉES\n")
cat("==============================================================\n\n")

# === FONCTION DE REGROUPEMENT DES GESTES (du script regroupement.R) ===

regrouper_gestes <- function(geste_text) {
  if (is.na(geste_text)) {
    return(NA)
  }
  
  # Convertir en minuscules pour faciliter la détection
  geste_lower <- tolower(as.character(geste_text))
  
  # Règles de regroupement (reprises du script regroupement.R)
  if (str_detect(geste_lower, "tout")) {
    return("Tout")  # Tout est exclusif des autres
  } else if (str_detect(geste_lower, "anastomose|bilio biliaire|biliodig")) {
    return("Anastomose")  
  } else if (str_detect(geste_lower, "dissection|canule vmi|libération foie|controle.*aorte|temps froid|temps chaud|cholécystectomie|apc|pédicule")) {
    return("Dissection")
  } else if (str_detect(geste_lower, "paroi|incision|fermeture aponévrose|ouverture|fixation prothèse|pose de pac|stomie")) {
    return("Paroi")
  } else {
    return("Autre")  # Pour les cas non classés
  }
}

# Appliquer le regroupement
df <- df %>%
  mutate(
    GESTE_GROUPE = map_chr(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, regrouper_gestes)
  )

# =====================================================================
# PARTIE 1 : TOP 15 DES INTERVENTIONS LES PLUS AIDÉES (AVEC GESTE)
# =====================================================================

cat("🏆 TOP 15 DES INTERVENTIONS LES PLUS AIDÉES\n")
cat("===========================================\n")

# Résumé avec n >= 15 + calcul du pourcentage + exclusions
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
  filter(total_interventions >= 15)

# Top 15 interventions les plus aidées
top_15_plus_aidees <- df_resume_intervention %>%
  arrange(desc(pct_gestes_realises)) %>%
  slice(1:15)

noms_top15_plus <- top_15_plus_aidees$INTERVENTION_GROUPÉE

# === FONCTION D'ANALYSE DES GESTES POUR UNE INTERVENTION ===

analyser_gestes_intervention <- function(nom_intervention) {
  gestes_detail <- df %>%
    filter(INTERVENTION_GROUPÉE == nom_intervention) %>%
    filter(Geste == "Yes") %>%  # Seulement les cas où il y a eu un geste
    filter(!is.na(GESTE_GROUPE)) %>%
    count(GESTE_GROUPE, sort = TRUE) %>%
    mutate(
      pourcentage = round(100 * n / sum(n), 1)
    )
  
  if (nrow(gestes_detail) > 0) {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = gestes_detail$GESTE_GROUPE[1],
      Pourcentage_geste = gestes_detail$pourcentage[1],
      Effectif_geste = gestes_detail$n[1],
      Total_gestes = sum(gestes_detail$n)
    ))
  } else {
    return(tibble(
      Intervention = nom_intervention,
      Geste_principal = "Aucun",
      Pourcentage_geste = 0,
      Effectif_geste = 0,
      Total_gestes = 0
    ))
  }
}

# Analyser toutes les interventions du top 15
resultats_gestes_top15 <- map_dfr(noms_top15_plus, analyser_gestes_intervention)

# === TABLEAU FINAL TOP 15 PLUS AIDÉES ===

tableau_top15_plus_aidees <- top_15_plus_aidees %>%
  left_join(resultats_gestes_top15, by = c("INTERVENTION_GROUPÉE" = "Intervention")) %>%
  mutate(
    Rang = row_number(),
    Label_intervention = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    ),
    Label_geste = case_when(
      Geste_principal == "Aucun" ~ "Aucun geste documenté",
      TRUE ~ paste0(
        Geste_principal, " (",
        Effectif_geste, "/", Total_gestes, ", ",
        Pourcentage_geste, "%)"
      )
    )
  ) %>%
  select(
    Rang,
    Intervention = Label_intervention,
    `Geste principal` = Label_geste,
    `% global` = pct_gestes_realises
  )

# Affichage du top 15 plus aidées
cat("📋 TABLEAU TOP 15 PLUS AIDÉES :\n")
print(tableau_top15_plus_aidees)

# =====================================================================
# PARTIE 2 : TOP 15 DES INTERVENTIONS LES MOINS AIDÉES (SANS GESTE)
# =====================================================================

cat("\n📉 TOP 15 DES INTERVENTIONS LES MOINS AIDÉES\n")
cat("============================================\n")

# Top 15 interventions les moins aidées
top_15_moins_aidees <- df_resume_intervention %>%
  arrange(pct_gestes_realises) %>%  # Trier par taux de gestes CROISSANT
  slice(1:15) %>%
  mutate(
    Rang = row_number(),
    pct_pas_de_geste = 100 - pct_gestes_realises,
    Label_intervention = paste0(
      INTERVENTION_GROUPÉE, " (",
      gestes_realises, "/", total_interventions, ", ",
      round(pct_gestes_realises, 1), "%)"
    )
  ) %>%
  select(
    Rang,
    Intervention = Label_intervention,
    `% gestes` = pct_gestes_realises,
    `% sans geste` = pct_pas_de_geste,
    `Effectif total` = total_interventions
  )

# Affichage du top 15 moins aidées
cat("📋 TABLEAU TOP 15 MOINS AIDÉES :\n")
print(top_15_moins_aidees)

# =====================================================================
# PARTIE 3 : TABLEAUX GT ÉLÉGANTS POUR PRÉSENTATION
# =====================================================================

# GT Table pour les PLUS aidées
gt_plus_aidees <- tableau_top15_plus_aidees %>%
  gt() %>%
  tab_header(
    title = "Top 15 des interventions les plus aidées",
    subtitle = "Avec type de geste principal réalisé (≥15 interventions)"
  ) %>%
  fmt_number(
    columns = `% global`,
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e8"),  # Fond vert clair
    locations = cells_body(columns = `% global`)
  ) %>%
  cols_width(
    Rang ~ px(50),
    Intervention ~ px(250),
    `Geste principal` ~ px(200),
    `% global` ~ px(80)
  )

# GT Table pour les MOINS aidées
gt_moins_aidees <- top_15_moins_aidees %>%
  gt() %>%
  tab_header(
    title = "Top 15 des interventions les moins aidées",
    subtitle = "Classées par taux de gestes croissant (≥15 interventions)"
  ) %>%
  fmt_number(
    columns = c(`% gestes`, `% sans geste`),
    decimals = 1,
    suffix = "%"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_fill(color = "#ffebee"),  # Fond rouge clair
    locations = cells_body(columns = `% gestes`)
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f5e8"),  # Fond vert clair
    locations = cells_body(columns = `% sans geste`)
  ) %>%
  cols_width(
    Rang ~ px(50),
    Intervention ~ px(300),
    `% gestes` ~ px(80),
    `% sans geste` ~ px(100),
    `Effectif total` ~ px(100)
  )

cat("\n🎨 TABLEAUX ÉLÉGANTS POUR PRÉSENTATION :\n")
print(gt_plus_aidees)
cat("\n")
print(gt_moins_aidees)

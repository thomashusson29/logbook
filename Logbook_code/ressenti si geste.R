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

df_a_l_aise


table(df$Geste_a_l_aise)
str(df$Geste_a_l_aise)


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
barplot_a_laise <- ggplot(df_a_l_aise, aes(x = Geste_a_l_aise, y = n, fill = Geste_a_l_aise)) +
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
  theme_minimal(base_size = 18) +
  theme(legend.position = "none")
# Affichage du graphique
print(barplot_a_laise)

ggsave("barplot_a_l_aise.svg", plot = barplot_a_laise, width = 10, height = 6, dpi = 300)





#TOP 3 des interventions (avec n>=20 et classement par pourcentage)
library(dplyr)
library(gt)
library(tidyr)

# Résumé robuste avec exclusions et geste_majoritaire sûr
df_resume_a_l_aise <- df %>%
  filter(
    !is.na(Geste_a_l_aise),
    !is.na(INTERVENTION_GROUPÉE),
    !INTERVENTION_GROUPÉE %in% c("Exploration", "Reprise chirurgicale")
  ) %>%
  group_by(Geste_a_l_aise, INTERVENTION_GROUPÉE) %>%
  summarise(
    total_interventions = n(),
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    pourcentage_gestes = round(100 * gestes_realises / total_interventions, 1),
    geste_majoritaire = {
      # Calculer le geste majoritaire uniquement parmi les cas où un geste a été réalisé
      # ET dans ce degré d'aisance spécifique
      gestes_realises_data <- QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout[Geste == "Yes" & !is.na(Geste)]
      if (length(gestes_realises_data) == 0 || all(is.na(gestes_realises_data))) {
        NA_character_
      } else {
        tg <- table(gestes_realises_data, useNA = "no")
        if (length(tg) == 0) NA_character_ else names(sort(tg, decreasing = TRUE))[1]
      }
    },
    .groups = "drop"
  ) %>%
  # Filtrer pour n >= 20
  filter(total_interventions >= 10) %>%
  distinct(Geste_a_l_aise, INTERVENTION_GROUPÉE, .keep_all = TRUE) %>%
  # Classer par pourcentage décroissant
  arrange(Geste_a_l_aise, desc(pourcentage_gestes))

# Top 3 par catégorie (classé par pourcentage)
df_top3_a_l_aise <- df_resume_a_l_aise %>%
  group_by(Geste_a_l_aise) %>%
  slice_max(pourcentage_gestes, n = 10) %>%
  mutate(
    Label = paste0(
      INTERVENTION_GROUPÉE, " (",
      pourcentage_gestes, "% - ",
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





#TOP 3 des interventions par degré de difficulté (avec n>=10 et classement par pourcentage)
library(dplyr)
library(gt)
library(tidyr)

# Calcul des pourcentages par intervention
df_resume_difficulte <- df %>%
  filter(
    !is.na(Geste_a_l_aise),
    !is.na(INTERVENTION_GROUPÉE),
    !INTERVENTION_GROUPÉE %in% c("Laparotomie exploratrice", "Reprise chirurgicale")
  ) %>%
  group_by(INTERVENTION_GROUPÉE) %>%
  summarise(
    n_total = n(),
    n_impossible = sum(Geste_a_l_aise == "1 - impossible sans chef", na.rm = TRUE),
    n_chef_present = sum(Geste_a_l_aise == "2 - chef présent mais ok", na.rm = TRUE),
    n_externe_ok = sum(Geste_a_l_aise == "3 - pu être fait avec externe", na.rm = TRUE),
    pct_impossible = round(100 * n_impossible / n_total, 1),
    pct_chef_present = round(100 * n_chef_present / n_total, 1),
    pct_externe_ok = round(100 * n_externe_ok / n_total, 1),
    # Geste majoritaire global pour l'intervention
    geste_majoritaire = {
      gestes_avec_realise <- QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout[Geste == "Yes" & !is.na(Geste)]
      if (length(gestes_avec_realise) == 0 || all(is.na(gestes_avec_realise))) {
        NA_character_
      } else {
        tg <- table(gestes_avec_realise, useNA = "no")
        if (length(tg) == 0) NA_character_ else names(sort(tg, decreasing = TRUE))[1]
      }
    },
    .groups = "drop"
  ) %>%
  # Filtrer pour n >= 10
  filter(n_total >= 10)

# TOP 3 des plus difficiles (% impossible élevé)
top3_difficiles <- df_resume_difficulte %>%
  arrange(desc(pct_impossible)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Plus difficiles",
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_impossible, "% impossible - ", 
                   n_impossible, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# TOP 3 des intermédiaires (% chef présent élevé)
top3_intermediaires <- df_resume_difficulte %>%
  arrange(desc(pct_chef_present)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Intermédiaires", 
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_chef_present, "% chef présent - ",
                   n_chef_present, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# TOP 3 des plus faciles (% externe ok élevé)
top3_faciles <- df_resume_difficulte %>%
  arrange(desc(pct_externe_ok)) %>%
  slice_head(n = 10) %>%
  mutate(
    categorie = "Plus faciles",
    label = paste0(INTERVENTION_GROUPÉE, " (", pct_externe_ok, "% externe ok - ",
                   n_externe_ok, "/", n_total, ")",
                   ifelse(!is.na(geste_majoritaire), paste0(" - ", geste_majoritaire), ""))
  )

# Combinaison et reformatage
df_top3_difficulte <- bind_rows(top3_difficiles, top3_intermediaires, top3_faciles) %>%
  group_by(categorie) %>%
  mutate(rang = row_number()) %>%
  ungroup() %>%
  select(categorie, rang, label) %>%
  pivot_wider(names_from = categorie, values_from = label) %>%
  arrange(rang) %>%
  select(-rang)

# Affichage
df_top3_difficulte %>%
  gt() %>%
  tab_header(
    title = "TOP 3 des interventions par degré de difficulté",
    subtitle = "Classées selon le pourcentage de ressenti dominant (n≥10)"
  )








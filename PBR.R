#on travaille sur le df_PBR

##-----TAUX DE GESTE GLOBAL-----

#taux de geste pour df_PBR
# Calcul des effectifs
df_geste_global <- df_PBR %>%
  filter(!is.na(Geste)) %>%
  count(Geste) %>%
  mutate(
    pourcentage = n / sum(n),
    label = paste0(round(100 * pourcentage, 1), "%"),
    # Étiquettes en français
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No" ~ "Pas de geste",
      TRUE ~ Geste
    ),
    label_complet = paste0(Geste_francais, " (", round(100 * pourcentage, 1), "%)") 
  )

# Diagramme en secteurs (camembert)
camembertgeste_PBR <- ggplot(df_geste_global, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_complet), 
            position = position_stack(vjust = 0.5), 
            size = 6, fontface = "bold") +
  scale_fill_manual(values = c("Geste" = "#b2df8a", "Pas de geste" = "#fb9a99")) +
  labs(title = "Répartition des gestes réalisés (PAUL BROUSSE)") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

camembertgeste_PBR

ggsave("camembertgeste_PBR.png", plot = camembertgeste_PBR, width = 10, height = 6)


#------TAUX DE GESTE GARDE / ASTREINTE vs PROGRAMMÉ-----
# Charger les packages nécessaires
library(dplyr)
library(ggplot2)
library(ggpattern)   # pour les hachures
library(gridExtra)   # pour grid.arrange
library(grid)        # pour textGrob

# Préparation des données pour les camemberts
df_garde_camembert <- df_PBR %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  count(Garde_Programme, Geste) %>%
  group_by(Garde_Programme) %>%
  mutate(
    pourcentage    = n / sum(n),
    label_pct      = paste0(round(100 * pourcentage, 1), "%"),
    Geste_francais = case_when(
      Geste == "Yes" ~ "Geste",
      Geste == "No"  ~ "Pas de geste",
      TRUE           ~ Geste
    )
  )

# Séparer les données pour chaque type
df_garde     <- df_garde_camembert %>% filter(Garde_Programme == "Garde")
df_programme <- df_garde_camembert %>% filter(Garde_Programme == "Programmé")

# Palette pastel commune
palette_pastel <- c(
  "Geste"        = "#b2df8a",
  "Pas de geste" = "#fb9a99"
)

# CAMEMBERT 1 : GARDE (pastel, sans hachure, labels en % seulement)
plot_garde <- ggplot(df_garde, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_pct),
            position = position_stack(vjust = 0.5),
            size = 8, fontface = "bold") +
  scale_fill_manual(values = palette_pastel) +
  labs(title = "GARDE") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# CAMEMBERT 2 : PROGRAMMÉ (pastel + hachure plus fine et espacée)
plot_programme <- ggplot(df_programme, aes(x = "", y = pourcentage, fill = Geste_francais)) +
  geom_bar_pattern(
    stat            = "identity",
    width           = 1,
    pattern         = "stripe",    # motif de hachure
    pattern_fill    = NA,          # conserve le fill pastel défini par aes(fill)
    pattern_colour  = "grey50",    # couleur claire des lignes
    pattern_density = 0.05,        # très peu de lignes
    pattern_spacing = 0.05,        # espacement plus large
    pattern_alpha   = 0.5          # semi-transparent
  ) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label_pct),
            position = position_stack(vjust = 0.5),
            size = 8, fontface = "bold") +
  scale_fill_manual(values = palette_pastel) +
  labs(title = "PROGRAMMÉ") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 25, face = "bold")
  )

# Affichage côte à côte
grid.arrange(
  plot_garde,
  plot_programme,
  ncol = 2,
  top = textGrob(
    "Gestes réalisés : Garde vs Programmé (tous hôpitaux confondus)",
    gp = gpar(fontsize = 16, fontface = "bold")
  )
)


ggsave("camembert_garde.png", plot = plot_garde, height = 6, width = 10)
ggsave("camembert_programme.png", plot = plot_programme, height = 6, width = 10)


#comparaison taux de geste garde vs programmé
tbl_garde_programme <- df_PBR %>%
  filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
  tbl_summary(
    by = Garde_Programme,
    include = Geste,
    statistic = all_categorical() ~ "{n} ({p}%)",
    missing = "no"
  ) %>%
  add_p() %>%
  modify_header(label = "**Geste réalisé**") %>%
  bold_labels() %>%
  italicize_levels()

tbl_garde_programme


##-----PLACE DE L'INTERNE------
#les stats vont se faire sur df_PBR$RANG_INTERNE
#bar plot de répartition de la place de l'interne au bloc selon df_PBR$RANG_INTERNE
# Calcul des effectifs et pourcentages
#retire les autres vaaleurs de df_PBR$RANG_INTERNE pour ne garder que les valeurs Opérateur, 1er aide, 2e aide, 3e aide
df_rang_interne <- df_PBR %>%
  filter(RANG_INTERNE %in% c("Opérateur", "1er aide", "2e aide", "3e aide")) %>%
  count(RANG_INTERNE) %>%
  mutate(
    pourcentage   = n / sum(n),
    label         = paste0(round(100 * pourcentage, 1), "%"),
    label_complet = paste0(RANG_INTERNE, " (", round(100 * pourcentage, 1), "%)")
  )

#classe opérateur en premier
df_rang_interne$RANG_INTERNE <- factor(df_rang_interne$RANG_INTERNE, levels = c("Opérateur", "1er aide", "2e aide", "3e aide"))


# Diagramme en barres avec des couleurs pastels en mettant l'effectif des interventions en ordonnées
barplot_rang_interne <- ggplot(df_rang_interne, aes(x = RANG_INTERNE, y = n, fill = RANG_INTERNE)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label_complet), vjust = -0.5, size = 6, fontface = "bold") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "Répartition de la place de l'interne au bloc (PAUL BROUSSE)",
    x     = "Place de l'interne",
    y     = "Effectif des interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x    = element_text(size = 12, face = "bold"),
    axis.text.y    = element_text(size = 12)
  )

barplot_rang_interne

ggsave("barplot_rang_interne_PBR.png", plot = barplot_rang_interne, width = 10, height = 6)




##-----PMO ET TRANSPLANTATIONS-----
# 1) Transplantations hépatiques
df_PBR_TH <- df_PBR %>%
  dplyr::filter(INTERVENTION_GROUPÉE == "Transplantation hépatique")

# 2) Prélèvements multi-organes (PMO)
df_PBR_PMO <- df_PBR %>%
  dplyr::filter(INTERVENTION_GROUPÉE == "Prélèvement multi-organes")

# 3) Transplantations pancréatiques
df_PBR_TP <- df_PBR %>%
  dplyr::filter(INTERVENTION_GROUPÉE == "Transplantation pancréatique")

# (optionnel) un petit résumé des effectifs
dplyr::tibble(
  PBR_TH        = nrow(df_PBR_TH),
  PBR_PMO           = nrow(df_PBR_PMO),
  PBR_TP   = nrow(df_PBR_TP)
)
#on peut faire un tableau récapitulatif des taux de geste pour chaque type d'intervention
# Fonction pour calculer les taux de geste
calcul_taux_geste <- function(data, intervention_type) {
  data %>%
    filter(!is.na(Geste)) %>%
    count(Geste) %>%
    mutate(
      pourcentage = n / sum(n),
      label       = paste0(round(100 * pourcentage, 1), "%"),
      intervention = intervention_type,
      Geste_francais = case_when(
        Geste == "Yes" ~ "Geste",
        Geste == "No"  ~ "Pas de geste",
        TRUE           ~ Geste
      )
    ) %>%
    select(intervention, Geste_francais, n, pourcentage, label)
}

# Calcul des taux de geste pour chaque type d'intervention
taux_geste_TH  <- calcul_taux_geste(df_PBR_TH, "Transplantation hépatique")
taux_geste_PMO <- calcul_taux_geste(df_PBR_PMO, "Prélèvement multi-organes")
taux_geste_TP  <- calcul_taux_geste(df_PBR_TP, "Transplantation pancréatique")
# Combinaison des résultats
taux_geste_total <- bind_rows(taux_geste_TH, taux_geste_PMO, taux_geste_TP)
taux_geste_total
# Création du tableau récapitulatif avec gt
tableau_taux_geste <- taux_geste_total %>%
  mutate(
    pourcentage = scales::percent(pourcentage, accuracy = 0.1)
  ) %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Taux de geste par type d'intervention (PAUL BROUSSE)"
  ) %>%
  gt::cols_label(
    intervention   = "Type d'intervention",
    Geste_francais = "Geste réalisé",
    n              = "Effectif",
    pourcentage    = "Pourcentage",
    label          = "Label"
  ) %>%
  gt::fmt_number(
    columns = vars(n),
    decimals = 0
  ) %>%
  gt::tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(18),
    heading.title.font.weight = "bold"
  )

tableau_taux_geste

#représentation en graphique
# Diagramme en barres pour chaque type d'intervention AVEC GESTE EN VERT (METTRE QUE GESTE == YES)
#bien mettre en vert


##-----GESTES MAJORITAIRES POUR PMO-----
#QUEL GESTE MAJORITAIRE POUR PMO (df_PBR$QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout)
df_PBR_PMO$QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout
# Calcul des effectifs et pourcentages
df_geste_PMO <- df_PBR_PMO %>%
  filter(!is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout)) %>%
  count(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
  mutate(
    pourcentage   = n / sum(n),
    label         = paste0(round(100 * pourcentage, 1), "%"),
    label_complet = paste0(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout, " (", round(100 * pourcentage, 1), "%)")
  )

df_PBR_PMO$QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout

library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(scales)
library(ggplot2)

# --- 1) Effectif total des PMO ---
n_total_pmo <- nrow(df_PBR_PMO)

# --- 2) Nettoyage et comptage des gestes ---
gestes_pmo_all <- df_PBR_PMO %>%
  transmute(gestes_brut = QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
  filter(!is.na(gestes_brut), gestes_brut != "") %>%
  separate_rows(gestes_brut, sep = ",") %>%
  mutate(
    geste = gestes_brut %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all("\\s*,$", "") %>%
      str_squish()
  ) %>%
  mutate(
    geste = case_when(
      str_detect(geste, regex("^Temps chaud", ignore_case = TRUE)) ~ "Temps chaud",
      str_detect(geste, regex("^Temps froid", ignore_case = TRUE)) ~ "Temps froid",
      str_detect(geste, regex("^Canule VMI", ignore_case = TRUE)) ~ "Canule VMI",
      str_detect(geste, regex("^P[ée]dicule à chaud", ignore_case = TRUE)) ~ "Pédicule à chaud",
      str_detect(geste, regex("^Canulation vaisseaux", ignore_case = TRUE)) ~ "Canulation vaisseaux + lavage",
      str_detect(geste, regex("^Contr(ô|o)le de l'aorte c(oe|œ)liaque", ignore_case = TRUE)) ~ "Contrôle de l’aorte coeliaque",
      str_detect(geste, regex("^Libération foie droit", ignore_case = TRUE)) ~ "Libération du foie droit",
      str_detect(geste, regex("^Cholécystectomie", ignore_case = TRUE)) ~ "Cholécystectomie",
      str_detect(geste, regex("^Paroi", ignore_case = TRUE)) ~ "Paroi (incision/fermeture)",
      str_detect(geste, regex("^Ouverture", ignore_case = TRUE)) ~ "Ouverture",
      str_detect(geste, regex("^Dissection", ignore_case = TRUE)) ~ "Dissection",
      str_detect(geste, regex("^Tout$", ignore_case = TRUE)) ~ "Tout (séquence complète)",
      TRUE ~ geste
    )
  ) %>%
  count(geste, name = "n") %>%
  arrange(desc(n)) %>%
  mutate(
    pct_total = n / n_total_pmo,
    label_pct = percent(pct_total, accuracy = 0.1),
    geste = fct_reorder(geste, n)
  )

# --- 3) Palette manuelle avec la couleur spéciale ---
pal_gestes <- c(
  "Temps chaud"                   = "#E64B35",
  "Temps froid"                   = "#4DBBD5",
  "Pédicule à chaud"              = "#7E6148",
  "Canule VMI"                    = "#3C5488",
  "Contrôle de l’aorte coeliaque" = "#d10404",  # couleur demandée
  "Canulation vaisseaux + lavage" = "#00A087",
  "Dissection"                    = "#8491B4",
  "Libération du foie droit"      = "#F39B7F",
  "Cholécystectomie"              = "#B09C85",
  "Paroi (incision/fermeture)"    = "#00A8A8",
  "Ouverture"                     = "#7EBC89",
  "Tout (séquence complète)"      = "#8FBC8F"
)

# --- 4) Graphique stylé ---
p_pmo_all <- ggplot(gestes_pmo_all, aes(x = geste, y = n, fill = geste)) +
  geom_col(width = 0.7) +
  # % dans la barre
  geom_text(aes(label = label_pct),
            hjust = 1.05, color = "white", size = 5, fontface = "bold") +
  # n au bout
  geom_text(aes(label = n),
            hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal_gestes, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "PMO — Gestes effectués",
    subtitle = paste0("Pourcentage calculé sur ", n_total_pmo, " PMO (y compris ceux sans geste)"),
    x = NULL,
    y = "Effectif (mentions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.y   = element_text(size = 12, face = "bold"),
    axis.text.x   = element_text(size = 11),
    plot.margin   = margin(10, 28, 10, 10)
  )

p_pmo_all









library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(stringr)

# 1) Récupérer le dénominateur N par type d'intervention
N_par_type <- taux_geste_total %>%
  group_by(intervention) %>%
  summarise(N_total = sum(n), .groups = "drop")

# 2) Garder uniquement "Geste == Yes" et préparer les labels
taux_geste_yes <- taux_geste_total %>%
  filter(Geste_francais == "Geste") %>%
  left_join(N_par_type, by = "intervention") %>%
  mutate(
    label_pct = percent(pourcentage, accuracy = 0.1),
    label_nN  = paste0(n, "/", N_total),
    intervention = fct_reorder(intervention, pourcentage) # ordre croissant (change fct_rev() si besoin)
  )

# 3) Barplot vert uniquement (3 barres)
barplot_taux_geste_vert <- ggplot(taux_geste_yes,
                                  aes(x = intervention, y = pourcentage)) +
  geom_col(fill = "forestgreen", width = 0.6) +
  # % DANS la barre (texte blanc)
  geom_text(aes(label = label_pct),
            vjust = 1.5, color = "white", size = 6, fontface = "bold") +
  # n/N AU-DESSUS de la barre
  geom_text(aes(label = label_nN),
            vjust = -0.5, size = 6, fontface = "bold") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1),
                     expand = expansion(mult = c(0, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Taux de geste (Geste == Yes) par type d'intervention (PAUL BROUSSE)",
    subtitle = "Barre = % de gestes rapporté au total de chaque type (n/N affiché au-dessus)",
    x = "Type d'intervention",
    y = "Taux de geste"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title     = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle  = element_text(hjust = 0.5),
    axis.text.x    = element_text(size = 12, face = "bold"),
    axis.text.y    = element_text(size = 12)
  )

barplot_taux_geste_vert

# 4) Export
ggsave("barplot_taux_geste_TH_PMO_TP_vert.png",
       plot = barplot_taux_geste_vert, width = 10, height = 6, dpi = 300)




##-----GESTES MAJORITAIRES POUR TH-----
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(scales)
library(ggplot2)

# 0) Dénominateur = toutes les TH
n_total_th <- nrow(df_PBR_TH)

# 1) Préparer un id et rassembler les deux sources de texte
df_th <- df_PBR_TH %>%
  mutate(.id_th = dplyr::row_number()) %>%
  select(.id_th,
         quel = QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout,
         txt  = Geste_whole_text)

# 2) Exploser en tokens
tokens <- df_th %>%
  pivot_longer(cols = c(quel, txt), names_to = "src", values_to = "val") %>%
  filter(!is.na(val), val != "") %>%
  separate_rows(val, sep = ",") %>%
  mutate(token = val %>%
           str_replace_all("\\s+", " ") %>%
           str_replace_all("\\s*,$", "") %>%
           str_squish()) %>%
  select(.id_th, token)

# 3) Mapping (fusion biliaire/bilio-digestive -> "Biliaire" via tag RAW ; "Pied de l’anse" isolé)
map_token <- function(x){
  x_low <- tolower(x)
  case_when(
    # APC / porto-cave
    str_detect(x_low, "\\bapc\\b") |
      str_detect(x_low, "anastomose\\s*porto[- ]?cave") ~ "APC",
    # "Anastomose" générique -> APC
    str_detect(x_low, "^anastomose\\s*$") ~ "APC",
    
    # Pied de l'anse -> catégorie dédiée
    str_detect(x_low, "pied de l'?anse") ~ "Pied de l’anse",
    
    # Tout ce qui est biliaire OU bilio-digestif -> tag brut "Biliaire_RAW" (fusion plus loin)
    str_detect(x_low, "anastomose biliaire") |
      str_detect(x_low, "bilio.?digest") |
      str_detect(x_low, "bilio\\s*biliaire") |
      str_detect(x_low, "biliodig") ~ "Biliaire_RAW",
    
    # Autres gestes utiles
    str_detect(x_low, "^paroi") ~ "Paroi",
    str_detect(x_low, "dissection du p[ée]dicule h[ée]patique") ~ "Dissection du pédicule hépatique",
    str_detect(x_low, "^dissection$") ~ "Dissection",
    TRUE ~ x
  )
}

tokens_norm <- tokens %>%
  mutate(cat = map_token(token))

# 4) Résolution par intervention : si "Biliaire_RAW" présent -> "Biliaire" (une seule fois)
resolved <- tokens_norm %>%
  group_by(.id_th) %>%
  summarise(cats = list(unique(cat)), .groups = "drop") %>%
  rowwise() %>%
  mutate(cats_resolved = list({
    cs <- cats
    if ("Biliaire_RAW" %in% cs) {
      cs <- setdiff(cs, "Biliaire_RAW")
      cs <- unique(c(cs, "Biliaire"))
    }
    cs
  })) %>%
  ungroup() %>%
  select(.id_th, cats_resolved) %>%
  unnest_longer(cats_resolved, values_to = "cat_final")

# 5) Comptage par catégorie (présence par patient), % sur total TH
counts <- resolved %>%
  distinct(.id_th, cat_final) %>%
  count(cat_final, name = "n_cases") %>%
  mutate(
    pct_total = n_cases / n_total_th,
    label_pct = percent(pct_total, accuracy = 0.1),
    label_nN  = paste0(n_cases, "/", n_total_th)
  ) %>%
  arrange(desc(n_cases)) %>%
  mutate(cat_final = fct_reorder(cat_final, n_cases))

# 6) Palette (Biliaire fusionnée + nouvelle catégorie Pied de l’anse)
pal <- c(
  "APC"      = "#3C5488",
  "Biliaire"                         = "#E64B35",
  "Pied de l’anse"                   = "#00A087",
  "Paroi"                            = "#7E6148",
  "Dissection du pédicule hépatique" = "#4DBBD5",
  "Dissection"                       = "#B09C85"
)
missing_cols <- setdiff(levels(counts$cat_final), names(pal))
if (length(missing_cols) > 0) {
  pal <- c(pal, setNames(colorRampPalette(c("#6C757D", "#ADB5BD", "#CED4DA"))(length(missing_cols)), missing_cols))
}

# 7) Barplot horizontal (n/N au bout, % dans la barre, espace haut)
p_th_gestes <- ggplot(counts, aes(x = cat_final, y = n_cases, fill = cat_final)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label_pct), hjust = 1.05, color = "white", size = 5, fontface = "bold") +
  geom_text(aes(label = label_nN), hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Transplantation hépatique — Gestes/anastomoses (effectif total)",
    subtitle = "« Anastomose » non précisée → APC. Biliaire = biliaire + bilio-digestive. « Pied de l’anse » dédié.",
    x = NULL, y = "Nombre d’interventions concernées"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y   = element_text(size = 12, face = "bold"),
    axis.text.x   = element_text(size = 11),
    plot.margin   = margin(10, 28, 10, 10)
  )

p_th_gestes





##-------PÉDAGOGIE--------
# Tableau de répartition
df_PBR %>%
  select(PEDAGOGIE) %>%
  tbl_summary(
    label = list(PEDAGOGIE ~ "Pédagogie perçue"),
    missing = "no"
  ) %>%
  modify_header(label = "**Pédagogie**") %>%
  bold_labels()

# Bar plot vertical
couleurs_pedagogie <- c(
  "1-rien" = "#F4A3A3",
  "2-quasi rien" = "#F4C4A3",
  "3-ok" = "#A3C4F4",
  "4-bien" = "#A3F4A3",
  "5-incroyable!!" = "#73E673"
)

#bar plot vertical de répartition de la pédagogie perçue
#GRAPHIQUE 3 : Répartition de la pédagogie perçue (même style que 1 & 2)
df_pedagogie <- df_PBR %>%
  filter(!is.na(PEDAGOGIE)) %>%
  count(PEDAGOGIE) %>%
  mutate(
    pourcentage = round(100 * n / sum(n), 1),
    label = paste0(pourcentage, "%")
  )

# Ordre cohérent des niveaux (adapter si nécessaire aux libellés exacts de ta base)
lvl_pedagogie <- c("1-rien", "2-quasi rien", "3-ok", "4-bien", "5-incroyable!!")
df_pedagogie <- df_pedagogie %>%
  mutate(PEDAGOGIE = factor(PEDAGOGIE, levels = lvl_pedagogie))

barplot_pedagogie <- ggplot(
  df_pedagogie,
  aes(x = PEDAGOGIE, y = n, fill = PEDAGOGIE)
) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label), vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_pedagogie) +
  common_y +
  labs(
    title = "Répartition de la pédagogie perçue",
    x = "Pédagogie",
    y = "Nombre de cas"
  ) +
  common_theme +
  coord_cartesian(clip = "off")

barplot_pedagogie
ggsave("barplot_pedagogie.svg", plot = barplot_pedagogie,
       width = w, height = h, dpi = dpi, units = "in")





##-------AMBIANCE--------
# Vérifie les valeurs uniques pour être sûr
unique(df_PBR$AMBIANCE)

# Crée la table de répartition
df_ambiance <- df_PBR %>%
  filter(!is.na(AMBIANCE)) %>%
  count(AMBIANCE) %>%
  mutate(
    pourcent = n / sum(n),
    label = paste0(n, " (", round(100 * pourcent, 1), "%)")
  )

# Couleurs perso si tu veux
couleurs_ambiance <- c(
  "1 - je veux partir" = "#F4A3A3",
  "2 - c'est ok" = "#A3C4F4",
  "3 - on recommence" = "#A3F4A3"
)

# Bar plot
barplot_ambiance <- ggplot(df_ambiance, aes(x = AMBIANCE, y = n, fill = AMBIANCE)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = label),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = couleurs_ambiance) +
  labs(
    x = "Expérience ressentie",
    y = "Nombre d’interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

barplot_ambiance
ggsave("expérience.svg", plot = barplot_ambiance, width = 10, height = 6, dpi = 1000)

##----REGROUPEMENT----
library(dplyr)
library(stringr)
library(tidyr)

# 0) Exclusions demandées
to_drop <- c("Curage ganglionnaire", "Parathyroïdectomie")

df_PBR_clean <- df_PBR %>%
  filter(!INTERVENTION_GROUPÉE %in% to_drop)

# 1) Regroupement en grands groupes (sans "Autre")
df_PBR_grouped <- df_PBR_clean %>%
  mutate(
    INTERVENTION_GROUPE_MAJEUR = case_when(
      # --- Transplantation hépatique  ---
      str_detect(INTERVENTION_GROUPÉE, "^Transplantation hépatique$") |
        str_detect(INTERVENTION_GROUPÉE, "^Explantation hépatique$")   |
        str_detect(INTERVENTION_GROUPÉE, "^Donneur vivant \\((laparo|robot)\\)$") ~ "Transplantation hépatique ",
      
      # --- PMO ---
      str_detect(INTERVENTION_GROUPÉE, "^Prélèvement multi-organes$") ~ "Prélèvements multi-organes",
      
      # --- Transplantation pancréatique ---
      str_detect(INTERVENTION_GROUPÉE, "^Transplantation pancréatique$") ~ "Transplantation pancréatique",
      
      # --- Hépatectomies ---
      str_detect(INTERVENTION_GROUPÉE, "^Hépatectomie (majeure|mineure) \\((laparo|coelio|robot)\\)$") |
        str_detect(INTERVENTION_GROUPÉE, "^Hépatectomie complexe") |
        str_detect(INTERVENTION_GROUPÉE, "^Ré-hépatectomie$")      |
        str_detect(INTERVENTION_GROUPÉE, "^Lobectomie gauche$")    ~ "Hépatectomies",
      
      # --- Chirurgie pancréatique ---
      str_detect(INTERVENTION_GROUPÉE, "^Pancreatectomie") |
        str_detect(INTERVENTION_GROUPÉE, "^Pancréas - autre$") ~ "Chirurgie pancréatique",
      
      # --- Cholécystectomies ---
      str_detect(INTERVENTION_GROUPÉE, "^Cholécystectomie") ~ "Cholécystectomies",
      
      # --- Chirurgie biliaire / foie (hors résection) ---
      str_detect(INTERVENTION_GROUPÉE, "^Réparation biliaire$") |
        str_detect(INTERVENTION_GROUPÉE, "^Fenestration kyste hépatique") ~ "Chirurgie biliaire/foie (hors résection)",
      
      # --- Chirurgie colorectale ---
      str_detect(INTERVENTION_GROUPÉE, "^Rectum \\(coelio\\)$") |
        str_detect(INTERVENTION_GROUPÉE, "^Hartmann")             |
        str_detect(INTERVENTION_GROUPÉE, "^Colon droit \\(coelio\\)$") |
        str_detect(INTERVENTION_GROUPÉE, "^RIC \\((laparo|coelio)\\)$") |
        str_detect(INTERVENTION_GROUPÉE, "^Résection de grêle$")  ~ "Chirurgie colorectale",
      #rajouter Appendicectomie (coelio)
      str_detect(INTERVENTION_GROUPÉE, "^Appendicectomie \\(coelio\\)$") ~ "Chirurgie colorectale",
      
      # --- Paroi / hernies / stomies ---
      str_detect(INTERVENTION_GROUPÉE, "^Cure d'éventration") |
        str_detect(INTERVENTION_GROUPÉE, "^Hernie (inguinale|ombilicale)$") |
        str_detect(INTERVENTION_GROUPÉE, "^Stomie digestive$") |
        str_detect(INTERVENTION_GROUPÉE, "^Fermeture de stomie$") |
        str_detect(INTERVENTION_GROUPÉE, "^Éviscération$")       ~ "Paroi, hernies et stomies",
      
      # --- Procédures interventionnelles (incl. tes déplacements) ---
      str_detect(INTERVENTION_GROUPÉE, "^Procédure interventionnelle$") |
        str_detect(INTERVENTION_GROUPÉE, "^Pose / révision de TIPS$")     |
        str_detect(INTERVENTION_GROUPÉE, "^Drainage abcès hépatique$")    |
        str_detect(INTERVENTION_GROUPÉE, "^Drainage d[’']abcès$")         |
        str_detect(INTERVENTION_GROUPÉE, "^Drainage chirurgical$")        ~ "Procédures interventionnelles",
      
      # --- Gestes vasculaires ---
      str_detect(INTERVENTION_GROUPÉE, "^Anastomose / dérivation vasculaire$") ~ "Gestes vasculaires",
      
      # --- REPRISE (incl. explorations et depacking) ---
      str_detect(INTERVENTION_GROUPÉE, "^Exploration$") |
        str_detect(INTERVENTION_GROUPÉE, "^Laparotomie exploratrice$") |
        str_detect(INTERVENTION_GROUPÉE, "^Depacking$")   ~ "REPRISE",
      
      # --- Estomac / œso ---
      str_detect(INTERVENTION_GROUPÉE, "^Gastrectomie totale \\(laparo\\)$") |
        str_detect(INTERVENTION_GROUPÉE, "^Cure RGO \\(coelio\\)$") ~ "Estomac/œso",
      
      # --- Rate ---
      str_detect(INTERVENTION_GROUPÉE, "^Splénectomie$") ~ "Rate",
      
      # --- Cytoréduction péritonéale ---
      str_detect(INTERVENTION_GROUPÉE, "^Cytoréduction \\(laparo\\)$") ~ "Cytoréduction péritonéale",
      
      # fallback (si une modalité nouvelle apparaît, on garde le libellé d'origine pour repérage)
      TRUE ~ INTERVENTION_GROUPÉE
    )
  )

# 2) Contrôles : quelles modalités seraient restées non regrouppées ?
leftovers <- df_PBR_grouped %>%
  filter(!INTERVENTION_GROUPE_MAJEUR %in% c(
    "Transplantation hépatique ",
    "Prélèvements multi-organes",
    "Transplantation pancréatique",
    "Hépatectomies",
    "Chirurgie pancréatique",
    "Cholécystectomies",
    "Chirurgie biliaire/foie (hors résection)",
    "Chirurgie colorectale",
    "Paroi, hernies et stomies",
    "Procédures interventionnelles",
    "Gestes vasculaires",
    "REPRISE",
    "Estomac/œso",
    "Rate",
    "Cytoréduction péritonéale"
  )) %>%
  count(INTERVENTION_GROUPÉE, sort = TRUE)

# (optionnel) aperçu des comptes par groupe
resume_groupes <- df_PBR_grouped %>%
  count(INTERVENTION_GROUPE_MAJEUR, sort = TRUE)

list(
  apercu_groupes = resume_groupes,
  a_verifier = leftovers
)








##----BARPLOT GROUPES REGROUPEES-----
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)

# Données agrégées
df_groupes_plot <- df_PBR_grouped %>%
  filter(!is.na(INTERVENTION_GROUPE_MAJEUR)) %>%
  count(INTERVENTION_GROUPE_MAJEUR, name = "n") %>%
  mutate(
    pourcentage = n / sum(n),
    label_pct   = percent(pourcentage, accuracy = 0.1)
  ) %>%
  arrange(pourcentage) %>%
  mutate(INTERVENTION_GROUPE_MAJEUR = fct_inorder(INTERVENTION_GROUPE_MAJEUR))

# Barplot horizontal (lisible, pas de chevauchement)
plot_groupes <- ggplot(df_groupes_plot,
                       aes(x = INTERVENTION_GROUPE_MAJEUR, y = pourcentage)) +
  geom_col(fill = "#2E86DE", width = 0.6) +
  # % à droite des barres
  geom_text(aes(label = label_pct), hjust = -0.2, size = 5, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +  # wrap des libellés longs
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.10))) +         # espace pour le label à droite
  labs(
    title = "Répartition des interventions par grands groupes (PAUL BROUSSE)",
    x = NULL, y = "Proportion des interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 11)
  )

plot_groupes

# Export avec hauteur adaptée au nombre de groupes
h <- max(6, 0.5 * nrow(df_groupes_plot) + 2)
ggsave("groupes_barplot_PBR.png", plot = plot_groupes, width = 10, height = h, dpi = 300)






##----TAUX DE GESTE SELON GROUPES REGROUPÉES-----
# --- Taux de geste "dans" la part totale de chaque grand groupe (barres bicolores) ---
library(dplyr)
library(stringr)
library(forcats)
library(scales)
library(ggplot2)

# 1) Base propre
df_base <- df_PBR_grouped %>%
  filter(!is.na(INTERVENTION_GROUPE_MAJEUR)) %>%
  mutate(
    groupe = str_squish(INTERVENTION_GROUPE_MAJEUR),
    geste  = ifelse(Geste %in% c("Yes", "No"), Geste, NA_character_)
  ) %>%
  filter(!is.na(geste))

# 2) Comptes par groupe
resume <- df_base %>%
  group_by(groupe) %>%
  summarise(
    N        = n(),
    n_geste  = sum(geste == "Yes"),
    n_no     = N - n_geste,
    .groups  = "drop"
  )

N_total <- sum(resume$N)

resume <- resume %>%
  mutate(
    # Parts sur le total global (pour la longueur des segments)
    p_tot   = N / N_total,
    p_geste = n_geste / N_total,
    p_no    = n_no / N_total,
    
    # % AU SEIN DU GROUPE (pour le label dans le segment vert)
    taux_groupe    = n_geste / N,
    label_geste_in = percent(taux_groupe, accuracy = 0.1),
    
    # % total du groupe (label à droite de la barre)
    label_tot = percent(p_tot, accuracy = 0.1)
  ) %>%
  arrange(p_tot) %>%
  mutate(groupe = fct_inorder(groupe))

# 3) Données empilées — forcer "Geste" à GAUCHE
df_bar <- bind_rows(
  resume %>% transmute(groupe, part = "Geste",      pct = p_geste, label_geste_in),
  resume %>% transmute(groupe, part = "Sans geste", pct = p_no,    label_geste_in = NA_character_)
) %>%
  mutate(part = factor(part, levels = c("Geste", "Sans geste")))

# 4) Plot — bleu pastel pour "Sans geste", vert à gauche pour "Geste"
plot_groupes_geste <- ggplot(df_bar, aes(x = groupe, y = pct, fill = part)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  # Label dans le segment vert = % AU SEIN DU GROUPE
  geom_text(
    data = df_bar %>% filter(part == "Geste", pct >= 0.01),
    aes(label = label_geste_in),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white", size = 4.8, fontface = "bold"
  ) +
  # % total du groupe à droite de la barre
  geom_text(
    data = resume, inherit.aes = FALSE,
    aes(x = groupe, y = p_tot, label = label_tot),
    hjust = -0.2, size = 5, fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Geste" = "forestgreen", "Sans geste" = "#9EC9FF"), name = NULL) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Répartition par grands groupes, avec part des gestes (sur le total global)",
    subtitle = paste0(
      "Barre = part du groupe sur l’ensemble des interventions (", N_total, "). ",
      "Segment vert (à gauche) = part AVEC geste (sur le total). ",
      "Label vert = % de gestes AU SEIN du groupe."
    ),
    x = NULL, y = "Part sur l’ensemble des interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y   = element_text(size = 12, face = "bold"),
    axis.text.x   = element_text(size = 11),
    plot.margin   = margin(10, 24, 10, 10),
    legend.position = "bottom"
  )

plot_groupes_geste

# Export
h <- max(6, 0.5 * n_distinct(resume$groupe) + 2)
ggsave("groupes_barplot_part_geste_total_label_in_group.png",
       plot_groupes_geste, width = 14, height = 8, dpi = 300)



df_PBR_grouped$QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout




##----GESTE MAJORITAIRE SELON GROUPES REGROUPEES-----
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)
library(gt)
library(scales)

# ===== 0) Préparer base propre
df_base <- df_PBR_grouped %>%
  filter(!is.na(INTERVENTION_GROUPE_MAJEUR)) %>%
  mutate(
    groupe    = str_squish(INTERVENTION_GROUPE_MAJEUR),
    gestes_br = QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout
  )

# ===== 1) Tokeniser la colonne des gestes 
tokens <- df_base %>%
  mutate(.id = row_number()) %>%
  select(.id, groupe, gestes_br) %>%
  filter(!is.na(gestes_br), gestes_br != "") %>%
  separate_rows(gestes_br, sep = ",") %>%
  mutate(token = gestes_br %>%
           str_replace_all("\\s+", " ") %>%  # espaces multiples
           str_replace_all("\\s*,$", "") %>%
           str_squish()) %>%
  select(.id, groupe, token)

# ===== 2) Normalisation fine des gestes
map_geste <- function(x) {
  xl <- tolower(x)
  
  case_when(
    # ignorer "tout"
    str_detect(xl, "^tout\\b") ~ NA_character_,
    
    # général
    str_detect(xl, "^paroi\\b")                         ~ "Paroi",
    str_detect(xl, "^dissection\\b")                    ~ "Dissection",
    str_detect(xl, "^ouverture\\b")                     ~ "Ouverture",
    str_detect(xl, "^temps\\s*chaud\\b")                ~ "Temps chaud",
    str_detect(xl, "^temps\\s*froid\\b")                ~ "Temps froid",
    str_detect(xl, "p[ée]dicule\\s*à\\s*chaud")         ~ "Pédicule à chaud",
    str_detect(xl, "canule\\s*vmi")                     ~ "Canule VMI",
    str_detect(xl, "canulation\\s*vaisseaux")           ~ "Canulation vaisseaux + lavage",
    str_detect(xl, "contr(ô|o)le\\s+de\\s+l'?aorte\\s+c(oe|œ)liaque") ~ "Contrôle de l’aorte coeliaque",
    str_detect(xl, "lib[ée]ration\\s+foie\\s+droit")    ~ "Libération du foie droit",
    str_detect(xl, "chol[ée]cystectomie")               ~ "Cholécystectomie",
    str_detect(xl, "^apc\\b")                           ~ "APC",
    str_detect(xl, "anastomose\\s*porto[- ]?cave")      ~ "APC",
    str_detect(xl, "pied\\s+de\\s+l'?anse")             ~ "Pied de l’anse",
    str_detect(xl, "bilio\\s*biliaire") |
      str_detect(xl, "bilio\\s*dig") |
      str_detect(xl, "bilio.?digest")                   ~ "Biliaire",
    str_detect(xl, "fixation\\s*proth[èe]se")           ~ "Fixation de prothèse",
    str_detect(xl, "pose\\s*de\\s*pac")                 ~ "Pose de PAC",
    
    # mention de l'acte global → on écarte (pas un geste fin)
    str_detect(xl, "h[ée]patectomie")                   ~ NA_character_,
    
    # "anastomose" générique -> traité plus bas (dépend du groupe)
    str_detect(xl, "^anastomose\\s*$")                  ~ "ANASTO_NS",
    
    TRUE ~ str_to_sentence(str_squish(x)) # défaut propre
  )
}

tokens_norm <- tokens %>%
  mutate(geste0 = map_geste(token)) %>%
  # règle contextuelle : "Anastomose" non précisée => APC uniquement pour TXH
  mutate(geste = case_when(
    geste0 == "ANASTO_NS" & str_detect(str_to_lower(groupe), "transplantation h[ée]patique") ~
      "APC",
    geste0 == "ANASTO_NS" ~ "Anastomose (non précisée)",
    TRUE ~ geste0
  )) %>%
  filter(!is.na(geste), geste != "")

# ===== 3) Comptage des gestes par groupe (mentions)
gestes_par_groupe <- tokens_norm %>%
  count(groupe, geste, name = "n_mentions") %>%
  group_by(groupe) %>%
  mutate(pct_mentions = n_mentions / sum(n_mentions)) %>%
  ungroup()

# ===== 4) Geste majoritaire par groupe
majoritaires <- gestes_par_groupe %>%
  group_by(groupe) %>%
  slice_max(order_by = n_mentions, n = 1, with_ties = FALSE) %>%  # s'il y a égalité, prend le premier (on peut gérer ties différemment si tu veux)
  ungroup()

# ===== 5) Contexte : effectifs d’interventions et taux de geste au sein du groupe
contexte_groupe <- df_base %>%
  group_by(groupe) %>%
  summarise(
    N_interv = n(),
    n_geste_yes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste_groupe = n_geste_yes / N_interv,
    .groups = "drop"
  )

# ===== 6) Résultat final : tableau lisible
resultat_major <- majoritaires %>%
  left_join(contexte_groupe, by = "groupe") %>%
  mutate(
    pct_mentions_lab   = percent(pct_mentions, accuracy = 0.1),
    taux_geste_lab     = percent(taux_geste_groupe, accuracy = 0.1)
  ) %>%
  arrange(desc(N_interv))

# Affichage GT
resultat_major %>%
  select(
    Groupe              = groupe,
    `Geste majoritaire` = geste,
    `Mentions (n)`      = n_mentions,
    `Part des mentions dans le groupe` = pct_mentions_lab,
    `Interventions du groupe (N)` = N_interv,
    `Taux de geste (Yes) au sein du groupe` = taux_geste_lab
  ) %>%
  gt() %>%
  tab_header(title = "Geste majoritaire par grand groupe d’intervention") %>%
  tab_options(table.font.size = px(14),
              heading.title.font.size = px(18),
              heading.title.font.weight = "bold")








library(dplyr)
library(stringr)
library(forcats)
library(scales)
library(ggplot2)
library(tidyr)

# ===== Base propre
df_base <- df_PBR_grouped %>%
  filter(!is.na(INTERVENTION_GROUPE_MAJEUR)) %>%
  mutate(
    groupe = str_squish(INTERVENTION_GROUPE_MAJEUR),
    gesteYN  = ifelse(Geste %in% c("Yes", "No"), Geste, NA_character_)
  ) %>%
  filter(!is.na(gesteYN))

# ===== Comptes par groupe (pour longueurs de barres
resume <- df_base %>%
  group_by(groupe) %>%
  summarise(
    N        = n(),
    n_geste  = sum(gesteYN == "Yes"),
    n_no     = N - n_geste,
    .groups  = "drop"
  )

N_total <- sum(resume$N)

resume <- resume %>%
  mutate(
    # Parts sur le total global (longueurs)
    p_tot   = N / N_total,
    p_geste = n_geste / N_total,
    p_no    = n_no / N_total,
    
    # % AU SEIN DU GROUPE (pour le label dans le vert)
    taux_groupe    = n_geste / N,
    label_geste_in = percent(taux_groupe, accuracy = 0.1),
    
    # % total du groupe (pour le bout de barre)
    label_tot = percent(p_tot, accuracy = 0.1)
  ) %>%
  arrange(p_tot) %>%
  mutate(groupe = fct_inorder(groupe))

# ===== Geste majoritaire par groupe (depuis la colonne multi-gestes
# 1) Tokenisation
tokens <- df_PBR_grouped %>%
  mutate(
    groupe    = str_squish(INTERVENTION_GROUPE_MAJEUR),
    gestes_br = QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout
  ) %>%
  filter(!is.na(groupe), !is.na(gestes_br), gestes_br != "") %>%
  mutate(.id = row_number()) %>%
  separate_rows(gestes_br, sep = ",") %>%
  mutate(token = gestes_br %>%
           str_replace_all("\\s+", " ") %>%
           str_replace_all("\\s*,$", "") %>%
           str_squish()) %>%
  select(.id, groupe, token)

# 2) Mapping/normalisation
map_geste <- function(x){
  xl <- tolower(x)
  case_when(
    str_detect(xl, "^tout\\b")                        ~ NA_character_,
    str_detect(xl, "^paroi\\b")                       ~ "Paroi",
    str_detect(xl, "^dissection\\b")                  ~ "Dissection",
    str_detect(xl, "^ouverture\\b")                   ~ "Ouverture",
    str_detect(xl, "^temps\\s*chaud\\b")              ~ "Temps chaud",
    str_detect(xl, "^temps\\s*froid\\b")              ~ "Temps froid",
    str_detect(xl, "p[ée]dicule\\s*à\\s*chaud")       ~ "Pédicule à chaud",
    str_detect(xl, "canule\\s*vmi")                   ~ "Canule VMI",
    str_detect(xl, "canulation\\s*vaisseaux")         ~ "Canulation vaisseaux + lavage",
    str_detect(xl, "contr(ô|o)le de l'?aorte c(oe|œ)liaque") ~ "Contrôle de l’aorte coeliaque",
    str_detect(xl, "lib[ée]ration foie droit")        ~ "Libération du foie droit",
    str_detect(xl, "chol[ée]cystectomie")             ~ "Cholécystectomie",
    str_detect(xl, "^apc\\b")                         ~ "APC",
    str_detect(xl, "anastomose\\s*porto[- ]?cave")    ~ "APC",
    str_detect(xl, "pied de l'?anse")                 ~ "Pied de l’anse",
    str_detect(xl, "bilio\\s*biliaire") |
      str_detect(xl, "bilio\\s*dig") |
      str_detect(xl, "bilio.?digest")                 ~ "Biliaire",
    str_detect(xl, "fixation proth[èe]se")            ~ "Fixation de prothèse",
    str_detect(xl, "pose de pac")                     ~ "Pose de PAC",
    str_detect(xl, "h[ée]patectomie")                 ~ NA_character_,
    str_detect(xl, "^anastomose\\s*$")                ~ "ANASTO_NS",
    TRUE ~ str_to_sentence(str_squish(x))
  )
}

tokens_norm <- tokens %>%
  mutate(geste0 = map_geste(token)) %>%
  mutate(geste = case_when(
    geste0 == "ANASTO_NS" & str_detect(str_to_lower(groupe), "transplantation h[ée]patique") ~
      "APC",
    geste0 == "ANASTO_NS" ~ "Anastomose (non précisée)",
    TRUE ~ geste0
  )) %>%
  filter(!is.na(geste), geste != "")

# 3) Comptage & majoritaire
gestes_par_groupe <- tokens_norm %>%
  count(groupe, geste, name = "n_mentions") %>%
  group_by(groupe) %>%
  mutate(pct_mentions = n_mentions / sum(n_mentions)) %>%
  ungroup()

majoritaires <- gestes_par_groupe %>%
  group_by(groupe) %>%
  slice_max(order_by = n_mentions, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    # petit label propre pour le bout de barre
    geste_maj_lab = paste0(geste) # si tu veux: paste0(geste, " (", percent(pct_mentions,0.1), ")")
  )

# 4) Fusion dans 'resume'
resume2 <- resume %>%
  left_join(majoritaires %>% select(groupe, geste_maj_lab), by = "groupe") %>%
  mutate(
    label_right = paste0(label_tot, " — ", geste_maj_lab),
    groupe = fct_inorder(groupe)
  )

# ===== Données empilées pour la barre bicolore
df_bar <- bind_rows(
  resume2 %>% transmute(groupe, part = "Geste",      pct = p_geste, label_geste_in),
  resume2 %>% transmute(groupe, part = "Sans geste", pct = p_no,    label_geste_in = NA_character_)
) %>%
  mutate(part = factor(part, levels = c("Geste", "Sans geste")))

# ===== Plot : vert à gauche, bleu pastel à droite, + geste maj à droite
plot_groupes_geste <- ggplot(df_bar, aes(x = groupe, y = pct, fill = part)) +
  geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  # Label dans le segment vert = % AU SEIN DU GROUPE
  geom_text(
    data = df_bar %>% filter(part == "Geste", pct >= 0.01),
    aes(label = label_geste_in),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white", size = 4.6, fontface = "bold"
  ) +
  # À droite : % total du groupe + geste majoritaire
  geom_text(
    data = resume2, inherit.aes = FALSE,
    aes(x = groupe, y = p_tot, label = label_right),
    hjust = -0.05, size = 4.6, fontface = "bold"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = c("Geste" = "forestgreen", "Sans geste" = "#9EC9FF"), name = NULL) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.35))) +  # plus d'espace à droite pour le texte long
  labs(
    title    = "Répartition par grands groupes, part des gestes et geste majoritaire",
    subtitle = paste0(
      "Barre = part du groupe sur l’ensemble des interventions (", N_total, "). ",
      "Segment vert (à gauche) = part AVEC geste (sur le total). ",
      "Label vert = % de gestes AU SEIN du groupe. ",
      "Texte à droite = % total du groupe — geste majoritaire."
    ),
    x = NULL, y = "Part sur l’ensemble des interventions"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.y   = element_text(size = 12, face = "bold"),
    axis.text.x   = element_text(size = 11),
    plot.margin   = margin(10, 36, 10, 10),
    legend.position = "bottom"
  )

plot_groupes_geste

# Export (hauteur auto selon nb de groupes)
h <- max(6, 0.5 * n_distinct(resume2$groupe) + 2)
ggsave("groupes_barplot_geste_maj_a_droite.png",
       plot_groupes_geste, width = 14, height = 8, dpi = 300)



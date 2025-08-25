
# === 1. CRÉATION DES MÉTA-GROUPES CORRIGÉE ===
df <- df %>%
  mutate(
    META_GROUPE = case_when(
      str_detect(INTERVENTION_GROUPÉE, "Appendicectomie") ~ "Appendicectomies",
      str_detect(INTERVENTION_GROUPÉE, "Cholécystectomie") ~ "Cholécystectomies",
      # ERREUR CORRIGÉE : suppression du | vide à la fin qui matchait TOUT
      str_detect(INTERVENTION_GROUPÉE, "Hépatectomie|Lobectomie|Fenestration kyste|VBP|Réparation biliaire|Pancreatectomie|Pancréas|DPC|DPT|SPG|Ré-hépatectomie") ~ "Chirurgie hépato-bilio-pancréatique",
      str_detect(INTERVENTION_GROUPÉE, "Colon|Rectum|Hartmann|RIC|Colostomie|Stomie|Fermeture de stomie|Résection de grêle|Intervention grêle|Rétablissement de continuité|Colectomie totale|Rectopexie|TEM|Amputation abdomino-périnéale") ~ "Chirurgie colorectale",
      str_detect(INTERVENTION_GROUPÉE, "Hernie|Éventration|éventration") ~ "Chirurgie pariétale",
      str_detect(INTERVENTION_GROUPÉE, "Exploration|Ulcère perforé|Occlusion|Drainage|Laparotomie exploratrice") ~ "Chirurgie d'urgence",
      str_detect(INTERVENTION_GROUPÉE, "Thyroïdectomie|Parathyroïdectomie|Surrénalectomie|Lobo-isthmectomie") ~ "Chirurgie endocrine",
      str_detect(INTERVENTION_GROUPÉE, "Gastrectomie|Lewis|Oesophage|RGO|Sleeve|Bypass|3 voies|Coloplastie|Duodénectomie|Gastrotomie|Diverticulectomie œsophagienne|Stripping oesophage|Diverticule oesophagien|Démontage gastroplastie|Gastroplastie|Myotomie de Heller|Pharyngo-gastroplastie|Zenker|Ablation anneau gastrique") ~ "Chirurgie digestive haute",
      str_detect(INTERVENTION_GROUPÉE, "Abcès de marge|fistule anale|Hémorroïdes|Sinus pilonidal|Recoupe|Vaginoplastie|Réparation de prolapsus|Abcès périnéal|Fournier|Fissure anale|Examen anal") ~ "Proctologie",
      str_detect(INTERVENTION_GROUPÉE, "Cytoréduction") ~ "Chirurgie péritonéale",
      str_detect(INTERVENTION_GROUPÉE, "Transplantation|Prélèvement|Donneur|Splénectomie|Curage ganglionnaire|Anastomose / dérivation vasculaire") ~ "Prélèvement multi-organe et transplantation",
      TRUE ~ "Autres"
    )
  )

# Vérification des méta-groupes
meta_repartition <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  count(META_GROUPE, sort = TRUE) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(meta_repartition)


# Convertir PEDAGOGIE en numérique 
df <- df %>%
  mutate(
    PEDAGOGIE_num = case_when(
      as.character(PEDAGOGIE) == "1-rien" ~ 1,
      as.character(PEDAGOGIE) == "2-quasi rien" ~ 2,
      as.character(PEDAGOGIE) == "3-ok" ~ 3,
      as.character(PEDAGOGIE) == "4-bien" ~ 4,
      as.character(PEDAGOGIE) == "5-incroyable!!" ~ 5,
      TRUE ~ NA_real_
    )
  )

# Vérification des méta-groupes
meta_repartition <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE)) %>%
  count(META_GROUPE, sort = TRUE) %>%
  mutate(pourcentage = round(100 * n / sum(n), 1))

print(meta_repartition)

# === 2. ANALYSE GLOBALE PAR MÉTA-GROUPE ===
analyse_metagroupes <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE)) %>%
  group_by(META_GROUPE) %>%
  summarise(
    # Volume
    total_interventions = n(),
    
    # Taux de geste
    gestes_realises = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round(100 * gestes_realises / total_interventions, 1),
    
    # Note pédagogie moyenne /5 puis convertie /20
    note_pedagogie_moyenne = round(mean(PEDAGOGIE_num, na.rm = TRUE), 1),
    note_pedagogie_sur_20 = round(mean(PEDAGOGIE_num, na.rm = TRUE) * 4, 1),
    note_pedagogie_mediane = round(median(PEDAGOGIE_num, na.rm = TRUE), 1),
    n_avec_note = sum(!is.na(PEDAGOGIE_num)),
    
    .groups = "drop"
  ) %>%
  arrange(desc(taux_geste))

print(analyse_metagroupes)

# === 3. ÉVOLUTION PAR ANNÉE D'INTERNAT (1-4) ===
evolution_complete <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE), !is.na(annee_DES)) %>%
  filter(annee_DES %in% c("1", "2", "3", "4")) %>%
  group_by(annee_DES, META_GROUPE) %>%
  summarise(
    # Volume
    total = n(),
    
    # Taux de geste
    gestes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round(100 * gestes / total, 1),
    
    # Note pédagogie
    note_pedagogie = round(mean(PEDAGOGIE_num, na.rm = TRUE), 1),
    n_notes = sum(!is.na(PEDAGOGIE_num)),
    
    .groups = "drop"
  ) %>%
  filter(total >= 3) %>%  # Au moins 3 interventions
  arrange(META_GROUPE, annee_DES)

print(head(evolution_complete, 15))

# === 4. GRAPHIQUES ===
# Graphique 1: Répartition des méta-groupes
graphique_repartition <- ggplot(meta_repartition, aes(x = reorder(META_GROUPE, n), y = n)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(n, "\n(", pourcentage, "%)")), hjust = -0.1, size = 2.8) +
  coord_flip() +
  labs(
    title = "✅ RÉPARTITION CORRECTE DES MÉTA-GROUPES",
    subtitle = "Nombre d'interventions par spécialité chirurgicale",
    x = "Méta-groupe",
    y = "Nombre d'interventions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_repartition)

# Graphique 2: Taux de geste par méta-groupe
graphique_taux_geste <- ggplot(analyse_metagroupes, aes(x = reorder(META_GROUPE, taux_geste), y = taux_geste)) +
  geom_col(fill = "darkgreen", alpha = 0.8) +
  geom_text(aes(label = paste0(taux_geste, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "🎯 TAUX DE GESTE PAR MÉTA-GROUPE",
    subtitle = "Pourcentage d'interventions où l'interne a réalisé un geste",
    x = "Méta-groupe",
    y = "Taux de geste (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkgreen"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_taux_geste)

# Graphique 3: Score pédagogie par méta-groupe
graphique_pedagogie <- ggplot(analyse_metagroupes, aes(x = reorder(META_GROUPE, note_pedagogie_sur_20), y = note_pedagogie_sur_20)) +
  geom_col(fill = "orange", alpha = 0.8) +
  geom_text(aes(label = paste0(note_pedagogie_sur_20, "/20")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "📚 SCORE PÉDAGOGIE PAR MÉTA-GROUPE",
    subtitle = "Note moyenne de pédagogie sur 20",
    x = "Méta-groupe",
    y = "Score pédagogie (/20)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", color = "darkorange"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 9)
  )

print(graphique_pedagogie)


# === 3. TABLEAUX SYNTHÉTIQUES ===

# Répartition % par année
repartition_par_annee <- df %>%
  filter(!is.na(INTERVENTION_GROUPÉE), !is.na(META_GROUPE), !is.na(annee_DES)) %>%
  filter(annee_DES >= 1 & annee_DES <= 4) %>%
  group_by(annee_DES, META_GROUPE) %>%
  summarise(nombre = n(), .groups = "drop") %>%
  group_by(annee_DES) %>%
  mutate(
    total_annee = sum(nombre),
    pourcentage = round(100 * nombre / total_annee, 1)
  ) %>%
  ungroup() %>%
  select(annee_DES, META_GROUPE, pourcentage) %>%
  pivot_wider(names_from = annee_DES, values_from = pourcentage, values_fill = 0) %>%
  arrange(desc(`1`))

print(repartition_par_annee)

# === 4. GRAPHIQUES ===

# Graphique 1: Évolution du taux de geste
graphique_taux_geste <- ggplot(evolution_complete, aes(x = annee_DES, y = taux_geste, color = META_GROUPE)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:4, labels = paste0("D", 1:4)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Évolution du taux de geste par méta-groupe",
    subtitle = "Pourcentage de gestes réalisés par les internes (D1 à D4)",
    x = "Année d'internat",
    y = "Taux de geste (%)",
    color = "Méta-groupe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 2))

print(graphique_taux_geste)

# Graphique 2: Heatmap du taux de geste
heatmap_data <- evolution_complete %>%
  select(annee_DES, META_GROUPE, taux_geste) %>%
  complete(annee_DES, META_GROUPE, fill = list(taux_geste = 0))

graphique_heatmap <- ggplot(heatmap_data, aes(x = factor(annee_DES), y = META_GROUPE, fill = taux_geste)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = ifelse(taux_geste > 0, paste0(taux_geste, "%"), "")), 
            color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient2(low = "navy", mid = "steelblue", high = "orange", 
                       midpoint = 50, name = "Taux de geste (%)") +
  scale_x_discrete(labels = paste0("D", 1:4)) +
  labs(
    title = "Heatmap : Taux de geste par méta-groupe et année",
    subtitle = "Intensité = pourcentage de gestes réalisés",
    x = "Année d'internat",
    y = "Méta-groupe"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

print(graphique_heatmap)
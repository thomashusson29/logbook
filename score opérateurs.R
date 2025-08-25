# Système de notation des opérateurs sur 20
# 1/3 pour le taux de geste, 1/3 pour la pédagogie, 1/3 pour l'ambiance

library(dplyr)

# 1. Calculer le taux de geste par opérateur (quand c'est un bloc avec eux)
taux_geste_operateur <- df %>%
  filter(!is.na(OPERATEUR), !is.na(Geste)) %>%
  group_by(OPERATEUR) %>%
  summarise(
    nombre_interventions = n(),
    nombre_gestes = sum(Geste == "Yes", na.rm = TRUE),
    taux_geste = round((nombre_gestes / nombre_interventions) * 100, 2),
    .groups = 'drop'
  )

# 2. Calculer les scores moyens de PEDAGOGIE et AMBIANCE par opérateur
scores_operateur <- df %>%
  filter(!is.na(OPERATEUR)) %>%
  group_by(OPERATEUR) %>%
  summarise(
    # Pédagogie (1-5)
    score_pedagogie_moyen = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE), 2),
    # Ambiance (1-3) 
    score_ambiance_moyen = round(mean(as.numeric(AMBIANCE), na.rm = TRUE), 2),
    nombre_evaluations = n(),
    .groups = 'drop'
  )

# 3. Joindre les données et calculer la note globale sur 20
notation_operateurs <- taux_geste_operateur %>%
  left_join(scores_operateur, by = "OPERATEUR") %>%
  filter(!is.na(score_pedagogie_moyen), !is.na(score_ambiance_moyen)) %>%
  mutate(
    # Normaliser chaque composante sur 20/3 (soit environ 6.67 points chacune)
    note_taux_geste = (taux_geste / 100) * (20/3),  # Taux de geste normalisé
    note_pedagogie = ((score_pedagogie_moyen - 1) / (5 - 1)) * (20/3),  # Pédagogie normalisée (1-5 -> 0-6.67)
    note_ambiance = ((score_ambiance_moyen - 1) / (3 - 1)) * (20/3),   # Ambiance normalisée (1-3 -> 0-6.67)
    
    # Note globale = somme des 3 composantes
    note_globale = round(note_taux_geste + note_pedagogie + note_ambiance, 2)
  ) %>%
  # Arrondir les composantes pour plus de lisibilité
  mutate(
    note_taux_geste = round(note_taux_geste, 2),
    note_pedagogie = round(note_pedagogie, 2),
    note_ambiance = round(note_ambiance, 2)
  )

notation_operateurs <- notation_operateurs %>%
  filter(nombre_interventions >= 10)


# 4. Créer les tableaux de classement

# Tableau 1: Classement par note globale
classement_note_globale <- notation_operateurs %>%
  arrange(desc(note_globale)) %>%
  select(OPERATEUR, note_globale, note_taux_geste, note_pedagogie, note_ambiance, 
         taux_geste, score_pedagogie_moyen, score_ambiance_moyen, nombre_interventions) %>%
  mutate(rang_global = row_number())

# Tableau 2: Classement par taux de geste
classement_taux_geste <- notation_operateurs %>%
  arrange(desc(taux_geste)) %>%
  select(OPERATEUR, taux_geste, note_taux_geste, nombre_gestes, nombre_interventions) %>%
  mutate(rang_taux_geste = row_number())

# Tableau 3: Classement par pédagogie
classement_pedagogie <- notation_operateurs %>%
  arrange(desc(score_pedagogie_moyen)) %>%
  select(OPERATEUR, score_pedagogie_moyen, note_pedagogie, nombre_evaluations) %>%
  mutate(rang_pedagogie = row_number())

# Tableau 4: Classement par ambiance
classement_ambiance <- notation_operateurs %>%
  arrange(desc(score_ambiance_moyen)) %>%
  select(OPERATEUR, score_ambiance_moyen, note_ambiance, nombre_evaluations) %>%
  mutate(rang_ambiance = row_number())

# 5. Tableau final consolidé avec tous les classements
tableau_final_operateurs <- notation_operateurs %>%
  left_join(
    classement_note_globale %>% select(OPERATEUR, rang_global), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_taux_geste %>% select(OPERATEUR, rang_taux_geste), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_pedagogie %>% select(OPERATEUR, rang_pedagogie), 
    by = "OPERATEUR"
  ) %>%
  left_join(
    classement_ambiance %>% select(OPERATEUR, rang_ambiance), 
    by = "OPERATEUR"
  ) %>%
  select(
    OPERATEUR, 
    note_globale, rang_global,
    taux_geste, rang_taux_geste,
    score_pedagogie_moyen, rang_pedagogie,
    score_ambiance_moyen, rang_ambiance,
    nombre_interventions, nombre_evaluations
  ) %>%
  arrange(rang_global)

# Affichage des résultats
print("=== TOP 10 - CLASSEMENT GÉNÉRAL ===")
print(tableau_final_operateurs %>% head(10))

print("\n=== TOP 10 - TAUX DE GESTE ===")
print(classement_taux_geste %>% head(10))

print("\n=== TOP 10 - PÉDAGOGIE ===")
print(classement_pedagogie %>% head(10))

print("\n=== TOP 10 - AMBIANCE ===")
print(classement_ambiance %>% head(10))

# Pour accéder aux dataframes :
tableau_final_operateurs
# - classement_note_globale : classement par note globale
# - classement_taux_geste : classement par taux de geste
# - classement_pedagogie : classement par pédagogie  
# - classement_ambiance : classement par ambiance
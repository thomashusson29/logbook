# 1. Correction du nom Gaby -> Gabrielle
df <- df %>%
  mutate(NOM_interne = case_when(
    NOM_interne == "Gaby" ~ "Gabrielle",
    TRUE ~ NOM_interne
  ))

# 2. Attribution des années DES (AVEC LES INTERNES MANQUANTS)
df <- df %>%
  mutate(
    annee_DES = case_when(
      # Années fixes (pas de variation par hôpital)
      NOM_interne == "Alice" ~ 4,
      NOM_interne == "Antoine" ~ 3,
      NOM_interne == "Aubin" ~ 2,
      NOM_interne == "Charlotte" ~ 2,
      NOM_interne == "Chloé" ~ 4,
      NOM_interne == "Clara" ~ 2,
      NOM_interne == "François" ~ 2,
      NOM_interne == "Gabrielle" ~ 3,
      NOM_interne == "Kevin" ~ 4,
      NOM_interne == "Léa" ~ 3,
      NOM_interne == "Marc Anthony" ~ 4,
      NOM_interne == "Marie Amélie" ~ 1,
      NOM_interne == "Mathilde" ~ 2,
      NOM_interne == "Philippine" ~ 1,
      NOM_interne == "Rodolphe" ~ 3,
      
      # Années variables selon hôpital
      NOM_interne == "Thomas" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Thomas" & Hôpital != "HEGP" ~ 3,
      NOM_interne == "Pauline" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Pauline" & Hôpital != "HEGP" ~ 3,
      NOM_interne == "Ghita" & Hôpital == "HEGP" ~ 2,
      NOM_interne == "Ghita" & Hôpital != "HEGP" ~ 3,
      
      # INTERNES MANQUANTS À AJOUTER (mettez les années que vous voulez)
      NOM_interne == "Laya" ~ 1,        # À définir
      NOM_interne == "Edoardo" ~ 2,     # À définir  
      NOM_interne == "Christiana" ~ 3,  # À définir
      NOM_interne == "Ioanna" ~ 2,      # À définir
      NOM_interne == "Bilal" ~ 2,       # À définir
      NOM_interne == "Eymeline" ~ 1,    # À définir
      NOM_interne == "Martina" ~ 3,     # À définir
      NOM_interne == "Mélanie" ~ 2,     # À définir
      NOM_interne == "Sukaynah" ~ 2,    # À définir
      
      TRUE ~ NA_real_
    )
  )

# 3. Statut DES (AVEC TOUS LES INTERNES)
df <- df %>%
  mutate(
    DES = case_when(
      NOM_interne %in% c(
        "Alice", "Antoine", "Aubin", "Charlotte", "Chloé", "Clara", "François",
        "Gabrielle", "Ghita", "Kevin", "Léa", "Marc Anthony", "Marie Amélie",
        "Mathilde", "Pauline", "Philippine", "Rodolphe", "Thomas",
        "Laya", "Edoardo", "Christiana", "Ioanna", "Bilal", 
        "Eymeline", "Martina", "Mélanie", "Sukaynah"
      ) ~ "oui",
      !is.na(NOM_interne) ~ "non",
      TRUE ~ NA_character_
    ),
    DES = factor(DES, levels = c("non", "oui"))
  )

# 4. Groupe socle SIMPLIFIÉ : socle = année 1
df <- df %>%
  mutate(
    groupe_socle = case_when(
      annee_DES == 1 ~ "socle",           # NOUVELLE RÈGLE SIMPLE
      !is.na(annee_DES) ~ "non socle",    # Toutes les autres années DES
      !is.na(NOM_interne) ~ "non socle",  # Non-DES
      TRUE ~ NA_character_
    ),
    groupe_socle = factor(groupe_socle, levels = c("non socle", "socle"))
  )

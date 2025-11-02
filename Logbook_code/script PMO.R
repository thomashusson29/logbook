# =====================================================================
# ANALYSE DES GESTES AVEC CHOIX MULTIPLES POUR PMO
# =====================================================================

library(dplyr)
library(stringr)
library(purrr)

# === FONCTION POUR ANALYSER TOUS LES GESTES (CHOIX MULTIPLES) ===

analyser_gestes_choix_multiples <- function(intervention_nom) {
  
  cat("=== ANALYSE AVEC CHOIX MULTIPLES POUR:", intervention_nom, "===\n")
  
  # Récupérer les données
  data_intervention <- df %>%
    filter(INTERVENTION_GROUPÉE == intervention_nom) %>%
    filter(Geste == "Yes") %>%
    filter(!is.na(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout))
  
  cat("Nombre total d'interventions avec geste:", nrow(data_intervention), "\n")
  
  # Fonction pour extraire tous les gestes individuels
  extraire_gestes <- function(geste_string) {
    if (is.na(geste_string)) return(character(0))
    
    # Séparer par les virgules et nettoyer
    gestes_separes <- str_split(geste_string, ",")[[1]] %>%
      str_trim() %>%
      str_to_lower()
    
    return(gestes_separes)
  }
  
  # Extraire tous les gestes individuels
  tous_gestes <- data_intervention %>%
    pull(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout) %>%
    map(extraire_gestes) %>%
    unlist()
  
  # Créer les catégories selon l'intervention
  if (intervention_nom == "Prélèvement multi-organes") {
    # Catégories spécifiques PMO
    categoriser_gestes <- function(geste) {
      case_when(
        str_detect(geste, "temps froid") ~ "Conservation (temps froid)",
        str_detect(geste, "temps chaud") ~ "Prélèvement (temps chaud)", 
        str_detect(geste, "canule vmi|vmi") ~ "Canulation VMI",
        str_detect(geste, "libération foie|foie droit") ~ "Mobilisation hépatique",
        str_detect(geste, "pédicule") ~ "Ligature pédicules",
        str_detect(geste, "dissection") ~ "Dissection",
        str_detect(geste, "cholécystectomie") ~ "Cholécystectomie",
        str_detect(geste, "tout") ~ "Procédure complète",
        str_detect(geste, "paroi") ~ "Paroi",
        str_detect(geste, "canulation vaisseaux") ~ "Canulation vasculaire",
        str_detect(geste, "controle") ~ "Contrôle",
        str_detect(geste, "ouverture") ~ "Ouverture",
        TRUE ~ paste0("Autre: ", geste)
      )
    }
  } else {
    # Catégories générales pour autres interventions
    categoriser_gestes <- function(geste) {
      case_when(
        str_detect(geste, "tout") ~ "Tout",
        str_detect(geste, "anastomose") ~ "Anastomose (+)",
        str_detect(geste, "dissection") ~ "Dissection (+)",  
        str_detect(geste, "paroi|incision|fermeture") ~ "Paroi (+)",
        TRUE ~ "Autre"
      )
    }
  }
  
  # Compter les gestes
  resultat_gestes <- tibble(geste = tous_gestes) %>%
    mutate(categorie_geste = map_chr(geste, categoriser_gestes)) %>%
    count(categorie_geste, sort = TRUE) %>%
    mutate(
      pourcentage_mentions = round(100 * n / sum(n), 1),
      pourcentage_interventions = round(100 * n / nrow(data_intervention), 1),
      label = paste0(categorie_geste, " (", pourcentage_interventions, "% des interventions)")
    )
  
  cat("\nGestes par fréquence (% des interventions):\n")
  print(resultat_gestes)
  
  # Retourner le geste principal
  return(list(
    intervention = intervention_nom,
    geste_principal = resultat_gestes$categorie_geste[1],
    pourcentage = resultat_gestes$pourcentage_interventions[1],
    detail = resultat_gestes
  ))
}

# === ANALYSE SPÉCIFIQUE PMO ===

resultat_pmo <- analyser_gestes_choix_multiples("Prélèvement multi-organes")

cat("\n🎯 RÉSULTAT FINAL POUR PMO:\n")
cat("Geste le plus fréquent:", resultat_pmo$geste_principal, "(", resultat_pmo$pourcentage, "% des interventions)\n")

# === RÉSUMÉ FINAL CORRIGÉ DU TOP 5 ===

cat("\n📊 TOP 5 CORRIGÉ AVEC ANALYSE CHOIX MULTIPLES:\n")
cat("=====================================\n")
cat("1. Appendicectomie (coelio) → Tout (86.4%)\n")
cat("2. RIC (laparo) → Tout (62.5%)\n")  
cat("3. Cure d'éventration → Dissection (+) (53.6%)\n")
cat("4. Prélèvement multi-organes → Mobilisation hépatique (50%)\n")
cat("5. Pancreatectomie céphalique DPC/DPT → Anastomose (+) (79.2%)\n")

cat("\n🔍 INSIGHTS POUR PMO:\n")
cat("• Mobilisation hépatique: 50% (geste principal)\n")
cat("• Canulation VMI: 37.5% (très fréquent)\n") 
cat("• Prélèvement temps chaud: 34.4%\n")
cat("• Dissection: 31.2%\n")
cat("• Conservation temps froid: 25%\n")

cat("\n💡 CONCLUSION:\n")
cat("Les PMO combinent plusieurs gestes spécialisés dans une même intervention.\n")
cat("La mobilisation hépatique est le geste le plus fréquent, mais la canulation VMI\n")
cat("et les temps chaud/froid sont aussi très représentés, confirmant la complexité\n")
cat("et la spécificité technique du prélèvement multi-organes.\n")

# === FONCTION GÉNÉRIQUE POUR AUTRES INTERVENTIONS ===

analyser_intervention_choix_multiples <- function(nom_intervention) {
  # Cette fonction peut être utilisée pour analyser n'importe quelle intervention
  # avec la même logique de choix multiples
  return(analyser_gestes_choix_multiples(nom_intervention))
}

# Exemple d'usage pour d'autres interventions du top 5:
# analyser_intervention_choix_multiples("Appendicectomie (coelio)")
# analyser_intervention_choix_multiples("Cure d'éventration")
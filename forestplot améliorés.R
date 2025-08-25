# =====================================================================
# FOREST PLOTS STYLISÉS ET AMÉLIORÉS POUR SFCD
# =====================================================================

library(forestplot)
library(broom)
library(dplyr)
library(RColorBrewer)

# === FONCTION AMÉLIORÉE POUR PRÉPARER LES DONNÉES ===
prep_forest_data_enhanced <- function(model, var_labels, add_pvalues = TRUE) {
  
  model_data <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      label = recode(term, !!!var_labels),
      label = ifelse(is.na(label), term, label)
    )
  
  if (add_pvalues) {
    model_data <- model_data %>%
      mutate(
        OR_formatted = sprintf("%.2f", estimate),
        IC_formatted = paste0("[", sprintf("%.2f", conf.low), "; ", sprintf("%.2f", conf.high), "]"),
        p_formatted = case_when(
          p.value < 0.001 ~ "< 0.001",
          p.value < 0.01 ~ sprintf("%.3f", p.value),
          TRUE ~ sprintf("%.2f", p.value)
        ),
        significance = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01 ~ "**", 
          p.value < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
  } else {
    model_data <- model_data %>%
      mutate(
        OR_formatted = sprintf("%.2f", estimate),
        IC_formatted = paste0("[", sprintf("%.2f", conf.low), "; ", sprintf("%.2f", conf.high), "]")
      )
  }
  
  return(model_data)
}

# === FONCTION POUR CRÉER UN FOREST PLOT STYLISÉ ===
create_stylized_forestplot <- function(model, var_labels, title, subtitle = NULL, 
                                       theme_color = "blue", add_pvalues = TRUE) {
  
  # Préparer les données
  df_fp <- prep_forest_data_enhanced(model, var_labels, add_pvalues)
  
  # Définir les couleurs selon le thème
  color_schemes <- list(
    blue = list(
      box = "#2E86AB", 
      lines = "#2E86AB", 
      zero = "#A23B72",
      bg_header = "#F18F01",
      text_header = "white"
    ),
    green = list(
      box = "#27AE60", 
      lines = "#27AE60", 
      zero = "#E74C3C",
      bg_header = "#3498DB",
      text_header = "white"
    ),
    purple = list(
      box = "#8E44AD", 
      lines = "#8E44AD", 
      zero = "#F39C12",
      bg_header = "#E67E22",
      text_header = "white"
    )
  )
  
  colors <- color_schemes[[theme_color]]
  
  # Créer le tableau de texte avec ou sans p-values
  if (add_pvalues) {
    tabletext <- cbind(
      c("Variable", df_fp$label),
      c("OR", df_fp$OR_formatted),
      c("IC 95%", df_fp$IC_formatted),
      c("p-value", df_fp$p_formatted)
    )
    
    # Ajouter les en-têtes stylisés
    header_style <- fpTxtGp(
      label = gpar(fontface = "bold", cex = 1.1, col = colors$text_header),
      ticks = gpar(fontface = "bold", cex = 0.9),
      xlab = gpar(fontface = "bold", cex = 1)
    )
  } else {
    tabletext <- cbind(
      c("Variable", df_fp$label),
      c("OR", df_fp$OR_formatted),
      c("IC 95%", df_fp$IC_formatted)
    )
  }
  
  # Préparer les données numériques (ajouter NA pour la ligne d'en-tête)
  means <- c(NA, df_fp$estimate)
  lower <- c(NA, df_fp$conf.low)
  upper <- c(NA, df_fp$conf.high)
  
  # Créer le forest plot stylisé
  fp <- forestplot(
    labeltext = tabletext,
    mean = means,
    lower = lower,
    upper = upper,
    
    # Configuration de base
    zero = 1,
    xlog = TRUE,
    
    # Titre et labels
    title = title,
    subtitle = subtitle,
    xlab = "Odds Ratio (échelle logarithmique)",
    
    # Style des éléments
    ci.vertices = TRUE,
    ci.vertices.height = 0.3,
    boxsize = 0.3,
    lwd.ci = 2.5,
    lwd.zero = 2,
    
    # Couleurs personnalisées
    col = fpColors(
      box = colors$box,
      lines = colors$lines, 
      zero = colors$zero
    ),
    
    # Configuration du texte
    txt_gp = fpTxtGp(
      label = gpar(fontface = "bold", cex = 1),
      ticks = gpar(fontface = "bold", cex = 0.9),
      xlab = gpar(fontface = "bold", cex = 1.1),
      title = gpar(fontface = "bold", cex = 1.3)
    ),
    
    # Grille et axes
    grid = structure(c(0.5, 1, 2, 4), 
                     gp = gpar(lty = 2, col = "grey70")),
    
    # Marges et espacement
    mar = unit(rep(5, 4), "mm"),
    
    # En-têtes de colonnes
    hrzl_lines = list(
      "1" = gpar(lwd = 2, col = colors$bg_header),
      "2" = gpar(lwd = 1.5, col = "grey60")
    ),
    
    # Style de l'arrière-plan des en-têtes
    clip = c(-0.3, 0.3),
    
    # Configuration des axes
    xticks = c(0.25, 0.5, 1, 2, 4, 8),
    
    # Ligne de référence améliorée
    lwd.zero = 2,
    zero = 1
  )
  
  return(fp)
}

# === APPLICATION AUX MODÈLES EXISTANTS ===

# Modèle 1 : Effet de Geste sur ambiance 'on recommence'
labels_ambi1 <- c(
  "GesteYes" = "Geste réalisé : Oui vs Non",
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde", 
  "PEDAGOGIE_grouped4-5" = "Pédagogie 4-5 vs 1-2",
  "RANG_BOSS_groupedCCA" = "Boss CCA vs autres"
)

# Forest plot stylisé 1
cat("=== FOREST PLOT 1 : EFFET GESTE SUR AMBIANCE ===\n")
fp1 <- create_stylized_forestplot(
  model = model_ambi,
  var_labels = labels_ambi1,
  title = "Facteurs associés à une ambiance 'on recommence'",
  subtitle = "Modèle 1 : Effet du geste réalisé",
  theme_color = "blue",
  add_pvalues = TRUE
)

print(fp1)

# Modèle 2 : Effet du type de geste sur ambiance 'on recommence'
labels_ambi2 <- c(
  "GESTE_SIMPLEGros" = "Gros vs Petit geste",
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde",
  "PEDAGOGIE_grouped4-5" = "Pédagogie 4-5 vs 1-2", 
  "RANG_BOSS_groupedCCA" = "Boss CCA vs autres"
)

# Forest plot stylisé 2
cat("\n=== FOREST PLOT 2 : EFFET TYPE GESTE SUR AMBIANCE ===\n")
fp2 <- create_stylized_forestplot(
  model = model_ambi2,
  var_labels = labels_ambi2,
  title = "Facteurs associés à une ambiance 'on recommence'",
  subtitle = "Modèle 2 : Effet du type de geste",
  theme_color = "green",
  add_pvalues = TRUE
)

print(fp2)

# Modèle 3 : Facteurs associés à une pédagogie perçue élevée
labels_pedago <- c(
  "INTERNE_SENIORITE3e et 4e année" = "3e & 4e année vs 1-2",
  "OPERATEUR_STATUTJunior" = "Junior vs Senior", 
  "Garde_ProgrammeProgrammé" = "Programmé vs Garde",
  "Geste_YNOui" = "Geste réalisé : Oui vs Non"
)

# Forest plot stylisé 3
cat("\n=== FOREST PLOT 3 : FACTEURS PÉDAGOGIE ÉLEVÉE ===\n")
fp3 <- create_stylized_forestplot(
  model = modele_ambiance_pool,  # ou le bon nom de votre modèle
  var_labels = labels_pedago,
  title = "Facteurs associés à une pédagogie perçue élevée",
  subtitle = "Analyse multivariée",
  theme_color = "purple",
  add_pvalues = TRUE
)

print(fp3)

# === FONCTION POUR SAUVEGARDER LES PLOTS ===
save_forestplots <- function(plot, filename, width = 12, height = 8) {
  # Sauvegarder en PNG haute résolution
  png(filename = paste0(filename, ".png"), 
      width = width, height = height, units = "in", 
      res = 300, type = "cairo")
  print(plot)
  dev.off()
  
  # Sauvegarder en PDF pour publications
  pdf(file = paste0(filename, ".pdf"), 
      width = width, height = height)
  print(plot)
  dev.off()
  
  cat("Forest plot sauvegardé :", filename, "\n")
}

# Sauvegarder les plots (optionnel)
# save_forestplots(fp1, "forestplot_ambiance_geste")
# save_forestplots(fp2, "forestplot_ambiance_type_geste") 
# save_forestplots(fp3, "forestplot_pedagogie")

# === VARIANTE AVEC GGPLOT POUR ENCORE PLUS DE CONTRÔLE ===

library(ggplot2)
library(ggrepel)

create_ggplot_forestplot <- function(model, var_labels, title) {
  
  df_fp <- prep_forest_data_enhanced(model, var_labels, add_pvalues = TRUE)
  
  # Créer le plot ggplot
  p <- ggplot(df_fp, aes(y = reorder(label, estimate))) +
    
    # Ligne de référence (OR = 1)
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", size = 1) +
    
    # Intervalles de confiance
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                   height = 0.3, size = 1.2, color = "#2E86AB") +
    
    # Points des OR
    geom_point(aes(x = estimate), size = 4, color = "#2E86AB", shape = 18) +
    
    # Annotations des valeurs
    geom_text(aes(x = conf.high + 0.3, label = paste0("OR: ", OR_formatted)), 
              hjust = 0, size = 3.5, fontface = "bold") +
    
    # Échelle logarithmique
    scale_x_log10(breaks = c(0.25, 0.5, 1, 2, 4, 8),
                  labels = c("0.25", "0.5", "1", "2", "4", "8")) +
    
    # Labels et titre
    labs(
      title = title,
      x = "Odds Ratio (échelle logarithmique)",
      y = "Variables"
    ) +
    
    # Thème personnalisé
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 11),
      axis.text.y = element_text(size = 11, face = "bold"),
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    
    # Ajuster les limites
    coord_cartesian(xlim = c(0.2, max(df_fp$conf.high) * 1.5))
  
  return(p)
}

# Exemple d'utilisation avec ggplot
# ggplot_fp1 <- create_ggplot_forestplot(model_ambi, labels_ambi1, 
#                                        "Facteurs associés à une ambiance 'on recommence'")
# print(ggplot_fp1)

cat("\n🎨 FOREST PLOTS STYLISÉS CRÉÉS AVEC SUCCÈS !\n")
cat("✅ 3 thèmes de couleurs disponibles (blue, green, purple)\n")
cat("✅ P-values ajoutées automatiquement\n") 
cat("✅ Grille et axes améliorés\n")
cat("✅ Titres et sous-titres personnalisables\n")
cat("✅ Version ggplot alternative disponible\n")
cat("✅ Fonction de sauvegarde incluse\n")

# --- Chargement des librairies ---
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)   # pour month(), ymd(), interval(), %/%

# --- Lecture des données ---
df_shiny <- readRDS("logbook_data.rds")

# --- Données et prétraitements ---
df_shiny <- df_shiny %>%
  mutate(
    DATE = as.Date(DATE),
    mois_du_semestre = case_when(
      month(DATE) %in% 5:10 ~ interval(
        ymd(paste0(year(DATE), "-05-01")),
        DATE
      ) %/% months(1),
      month(DATE) %in% c(11,12,1,2,3,4) ~ interval(
        ymd(paste0(
          ifelse(month(DATE) < 5, year(DATE) - 1, year(DATE)),
          "-11-01"
        )),
        DATE
      ) %/% months(1),
      TRUE ~ NA_integer_
    ),
    geste_realise = ifelse(Geste == "Yes", 1, 0),
    on_recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    seniorite = case_when(
      RANG_BOSS %in% c("PH", "MCU", "PU") ~ "Senior",
      RANG_BOSS %in% c("CCA", "DJ")       ~ "Junior",
      TRUE                                ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(mois_du_semestre),
    !is.na(INTERVENTION_GROUPÉE),
    !is.na(Garde_Programme),
    !is.na(Hôpital),
    !is.na(geste_realise),
    !is.na(on_recommence)
  ) %>%
  mutate(
    INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE),
    Garde_Programme      = factor(Garde_Programme),
    seniorite            = factor(seniorite, levels = c("Junior", "Senior"))
  )

# --- Construction des modèles ---
modele_geste_pool <- glm(
  geste_realise ~ INTERVENTION_GROUPÉE + mois_du_semestre +
    Garde_Programme + seniorite + annee_DES,
  data   = df_shiny,
  family = binomial()
)

modele_ambiance_pool <- glm(
  on_recommence ~ INTERVENTION_GROUPÉE + mois_du_semestre +
    Garde_Programme + seniorite + annee_DES,
  data   = df_shiny,
  family = binomial()
)

# --- Fonction prédictive (hors Shiny) ---
predict_calculateur <- function(intervention,
                                mois_du_semestre,
                                garde_programme,
                                seniorite,
                                annee_DES) {
  new_data <- data.frame(
    INTERVENTION_GROUPÉE = factor(intervention,
                                  levels = levels(df_shiny$INTERVENTION_GROUPÉE)
    ),
    mois_du_semestre = mois_du_semestre,
    Garde_Programme  = factor(garde_programme,
                              levels = levels(df_shiny$Garde_Programme)
    ),
    seniorite = factor(seniorite,
                       levels = levels(df_shiny$seniorite)
    ),
    annee_DES = annee_DES
  )
  proba_geste <- predict(modele_geste_pool,
                         newdata = new_data,
                         type    = "response")
  proba_bloc  <- predict(modele_ambiance_pool,
                         newdata = new_data,
                         type    = "response")
  list(
    proba_geste              = round(100 * proba_geste, 1),
    proba_bloc_on_recommence = round(100 * proba_bloc, 1)
  )
}

# --- Interface utilisateur ---
ui <- fluidPage(
  titlePanel("Calculateur de probabilité (Geste / On recommence)"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "intervention",
        "Intervention :",
        choices = levels(df_shiny$INTERVENTION_GROUPÉE)
      ),
      numericInput(
        "mois",
        "Mois dans le semestre :",
        value = 3, min = 0, max = 6
      ),
      selectInput(
        "garde",
        "Type d'intervention :",
        choices = levels(df_shiny$Garde_Programme)
      ),
      selectInput(
        "seniorite",
        "Rang du senior (Junior = CCA ou DJ) :",
        choices = levels(df_shiny$seniorite)
      ),
      numericInput(
        "annee_DES",
        "Ancienneté (année d'internat) :",
        value = 2, min = 1, max = 6
      ),
      actionButton("calculer", "Calculer !"),
      br(), br(),
      actionButton("quitter", "Quitter l'application")
    ),
    mainPanel(
      tableOutput("tableau_distribution"),
      br(),
      uiOutput("note_intervention"),
      br(),
      verbatimTextOutput("resultats"),
      br(),
      plotOutput("histogramme_pred"),
      p("Position de l’intervention sélectionnée (ligne rouge) parmi la distribution prédite de 'on recommence'."),
      br(),
      plotOutput("histogramme_geste"),
      p("Position de l’intervention sélectionnée (ligne rouge) parmi la distribution prédite de 'faire un geste'."),
      br(),
      plotOutput("courbe_calibration"),
      p("Courbe de calibration : ‘on recommence’."),
      br(),
      plotOutput("courbe_calibration_geste"),
      p("Courbe de calibration : ‘faire un geste’."),
      br(),
      plotOutput("scatter_plot"),
      p("Taux observé de 'on recommence' selon les variables explicatives.")
    )
  )
)

# --- Serveur ---
server <- function(input, output, session) {
  
  observeEvent(input$calculer, {
    # Préparation des données
    new_data <- data.frame(
      INTERVENTION_GROUPÉE = factor(input$intervention,
                                    levels = levels(df_shiny$INTERVENTION_GROUPÉE)
      ),
      mois_du_semestre = input$mois,
      Garde_Programme  = factor(input$garde,
                                levels = levels(df_shiny$Garde_Programme)
      ),
      seniorite = factor(input$seniorite,
                         levels = levels(df_shiny$seniorite)
      ),
      annee_DES = input$annee_DES
    )
    
    # Prédictions
    proba_geste <- predict(
      modele_geste_pool,
      newdata = new_data,
      type    = "response"
    )
    proba_bloc  <- predict(
      modele_ambiance_pool,
      newdata = new_data,
      type    = "response"
    )
    
    # Affichage des résultats textuels
    output$resultats <- renderPrint({
      cat(
        "Probabilité de faire un geste :",
        round(100 * proba_geste, 1), "%\n\n",
        "Probabilité de 'on recommence' :",
        round(100 * proba_bloc, 1), "%"
      )
    })
    
    # Comptage sans NA pour éviter if(NA < ...)
    n_intervention <- sum(
      df_shiny$INTERVENTION_GROUPÉE == input$intervention,
      na.rm = TRUE
    )
    
    # Calcul des marges d'erreur
    marge_geste <- 1.96 * sqrt(proba_geste * (1 - proba_geste) / n_intervention)
    marge_bloc  <- 1.96 * sqrt(proba_bloc  * (1 - proba_bloc ) / n_intervention)
    
    # Choix de la couleur et du texte de fiabilité
    couleur <- if (n_intervention < 15) {
      "#FFCCCC"
    } else if (n_intervention < 35) {
      "#FFE5B4"
    } else {
      "#D9F2D9"
    }
    fiabilite <- if (n_intervention < 15) {
      "Fiabilité faible"
    } else if (n_intervention < 35) {
      "Fiabilité intermédiaire"
    } else {
      "Fiabilité élevée"
    }
    
    # Affichage de la note de fiabilité
    output$note_intervention <- renderUI({
      HTML(paste0(
        "<div style='padding:10px; border-radius:5px; background-color:",
        couleur, ";'>",
        "<strong>", fiabilite, "</strong> (", n_intervention, " rappels)<br/>",
        "• Geste : ±", round(100 * marge_geste, 1), "%<br/>",
        "• On recommence : ±", round(100 * marge_bloc, 1), "%",
        "</div>"
      ))
    })
    
    # Tableau de distribution des modalités
    output$tableau_distribution <- renderTable({
      data_filtre <- df_shiny %>%
        filter(INTERVENTION_GROUPÉE == input$intervention)
      annee_levels <- sort(unique(df_shiny$annee_DES))
      
      Variable <- c(
        "Intervention",
        rep("Mois du semestre", 6),
        rep("Type d’intervention", length(levels(df_shiny$Garde_Programme))),
        rep("Rang du senior",    length(levels(df_shiny$seniorite))),
        rep("Année d’internat",  length(annee_levels))
      )
      Valeur <- c(
        input$intervention,
        as.character(1:6),
        levels(df_shiny$Garde_Programme),
        levels(df_shiny$seniorite),
        as.character(annee_levels)
      )
      Nombre <- c(
        nrow(data_filtre),
        sapply(1:6, function(m) sum(data_filtre$mois_du_semestre == m)),
        sapply(levels(df_shiny$Garde_Programme),
               function(g) sum(data_filtre$Garde_Programme == g)),
        sapply(levels(df_shiny$seniorite),
               function(s) sum(data_filtre$seniorite == s)),
        sapply(annee_levels,
               function(a) sum(data_filtre$annee_DES == a))
      )
      tibble(
        Variable = Variable,
        Valeur   = Valeur,
        `N interventions` = Nombre
      )
    })
    
    # Histogramme des prédictions "on recommence"
    output$histogramme_pred <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          pred = predict(modele_ambiance_pool, newdata = ., type = "response")
        )
      ggplot(df_temp, aes(x = pred)) +
        geom_histogram(bins = 30, color = "black", fill = "#0072B2", alpha = 0.6) +
        geom_vline(xintercept = proba_bloc, color = "red", size = 1.2) +
        labs(
          title = "Distribution des prédictions (on recommence)",
          x = "Probabilité prédite", y = "Effectif"
        )
    })
    
    # Histogramme des prédictions "geste"
    output$histogramme_geste <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          pred = predict(modele_geste_pool, newdata = ., type = "response")
        )
      ggplot(df_temp, aes(x = pred)) +
        geom_histogram(bins = 30, color = "black", fill = "#009E73", alpha = 0.6) +
        geom_vline(xintercept = proba_geste, color = "red", size = 1.2) +
        labs(
          title = "Distribution des prédictions (faire un geste)",
          x = "Probabilité prédite", y = "Effectif"
        )
    })
    
    # Courbe de calibration "on recommence"
    output$courbe_calibration <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(pred = predict(modele_ambiance_pool, newdata = ., type = "response")) %>%
        mutate(bin = ntile(pred, 10)) %>%
        group_by(bin) %>%
        summarize(
          pred_moy = mean(pred),
          obs_moy  = mean(on_recommence),
          .groups  = "drop"
        )
      ggplot(df_temp, aes(x = pred_moy, y = obs_moy)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_point(size = 3) +
        geom_line() +
        labs(
          title = "Calibration du modèle (on recommence)",
          x = "Probabilité prédite", y = "Taux observé"
        ) +
        xlim(0, 1) + ylim(0, 1)
    })
    
    # Courbe de calibration "geste"
    output$courbe_calibration_geste <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(pred = predict(modele_geste_pool, newdata = ., type = "response")) %>%
        mutate(bin = ntile(pred, 10)) %>%
        group_by(bin) %>%
        summarize(
          pred_moy = mean(pred),
          obs_moy  = mean(geste_realise),
          .groups  = "drop"
        )
      ggplot(df_temp, aes(x = pred_moy, y = obs_moy)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_point(size = 3) +
        geom_line() +
        labs(
          title = "Calibration du modèle (faire un geste)",
          x = "Probabilité prédite", y = "Taux observé"
        ) +
        xlim(0, 1) + ylim(0, 1)
    })
    
    # Scatter plot des taux observés
    output$scatter_plot <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          annee_DES       = as.factor(annee_DES),
          mois_du_semestre = as.factor(mois_du_semestre)
        ) %>%
        pivot_longer(
          cols = c(Garde_Programme, seniorite, mois_du_semestre, annee_DES),
          names_to  = "variable",
          values_to = "valeur"
        ) %>%
        group_by(variable, valeur) %>%
        summarize(
          taux_on_recommence = mean(on_recommence) * 100,
          .groups = "drop"
        )
      ggplot(df_temp, aes(x = valeur, y = taux_on_recommence)) +
        geom_point(size = 3) +
        facet_wrap(~ variable, scales = "free_x") +
        geom_hline(
          yintercept = mean(df_shiny$on_recommence) * 100,
          linetype   = "dashed"
        ) +
        labs(
          title = "Taux de 'on recommence' selon les variables",
          x = "", y = "Taux (%)"
        )
    })
  })
  
  # Bouton Quitter
  observeEvent(input$quitter, {
    stopApp()
  })
}

# --- Lancement de l'application Shiny ---
shinyApp(ui = ui, server = server)


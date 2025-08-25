# Packages
library(dplyr)
library(lubridate)
library(broom)
library(shiny)
library(ggplot2)

# --- Données et prétraitements ---
df_shiny <- df %>%
  mutate(DATE = as.Date(DATE)) %>%
  mutate(
    mois_du_semestre = case_when(
      month(DATE) %in% 5:10 ~ interval(ymd(paste0(year(DATE), "-05-01")), DATE) %/% months(1),
      month(DATE) %in% c(11,12,1,2,3,4) ~ interval(ymd(paste0(ifelse(month(DATE) < 5, year(DATE)-1, year(DATE)), "-11-01")), DATE) %/% months(1),
      TRUE ~ NA_integer_
    ),
    geste_realise = ifelse(Geste == "Yes", 1, 0),
    on_recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
    seniorite = case_when(
      RANG_BOSS %in% c("PH", "MCU", "PU") ~ "Senior",
      RANG_BOSS %in% c("CCA", "DJ") ~ "Junior",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(mois_du_semestre),
    !is.na(INTERVENTION),
    !is.na(Garde_Programme),
    !is.na(Hôpital),
    !is.na(geste_realise),
    !is.na(on_recommence)
  ) %>%
  mutate(
    INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE),
    Garde_Programme = factor(Garde_Programme),
    seniorite = factor(seniorite, levels = c("Junior", "Senior"))
  )

df_shiny$seniorite <- factor(df_shiny$seniorite, levels = c("Junior", "Senior"))

# --- Modèles ---
modele_geste_pool <- glm(
  geste_realise ~ INTERVENTION_GROUPÉE + mois_du_semestre + Garde_Programme + seniorite + annee_DES,
  data = df_shiny,
  family = binomial()
)

modele_ambiance_pool <- glm(
  on_recommence ~ INTERVENTION_GROUPÉE + mois_du_semestre + Garde_Programme + seniorite + annee_DES,
  data = df_shiny,
  family = binomial()
)

# --- Fonction prédictive ---
predict_calculateur <- function(intervention, mois_du_semestre, garde_programme, seniorite, annee_DES) {
  new_data <- data.frame(
    INTERVENTION_GROUPÉE = factor(intervention, levels = levels(df_shiny$INTERVENTION_GROUPÉE)),
    mois_du_semestre = mois_du_semestre,
    Garde_Programme = factor(garde_programme, levels = levels(df_shiny$Garde_Programme)),
    seniorite = factor(seniorite, levels = levels(df_shiny$seniorite)),
    annee_DES = annee_DES
  )
  
  proba_geste <- predict(modele_geste_pool, newdata = new_data, type = "response")
  proba_bloc <- predict(modele_ambiance_pool, newdata = new_data, type = "response")
  
  list(
    proba_geste = round(100 * proba_geste, 1),
    proba_bloc_on_recommence = round(100 * proba_bloc, 1)
  )
}

# --- Interface utilisateur ---
library(shiny)
library(ggplot2)
library(dplyr)

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Calculateur de probabilité (Geste / On recommence)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("intervention", "Intervention :", choices = levels(df_shiny$INTERVENTION_GROUPÉE)),
      numericInput("mois", "Mois dans le semestre :", value = 3, min = 0, max = 6),
      selectInput("garde", "Type d'intervention :", choices = levels(df_shiny$Garde_Programme)),
      selectInput("seniorite", "Rang du senior (Junior = CCA ou DJ) :", choices = levels(df_shiny$seniorite)),
      numericInput("annee_DES", "Ancienneté (année d'internat) :", value = 2, min = 1, max = 6),
      actionButton("calculer", "Calculer !"),
      br(), br(),
      actionButton("quitter", "Quitter l'application")
    ),
    mainPanel(
      tableOutput("tableau_distribution"),
      br(),
      verbatimTextOutput("resultats"),
      br(),
      plotOutput("histogramme_pred"),
      p("Position de l’intervention sélectionnée (ligne rouge) parmi la distribution prédite de 'on recommence', pour l’ensemble des interventions selon les variables incluses dans le modèle multivarié."),
      br(),
      plotOutput("histogramme_geste"),
      p("Position de l’intervention sélectionnée (ligne rouge) parmi la distribution prédite de 'faire un geste', pour l’ensemble des interventions selon les variables incluses dans le modèle multivarié."),
      br(),
      plotOutput("courbe_calibration"),
      p("Courbe de calibration : comparaison entre la probabilité prédite et le taux observé de 'on recommence' (points). La diagonale représente la calibration idéale."),
      br(),
      plotOutput("courbe_calibration_geste"),
      p("Courbe de calibration : comparaison entre la probabilité prédite et le taux observé de 'faire un geste' (points). La diagonale représente la calibration idéale."),
      br(),
      plotOutput("scatter_plot"),
      p("Taux observé de 'on recommence' selon chaque variable explicative (barres horizontales = moyenne globale).")
    )
  )
)


# Serveur
server <- function(input, output, session) {
  
  observeEvent(input$calculer, {
    new_data <- data.frame(
      INTERVENTION_GROUPÉE = factor(input$intervention, levels = levels(df_shiny$INTERVENTION_GROUPÉE)),
      mois_du_semestre = input$mois,
      Garde_Programme = factor(input$garde, levels = levels(df_shiny$Garde_Programme)),
      seniorite = factor(input$seniorite, levels = levels(df_shiny$seniorite)),
      annee_DES = input$annee_DES
    )
    
    proba_geste <- predict(modele_geste_pool, newdata = new_data, type = "response")
    proba_bloc <- predict(modele_ambiance_pool, newdata = new_data, type = "response")
    
    output$resultats <- renderPrint({
      cat("Probabilité de faire un geste :", round(100 * proba_geste, 1), "%\n\n")
      cat("Probabilité de 'on recommence' :", round(100 * proba_bloc, 1), "%")
    })
    
    output$tableau_distribution <- renderTable({
      tibble(
        Variable = c(
          "Intervention",
          rep("Mois du semestre", 6),
          rep("Type d’intervention", length(levels(df_shiny$Garde_Programme))),
          rep("Rang du senior", length(levels(df_shiny$seniorite))),
          rep("Année d’internat", length(unique(df_shiny$annee_DES)))
        ),
        Valeur = c(
          input$intervention,
          as.character(1:6),
          levels(df_shiny$Garde_Programme),
          levels(df_shiny$seniorite),
          as.character(sort(unique(df_shiny$annee_DES)))
        ),
        `Nombre d'interventions correspondantes` = c(
          sum(df_shiny$INTERVENTION_GROUPÉE == input$intervention),
          sapply(1:6, function(m) sum(df_shiny$mois_du_semestre == m)),
          sapply(levels(df_shiny$Garde_Programme), function(g) sum(df_shiny$Garde_Programme == g)),
          sapply(levels(df_shiny$seniorite), function(s) sum(df_shiny$seniorite == s)),
          sapply(sort(unique(df_shiny$annee_DES)), function(a) sum(df_shiny$annee_DES == a))
        )
      )
    })
    
    output$histogramme_pred <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE, levels = levels(model.frame(modele_ambiance_pool)$INTERVENTION_GROUPÉE)),
          Garde_Programme = factor(Garde_Programme, levels = levels(model.frame(modele_ambiance_pool)$Garde_Programme)),
          seniorite = factor(seniorite, levels = levels(model.frame(modele_ambiance_pool)$seniorite))
        ) %>%
        mutate(pred = predict(modele_ambiance_pool, newdata = ., type = "response"))
      
      ggplot(df_temp, aes(x = pred)) +
        geom_histogram(bins = 30, fill = "#0072B2", alpha = 0.6, color = "black") +
        geom_vline(xintercept = proba_bloc, color = "red", size = 1.2) +
        labs(title = "Distribution des prédictions du modèle (on recommence)",
             x = "Probabilité prédite de 'on recommence'", y = "Nombre d’interventions")
    })
    
    output$histogramme_geste <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE, levels = levels(model.frame(modele_geste_pool)$INTERVENTION_GROUPÉE)),
          Garde_Programme = factor(Garde_Programme, levels = levels(model.frame(modele_geste_pool)$Garde_Programme)),
          seniorite = factor(seniorite, levels = levels(model.frame(modele_geste_pool)$seniorite))
        ) %>%
        mutate(pred = predict(modele_geste_pool, newdata = ., type = "response"))
      
      ggplot(df_temp, aes(x = pred)) +
        geom_histogram(bins = 30, fill = "#009E73", alpha = 0.6, color = "black") +
        geom_vline(xintercept = proba_geste, color = "red", size = 1.2) +
        labs(title = "Distribution des prédictions du modèle (geste)",
             x = "Probabilité prédite de faire un geste", y = "Nombre d’interventions")
    })
    
    output$courbe_calibration <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE, levels = levels(model.frame(modele_ambiance_pool)$INTERVENTION_GROUPÉE)),
          Garde_Programme = factor(Garde_Programme, levels = levels(model.frame(modele_ambiance_pool)$Garde_Programme)),
          seniorite = factor(seniorite, levels = levels(model.frame(modele_ambiance_pool)$seniorite))
        ) %>%
        mutate(pred = predict(modele_ambiance_pool, newdata = ., type = "response")) %>%
        filter(!is.na(pred)) %>%
        mutate(bin = ntile(pred, 10)) %>%
        group_by(bin) %>%
        summarise(pred_moy = mean(pred), obs_moy = mean(on_recommence)) %>%
        ungroup()
      
      ggplot(df_temp, aes(x = pred_moy, y = obs_moy)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
        geom_point(size = 3, color = "#E69F00") +
        geom_line(linewidth = 1, color = "#E69F00") +
        labs(title = "Calibration du modèle (on recommence)",
             x = "Probabilité prédite", y = "Taux observé") +
        xlim(0, 1) + ylim(0, 1)
    })
    
    output$courbe_calibration_geste <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE, levels = levels(model.frame(modele_geste_pool)$INTERVENTION_GROUPÉE)),
          Garde_Programme = factor(Garde_Programme, levels = levels(model.frame(modele_geste_pool)$Garde_Programme)),
          seniorite = factor(seniorite, levels = levels(model.frame(modele_geste_pool)$seniorite))
        ) %>%
        mutate(pred = predict(modele_geste_pool, newdata = ., type = "response")) %>%
        filter(!is.na(pred)) %>%
        mutate(bin = ntile(pred, 10)) %>%
        group_by(bin) %>%
        summarise(pred_moy = mean(pred), obs_moy = mean(geste_realise)) %>%
        ungroup()
      
      ggplot(df_temp, aes(x = pred_moy, y = obs_moy)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
        geom_point(size = 3, color = "#009E73") +
        geom_line(linewidth = 1, color = "#009E73") +
        labs(title = "Calibration du modèle (geste)",
             x = "Probabilité prédite", y = "Taux observé") +
        xlim(0, 1) + ylim(0, 1)
    })
    
    output$scatter_plot <- renderPlot({
      df_temp <- df_shiny %>%
        mutate(
          INTERVENTION_GROUPÉE = factor(INTERVENTION_GROUPÉE, levels = levels(model.frame(modele_ambiance_pool)$INTERVENTION_GROUPÉE)),
          annee_DES = as.factor(annee_DES),
          mois_du_semestre = as.factor(mois_du_semestre)
        ) %>%
        pivot_longer(cols = c(Garde_Programme, seniorite, mois_du_semestre, annee_DES),
                     names_to = "variable", values_to = "valeur", values_transform = list(valeur = as.character)) %>%
        group_by(variable, valeur) %>%
        summarise(taux_on_recommence = mean(on_recommence), .groups = "drop")
      
      ggplot(df_temp, aes(x = valeur, y = taux_on_recommence * 100)) +
        geom_point(size = 3, color = "#D55E00") +
        facet_wrap(~ variable, scales = "free_x") +
        geom_hline(yintercept = mean(df_shiny$on_recommence) * 100,
                   linetype = "dashed", color = "gray") +
        labs(title = "Taux de 'on recommence' selon les variables explicatives",
             x = "", y = "Taux (%)")
    })
  })
  
  observeEvent(input$quitter, {
    stopApp()
  })
}


# Lancer l'application
shinyApp(ui = ui, server = server)

saveRDS(df_shiny, file = "ShinyApp/df_shiny.rds")


install.packages('rsconnect')

rsconnect::setAccountInfo(name='sns5w6-thomas-husson', token='F86928AE3B04B208C12CFF5F5324B05F', secret='E9teWbmpEpRdaNFdP5gJYZKnNJDh8nOJIcM0XtXG')
library(rsconnect)
rsconnect::deployApp("/Users/thomashusson/ShinyApp")

rsconnect::deployApp(
  appDir = "~/ShinyApp",
  appName = "calculateur-logbook-v1"  # <- ou autre nom unique
)




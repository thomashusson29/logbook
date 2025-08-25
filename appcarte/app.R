library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(scales)
library(stringi)

df <- readRDS("logbook_data.rds")

coord_hopitaux <- data.frame(
  Hôpital = c("HEGP", "Paul_Brousse", "PSL", "Cochin", "St_Louis"),
  lat = c(48.8407, 48.7851, 48.8402, 48.8475, 48.8853),
  lng = c(2.2711, 2.3351, 2.3600, 2.3420, 2.3590),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("Carte interactive des hôpitaux - Logbook (Leaflet)"),
  leafletOutput("carte_leaflet", height = "500px"),
  br(),
  uiOutput("selected_hopital"),
  htmlOutput("resume_hopital"),
  plotOutput("barplot_garde"),
  plotOutput("barplot_ambiance"),
  plotOutput("barplot_pedagogie"),
  br(),
  actionButton("quitter", "Quitter l'application", class = "btn-danger")
)

server <- function(input, output, session) {
  
  df <- df %>%
    mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
    filter(!is.na(AMBIANCE), !is.na(PEDAGOGIE), !is.na(Geste), !is.na(Garde_Programme))
  
  df_hop <- reactive({
    df %>%
      filter(!is.na(Hôpital)) %>%
      mutate(mois = format(as.Date(DATE), "%Y-%m")) %>%
      group_by(Hôpital) %>%
      summarise(
        n_interventions = n(),
        n_mois_recueil = n_distinct(mois),
        n_semestres = round(n_mois_recueil / 6, 1),
        taux_geste = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE),
        pedagogie = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        ambiance = mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20 / 3),
        self_estime = mean(as.numeric(gsub("^(\\d+)-.*", "\\1", SELF_ESTIME_SORTIE)), na.rm = TRUE) * 4,
        prop_garde = mean(Garde_Programme == "Garde", na.rm = TRUE),
        prop_programme = mean(Garde_Programme == "Programmé", na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(coord_hopitaux, by = "Hôpital")
  })
  
  # Variable réactive pour l'hôpital sélectionné
  selected_hopital <- reactiveVal(NULL)
  
  # Création de la carte Leaflet
  output$carte_leaflet <- renderLeaflet({
    df_plot <- df_hop()
    
    # Fonction pour créer la couleur en fonction du taux de geste (dégradé plus fluide)
    getColor <- function(taux) {
      if (taux >= 0.50) {
        # Vert : dégradé de vert clair à vert foncé pour >50%
        if (taux >= 0.80) return("#006400")      # Vert très foncé ≥80%
        if (taux >= 0.70) return("#228B22")      # Vert foncé 70-80%
        if (taux >= 0.60) return("#32CD32")      # Vert moyen 60-70%
        return("#90EE90")                        # Vert clair 50-60%
      } else if (taux >= 0.30) {
        # Dégradé orange-jaune : transition entre rouge et vert
        if (taux >= 0.45) return("#FFD700")      # Jaune doré 45-50%
        if (taux >= 0.40) return("#FFA500")      # Orange 40-45%
        if (taux >= 0.35) return("#FF8C00")      # Orange foncé 35-40%
        return("#FF6347")                        # Orange-rouge 30-35%
      } else {
        # Rouge : performances faibles <30%
        if (taux >= 0.20) return("#FF4500")      # Rouge-orange 20-30%
        if (taux >= 0.10) return("#DC143C")      # Rouge 10-20%
        return("#8B0000")                        # Rouge foncé <10%
      }
    }
    
    leaflet(df_plot) %>%
      addTiles() %>%  # Carte OpenStreetMap gratuite
      setView(lng = 2.31, lat = 48.84, zoom = 12) %>%
      addCircleMarkers(
        lng = ~lng, 
        lat = ~lat,
        radius = ~sqrt(n_interventions) * 1.2,  # Réduction de la taille (était *2)
        color = ~sapply(taux_geste, getColor),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste0(
          "<b>", Hôpital, "</b><br/>",
          "Interventions: ", n_interventions, "<br/>",
          "Taux geste: ", round(taux_geste * 100, 1), "%<br/>",
          "Semestres: ", n_semestres
        ),
        layerId = ~Hôpital
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#8B0000", "#DC143C", "#FF4500", "#FF6347", "#FF8C00", "#FFA500", "#FFD700", "#90EE90", "#32CD32", "#228B22", "#006400"),
        labels = c("<10%", "10-20%", "20-30%", "30-35%", "35-40%", "40-45%", "45-50%", "50-60%", "60-70%", "70-80%", "≥80%"),
        title = "Taux de geste",
        opacity = 0.7
      )
  })
  
  # Observer pour les clics sur la carte
  observeEvent(input$carte_leaflet_marker_click, {
    click <- input$carte_leaflet_marker_click
    selected_hopital(click$id)
  })
  
  # Menu déroulant pour sélectionner l'hôpital
  output$selected_hopital <- renderUI({
    selectInput("hopital", "Choisissez un hôpital :", 
                choices = df_hop()$Hôpital, 
                selected = selected_hopital())
  })
  
  # Observer pour le menu déroulant
  observeEvent(input$hopital, {
    selected_hopital(input$hopital)
  })
  
  df_selected <- reactive({
    req(selected_hopital())
    df %>% filter(Hôpital == selected_hopital())
  })
  
  # Résumé affiché
  output$resume_hopital <- renderUI({
    if (is.null(selected_hopital())) {
      HTML("<p><i>Cliquez sur un hôpital sur la carte ou sélectionnez-le dans le menu pour voir les détails</i></p>")
    } else {
      infos <- df_hop() %>% filter(Hôpital == selected_hopital())
      HTML(paste0(
        "<h3>", selected_hopital(), "</h3>",
        "<b>Nombre d'interventions : </b>", infos$n_interventions, "<br/>",
        "<b>Nombre de semestres de recueil : </b>", infos$n_semestres, "<br/>",
        "<b>Taux de geste : </b>", round(infos$taux_geste * 100, 1), "%"
      ))
    }
  })
  
  df_plot_garde <- reactive({
    req(df_selected())
    df_selected() %>%
      filter(!is.na(Garde_Programme), !is.na(Geste)) %>%
      count(Garde_Programme, Geste) %>%
      group_by(Garde_Programme) %>%
      mutate(
        pourcentage = round(100 * n / sum(n), 1),
        label = paste0(pourcentage, "%")
      ) %>%
      ungroup() %>%
      mutate(group = paste0(Garde_Programme, "_", Geste))
  })
  
  output$barplot_garde <- renderPlot({
    req(df_plot_garde())
    couleurs <- c(
      "Programmé_Yes" = "#b2df8a",
      "Programmé_No" = "#fb9a99",
      "Garde_Yes" = "#33a02c",
      "Garde_No" = "#e31a1c"
    )
    
    ggplot(df_plot_garde(), aes(x = Garde_Programme, y = n, fill = group)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
      geom_text(aes(label = label), position = position_dodge(width = 0.7), vjust = -0.5, size = 5) +
      scale_fill_manual(values = couleurs) +
      labs(title = paste("Gestes réalisés en garde vs programmé -", selected_hopital()),
           x = "Type d'intervention", y = "Nombre d'interventions") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  output$barplot_ambiance <- renderPlot({
    req(df_selected())
    ggplot(df_selected(), aes(x = AMBIANCE, fill = AMBIANCE)) +
      geom_bar() +
      scale_fill_manual(values = c(
        "1 - je veux partir" = "#FAD4D4",
        "2 - c'est ok" = "#F9E2AE",
        "3 - on recommence" = "#A8D5BA"
      )) +
      labs(title = paste("Répartition AMBIANCE -", selected_hopital()),
           x = "Niveau", y = "Effectif") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$barplot_pedagogie <- renderPlot({
    req(df_selected())
    ggplot(df_selected(), aes(x = PEDAGOGIE, fill = PEDAGOGIE)) +
      geom_bar() +
      scale_fill_manual(values = c(
        "1-rien" = "#FAD4D4",
        "2-quasi rien" = "#F9E2AE",
        "3-ok" = "#A8D5BA",
        "4-bien" = "#7BC47F",
        "5-incroyable!!" = "#5AA469"
      )) +
      labs(title = paste("Répartition PEDAGOGIE -", selected_hopital()),
           x = "Niveau", y = "Effectif") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  observeEvent(input$quitter, {
    stopApp()
  })
  
}

shinyApp(ui, server)
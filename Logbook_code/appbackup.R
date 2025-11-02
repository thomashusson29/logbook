library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(gt)
library(shinydashboard)
library(shinymanager)
library(stringi)
library(plotly)  # Pour les graphiques interactifs
library(viridis) # Pour la palette de couleurs de la heatmap
library(openxlsx) # Pour l'export Excel
library(leaflet) # Pour la carte interactive

df <- readRDS("logbook_data.rds")

# Coordonnées des hôpitaux pour la carte
coord_hopitaux <- data.frame(
  Hôpital = c("HEGP", "Paul_Brousse", "PSL", "Cochin", "St_Louis"),
  lat = c(48.8407, 48.7851, 48.8402, 48.8475, 48.8853),
  lng = c(2.2711, 2.3351, 2.3600, 2.3420, 2.3590),
  stringsAsFactors = FALSE
)

# Fonction pour calculer les semestres (1er nov - 30 avril = S1, 1er mai - 31 oct = S2)
get_semestre <- function(date_vec) {
  month_val <- month(date_vec)
  year_val <- year(date_vec)
  
  # Utilisation de ifelse vectorisé avec | au lieu de ||
  semestre_num <- ifelse(month_val >= 11 | month_val <= 4, 1, 2)
  
  # Pour le semestre 1, l'année académique commence en novembre
  annee_academique <- ifelse(month_val >= 11, year_val, year_val - 1)
  
  paste0("S", semestre_num, " ", annee_academique, "-", annee_academique + 1)
}

prenom_to_code <- function(prenom) {
  prenom_sans_accents <- stri_trans_general(prenom, "Latin-ASCII")
  prenom_nettoye <- gsub("[^A-Za-z]", "", prenom_sans_accents)
  lettres <- toupper(unlist(strsplit(prenom_nettoye, "")))
  codes <- match(lettres, LETTERS)
  codes[is.na(codes)] <- 0
  paste0(codes, collapse = "")
}

liste_noms <- unique(df$NOM_interne)
liste_noms <- liste_noms[!is.na(liste_noms)]

credentials <- data.frame(
  user = c("admin", tolower(gsub("[^a-zA-Z]", "", liste_noms))),
  password = c("admin", tolower(gsub("[^a-zA-Z]", "", liste_noms))),  # Prénom en minuscules
  nom_interne = c(NA_character_, liste_noms),
  stringsAsFactors = FALSE
)

# UI sécurisé avec nouvelles sections
ui <- secure_app(
  fluidPage(
    titlePanel("Statistiques individuelles - Logbook"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("select_interne_ui"),
        br(),
        # Bouton export Excel (visible pour tous sauf admin sur l'onglet admin)
        conditionalPanel(
          condition = "input.tabs != 'admin'",
          downloadButton("export_excel", "📊 Exporter mes données Excel", 
                         class = "btn-success btn-block"),
          br(), br()
        ),
        # Filtres temporels pour la courbe des gestes
        conditionalPanel(
          condition = "input.tabs == 'vue_ensemble'",
          h4("Filtres temporels"),
          selectInput("periode_preset", "Période prédéfinie:",
                      choices = list("Toutes les données" = "all",
                                     "3 derniers mois" = "3m",
                                     "6 derniers mois" = "6m",
                                     "12 derniers mois" = "12m",
                                     "Personnalisé" = "custom"),
                      selected = "all"),
          conditionalPanel(
            condition = "input.periode_preset == 'custom'",
            dateRangeInput("date_range", "Période personnalisée:",
                           start = NULL, end = NULL,
                           format = "dd/mm/yyyy", language = "fr")
          ),
          br()
        ),
        actionButton("quitter", "Quitter l'application", class = "btn-danger")
      ),
      mainPanel(
        uiOutput("main_tabs")  # Interface dynamique selon le type d'utilisateur
      )
    )
  )
)

server <- function(input, output, session) {
  user <- secure_server(check_credentials = check_credentials(credentials))
  
  # Interface dynamique selon le type d'utilisateur
  output$main_tabs <- renderUI({
    req(user$user)
    
    # Onglets de base pour tous les utilisateurs
    base_tabs <- list(
      tabPanel("Vue d'ensemble", value = "vue_ensemble",
               h3("Taux de gestes réalisés par mois"),
               plotlyOutput("courbe_gestes"),
               fluidRow(
                 column(4, valueBoxOutput("box_geste")),
                 column(4, valueBoxOutput("box_total")),
                 column(4, valueBoxOutput("box_recommence"))
               ),
               br(),
               h3("Performance par semestre"),
               plotlyOutput("courbe_semestres"),
               br(),
               h3("Résumé des scores pédagogiques"),
               gt_output("table_scores")
      ),
      tabPanel("Analyse par méta-groupe", value = "meta_groupe",
               fluidRow(
                 column(6,
                        h3("Répartition des interventions par méta-groupe"),
                        plotlyOutput("camembert_metagroupe"),
                        br()
                 ),
                 column(6,
                        h3("Détails par méta-groupe"),
                        gt_output("details_metagroupe")
                 )
               ),
               br(),
               h3("Taux de geste par méta-groupe et par année (Heatmap)"),
               plotlyOutput("heatmap_gestes", height = "600px")
      ),
      tabPanel("Analyse par service", value = "service",
               h3("Taux de gestes par service/hôpital"),
               plotlyOutput("courbe_hopital"),
               br(),
               fluidRow(
                 column(6,
                        h3("Répartition par service"),
                        plotlyOutput("camembert_hopital")
                 ),
                 column(6,
                        h3("Performance par service"),
                        gt_output("details_hopital")
                 )
               ),
               br(),
               h3("Évolution par service dans le temps"),
               plotlyOutput("evolution_hopital")
      )
    )
    
    # Ajout de l'onglet admin si nécessaire
    if (user$user == "admin") {
      admin_tab <- tabPanel("Administration", value = "admin",
                            h2("📊 Données globales - Tous les internes"),
                            br(),
                            fluidRow(
                              column(3, valueBoxOutput("admin_box_total_internes")),
                              column(3, valueBoxOutput("admin_box_total_interventions")),
                              column(3, valueBoxOutput("admin_box_taux_global")),
                              column(3, valueBoxOutput("admin_box_total_hopitaux"))
                            ),
                            br(),
                            
                            h3("🗺️ Carte interactive des hôpitaux"),
                            leafletOutput("admin_carte_leaflet", height = "400px"),
                            br(),
                            
                            h3("🏥 Performance par hôpital - Taux de gestes"),
                            fluidRow(
                              column(6,
                                     plotlyOutput("admin_courbe_hopitaux")
                              ),
                              column(6,
                                     gt_output("admin_table_hopitaux")
                              )
                            ),
                            br(),
                            
                            h3("😊 Ambiance par hôpital"),
                            fluidRow(
                              column(6,
                                     plotlyOutput("admin_ambiance_hopitaux")
                              ),
                              column(6,
                                     gt_output("admin_table_ambiance_hopitaux")
                              )
                            ),
                            br(),
                            
                            h3("🎓 Pédagogie par hôpital"),
                            fluidRow(
                              column(6,
                                     plotlyOutput("admin_pedagogie_hopitaux")
                              ),
                              column(6,
                                     gt_output("admin_table_pedagogie_hopitaux")
                              )
                            ),
                            br(),
                            
                            h3("📈 Benchmarking des internes"),
                            fluidRow(
                              column(4,
                                     selectInput("admin_interne_benchmark", "Choisir un interne:",
                                                 choices = NULL, selected = NULL)
                              ),
                              column(4,
                                     h4("Taux de geste de l'interne"),
                                     verbatimTextOutput("benchmark_taux_interne")
                              ),
                              column(4,
                                     h4("Positionnement"),
                                     verbatimTextOutput("benchmark_position")
                              )
                            ),
                            br(),
                            plotlyOutput("admin_benchmark_plot"),
                            br(),
                            
                            h3("📊 Tableau de bord global par interne"),
                            gt_output("admin_table_tous_internes"),
                            br(),
                            
                            h3("🎓 Performance par année DES"),
                            fluidRow(
                              column(6,
                                     plotlyOutput("admin_des_boxplot")
                              ),
                              column(6,
                                     gt_output("admin_table_des")
                              )
                            )
      )
      base_tabs <- c(base_tabs, list(admin_tab))
    }
    
    do.call(tabsetPanel, c(list(id = "tabs"), base_tabs))
  })
  
  # Gestion de l'interne sélectionné : admin peut choisir, sinon propre nom
  interne_courant <- reactiveVal(NULL)
  
  observe({
    req(user$user)
    if (user$user == "admin") {
      if (is.null(interne_courant())) {
        interne_courant(liste_noms[1])
      }
    } else {
      nom <- credentials$nom_interne[credentials$user == user$user]
      if(length(nom) > 0 && !is.na(nom)) {
        interne_courant(nom)
      }
    }
  })
  
  # Mise à jour de la liste des internes pour le benchmark (seulement si admin)
  observe({
    req(user$user)
    if (user$user == "admin") {
      updateSelectInput(session, "admin_interne_benchmark",
                        choices = liste_noms,
                        selected = liste_noms[1])
    }
  })
  
  output$select_interne_ui <- renderUI({
    if (user$user == "admin") {
      selectInput("interne", "Choisissez un interne :", choices = liste_noms,
                  selected = interne_courant())
    } else {
      tagList(
        strong("Interne : "), span(interne_courant())
      )
    }
  })
  
  observeEvent(input$interne, {
    if (user$user == "admin") {
      interne_courant(input$interne)
    }
  })
  
  # Données filtrées avec création des variables binaires
  data_interne_base <- reactive({
    req(interne_courant())
    df %>%
      filter(NOM_interne == interne_courant()) %>%
      mutate(
        geste_realise = ifelse(Geste == "Yes", 1, 0),
        on_recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
        annee = year(as.Date(DATE)),
        semestre = get_semestre(as.Date(DATE))
      )
  })
  
  # Initialisation des dates par défaut
  observe({
    req(data_interne_base())
    data_dates <- data_interne_base() %>%
      filter(!is.na(DATE)) %>%
      mutate(DATE = as.Date(DATE))
    
    if(nrow(data_dates) > 0) {
      min_date <- min(data_dates$DATE, na.rm = TRUE)
      max_date <- max(data_dates$DATE, na.rm = TRUE)
      
      updateDateRangeInput(session, "date_range",
                           start = min_date,
                           end = max_date,
                           min = min_date,
                           max = max_date)
    }
  })
  
  # Gestion des périodes prédéfinies
  observeEvent(input$periode_preset, {
    req(data_interne_base())
    data_dates <- data_interne_base() %>%
      filter(!is.na(DATE)) %>%
      mutate(DATE = as.Date(DATE))
    
    if(nrow(data_dates) > 0) {
      min_date <- min(data_dates$DATE, na.rm = TRUE)
      max_date <- max(data_dates$DATE, na.rm = TRUE)
      
      if(input$periode_preset == "3m") {
        start_date <- max_date - months(3)
      } else if(input$periode_preset == "6m") {
        start_date <- max_date - months(6)
      } else if(input$periode_preset == "12m") {
        start_date <- max_date - months(12)
      } else {
        start_date <- min_date
      }
      
      updateDateRangeInput(session, "date_range",
                           start = start_date,
                           end = max_date)
    }
  })
  
  # Données avec filtre temporel (pour la courbe des gestes uniquement)
  data_interne_filtered <- reactive({
    req(data_interne_base())
    
    if(input$periode_preset == "all") {
      return(data_interne_base())
    }
    
    if(input$periode_preset == "custom") {
      req(input$date_range)
      data_interne_base() %>%
        filter(
          as.Date(DATE) >= input$date_range[1],
          as.Date(DATE) <= input$date_range[2]
        )
    } else {
      # Pour les périodes prédéfinies
      data_dates <- data_interne_base() %>%
        filter(!is.na(DATE)) %>%
        mutate(DATE = as.Date(DATE))
      
      if(nrow(data_dates) > 0) {
        max_date <- max(data_dates$DATE, na.rm = TRUE)
        
        if(input$periode_preset == "3m") {
          start_date <- max_date - months(3)
        } else if(input$periode_preset == "6m") {
          start_date <- max_date - months(6)
        } else if(input$periode_preset == "12m") {
          start_date <- max_date - months(12)
        }
        
        data_interne_base() %>%
          filter(as.Date(DATE) >= start_date, as.Date(DATE) <= max_date)
      } else {
        data_interne_base()
      }
    }
  })
  
  # Données sans filtre temporel (pour les autres graphiques)
  data_interne <- reactive({
    data_interne_base()
  })
  
  # Graphique courbe des gestes (avec filtre temporel et interactif)
  output$courbe_gestes <- renderPlotly({
    df_plot <- data_interne_filtered() %>%
      filter(!is.na(DATE), !is.na(Geste)) %>%
      mutate(DATE = as.Date(DATE),
             mois = floor_date(DATE, "month")) %>%
      group_by(mois) %>%
      summarise(taux = mean(geste_realise, na.rm = TRUE),
                n = n(),
                .groups = "drop")
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée sur cette période",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      p <- ggplot(df_plot, aes(x = mois, y = taux)) +
        geom_line(color = "#377eb8", linewidth = 1.5) +
        geom_point(size = 3, color = "#377eb8") +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                     expand = expansion(mult = c(0.01, 0.05))) +
        labs(title = paste("Taux de gestes réalisés par mois -", interne_courant()),
             x = "Mois", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = 'x unified')
    }
  })
  
  # Nouvelle courbe par semestre
  output$courbe_semestres <- renderPlotly({
    df_plot <- data_interne() %>%
      filter(!is.na(DATE), !is.na(Geste), !is.na(semestre)) %>%
      group_by(semestre) %>%
      summarise(
        taux = mean(geste_realise, na.rm = TRUE),
        n = n(),
        taux_pedagogie = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        taux_ambiance = mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3),
        .groups = "drop"
      ) %>%
      arrange(semestre)
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée disponible",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      p <- ggplot(df_plot, aes(x = semestre, y = taux, group = 1)) +
        geom_line(color = "#e31a1c", linewidth = 1.5) +
        geom_point(size = 4, color = "#e31a1c") +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = paste("Performance par semestre -", interne_courant()),
             x = "Semestre", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = 'x unified')
    }
  })
  
  # ValueBoxes (sans filtre temporel)
  output$box_geste <- renderValueBox({
    taux <- mean(data_interne()$geste_realise, na.rm = TRUE)
    valueBox(paste0(round(taux * 100), "%"), "Geste réalisé", icon = icon("hand-holding-medical"), color = "aqua")
  })
  
  output$box_total <- renderValueBox({
    n <- nrow(data_interne())
    valueBox(n, "Interventions enregistrées", icon = icon("notes-medical"), color = "blue")
  })
  
  output$box_recommence <- renderValueBox({
    taux <- mean(data_interne()$on_recommence, na.rm = TRUE)
    valueBox(paste0(round(taux * 100), "%"), "On recommence", icon = icon("smile"), color = "green")
  })
  
  # Tableau scores (sans filtre temporel)
  output$table_scores <- render_gt({
    data_interne() %>%
      summarise(
        `Note pédagogique (sur 20)` = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        `Ambiance (sur 20)` = mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20 / 3),
        `Self-estime (sur 20)` = mean(as.numeric(gsub("^(\\d+)-.*", "\\1", SELF_ESTIME_SORTIE)), na.rm = TRUE) * 4
      ) %>%
      gt() %>%
      fmt_number(columns = everything(), decimals = 1)
  })
  
  # Camembert méta-groupes (interactif)
  output$camembert_metagroupe <- renderPlotly({
    data_camembert <- data_interne() %>%
      filter(!is.na(META_GROUPE)) %>%
      count(META_GROUPE, name = "n_interventions") %>%
      mutate(
        pourcentage = round(100 * n_interventions / sum(n_interventions), 1)
      ) %>%
      arrange(desc(n_interventions))
    
    if(nrow(data_camembert) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée disponible",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      plot_ly(data_camembert, 
              labels = ~META_GROUPE, 
              values = ~n_interventions,
              type = 'pie',
              textinfo = 'label+percent',
              textposition = 'inside',
              hovertemplate = "<b>%{label}</b><br>%{value} interventions<br>%{percent}<extra></extra>") %>%
        layout(showlegend = FALSE, margin = list(t = 0, b = 0, l = 0, r = 0))
    }
  })
  
  # Tableau détaillé méta-groupes
  output$details_metagroupe <- render_gt({
    details <- data_interne() %>%
      filter(!is.na(META_GROUPE)) %>%
      group_by(META_GROUPE) %>%
      summarise(
        `N interventions` = n(),
        `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
        `Note péda (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
        `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`N interventions`))
    
    if(nrow(details) == 0) {
      details <- data.frame(
        META_GROUPE = character(0),
        `N interventions` = numeric(0),
        `Taux geste (%)` = numeric(0),
        `Note péda (sur 20)` = numeric(0),
        `Ambiance (sur 20)` = numeric(0),
        check.names = FALSE
      )
    }
    
    details %>%
      gt() %>%
      tab_header(title = "Détails par méta-groupe") %>%
      fmt_number(columns = c("Note péda (sur 20)", "Ambiance (sur 20)"), decimals = 1) %>%
      data_color(
        columns = "Taux geste (%)",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#de2d26"),
          domain = c(0, 100)
        )
      )
  })
  
  # Heatmap méta-groupes (interactive)
  output$heatmap_gestes <- renderPlotly({
    data_heatmap <- data_interne() %>%
      filter(!is.na(META_GROUPE), !is.na(annee), !is.na(Geste)) %>%
      group_by(META_GROUPE, annee) %>%
      summarise(
        taux_geste = mean(geste_realise, na.rm = TRUE),
        n_interventions = n(),
        .groups = "drop"
      ) %>%
      filter(n_interventions >= 3)
    
    if(nrow(data_heatmap) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée disponible pour la heatmap",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      # Calculer la taille du texte en fonction du nombre de méta-groupes
      n_groups <- length(unique(data_heatmap$META_GROUPE))
      text_size <- max(2.5, min(4, 40/n_groups))  # Taille adaptative
      
      p <- ggplot(data_heatmap, aes(x = factor(annee), y = reorder(META_GROUPE, taux_geste))) +
        geom_tile(aes(fill = taux_geste), color = "white", size = 0.5) +
        geom_text(aes(label = paste0(round(taux_geste * 100), "%\n(n=", n_interventions, ")")), 
                  color = "white", fontface = "bold", size = text_size) +
        scale_fill_viridis_c(name = "Taux de\ngeste", labels = percent_format(), 
                             option = "plasma", begin = 0.1, end = 0.9) +
        labs(
          title = paste("Taux de gestes réalisés par méta-groupe et année -", interne_courant()),
          x = "Année", y = "Méta-groupe"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          axis.text.y = element_text(size = 9),
          axis.text.x = element_text(size = 11),
          plot.title = element_text(size = 13, hjust = 0.5),
          legend.position = "right",
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
        )
      
      ggplotly(p, tooltip = c("fill", "text")) %>%
        layout(
          height = max(400, min(700, n_groups * 35 + 150)),  # Hauteur adaptative
          margin = list(l = 150, r = 50, t = 80, b = 50)     # Marges pour les labels
        )
    }
  })
  
  # NOUVEL ONGLET : ANALYSE PAR SERVICE
  
  # Courbe taux de gestes par hôpital
  output$courbe_hopital <- renderPlotly({
    df_plot <- data_interne() %>%
      filter(!is.na(DATE), !is.na(Geste), !is.na(Hôpital)) %>%
      mutate(DATE = as.Date(DATE), mois = floor_date(DATE, "month")) %>%
      group_by(mois, Hôpital) %>%
      summarise(taux = mean(geste_realise, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 2)  # Au moins 2 interventions par mois
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée disponible",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      p <- ggplot(df_plot, aes(x = mois, y = taux, color = Hôpital)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(title = paste("Taux de gestes par service -", interne_courant()),
             x = "Mois", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("colour", "x", "y"))
    }
  })
  
  # Camembert répartition par hôpital
  output$camembert_hopital <- renderPlotly({
    data_camembert <- data_interne() %>%
      filter(!is.na(Hôpital)) %>%
      count(Hôpital, name = "n_interventions") %>%
      mutate(pourcentage = round(100 * n_interventions / sum(n_interventions), 1)) %>%
      arrange(desc(n_interventions))
    
    if(nrow(data_camembert) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Aucune donnée disponible",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      plot_ly(data_camembert, 
              labels = ~Hôpital, 
              values = ~n_interventions,
              type = 'pie',
              textinfo = 'label+percent',
              textposition = 'inside',
              hovertemplate = "<b>%{label}</b><br>%{value} interventions<br>%{percent}<extra></extra>") %>%
        layout(showlegend = FALSE, margin = list(t = 0, b = 0, l = 0, r = 0))
    }
  })
  
  # Tableau détaillé par hôpital
  output$details_hopital <- render_gt({
    details <- data_interne() %>%
      filter(!is.na(Hôpital)) %>%
      group_by(Hôpital) %>%
      summarise(
        `N interventions` = n(),
        `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
        `Note péda (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
        `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`N interventions`))
    
    if(nrow(details) == 0) {
      details <- data.frame(
        Hôpital = character(0),
        `N interventions` = numeric(0),
        `Taux geste (%)` = numeric(0),
        `Note péda (sur 20)` = numeric(0),
        `Ambiance (sur 20)` = numeric(0),
        check.names = FALSE
      )
    }
    
    details %>%
      gt() %>%
      tab_header(title = "Performance par service") %>%
      fmt_number(columns = c("Note péda (sur 20)", "Ambiance (sur 20)"), decimals = 1) %>%
      data_color(
        columns = "Taux geste (%)",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#de2d26"),
          domain = c(0, 100)
        )
      )
  })
  
  # Évolution des scores par hôpital dans le temps
  output$evolution_hopital <- renderPlotly({
    df_plot <- data_interne() %>%
      filter(!is.na(semestre), !is.na(Hôpital)) %>%
      group_by(semestre, Hôpital) %>%
      summarise(
        taux_geste = mean(geste_realise, na.rm = TRUE),
        note_pedagogie = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 5)  # Au moins 5 interventions
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(
          text = "Pas assez de données pour cette analyse",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        )
    } else {
      p <- ggplot(df_plot, aes(x = semestre, y = taux_geste, color = Hôpital, group = Hôpital)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = paste("Évolution par service -", interne_courant()),
             x = "Semestre", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("colour", "x", "y"))
    }
  })
  
  # EXPORT EXCEL PERSONNEL
  output$export_excel <- downloadHandler(
    filename = function() {
      req(interne_courant())
      paste0("Donnees_", gsub("[^a-zA-Z0-9]", "_", interne_courant()), "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(interne_courant())
      
      # Données de base de l'interne
      donnees_interne <- df %>%
        filter(NOM_interne == interne_courant()) %>%
        arrange(desc(as.Date(DATE)))
      
      # Création du workbook avec plusieurs onglets
      wb <- createWorkbook()
      
      # Onglet 1: Données brutes
      addWorksheet(wb, "Données brutes")
      writeData(wb, "Données brutes", donnees_interne)
      
      # Onglet 2: Résumé statistiques
      stats_resume <- donnees_interne %>%
        summarise(
          `Nom interne` = first(NOM_interne),
          `Année DES` = first(annee_DES),
          `Total interventions` = n(),
          `Période` = paste(min(as.Date(DATE), na.rm = TRUE), "au", max(as.Date(DATE), na.rm = TRUE)),
          `Taux geste réalisé (%)` = round(mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE) * 100, 1),
          `Note pédagogique moyenne (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
          `Ambiance moyenne (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
          `Self-estime moyenne (sur 20)` = round(mean(as.numeric(gsub("^(\\d+)-.*", "\\1", SELF_ESTIME_SORTIE)), na.rm = TRUE) * 4, 1),
          `Taux "on recommence" (%)` = round(mean(ifelse(AMBIANCE == "3 - on recommence", 1, 0), na.rm = TRUE) * 100, 1)
        )
      
      addWorksheet(wb, "Résumé statistiques")
      writeData(wb, "Résumé statistiques", t(stats_resume), colNames = FALSE, rowNames = TRUE)
      
      # Onglet 3: Par méta-groupe
      if (!all(is.na(donnees_interne$META_GROUPE))) {
        stats_metagroupe <- donnees_interne %>%
          filter(!is.na(META_GROUPE)) %>%
          group_by(META_GROUPE) %>%
          summarise(
            `Nombre interventions` = n(),
            `Taux geste (%)` = round(mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE) * 100, 1),
            `Note pédagogique (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
            `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
            .groups = "drop"
          ) %>%
          arrange(desc(`Nombre interventions`))
        
        addWorksheet(wb, "Par méta-groupe")
        writeData(wb, "Par méta-groupe", stats_metagroupe)
      }
      
      # Onglet 4: Par service/hôpital
      if (!all(is.na(donnees_interne$Hôpital))) {
        stats_hopital <- donnees_interne %>%
          filter(!is.na(Hôpital)) %>%
          group_by(Hôpital) %>%
          summarise(
            `Nombre interventions` = n(),
            `Taux geste (%)` = round(mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE) * 100, 1),
            `Note pédagogique (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
            `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
            .groups = "drop"
          ) %>%
          arrange(desc(`Nombre interventions`))
        
        addWorksheet(wb, "Par service")
        writeData(wb, "Par service", stats_hopital)
      }
      
      # Onglet 5: Évolution mensuelle
      evolution_mensuelle <- donnees_interne %>%
        filter(!is.na(DATE), !is.na(Geste)) %>%
        mutate(
          DATE = as.Date(DATE),
          mois = floor_date(DATE, "month")
        ) %>%
        group_by(mois) %>%
        summarise(
          `Nombre interventions` = n(),
          `Taux geste (%)` = round(mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE) * 100, 1),
          `Note pédagogique (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
          `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
          .groups = "drop"
        ) %>%
        arrange(desc(mois))
      
      addWorksheet(wb, "Évolution mensuelle")
      writeData(wb, "Évolution mensuelle", evolution_mensuelle)
      
      # Mise en forme basique
      for (sheet in names(wb)) {
        # Ajuster la largeur des colonnes
        setColWidths(wb, sheet, cols = 1:20, widths = "auto")
        
        # Style d'en-tête
        if (nrow(read.xlsx(wb, sheet = sheet)) > 0) {
          addStyle(wb, sheet, 
                   style = createStyle(textDecoration = "bold", fgFill = "#4F81BD", fontColour = "white"),
                   rows = 1, cols = 1:ncol(read.xlsx(wb, sheet = sheet)), gridExpand = TRUE)
        }
      }
      
      # Sauvegarder le fichier
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ONGLET ADMINISTRATION - RÉSERVÉ À L'ADMIN
  
  # Données réactives pour la carte et les analyses par hôpital
  df_hopitaux_admin <- reactive({
    df %>%
      filter(!is.na(Hôpital), !is.na(Geste)) %>%
      mutate(
        Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital),
        geste_realise = ifelse(Geste == "Yes", 1, 0),
        mois = format(as.Date(DATE), "%Y-%m")
      ) %>%
      group_by(Hôpital) %>%
      summarise(
        n_interventions = n(),
        n_internes = n_distinct(NOM_interne, na.rm = TRUE),
        n_mois_recueil = n_distinct(mois),
        n_semestres = round(n_mois_recueil / 6, 1),
        taux_geste = mean(geste_realise, na.rm = TRUE),
        pedagogie = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        ambiance = mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20 / 3),
        .groups = "drop"
      ) %>%
      left_join(coord_hopitaux, by = "Hôpital") %>%
      filter(!is.na(lat), !is.na(lng))  # Garder seulement les hôpitaux avec coordonnées
  })
  
  # ValueBoxes globales
  output$admin_box_total_internes <- renderValueBox({
    n_internes <- length(unique(df$NOM_interne[!is.na(df$NOM_interne)]))
    valueBox(n_internes, "Internes", icon = icon("user-md"), color = "purple")
  })
  
  output$admin_box_total_interventions <- renderValueBox({
    n_interventions <- nrow(df)
    valueBox(n_interventions, "Interventions totales", icon = icon("procedures"), color = "blue")
  })
  
  output$admin_box_taux_global <- renderValueBox({
    taux_global <- mean(ifelse(df$Geste == "Yes", 1, 0), na.rm = TRUE)
    valueBox(paste0(round(taux_global * 100), "%"), "Taux global gestes", icon = icon("hand-holding-medical"), color = "green")
  })
  
  output$admin_box_total_hopitaux <- renderValueBox({
    n_hopitaux <- length(unique(df$Hôpital[!is.na(df$Hôpital)]))
    valueBox(n_hopitaux, "Hôpitaux", icon = icon("hospital"), color = "orange")
  })
  
  # Carte Leaflet interactive
  output$admin_carte_leaflet <- renderLeaflet({
    df_carte <- df_hopitaux_admin()
    
    if(nrow(df_carte) == 0) return(leaflet() %>% addTiles())
    
    # Fonction pour créer la couleur en fonction du taux de geste
    getColor <- function(taux) {
      if (taux >= 0.70) return("#006400")      # Vert foncé ≥70%
      if (taux >= 0.60) return("#32CD32")      # Vert moyen 60-70%
      if (taux >= 0.50) return("#90EE90")      # Vert clair 50-60%
      if (taux >= 0.40) return("#FFD700")      # Jaune 40-50%
      if (taux >= 0.30) return("#FFA500")      # Orange 30-40%
      return("#FF4500")                        # Rouge <30%
    }
    
    leaflet(df_carte) %>%
      addTiles() %>%
      setView(lng = 2.31, lat = 48.84, zoom = 11) %>%
      addCircleMarkers(
        lng = ~lng, 
        lat = ~lat,
        radius = ~sqrt(n_interventions) * 1.5,
        color = ~sapply(taux_geste, getColor),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste0(
          "<b>", Hôpital, "</b><br/>",
          "Interventions: ", n_interventions, "<br/>",
          "Internes: ", n_internes, "<br/>",
          "Taux geste: ", round(taux_geste * 100, 1), "%<br/>",
          "Pédagogie: ", round(pedagogie, 1), "/20<br/>",
          "Ambiance: ", round(ambiance, 1), "/20"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#FF4500", "#FFA500", "#FFD700", "#90EE90", "#32CD32", "#006400"),
        labels = c("<30%", "30-40%", "40-50%", "50-60%", "60-70%", "≥70%"),
        title = "Taux de geste",
        opacity = 0.7
      )
  })
  
  # Performance par hôpital (taux de gestes)
  output$admin_courbe_hopitaux <- renderPlotly({
    df_plot <- df %>%
      filter(!is.na(Hôpital), !is.na(Geste), !is.na(DATE)) %>%
      mutate(
        DATE = as.Date(DATE), 
        mois = floor_date(DATE, "month"),
        geste_realise = ifelse(Geste == "Yes", 1, 0),
        Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)
      ) %>%
      group_by(mois, Hôpital) %>%
      summarise(taux = mean(geste_realise, na.rm = TRUE), n = n(), .groups = "drop") %>%
      filter(n >= 5)  # Au moins 5 interventions par mois
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(text = "Aucune donnée disponible", x = 0.5, y = 0.5,
                        showarrow = FALSE, font = list(size = 16))
    } else {
      p <- ggplot(df_plot, aes(x = mois, y = taux, color = Hôpital)) +
        geom_line(linewidth = 1.2) + geom_point(size = 2) +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(title = "Évolution du taux de gestes par hôpital",
             x = "Mois", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 12) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p, tooltip = c("colour", "x", "y"))
    }
  })
  
  # Tableau performance par hôpital (taux de gestes)
  output$admin_table_hopitaux <- render_gt({
    df_hopitaux_admin() %>%
      select(Hôpital, n_internes, n_interventions, taux_geste, n_semestres) %>%
      mutate(taux_geste = round(taux_geste * 100, 1)) %>%
      arrange(desc(taux_geste)) %>%
      gt() %>%
      tab_header(title = "Performance par hôpital - Taux de gestes") %>%
      cols_label(
        Hôpital = "Hôpital",
        n_internes = "N internes",
        n_interventions = "N interventions", 
        taux_geste = "Taux geste (%)",
        n_semestres = "Semestres"
      ) %>%
      data_color(
        columns = "taux_geste",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#de2d26"),
          domain = c(0, 100)
        )
      )
  })
  
  # Ambiance par hôpital - Graphique
  output$admin_ambiance_hopitaux <- renderPlotly({
    df_plot <- df %>%
      filter(!is.na(Hôpital), !is.na(AMBIANCE)) %>%
      mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
      group_by(Hôpital) %>%
      summarise(
        ambiance_score = mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%  # Au moins 10 interventions
      arrange(desc(ambiance_score))
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(text = "Aucune donnée disponible", x = 0.5, y = 0.5,
                        showarrow = FALSE, font = list(size = 16))
    } else {
      p <- ggplot(df_plot, aes(x = reorder(Hôpital, ambiance_score), y = ambiance_score, fill = Hôpital)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = paste0(round(ambiance_score, 1), "/20")), hjust = -0.1) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 20)) +
        labs(title = "Score d'ambiance par hôpital", x = "Hôpital", y = "Score ambiance (sur 20)") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      
      ggplotly(p)
    }
  })
  
  # Tableau ambiance par hôpital
  output$admin_table_ambiance_hopitaux <- render_gt({
    df %>%
      filter(!is.na(Hôpital), !is.na(AMBIANCE)) %>%
      mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
      group_by(Hôpital) %>%
      summarise(
        n_interventions = n(),
        ambiance_score = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
        pct_on_recommence = round(mean(AMBIANCE == "3 - on recommence", na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(ambiance_score)) %>%
      gt() %>%
      tab_header(title = "Ambiance par hôpital") %>%
      cols_label(
        Hôpital = "Hôpital",
        n_interventions = "N interventions",
        ambiance_score = "Score ambiance (/20)",
        pct_on_recommence = "% 'On recommence'"
      ) %>%
      data_color(
        columns = "ambiance_score",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#a1d99b"),
          domain = c(0, 20)
        )
      )
  })
  
  # Pédagogie par hôpital - Graphique
  output$admin_pedagogie_hopitaux <- renderPlotly({
    df_plot <- df %>%
      filter(!is.na(Hôpital), !is.na(PEDAGOGIE)) %>%
      mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
      group_by(Hôpital) %>%
      summarise(
        pedagogie_score = mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4,
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 10) %>%  # Au moins 10 interventions
      arrange(desc(pedagogie_score))
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(text = "Aucune donnée disponible", x = 0.5, y = 0.5,
                        showarrow = FALSE, font = list(size = 16))
    } else {
      p <- ggplot(df_plot, aes(x = reorder(Hôpital, pedagogie_score), y = pedagogie_score, fill = Hôpital)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = paste0(round(pedagogie_score, 1), "/20")), hjust = -0.1) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 20)) +
        labs(title = "Score de pédagogie par hôpital", x = "Hôpital", y = "Score pédagogie (sur 20)") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      
      ggplotly(p)
    }
  })
  
  # Tableau pédagogie par hôpital
  output$admin_table_pedagogie_hopitaux <- render_gt({
    df %>%
      filter(!is.na(Hôpital), !is.na(PEDAGOGIE)) %>%
      mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
      group_by(Hôpital) %>%
      summarise(
        n_interventions = n(),
        pedagogie_score = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
        pct_incroyable = round(mean(PEDAGOGIE == "5-incroyable!!", na.rm = TRUE) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(pedagogie_score)) %>%
      gt() %>%
      tab_header(title = "Pédagogie par hôpital") %>%
      cols_label(
        Hôpital = "Hôpital",
        n_interventions = "N interventions",
        pedagogie_score = "Score pédagogie (/20)",
        pct_incroyable = "% 'Incroyable'"
      ) %>%
      data_color(
        columns = "pedagogie_score",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#a1d99b"),
          domain = c(0, 20)
        )
      )
  })
  
  # Données de benchmark
  data_benchmark <- reactive({
    req(input$admin_interne_benchmark)
    
    # Données de l'interne sélectionné
    interne_data <- df %>%
      filter(NOM_interne == input$admin_interne_benchmark, !is.na(Geste)) %>%
      mutate(geste_realise = ifelse(Geste == "Yes", 1, 0))
    
    if(nrow(interne_data) == 0) return(NULL)
    
    annee_des_interne <- interne_data$annee_DES[1]
    taux_interne <- mean(interne_data$geste_realise, na.rm = TRUE)
    
    # Benchmark global (tous internes)
    taux_global <- df %>%
      filter(!is.na(Geste)) %>%
      summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE)) %>%
      pull(taux)
    
    # Benchmark par année DES
    taux_annee_des <- df %>%
      filter(!is.na(Geste), annee_DES == annee_des_interne) %>%
      summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE)) %>%
      pull(taux)
    
    # Distribution des taux par interne
    taux_par_interne <- df %>%
      filter(!is.na(NOM_interne), !is.na(Geste)) %>%
      group_by(NOM_interne) %>%
      summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE), .groups = "drop")
    
    # Percentile global
    percentile_global <- round(mean(taux_par_interne$taux <= taux_interne) * 100)
    
    # Percentile année DES
    taux_meme_annee <- df %>%
      filter(!is.na(NOM_interne), !is.na(Geste), annee_DES == annee_des_interne) %>%
      group_by(NOM_interne) %>%
      summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE), .groups = "drop")
    
    percentile_annee <- if(nrow(taux_meme_annee) > 0) {
      round(mean(taux_meme_annee$taux <= taux_interne) * 100)
    } else NA
    
    list(
      taux_interne = taux_interne,
      taux_global = taux_global,
      taux_annee_des = taux_annee_des,
      annee_des = annee_des_interne,
      percentile_global = percentile_global,
      percentile_annee = percentile_annee,
      distribution = taux_par_interne,
      distribution_annee = taux_meme_annee
    )
  })
  
  # Affichage du taux de l'interne
  output$benchmark_taux_interne <- renderText({
    data <- data_benchmark()
    if(is.null(data)) return("Aucune donnée")
    
    paste0(
      "Taux: ", round(data$taux_interne * 100, 1), "%\n",
      "Année DES: ", data$annee_des
    )
  })
  
  # Affichage du positionnement
  output$benchmark_position <- renderText({
    data <- data_benchmark()
    if(is.null(data)) return("Aucune donnée")
    
    paste0(
      "Vs tous internes:\n",
      "- Moyenne: ", round(data$taux_global * 100, 1), "%\n",
      "- Percentile: ", data$percentile_global, "e\n\n",
      "Vs même année DES:\n",
      "- Moyenne: ", round(data$taux_annee_des * 100, 1), "%\n",
      "- Percentile: ", ifelse(is.na(data$percentile_annee), "N/A", paste0(data$percentile_annee, "e"))
    )
  })
  
  # Graphique de benchmark
  output$admin_benchmark_plot <- renderPlotly({
    data <- data_benchmark()
    if(is.null(data)) {
      return(plot_ly() %>%
               add_annotations(text = "Sélectionnez un interne", x = 0.5, y = 0.5,
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    # Créer le graphique de distribution
    p <- ggplot(data$distribution, aes(x = taux)) +
      geom_histogram(aes(y = after_stat(density)), bins = 15, fill = "lightblue", alpha = 0.7, color = "white") +
      geom_density(color = "blue", linewidth = 1) +
      geom_vline(aes(xintercept = data$taux_interne), color = "red", linewidth = 1.5, linetype = "solid") +
      geom_vline(aes(xintercept = data$taux_global), color = "darkgreen", linewidth = 1, linetype = "dashed") +
      geom_vline(aes(xintercept = data$taux_annee_des), color = "orange", linewidth = 1, linetype = "dotted") +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Distribution des taux de gestes - Position de", input$admin_interne_benchmark),
        x = "Taux de gestes réalisés",
        y = "Densité",
        caption = "Rouge: Interne sélectionné | Vert: Moyenne générale | Orange: Moyenne année DES"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(p)
  })
  
  # Tableau global tous internes
  output$admin_table_tous_internes <- render_gt({
    df %>%
      filter(!is.na(NOM_interne), !is.na(Geste)) %>%
      mutate(geste_realise = ifelse(Geste == "Yes", 1, 0)) %>%
      group_by(NOM_interne, annee_DES) %>%
      summarise(
        `N interventions` = n(),
        `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
        `Note péda (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
        `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
        .groups = "drop"
      ) %>%
      arrange(desc(`Taux geste (%)`)) %>%
      gt() %>%
      tab_header(title = "Performance de tous les internes") %>%
      fmt_number(columns = c("Note péda (sur 20)", "Ambiance (sur 20)"), decimals = 1) %>%
      data_color(
        columns = "Taux geste (%)",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#de2d26"),
          domain = c(0, 100)
        )
      ) %>%
      cols_label(annee_DES = "Année DES")
  })
  
  # Boxplot par année DES
  output$admin_des_boxplot <- renderPlotly({
    df_plot <- df %>%
      filter(!is.na(annee_DES), !is.na(Geste), !is.na(NOM_interne)) %>%
      mutate(geste_realise = ifelse(Geste == "Yes", 1, 0)) %>%
      group_by(NOM_interne, annee_DES) %>%
      summarise(taux = mean(geste_realise, na.rm = TRUE), .groups = "drop")
    
    if(nrow(df_plot) == 0) {
      plot_ly() %>%
        add_annotations(text = "Aucune donnée disponible", x = 0.5, y = 0.5,
                        showarrow = FALSE, font = list(size = 16))
    } else {
      p <- ggplot(df_plot, aes(x = factor(annee_DES), y = taux, fill = factor(annee_DES))) +
        geom_boxplot(alpha = 0.7) +
        geom_jitter(width = 0.2, alpha = 0.6) +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
        labs(title = "Distribution des taux de gestes par année DES",
             x = "Année DES", y = "Taux de gestes réalisés") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none")
      
      ggplotly(p)
    }
  })
  
  # Tableau par année DES
  output$admin_table_des <- render_gt({
    df %>%
      filter(!is.na(annee_DES), !is.na(Geste)) %>%
      mutate(geste_realise = ifelse(Geste == "Yes", 1, 0)) %>%
      group_by(annee_DES) %>%
      summarise(
        `N internes` = n_distinct(NOM_interne, na.rm = TRUE),
        `N interventions` = n(),
        `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
        `Note péda (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
        `Ambiance (sur 20)` = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
        .groups = "drop"
      ) %>%
      arrange(annee_DES) %>%
      gt() %>%
      tab_header(title = "Performance par année DES") %>%
      fmt_number(columns = c("Note péda (sur 20)", "Ambiance (sur 20)"), decimals = 1) %>%
      data_color(
        columns = "Taux geste (%)",
        colors = scales::col_numeric(
          palette = c("#fee5d9", "#de2d26"),
          domain = c(0, 100)
        )
      ) %>%
      cols_label(annee_DES = "Année DES")
  })
  
  observeEvent(input$quitter, {
    stopApp()
  })
}

shinyApp(ui, server)
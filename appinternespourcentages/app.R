  library(shiny)
  library(calendR)
  library(dplyr)
  library(tidyr)
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
  library(DT) # Pour les tableaux interactifs
  library(remotes)
  
  dfapp <- readRDS("logbook_data.rds")
  
  # Coordonnées des hôpitaux pour la carte
  coord_hopitaux <- data.frame(
    Hôpital = c("HEGP", "Paul_Brousse", "PSL", "Cochin", "St_Louis", "Avicenne", "St_Antoine"),
    lat = c(48.8407, 48.7851, 48.8402, 48.8475, 48.874, 48.91, 48.8483),
    lng = c(2.2711, 2.3351, 2.3600, 2.3420, 2.36, 2.42, 2.382),
    stringsAsFactors = FALSE
  )
  
  get_mois_semestre <- function(date_vec) {
      date_vec <- as.Date(date_vec)
      month_val <- month(date_vec)
      year_val <- year(date_vec)
      
      # Déterminer le type de semestre et l'année académique
      semestre_num <- ifelse(month_val >= 11 | month_val <= 4, 1, 2)
      annee_academique <- ifelse(month_val >= 11, year_val, year_val - 1)
      
      # Calculer le mois dans le semestre
      mois_semestre <- ifelse(semestre_num == 1,
                              # Semestre d'hiver (nov à avril) : nov=M1, déc=M2, jan=M3, fév=M4, mar=M5, avr=M6
                              ifelse(month_val >= 11, month_val - 10, month_val + 2),
                              # Semestre d'été (mai à oct) : mai=M1, juin=M2, jul=M3, aoû=M4, sep=M5, oct=M6
                              month_val - 4
      )
      
      # Créer les labels
      semestre_label <- paste0("S", semestre_num, " ", annee_academique, "-", annee_academique + 1)
      mois_label <- paste0("M", mois_semestre)
      
      return(list(
        semestre = semestre_label,
        mois_semestre = mois_semestre,
        mois_label = mois_label,
        annee_academique = annee_academique
      ))
    }
    
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
  
  liste_noms <- unique(dfapp$NOM_interne)
  liste_noms <- liste_noms[!is.na(liste_noms)]
  liste_noms <- sort(liste_noms)
  
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
          width = 3,
          uiOutput("select_interne_ui"),
          br(),
          # Bouton export Excel (visible pour tous sauf admin sur l'onglet admin)
          conditionalPanel(
            condition = "input.tabs != 'admin'",
            downloadButton("export_excel", "📊 Exporter mes données Excel", 
                           class = "btn-success btn-block"),
            br(), br()
          ),
          actionButton("quitter", "Quitter l'application", class = "btn-danger")
        ),
        mainPanel(
          uiOutput("main_tabs")
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
                 h3("Évolution mensuelle"),
                 fluidRow(
                   column(4,
                          selectInput("type_courbe_gestes", "Type d'affichage :",
                                      choices = list(
                                        "Taux de geste avec (n=)" = "taux_avec_n",
                                        "Nombre d'interventions" = "nombre_interventions", 
                                        "Les deux courbes superposées" = "superposees"
                                      ),
                                      selected = "taux_avec_n")
                   ),
                   column(8, "")  # Espace vide pour l'alignement
                 ),
                 br(),
                 plotlyOutput("courbe_gestes"),  # ← CETTE LIGNE MANQUE !
                 br(),
                 h3("Activité calendaire"),
                 # Menu déroulant intégré directement ici
                 fluidRow(
                   column(4,
                          selectInput("heatmap_type", "Type de heatmap:",
                                      choices = list(
                                        "Jours au bloc" = "jours_bloc",
                                        "Jours avec geste effectué" = "jours_geste", 
                                        "Jours avec 'on recommence'" = "jours_recommence"
                                      ),
                                      selected = "jours_bloc")
                   ),
                   column(8, "")  # Espace vide pour l'alignement
                 ),
                 br(),
                 plotOutput("heatmap_calendaire_github", height = "400px"),  # CHANGÉ en plotOutput
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
        ),
        tabPanel("Analyses détaillées", value = "analyses_detaillees",
                 h3("Analyses par intervention spécifique"),
                 p("Sélectionnez les critères d'analyse ci-dessous. Ces paramètres s'appliquent aux deux onglets.", 
                   style = "font-style: italic; color: #666;"),
                 
                 # SÉLECTIONS COMMUNES TOUT EN HAUT
                 fluidRow(
                   column(3,
                          selectInput("meta_groupe_analyse", "Choisir un méta-groupe :",
                                      choices = NULL, selected = NULL)
                   ),
                   column(3,
                          selectInput("intervention_analyse", "Choisir une intervention :",
                                      choices = NULL, selected = NULL)
                   ),
                   column(3,
                          selectInput("periode_analyse_detaillee", "Période d'analyse :",
                                      choices = NULL, selected = NULL)
                   ),
                   column(3, "") # Espace vide
                 ),
                 br(),
                 
                 # SOUS-ONGLETS
                 tabsetPanel(id = "sous_analyses",
                             
                             # ONGLET 1: DONNÉES GÉNÉRALES
                             tabPanel("📊 Données générales", value = "donnees_generales",
                                      br(),
                                      # Tableaux de résumé SIMPLIFIÉS
                                      fluidRow(
                                        column(6,
                                               h4("📊 Résumé général"),
                                               gt_output("table_resume_intervention_v2")
                                        ),
                                        column(6,
                                               h4("📋 Détails par période"),
                                               gt_output("table_details_intervention_v2")
                                        )
                                      ),
                                      br(),
                                      
                                      # Sélecteur pour la métrique de la courbe
                                      fluidRow(
                                        column(4,
                                               selectInput("metrique_analyse", "Métrique à analyser :",
                                                           choices = list(
                                                             "Nombre d'interventions" = "nb_interventions",
                                                             "Taux de geste" = "taux_geste"
                                                           ),
                                                           selected = "taux_geste")
                                        ),
                                        column(8, "")
                                      ),
                                      br(),
                                      
                                      # Courbe d'évolution
                                      h4("📈 Évolution dans le temps"),
                                      plotlyOutput("courbe_intervention_specifique_v2"),
                                      br()
                             ),
                             
                             # ONGLET 2: ANALYSES DES RESSENTIS
                             tabPanel("💭 Analyses des ressentis", value = "ressentis",
                                      br(),
                                      p("Analyse des ressentis selon que le geste soit réalisé ou non.", 
                                        style = "font-style: italic; color: #666;"),
                                      
                                      # Camemberts côte à côte avec n= affiché
                                      h4("📊 Répartition des ressentis"),
                                      fluidRow(
                                        column(6,
                                               h5("🟢 Quand geste réalisé"),
                                               plotlyOutput("camembert_ressenti_geste_realise_v2"),
                                               br(),
                                               gt_output("table_ressenti_geste_realise_v2")
                                        ),
                                        column(6,
                                               h5("🔴 Quand geste non réalisé"), 
                                               plotlyOutput("camembert_ressenti_geste_non_realise_v2"),
                                               br(),
                                               gt_output("table_ressenti_geste_non_realise_v2")
                                        )
                                      )
                             ),
                             # ONGLET 3: DONNÉES DÉTAILLÉES
                             tabPanel("📋 Données détaillées", value = "donnees_detail",
                                      br(),
                                      p("Toutes les interventions avec leurs détails complets. Le tableau peut être trié et filtré.", 
                                        style = "font-style: italic; color: #666;"),
                                      
                                      # Résumé rapide
                                      fluidRow(
                                        column(4,
                                               h5("🔍 Nombre d'interventions affichées"),
                                               verbatimTextOutput("nb_interventions_detail")
                                        ),
                                        column(8, "")
                                      ),
                                      br(),
                                      
                                      # Tableau interactif DT
                                      h4("📊 Tableau détaillé des interventions"),
                                      DT::dataTableOutput("tableau_interventions_detail"),
                                      br(),
                                      
                                      p("💡 Astuce : Utilisez les filtres de colonnes pour rechercher par opérateur, intervention, etc.", 
                                        style = "font-style: italic; color: #888;")
                             )
                 )
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
                              p("Le graphique ci-dessous affiche toutes les données poolées.", style = "font-style: italic; color: #666;"),
                              plotlyOutput("admin_courbe_hopitaux", height = "500px"),
                              br(),
                              p("Filtrer le tableau par semestre :"),
                              fluidRow(
                                column(3,
                                       selectInput("admin_semestre_hopital", "Semestre :",
                                                   choices = NULL, selected = NULL)
                                ),
                                column(9, "")
                              ),
                              gt_output("admin_table_hopitaux"),
                              br(),
                              
                              h3("😊 Ambiance par hôpital"),
                              p("Le graphique ci-dessous affiche toutes les données poolées.", style = "font-style: italic; color: #666;"),
                              plotlyOutput("admin_ambiance_hopitaux"),
                              br(),
                              p("Filtrer le tableau par semestre :"),
                              fluidRow(
                                column(3,
                                       selectInput("admin_semestre_ambiance", "Semestre :",
                                                   choices = NULL, selected = NULL)
                                ),
                                column(9, "")
                              ),
                              gt_output("admin_table_ambiance_hopitaux"),
                              br(),
                              
                              h3("🎓 Pédagogie par hôpital"),
                              p("Le graphique ci-dessous affiche toutes les données poolées.", style = "font-style: italic; color: #666;"),
                              plotlyOutput("admin_pedagogie_hopitaux"),
                              br(),
                              p("Filtrer le tableau par semestre :"),
                              fluidRow(
                                column(3,
                                       selectInput("admin_semestre_pedagogie", "Semestre :",
                                                   choices = NULL, selected = NULL)
                                ),
                                column(9, "")
                              ),
                              gt_output("admin_table_pedagogie_hopitaux"),
                              
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
                              p("Internes avec au moins 5 interventions - Cliquez sur les colonnes pour trier"),
                              DT::dataTableOutput("admin_table_tous_internes_dt"), # ← Nouveau tableau interactif
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
    
    observe({
      req(user$user)
      if (user$user == "admin") {
        # Obtenir tous les semestres disponibles avec tri chronologique correct
        semestres_disponibles <- dfapp %>%
          filter(!is.na(DATE)) %>%
          mutate(
            semestre = get_semestre(as.Date(DATE)),
            # Extraire année et numéro de semestre pour le tri
            sem_num = as.numeric(substr(semestre, 2, 2)),
            annee_debut = as.numeric(substr(semestre, 4, 7)),
            # Créer clé de tri chronologique
            tri_chrono = annee_debut + (sem_num - 1) * 0.5
          ) %>%
          select(semestre, tri_chrono) %>%
          distinct() %>%
          arrange(desc(tri_chrono)) %>%  # Du plus récent au plus ancien
          pull(semestre)
        
        choices_semestre <- c("Toutes les données" = "all", 
                              setNames(semestres_disponibles, semestres_disponibles))
        
        # Mettre à jour les 3 menus
        updateSelectInput(session, "admin_semestre_hopital",
                          choices = choices_semestre, selected = "all")
        updateSelectInput(session, "admin_semestre_ambiance", 
                          choices = choices_semestre, selected = "all")
        updateSelectInput(session, "admin_semestre_pedagogie",
                          choices = choices_semestre, selected = "all")
      }
    })
    
    # Mise à jour des choix de méta-groupes
    observe({
      req(interne_courant())
      
      meta_groupes_disponibles <- data_interne() %>%
        filter(!is.na(META_GROUPE)) %>%
        count(META_GROUPE, sort = TRUE) %>%
        pull(META_GROUPE)
      
      # CORRECTION : TRI ALPHABÉTIQUE
      meta_groupes_disponibles <- sort(meta_groupes_disponibles)
      
      choices_meta <- c("Tous les méta-groupes" = "tous", 
                        setNames(meta_groupes_disponibles, meta_groupes_disponibles))
      
      updateSelectInput(session, "meta_groupe_analyse",
                        choices = choices_meta, selected = "tous")
    })
    
    # Mise à jour des choix d'interventions selon le méta-groupe
    observeEvent(input$meta_groupe_analyse, {
      req(input$meta_groupe_analyse, interne_courant())
      
      if(input$meta_groupe_analyse == "tous") {
        choices_interv <- c("Analyse poolée" = "poolee")
      } else {
        interventions_disponibles <- data_interne() %>%
          filter(!is.na(META_GROUPE), !is.na(INTERVENTION_GROUPÉE),
                 META_GROUPE == input$meta_groupe_analyse) %>%
          count(INTERVENTION_GROUPÉE, sort = TRUE) %>%
          pull(INTERVENTION_GROUPÉE)
        
        # CORRECTION : TRI ALPHABÉTIQUE
        interventions_disponibles <- sort(interventions_disponibles)
        
        choices_interv <- c("Analyse poolée du méta-groupe" = "poolee_meta",
                            setNames(interventions_disponibles, interventions_disponibles))
      }
      
      updateSelectInput(session, "intervention_analyse",
                        choices = choices_interv, selected = choices_interv[1])
    })
    
    
    # Données filtrées avec création des variables binaires
    data_interne_base <- reactive({
      req(interne_courant())
      dfapp %>%
        filter(NOM_interne == interne_courant()) %>%
        mutate(
          geste_realise = ifelse(Geste == "Yes", 1, 0),
          on_recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0),
          annee = year(as.Date(DATE)),
          semestre = get_semestre(as.Date(DATE))
        )
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
      req(interne_courant(), input$type_courbe_gestes)
      
      df_plot <- data_interne() %>%
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
            text = "Aucune donnée disponible",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 16)
          )
      } else {
        
        if(input$type_courbe_gestes == "taux_avec_n") {
          # Courbe du taux avec (n=) sur chaque point
          p <- ggplot(df_plot, aes(x = mois, y = taux)) +
            geom_line(color = "#377eb8", linewidth = 1.5) +
            geom_point(size = 3, color = "#377eb8") +
            geom_text(aes(label = paste0("(n=", n, ")")), 
                      hjust = 0.5, vjust = 0.5, 
                      nudge_y = 0.05,  # Décale de 5% vers le haut
                      size = 3, color = "#2c5aa0", fontface = "bold") +
            scale_y_continuous(labels = percent_format(accuracy = 1), 
                               limits = c(0, 1), 
                               expand = expansion(mult = c(0.02, 0.1))) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                         expand = expansion(mult = c(0.01, 0.05))) +
            labs(title = paste("Taux de gestes réalisés par mois -", interne_courant()),
                 x = "Mois", y = "Taux de gestes réalisés") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
        } else if(input$type_courbe_gestes == "nombre_interventions") {
          # Courbe du nombre d'interventions en vert
          p <- ggplot(df_plot, aes(x = mois, y = n)) +
            geom_line(color = "#4daf4a", linewidth = 1.5) +
            geom_point(size = 3, color = "#4daf4a") +
            geom_text(aes(label = paste0("(n=", n, ")")), 
                      hjust = 0.5, vjust = 0.5, 
                      nudge_y = max(df_plot$n) * 0.08,  # Décale proportionnellement
                      size = 3, color = "#2d5a2d", fontface = "bold") +
            scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                         expand = expansion(mult = c(0.01, 0.05))) +
            labs(title = paste("Nombre d'interventions par mois -", interne_courant()),
                 x = "Mois", y = "Nombre d'interventions") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
        } else {
          # Les deux courbes superposées
          # Normaliser le nombre d'interventions pour l'affichage sur la même échelle
          df_plot_norm <- df_plot %>%
            mutate(n_norm = n / max(n, na.rm = TRUE))  # Normalisation 0-1
          
          p <- ggplot(df_plot_norm, aes(x = mois)) +
            # Courbe du taux (axe gauche)
            geom_line(aes(y = taux, color = "Taux de geste"), linewidth = 1.5) +
            geom_point(aes(y = taux, color = "Taux de geste"), size = 3) +
            geom_text(aes(y = taux, label = paste0(round(taux*100), "% (n=", n, ")")), 
          hjust = 0.5, vjust = 0.5, 
          nudge_y = 0.06,  # Décale de 6% vers le haut
          size = 2.5, color = "#377eb8", fontface = "bold") +
            
            # Courbe du nombre d'interventions normalisée (axe droit)
            geom_line(aes(y = n_norm, color = "Nb interventions (normalisé)"), 
                      linewidth = 1.5, linetype = "dashed") +
            geom_point(aes(y = n_norm, color = "Nb interventions (normalisé)"), size = 3) +
            
            # Échelles et labels
            scale_y_continuous(
              name = "Taux de gestes réalisés",
              labels = percent_format(accuracy = 1),
              limits = c(0, 1),
              expand = expansion(mult = c(0.02, 0.15)),
              sec.axis = sec_axis(~ . * max(df_plot$n), 
                                  name = "Nombre d'interventions")
            ) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                         expand = expansion(mult = c(0.01, 0.05))) +
            scale_color_manual(
              name = "Métriques",
              values = c("Taux de geste" = "#377eb8", 
                         "Nb interventions (normalisé)" = "#4daf4a")
            ) +
            labs(title = paste("Taux de gestes et nombre d'interventions par mois -", interne_courant()),
                 x = "Mois") +
            theme_minimal(base_size = 14) +
            theme(
              axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom"
            )
        }
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          layout(hovermode = 'x unified')
      }
    })
    
    output$heatmap_calendaire_github <- renderPlot({
      req(input$heatmap_type, interne_courant())  # AJOUTÉ: req pour s'assurer que les inputs existent
      
      # Données de base
      data_cal <- data_interne() %>%
        filter(!is.na(DATE)) %>%
        mutate(
          DATE = as.Date(DATE),
          geste_realise = ifelse(Geste == "Yes", 1, 0),
          recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0)
        )
      
      if(nrow(data_cal) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Aucune donnée disponible", cex = 1.5, col = "gray50")
        return()
      }
      
      # Agrégation selon le type choisi
      if(input$heatmap_type == "jours_bloc") {
        data_agg <- data_cal %>%
          group_by(DATE) %>%
          summarise(valeur = n(), .groups = "drop")
        titre <- "Nombre d'interventions par jour"
        subtitle <- "Plus la couleur est foncée, plus il y a eu d'interventions"
        
      } else if(input$heatmap_type == "jours_geste") {
        data_agg <- data_cal %>%
          group_by(DATE) %>%
          summarise(
            total = n(),
            gestes = sum(geste_realise, na.rm = TRUE),
            valeur = gestes,
            .groups = "drop"
          )
        titre <- "Nombre de gestes réalisés par jour"
        subtitle <- "Plus la couleur est foncée, plus il y a eu de gestes"
        
      } else { # jours_recommence
        data_agg <- data_cal %>%
          group_by(DATE) %>%
          summarise(
            total = n(),
            recommences = sum(recommence, na.rm = TRUE),
            valeur = recommences,
            .groups = "drop"
          )
        titre <- "Nombre de 'on recommence' par jour"
        subtitle <- "Plus la couleur est foncée, plus il y a eu de 'on recommence'"
      }
      
      # Préparer les données pour calendR
      # Période : année courante uniquement
      annee_courante <- year(Sys.Date())
      
      # Filtrer pour l'année courante
      data_annee <- data_agg %>%
        filter(year(DATE) == annee_courante)
      
      if(nrow(data_annee) == 0) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, paste("Aucune donnée pour", annee_courante), cex = 1.5, col = "gray50")
        return()
      }
      
      # Créer une séquence complète de dates pour l'année
      debut_annee <- as.Date(paste0(annee_courante, "-01-01"))
      fin_annee <- as.Date(paste0(annee_courante, "-12-31"))
      toutes_dates <- seq.Date(from = debut_annee, to = fin_annee, by = "day")
      
      # Joindre avec nos données
      donnees_completes <- data.frame(DATE = toutes_dates) %>%
        left_join(data_annee, by = "DATE") %>%
        mutate(valeur = ifelse(is.na(valeur), 0, valeur))
      
      # Créer le vecteur de valeurs pour calendR
      valeurs_calendar <- donnees_completes$valeur
      
      # Déterminer les couleurs (style GitHub)
      max_val <- max(valeurs_calendar, na.rm = TRUE)
      if(max_val == 0) {
        couleurs <- c("#ebedf0", "#c6e48b")
      } else {
        # Palette GitHub-like (du plus clair au plus foncé)
        couleurs <- c("#ebedf0", "#c6e48b", "#7bc96f", "#239a3b", "#196127")
      }
      
      # Créer le calendrier heatmap (ARGUMENTS CORRIGÉS)
      calendR(
        year = annee_courante,
        special.days = valeurs_calendar,
        gradient = TRUE,
        low.col = couleurs[1],  # Couleur pour les valeurs faibles
        col = couleurs[length(couleurs)],  # Couleur pour les valeurs élevées
        title = paste(titre, "-", interne_courant()),
        subtitle = subtitle,
        title.size = 16,
        subtitle.size = 12,
        weeknames = c("L", "M", "M", "J", "V", "S", "D"),
        months.col = "gray30",
        weeknames.col = "gray30",
        special.col = couleurs[length(couleurs)],
        # SUPPRIMÉ: low = 0  (cet argument n'existe pas)
        orientation = "landscape",
        start = "M",  # Commencer par lundi
        mbg.col = "white",  # Couleur de fond des mois
        bg.col = "white"    # Couleur de fond général
      )
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
        mutate(DATE = as.Date(DATE)) %>%
        rowwise() %>%
        mutate(
          mois_info = list(get_mois_semestre(DATE)),
          mois_semestre = mois_info$mois_semestre,
          mois_label = mois_info$mois_label,
          semestre = mois_info$semestre
        ) %>%
        ungroup() %>%
        filter(mois_semestre >= 1 & mois_semestre <= 6) %>%  # Garder seulement M1 à M6
        group_by(mois_semestre, mois_label, Hôpital, semestre) %>%
        summarise(
          taux = mean(geste_realise, na.rm = TRUE), 
          n = n(), 
          .groups = "drop"
        ) %>%
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
        p <- ggplot(df_plot, aes(x = mois_semestre, y = taux, color = Hôpital, linetype = semestre)) +
          geom_line(linewidth = 1.2) +
          geom_point(size = 2) +
          scale_x_continuous(
            breaks = 1:6, 
            labels = paste0("M", 1:6),
            limits = c(0.5, 6.5)
          ) +
          scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
          labs(
            title = paste("Taux de gestes par service - Évolution M1 à M6 -", interne_courant()),
            subtitle = "Superposition des semestres (M1=Nov ou Mai, M6=Avr ou Oct)",
            x = "Mois du semestre", 
            y = "Taux de gestes réalisés",
            linetype = "Semestre"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "bottom",
            legend.box = "horizontal"
          )
        
        ggplotly(p, tooltip = c("colour", "linetype", "x", "y"))
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
        filter(n >= 5) %>%  # Au moins 5 interventions
        # CORRECTION DE L'ORDRE CHRONOLOGIQUE (méthode simplifiée)
        mutate(
          # Extraire le numéro de semestre et l'année de début
          sem_num = as.numeric(substr(semestre, 2, 2)),
          annee_debut = as.numeric(substr(semestre, 4, 7)),
          # Créer une clé de tri chronologique
          tri_chrono = annee_debut + (sem_num - 1) * 0.5
        ) %>%
        arrange(tri_chrono, Hôpital)
      
      if(nrow(df_plot) == 0) {
        plot_ly() %>%
          add_annotations(
            text = "Pas assez de données pour cette analyse",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 16)
          )
      } else {
        # Créer un facteur ordonné pour les semestres
        ordre_chronologique <- df_plot %>%
          select(semestre, tri_chrono) %>%
          distinct() %>%
          arrange(tri_chrono) %>%
          pull(semestre)
        
        df_plot$semestre <- factor(df_plot$semestre, levels = ordre_chronologique)
        
        p <- ggplot(df_plot, aes(x = semestre, y = taux_geste, color = Hôpital, group = Hôpital)) +
          geom_line(linewidth = 1.2, alpha = 0.8) +
          geom_point(size = 3) +
          scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
          labs(
            title = paste("Évolution par service dans le temps -", interne_courant()),
            subtitle = "Points reliés par des lignes - Ordre chronologique corrigé",
            x = "Semestre", 
            y = "Taux de gestes réalisés"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          )
        
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
        donnees_interne <- dfapp %>%
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
      dfapp %>%
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
      n_internes <- length(unique(dfapp$NOM_interne[!is.na(dfapp$NOM_interne)]))
      valueBox(n_internes, "Internes", icon = icon("user-md"), color = "purple")
    })
    
    output$admin_box_total_interventions <- renderValueBox({
      n_interventions <- nrow(dfapp)
      valueBox(n_interventions, "Interventions totales", icon = icon("procedures"), color = "blue")
    })
    
    output$admin_box_taux_global <- renderValueBox({
      taux_global <- mean(ifelse(dfapp$Geste == "Yes", 1, 0), na.rm = TRUE)
      valueBox(paste0(round(taux_global * 100), "%"), "Taux global gestes", icon = icon("hand-holding-medical"), color = "green")
    })
    
    output$admin_box_total_hopitaux <- renderValueBox({
      n_hopitaux <- length(unique(dfapp$Hôpital[!is.na(dfapp$Hôpital)]))
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
      df_plot <- dfapp %>%
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
      req(input$admin_semestre_hopital)
      
      # Filtrer par semestre si nécessaire
      data_filtered <- if(input$admin_semestre_hopital == "all") {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(Geste))
      } else {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(Geste), !is.na(DATE)) %>%
          mutate(semestre = get_semestre(as.Date(DATE))) %>%
          filter(semestre == input$admin_semestre_hopital)
      }
      
      # Calculer les statistiques
      result <- data_filtered %>%
        mutate(
          Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital),
          geste_realise = ifelse(Geste == "Yes", 1, 0),
          mois = format(as.Date(DATE), "%Y-%m")
        ) %>%
        group_by(Hôpital) %>%
        summarise(
          n_internes = n_distinct(NOM_interne, na.rm = TRUE),
          n_interventions = n(),
          taux_geste = if(n() > 0) round(mean(geste_realise, na.rm = TRUE) * 100, 1) else NA_real_,
          n_mois_recueil = n_distinct(mois),
          n_semestres = round(n_mois_recueil / 6, 1),
          .groups = "drop"
        ) %>%
        arrange(desc(taux_geste))
      
      # Créer le tableau
      result %>%
        select(Hôpital, n_internes, n_interventions, taux_geste, n_semestres) %>%
        gt() %>%
        tab_header(
          title = "Performance par hôpital - Taux de gestes",
          subtitle = if(input$admin_semestre_hopital == "all") "Toutes les données" else input$admin_semestre_hopital
        ) %>%
        cols_label(
          Hôpital = "Hôpital",
          n_internes = "N internes",
          n_interventions = "N interventions", 
          taux_geste = "Taux geste (%)",
          n_semestres = "Semestres"
        ) %>%
        sub_missing(columns = everything(), missing_text = "NA") %>%
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
      df_plot <- dfapp %>%
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
      req(input$admin_semestre_ambiance)
      
      # Filtrer par semestre si nécessaire
      data_filtered <- if(input$admin_semestre_ambiance == "all") {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(AMBIANCE))
      } else {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(AMBIANCE), !is.na(DATE)) %>%
          mutate(semestre = get_semestre(as.Date(DATE))) %>%
          filter(semestre == input$admin_semestre_ambiance)
      }
      
      # Calculer les statistiques
      result <- data_filtered %>%
        mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
        group_by(Hôpital) %>%
        summarise(
          n_interventions = n(),
          ambiance_score = if(n() > 0) round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1) else NA_real_,
          pct_on_recommence = if(n() > 0) round(mean(AMBIANCE == "3 - on recommence", na.rm = TRUE) * 100, 1) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(desc(ambiance_score))
      
      # Créer le tableau
      result %>%
        gt() %>%
        tab_header(
          title = "Ambiance par hôpital",
          subtitle = if(input$admin_semestre_ambiance == "all") "Toutes les données" else input$admin_semestre_ambiance
        ) %>%
        cols_label(
          Hôpital = "Hôpital",
          n_interventions = "N interventions",
          ambiance_score = "Score ambiance (/20)",
          pct_on_recommence = "% 'On recommence'"
        ) %>%
        sub_missing(columns = everything(), missing_text = "NA") %>%
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
      df_plot <- dfapp %>%
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
      req(input$admin_semestre_pedagogie)
      
      # Filtrer par semestre si nécessaire
      data_filtered <- if(input$admin_semestre_pedagogie == "all") {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(PEDAGOGIE))
      } else {
        dfapp %>%
          filter(!is.na(Hôpital), !is.na(PEDAGOGIE), !is.na(DATE)) %>%
          mutate(semestre = get_semestre(as.Date(DATE))) %>%
          filter(semestre == input$admin_semestre_pedagogie)
      }
      
      # Calculer les statistiques
      result <- data_filtered %>%
        mutate(Hôpital = ifelse(Hôpital %in% c("Cochin", "Cochin2"), "Cochin", Hôpital)) %>%
        group_by(Hôpital) %>%
        summarise(
          n_interventions = n(),
          pedagogie_score = if(n() > 0) round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1) else NA_real_,
          pct_incroyable = if(n() > 0) round(mean(PEDAGOGIE == "5-incroyable!!", na.rm = TRUE) * 100, 1) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(desc(pedagogie_score))
      
      # Créer le tableau
      result %>%
        gt() %>%
        tab_header(
          title = "Pédagogie par hôpital",
          subtitle = if(input$admin_semestre_pedagogie == "all") "Toutes les données" else input$admin_semestre_pedagogie
        ) %>%
        cols_label(
          Hôpital = "Hôpital",
          n_interventions = "N interventions",
          pedagogie_score = "Score pédagogie (/20)",
          pct_incroyable = "% 'Incroyable'"
        ) %>%
        sub_missing(columns = everything(), missing_text = "NA") %>%
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
      interne_data <- dfapp %>%
        filter(NOM_interne == input$admin_interne_benchmark, !is.na(Geste)) %>%
        mutate(geste_realise = ifelse(Geste == "Yes", 1, 0))
      
      if(nrow(interne_data) == 0) return(NULL)
      
      annee_des_interne <- interne_data$annee_DES[1]
      taux_interne <- mean(interne_data$geste_realise, na.rm = TRUE)
      
      # Benchmark global (tous internes)
      taux_global <- dfapp %>%
        filter(!is.na(Geste)) %>%
        summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE)) %>%
        pull(taux)
      
      # Benchmark par année DES
      taux_annee_des <- dfapp %>%
        filter(!is.na(Geste), annee_DES == annee_des_interne) %>%
        summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE)) %>%
        pull(taux)
      
      # Distribution des taux par interne
      taux_par_interne <- dfapp %>%
        filter(!is.na(NOM_interne), !is.na(Geste)) %>%
        group_by(NOM_interne) %>%
        summarise(taux = mean(ifelse(Geste == "Yes", 1, 0), na.rm = TRUE), .groups = "drop")
      
      # Percentile global
      percentile_global <- round(mean(taux_par_interne$taux <= taux_interne) * 100)
      
      # Percentile année DES
      taux_meme_annee <- dfapp %>%
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
    output$admin_table_tous_internes_dt <- DT::renderDataTable({
      table_data <- dfapp %>%
        filter(!is.na(NOM_interne), !is.na(Geste)) %>%
        mutate(geste_realise = ifelse(Geste == "Yes", 1, 0)) %>%
        group_by(NOM_interne, annee_DES) %>%
        summarise(
          `N interventions`    = n(),
          `Taux geste (%)`     = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
          `Note péda (sur 20)` = round(mean(as.numeric(PEDAGOGIE), na.rm = TRUE) * 4, 1),
          `Ambiance (sur 20)`  = round(mean(as.numeric(AMBIANCE), na.rm = TRUE) * (20/3), 1),
          .groups = "drop"
        ) %>%
        filter(`N interventions` >= 5) %>%
        arrange(desc(`Taux geste (%)`))
      
      # Renommage des colonnes
      colnames(table_data) <- c(
        "Nom interne", "Année DES", "N interventions",
        "Taux geste (%)", "Note péda (sur 20)", "Ambiance (sur 20)"
      )
      
      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          order      = list(list(3, 'desc')),  # 0-based → 3 = 4ᵉ colonne
          columnDefs = list(
            list(targets = 3:5, className = "dt-right"),
            list(
              targets = 3,
              render  = DT::JS(
                "function(data, type) {
                 if (type === 'display') {
                   var color = data >= 70 ? '#d4edda'
                              : data >= 50 ? '#fff3cd'
                              : '#f8d7da';
                   return '<span style=\"background-color:' + color +
                          '; padding:2px 6px; border-radius:3px;\">' +
                          data + '%</span>';
                 }
                 return data;
               }"
              )
            )
          ),
          language = list(
            search     = "Rechercher :",
            lengthMenu = "Afficher _MENU_ internes par page",
            info       = "Affichage de _START_ à _END_ sur _TOTAL_ internes",
            paginate   = list(
              first     = "Premier",
              last      = "Dernier",
              `next`    = "Suivant",
              `previous`= "Précédent"
            )
          )
        ),
        class   = "display",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Taux geste (%)",
          backgroundColor = DT::styleInterval(
            cuts   = c(50, 70),
            values = c("#f8d7da", "#fff3cd", "#d4edda")
          )
        ) %>%
        DT::formatStyle(
          columns        = c("Note péda (sur 20)", "Ambiance (sur 20)"),
          backgroundColor = "#f8f9fa"
        )
    }, server = FALSE)
    
    # Boxplot par année DES
    output$admin_des_boxplot <- renderPlotly({
      df_plot <- dfapp %>%
        filter(!is.na(annee_DES), !is.na(Geste), !is.na(NOM_interne)) %>%
        mutate(geste_realise = ifelse(Geste == "Yes", 1, 0)) %>%
        group_by(NOM_interne, annee_DES) %>%
        summarise(taux = mean(geste_realise, na.rm = TRUE), .groups = "drop")
      
      if (nrow(df_plot) == 0) {
        plot_ly() %>%
          add_annotations(
            text      = "Aucune donnée disponible",
            x         = 0.5, y = 0.5,
            showarrow = FALSE,
            font      = list(size = 16)
          )
      } else {
        p <- ggplot(df_plot, aes(x = factor(annee_DES), y = taux, fill = factor(annee_DES))) +
          geom_boxplot(alpha = 0.7) +
          geom_jitter(width = 0.2, alpha = 0.6) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
          labs(
            title = "Distribution des taux de gestes par année DES",
            x     = "Année DES",
            y     = "Taux de gestes réalisés"
          ) +
          theme_minimal(base_size = 12) +
          theme(legend.position = "none")
        
        ggplotly(p)
      }
    })
    
    
    # Tableau par année DES
    output$admin_table_des <- render_gt({
      dfapp %>%
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
    
    # Mise à jour du sélecteur de période basé sur les données disponibles
    observe({
      req(interne_courant())
      
      # Obtenir tous les semestres disponibles avec tri chronologique correct
      semestres_disponibles <- data_interne() %>%
        filter(!is.na(DATE)) %>%
        mutate(
          semestre = get_semestre(as.Date(DATE)),
          # Extraire année et numéro de semestre pour le tri
          sem_num = as.numeric(substr(semestre, 2, 2)),
          annee_debut = as.numeric(substr(semestre, 4, 7)),
          # Créer clé de tri chronologique
          tri_chrono = annee_debut + (sem_num - 1) * 0.5
        ) %>%
        select(semestre, tri_chrono) %>%
        distinct() %>%
        arrange(desc(tri_chrono)) %>%  # Du plus récent au plus ancien
        pull(semestre)
      
      choices_periode <- c("Toutes les données" = "all", 
                           setNames(semestres_disponibles, semestres_disponibles))
      
      updateSelectInput(session, "periode_analyse_detaillee",
                        choices = choices_periode, selected = "all")
    })
    
    # Fonction réactive pour filtrer les données selon la période
    data_interne_periode <- reactive({
      req(interne_courant())
      
      if(is.null(input$periode_analyse_detaillee) || input$periode_analyse_detaillee == "all") {
        return(data_interne())
      } else {
        data_interne() %>%
          filter(semestre == input$periode_analyse_detaillee)
      }
    })
    
    # NOUVEAU TABLEAU RÉSUMÉ SIMPLIFIÉ (sans "avec ressenti")
    output$table_resume_intervention_v2 <- render_gt({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrage selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      # Calculer les statistiques générales SIMPLIFIÉES
      resume <- data_filtered %>%
        summarise(
          `Total interventions` = n(),
          `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1)
        )
      
      resume %>%
        gt() %>%
        tab_header(title = "Résumé général") %>%
        data_color(columns = "Taux geste (%)", 
                   colors = scales::col_numeric(palette = c("#fee5d9", "#de2d26"), domain = c(0, 100)))
    })
    
    # NOUVEAU TABLEAU DÉTAILS PAR PÉRIODE
    output$table_details_intervention_v2 <- render_gt({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Même logique de filtrage
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      # Si on analyse une période spécifique, montrer par mois, sinon par semestre
      if(input$periode_analyse_detaillee == "all") {
        # Grouper par semestre avec tri chronologique
        details <- data_filtered %>%
          filter(!is.na(semestre)) %>%
          group_by(semestre) %>%
          summarise(
            `N interventions` = n(),
            `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
            .groups = "drop"
          ) %>%
          # TRI CHRONOLOGIQUE
          mutate(
            sem_num = as.numeric(substr(semestre, 2, 2)),
            annee_debut = as.numeric(substr(semestre, 4, 7)),
            tri_chrono = annee_debut + (sem_num - 1) * 0.5
          ) %>%
          arrange(desc(tri_chrono)) %>%
          select(-sem_num, -annee_debut, -tri_chrono) %>%
          head(6)
        
        titre <- "Détails par semestre"
      } else {
        # Grouper par mois pour une période spécifique
        details <- data_filtered %>%
          filter(!is.na(DATE)) %>%
          mutate(
            DATE = as.Date(DATE),
            mois = floor_date(DATE, "month"),
            mois_label = format(mois, "%b %Y")
          ) %>%
          group_by(mois, mois_label) %>%
          summarise(
            `N interventions` = n(),
            `Taux geste (%)` = round(mean(geste_realise, na.rm = TRUE) * 100, 1),
            .groups = "drop"
          ) %>%
          arrange(desc(mois)) %>%
          select(-mois) %>%
          rename(Mois = mois_label) %>%
          head(6)
        
        titre <- "Détails par mois"
      }
      
      if(nrow(details) == 0) {
        data.frame(`Période` = "Aucune donnée", `N interventions` = 0, `Taux geste (%)` = 0,
                   check.names = FALSE) %>%
          gt() %>%
          tab_header(title = titre)
      } else {
        details %>%
          gt() %>%
          tab_header(title = titre) %>%
          data_color(columns = "Taux geste (%)", 
                     colors = scales::col_numeric(palette = c("#fee5d9", "#de2d26"), domain = c(0, 100)))
      }
    })
    
    # NOUVELLE COURBE AVEC DONNÉES FILTRÉES PAR PÉRIODE
    output$courbe_intervention_specifique_v2 <- renderPlotly({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse, input$metrique_analyse)
      
      # Filtrer selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      # Calculer la métrique par mois
      df_plot <- data_filtered %>%
        filter(!is.na(DATE)) %>%
        mutate(DATE = as.Date(DATE), mois = floor_date(DATE, "month"))
      
      if(input$metrique_analyse == "nb_interventions") {
        df_plot <- df_plot %>%
          group_by(mois) %>%
          summarise(valeur = n(), .groups = "drop") %>%
          filter(valeur >= 1)
        titre <- "Nombre d'interventions par mois"
        y_label <- "Nombre d'interventions"
        couleur <- "#4daf4a"
        
      } else { # taux_geste
        df_plot <- df_plot %>%
          filter(!is.na(Geste)) %>%
          group_by(mois) %>%
          summarise(valeur = mean(geste_realise, na.rm = TRUE), n = n(), .groups = "drop") %>%
          filter(n >= 1)
        titre <- "Taux de geste réalisé par mois"
        y_label <- "Taux de geste"
        couleur <- "#377eb8"
      }
      
      if(nrow(df_plot) == 0) {
        plot_ly() %>%
          add_annotations(text = "Pas assez de données pour cette analyse",
                          x = 0.5, y = 0.5, showarrow = FALSE, font = list(size = 16))
      } else {
        # Ajuster les labels selon la métrique
        y_limits <- if(input$metrique_analyse == "taux_geste") c(0, 1) else NULL
        y_labels <- if(input$metrique_analyse == "taux_geste") percent_format(accuracy = 1) else waiver()
        
        p <- ggplot(df_plot, aes(x = mois, y = valeur)) +
          geom_line(color = couleur, linewidth = 1.5) +
          geom_point(size = 3, color = couleur) +
          {
            if(input$metrique_analyse == "taux_geste") {
              geom_text(aes(label = paste0(round(valeur * 100, 1), "%")), 
                        hjust = 0.5, vjust = 0.5, nudge_y = max(df_plot$valeur) * 0.05,
                        size = 3, color = scales::muted(couleur), fontface = "bold")
            } else {
              geom_text(aes(label = paste0("(n=", valeur, ")")), 
                        hjust = 0.5, vjust = 0.5, nudge_y = max(df_plot$valeur) * 0.05,
                        size = 3, color = scales::muted(couleur), fontface = "bold")
            }
          } +
          scale_y_continuous(labels = y_labels, limits = y_limits) +
          scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
          labs(title = paste(titre, "-", interne_courant()),
               subtitle = paste(
                 "Intervention :",
                 if(input$intervention_analyse == "poolee") "Toutes interventions"
                 else if(input$intervention_analyse == "poolee_meta") paste("Toutes -", input$meta_groupe_analyse)
                 else input$intervention_analyse
               ),
               x = "Mois", y = y_label) +
          theme_minimal(base_size = 12) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p, tooltip = c("x", "y"))
      }
    })
    
    # NOUVEAUX CAMEMBERTS POUR LES RESSENTIS
    output$camembert_ressenti_geste_realise_v2 <- renderPlotly({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrer selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      data_cam <- data_filtered %>%
        filter(Geste == "Yes", !is.na(Geste_a_l_aise)) %>%
        count(Geste_a_l_aise, name = "n") %>%
        mutate(pourcentage = round(100 * n / sum(n), 1))
      
      if(nrow(data_cam) == 0) {
        plot_ly() %>% add_annotations(text = "Aucune donnée", x = 0.5, y = 0.5, showarrow = FALSE)
      } else {
        total_geste_realise <- sum(data_cam$n)
        
        plot_ly(data_cam, 
                labels = ~Geste_a_l_aise, 
                values = ~n, 
                type = 'pie',
                textinfo = 'label+percent',
                textposition = 'inside',
                marker = list(colors = c("#FFB6C1", "#98FB98", "#87CEEB")),
                hovertemplate = "<b>%{label}</b><br>%{value} gestes<br>%{percent}<extra></extra>") %>%
          layout(
            showlegend = FALSE, 
            margin = list(t = 30, b = 0, l = 0, r = 0),
            annotations = list(
              text = paste0("n = ", total_geste_realise, " gestes"),
              x = 0.5, y = -0.1, showarrow = FALSE,
              font = list(size = 14, color = "gray")
            )
          )
      }
    })
    
    output$camembert_ressenti_geste_non_realise_v2 <- renderPlotly({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrer selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      data_cam <- data_filtered %>%
        filter(Geste == "No", !is.na(Si_pas_de_geste_RESSENTI)) %>%
        count(Si_pas_de_geste_RESSENTI, name = "n") %>%
        mutate(pourcentage = round(100 * n / sum(n), 1))
      
      if(nrow(data_cam) == 0) {
        plot_ly() %>% add_annotations(text = "Aucune donnée", x = 0.5, y = 0.5, showarrow = FALSE)
      } else {
        total_geste_non_realise <- sum(data_cam$n)
        
        plot_ly(data_cam, 
                labels = ~Si_pas_de_geste_RESSENTI, 
                values = ~n, 
                type = 'pie',
                textinfo = 'label+percent',
                textposition = 'inside',
                marker = list(colors = c("#FFA07A", "#FF6347")),
                hovertemplate = "<b>%{label}</b><br>%{value} interventions<br>%{percent}<extra></extra>") %>%
          layout(
            showlegend = FALSE, 
            margin = list(t = 30, b = 0, l = 0, r = 0),
            annotations = list(
              text = paste0("n = ", total_geste_non_realise, " sans geste"),
              x = 0.5, y = -0.1, showarrow = FALSE,
              font = list(size = 14, color = "gray")
            )
          )
      }
    })
    
    # NOUVEAUX TABLEAUX POUR LES RESSENTIS
    output$table_ressenti_geste_realise_v2 <- render_gt({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrer selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      data_filtered %>%
        filter(Geste == "Yes", !is.na(Geste_a_l_aise)) %>%
        count(Geste_a_l_aise, name = "N") %>%
        mutate(Pourcentage = round(100 * N / sum(N), 1)) %>%
        arrange(desc(N)) %>%
        gt() %>%
        tab_header(title = "Détail - Geste réalisé") %>%
        cols_label(Geste_a_l_aise = "Ressenti", N = "Nombre", Pourcentage = "%") %>%
        data_color(columns = "Pourcentage", 
                   colors = scales::col_numeric(palette = c("#f0f0f0", "#2E8B57"), domain = c(0, 100)))
    })
    
    output$table_ressenti_geste_non_realise_v2 <- render_gt({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrer selon les sélections
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      data_filtered %>%
        filter(Geste == "No", !is.na(Si_pas_de_geste_RESSENTI)) %>%
        count(Si_pas_de_geste_RESSENTI, name = "N") %>%
        mutate(Pourcentage = round(100 * N / sum(N), 1)) %>%
        arrange(desc(N)) %>%
        gt() %>%
        tab_header(title = "Détail - Geste non réalisé") %>%
        cols_label(Si_pas_de_geste_RESSENTI = "Ressenti", N = "Nombre", Pourcentage = "%") %>%
        data_color(columns = "Pourcentage",
                   colors = scales::col_numeric(palette = c("#f0f0f0", "#CD5C5C"), domain = c(0, 100)))
    })
    
    # AFFICHAGE DU NOMBRE D'INTERVENTIONS
    output$nb_interventions_detail <- renderText({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Même logique de filtrage que les autres onglets
      data_filtered <- if(input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      
      if(input$intervention_analyse != "poolee" && input$intervention_analyse != "poolee_meta") {
        data_filtered <- data_filtered %>% 
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      paste0(nrow(data_filtered), " intervention(s)")
    })
    
    # TABLEAU DÉTAILLÉ DES INTERVENTIONS
    output$tableau_interventions_detail <- DT::renderDataTable({
      req(interne_courant(), input$meta_groupe_analyse, input$intervention_analyse)
      
      # Filtrage par méta-groupe et intervention
      data_filtered <- if (input$meta_groupe_analyse == "tous") {
        data_interne_periode()
      } else {
        data_interne_periode() %>% filter(META_GROUPE == input$meta_groupe_analyse)
      }
      if (! input$intervention_analyse %in% c("poolee", "poolee_meta")) {
        data_filtered <- data_filtered %>%
          filter(INTERVENTION_GROUPÉE == input$intervention_analyse)
      }
      
      # Construction de table_data
      table_data <- data_filtered %>%
        select(
          DATE,
          INTERVENTION,
          OPERATEUR,
          RANG_BOSS,
          Geste,
          QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout,
          Geste_whole_text,
          AMBIANCE,
          PEDAGOGIE,
          Geste_a_l_aise,
          Si_pas_de_geste_RESSENTI
        ) %>%
        mutate(
          Date = as.Date(DATE),
          `Geste réalisé` = ifelse(Geste == "Yes", "✅ Oui", "❌ Non"),
          `Quel geste ?` = case_when(
            QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout == "0" ~ "No",
            QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout == "1" ~ "Paroi",
            QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout == "2" ~ "Dissection",
            QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout == "3" ~ "Anastomose",
            QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout == "4" ~ "Tout",
            TRUE ~ as.character(QUEL_GESTE_0No_1paroi_2dissection_3anastomose_4Tout)
          ),
          `Geste (texte)` = as.character(Geste_whole_text),
          Ambiance = case_when(
            AMBIANCE == "1 - je veux partir" ~ "😞 Je veux partir",
            AMBIANCE == "2 - c'est ok"       ~ "😕 C'est ok",
            AMBIANCE == "3 - on recommence"  ~ "😊 On recommence",
            TRUE                              ~ as.character(AMBIANCE)
          ),
          Pédagogie = case_when(
            PEDAGOGIE == "1-rien"         ~ "❌ Aucune",
            PEDAGOGIE == "2-quasi rien"   ~ "😕 Faible",
            PEDAGOGIE == "3-ok"           ~ "😐 Correcte",
            PEDAGOGIE == "4-bien"         ~ "😊 Bonne",
            PEDAGOGIE == "5-incroyable!!" ~ "🔥 Incroyable",
            TRUE                          ~ as.character(PEDAGOGIE)
          ),
          `Ressenti si geste` = case_when(
            Geste_a_l_aise == "1 - impossible sans chef"     ~ "😰 Impossible sans chef",
            Geste_a_l_aise == "2 - chef présent mais ok"      ~ "😐 Chef présent mais OK",
            Geste_a_l_aise == "3 - pu être fait avec externe" ~ "😊 Autonome avec externe",
            TRUE                                             ~ as.character(Geste_a_l_aise)
          ),
          `Ressenti sans geste` = case_when(
            Si_pas_de_geste_RESSENTI == "Je ne suis pas prêt pour le faire" ~ "😰 Pas prêt",
            Si_pas_de_geste_RESSENTI == "J'aurais aimé essayer"            ~ "😤 Aurait aimé essayer",
            TRUE                                                            ~ as.character(Si_pas_de_geste_RESSENTI)
          )
        ) %>%
        select(
          Date,
          Intervention = INTERVENTION,
          Opérateur = OPERATEUR,
          `Rang boss` = RANG_BOSS,
          `Geste réalisé`,
          `Quel geste ?`,
          `Geste (texte)`,
          Ambiance,
          Pédagogie,
          `Ressenti si geste`,
          `Ressenti sans geste`
        ) %>%
        arrange(desc(Date))
      
      # Retourner le datatable une seule fois
      DT::datatable(
        table_data,
        options = list(
          pageLength = 25,
          scrollX    = TRUE,
          autoWidth  = FALSE,
          columnDefs = list(
            list(width = "80px",  targets = 0),
            list(width = "200px", targets = 1),
            list(width = "120px", targets = 2),
            list(width = "80px",  targets = 3),
            list(width = "100px", targets = 4),
            list(width = "120px", targets = 5),
            list(width = "120px", targets = 6),
            list(width = "150px", targets = 7),
            list(width = "150px", targets = 8)
          ),
          order    = list(list(0, 'desc')),
          language = list(
            search      = "Rechercher :",
            lengthMenu  = "Afficher _MENU_ interventions par page",
            info        = "Affichage de _START_ à _END_ sur _TOTAL_ interventions",
            paginate    = list(first = "Premier", last = "Dernier", `next` = "Suivant", previous = "Précédent"),
            zeroRecords = "Aucune intervention trouvée",
            emptyTable  = "Aucune donnée disponible"
          ),
          dom      = 'Bfrtip',
          buttons  = list(
            list(extend = 'copy',  text = 'Copier'),
            list(extend = 'excel', text = 'Excel'),
            list(extend = 'csv',   text = 'CSV')
          ),
          searchHighlight = TRUE,
          stateSave       = TRUE
        ),
        extensions = 'Buttons',
        filter     = 'top',
        class      = "display nowrap",
        rownames   = FALSE
      ) %>%
        DT::formatStyle(
          "Geste réalisé",
          backgroundColor = DT::styleEqual(c("✅ Oui", "❌ Non"), c("#d4edda", "#f8d7da"))
        ) %>%
        DT::formatStyle(
          "Ambiance",
          backgroundColor = DT::styleEqual(
            c("😞 Je veux partir", "😕 C'est ok", "😊 On recommence"),
            c("#f8d7da",       "#fff3cd",      "#d4edda")
          )
        ) %>%
        DT::formatStyle(
          "Pédagogie",
          backgroundColor = DT::styleEqual(
            c("❌ Aucune", "😕 Faible", "😐 Correcte", "😊 Bonne", "🔥 Incroyable"),
            c("#f8d7da",   "#fff3cd",  "#e2e3e5",     "#d4edda",    "#c3f7c3")
          )
        ) %>%
        DT::formatDate("Date", method = "toLocaleDateString")
    }, server = FALSE)
    
    
    # Bouton Quitter
    observeEvent(input$quitter, {
      stopApp()
    })
    
  }  # <- fermeture du server function
  
  shinyApp(ui, server)
  
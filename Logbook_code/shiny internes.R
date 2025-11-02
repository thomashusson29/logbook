library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(gt)
library(shinydashboard)
library(shinymanager)
library(stringi)

library(stringi)

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
  password = c("admin", sapply(liste_noms, prenom_to_code)),
  nom_interne = c(NA_character_, liste_noms),
  stringsAsFactors = FALSE
)



# UI sécurisé
ui <- secure_app(
  fluidPage(
    titlePanel("Statistiques individuelles - Logbook"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("select_interne_ui"),
        br(),
        actionButton("quitter", "Quitter l'application", class = "btn-danger")
      ),
      mainPanel(
        h3("Taux de gestes réalisés par mois"),
        plotOutput("courbe_gestes"),
        fluidRow(
          column(4, valueBoxOutput("box_geste")),
          column(4, valueBoxOutput("box_total")),
          column(4, valueBoxOutput("box_recommence"))
        ),
        br(),
        h3("Résumé des scores pédagogiques"),
        gt_output("table_scores")
      )
    )
  )
)

server <- function(input, output, session) {
  user <- secure_server(check_credentials = check_credentials(credentials))
  
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
      interne_courant(nom)
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
  data_interne <- reactive({
    req(interne_courant())
    df %>%
      filter(NOM_interne == interne_courant()) %>%
      mutate(
        geste_realise = ifelse(Geste == "Yes", 1, 0),
        on_recommence = ifelse(AMBIANCE == "3 - on recommence", 1, 0)
      )
  })
  
  output$courbe_gestes <- renderPlot({
    df_plot <- data_interne() %>%
      filter(!is.na(DATE), !is.na(Geste)) %>%
      mutate(DATE = as.Date(DATE),
             mois = floor_date(DATE, "month")) %>%
      group_by(mois) %>%
      summarise(taux = mean(geste_realise, na.rm = TRUE),
                n = n(),
                .groups = "drop")
    
    ggplot(df_plot, aes(x = mois, y = taux)) +
      geom_line(color = "#377eb8", linewidth = 1.5) +
      geom_point(size = 3, color = "#377eb8") +
      geom_text(aes(label = paste0(round(100 * taux, 1), "%")), vjust = -0.8, size = 5) +
      geom_text(aes(label = paste0("n = ", n)), vjust = 1.8, size = 4, color = "grey30") +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
                   expand = expansion(mult = c(0.01, 0.05))) +
      labs(title = paste("Taux de gestes réalisés par mois -", interne_courant()),
           x = "Mois", y = "Taux de gestes réalisés") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
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
  
  observeEvent(input$quitter, {
    stopApp()
  })
}

shinyApp(ui, server)

# ---- App Shiny : unifier RANG_BOSS par opérateur (radio + auto-save) ----
# PRÉREQUIS : un objet df en mémoire avec colonnes OPERATEUR et RANG_BOSS

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# petite fonction "mode" pour proposer un rang par défaut
.mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE)
  top <- tb[tb == max(tb)]
  sort(names(top))[1]
}

# données de base
stopifnot(all(c("OPERATEUR","RANG_BOSS") %in% names(df)))
ops <- df %>%
  filter(!is.na(OPERATEUR), OPERATEUR != "") %>%
  mutate(OPERATEUR = trimws(OPERATEUR)) %>%
  distinct(OPERATEUR) %>%
  arrange(OPERATEUR) %>%
  pull(OPERATEUR)

rang_choices <- c("PU", "PH", "CCA", "MCU", "DJ")

# synthèse pour afficher les rangs observés par opérateur
synth <- df %>%
  filter(!is.na(OPERATEUR), OPERATEUR != "", !is.na(RANG_BOSS), RANG_BOSS != "") %>%
  mutate(across(c(OPERATEUR, RANG_BOSS), ~trimws(as.character(.)))) %>%
  count(OPERATEUR, RANG_BOSS, name = "n") %>%
  arrange(OPERATEUR, desc(n), RANG_BOSS) %>%
  group_by(OPERATEUR) %>%
  mutate(proposition = .mode_chr(RANG_BOSS),
         info = paste0(RANG_BOSS, ":", n, collapse = " | ")) %>%
  ungroup() %>%
  distinct(OPERATEUR, proposition, info)

# charge mapping existant si présent; sinon initialise avec NA (et une proposition)
map_path <- "operateur_rang_map.csv"
if (file.exists(map_path)) {
  base_map <- read_csv(map_path, show_col_types = FALSE) %>%
    mutate(across(everything(), ~trimws(as.character(.x)))) %>%
    right_join(tibble(OPERATEUR = ops), by = "OPERATEUR") %>%
    left_join(synth %>% select(OPERATEUR, proposition), by = "OPERATEUR") %>%
    transmute(
      OPERATEUR,
      RANG_BOSS_CANONIQUE = if_else(!is.na(RANG_BOSS_CANONIQUE) & RANG_BOSS_CANONIQUE != "",
                                    RANG_BOSS_CANONIQUE, NA_character_),
      PROPOSITION = proposition
    )
} else {
  base_map <- tibble(
    OPERATEUR = ops
  ) %>%
    left_join(synth %>% select(OPERATEUR, proposition), by = "OPERATEUR") %>%
    transmute(
      OPERATEUR,
      RANG_BOSS_CANONIQUE = NA_character_,
      PROPOSITION = proposition
    )
}

ui <- fluidPage(
  titlePanel("Unification des RANG_BOSS (choix unique, sauvegarde automatique)"),
  fluidRow(
    column(5,
           selectInput("operateur", "Opérateur :", choices = ops, width = "100%"),
           uiOutput("observes"),
           radioButtons("rang", "Rang retenu :", choices = rang_choices, selected = character(0)),
           helpText("Le choix est enregistré instantanément dans operateur_rang_map.csv")
    ),
    column(7,
           h4("Mapping actuel"),
           div(style = "max-height: 60vh; overflow-y: auto;",
               tableOutput("mapTable")),
           br(),
           downloadButton("dl_map", "Télécharger le mapping (CSV)")
    )
  )
)

server <- function(input, output, session) {
  mapping <- reactiveVal(base_map)
  
  # afficher les rangs observés + proposition pour l'opérateur sélectionné
  output$observes <- renderUI({
    op <- req(input$operateur)
    row <- synth %>% filter(OPERATEUR == op)
    info <- if (nrow(row)) row$info[[1]] else "(aucun rang observé)"
    prop <- if (nrow(row)) row$proposition[[1]] else NA_character_
    tagList(
      p(HTML(paste0("<b>Rangs observés :</b> ", htmltools::htmlEscape(info)))),
      if (!is.na(prop)) tags$small(paste("Proposition :", prop)) else NULL
    )
  })
  
  # quand on change d'opérateur, on positionne le radio :
  # 1) valeur déjà mappée si existante
  # 2) sinon, proposition (mode observé)
  observeEvent(input$operateur, {
    m <- mapping()
    op <- input$operateur
    current <- m$RANG_BOSS_CANONIQUE[m$OPERATEUR == op]
    prop <- m$PROPOSITION[m$OPERATEUR == op]
    sel <- if (!is.na(current) && nzchar(current)) current else if (!is.na(prop)) prop else character(0)
    updateRadioButtons(session, "rang", selected = sel)
  }, ignoreInit = TRUE)
  
  # quand on change le radio, on met à jour le mapping et on ÉCRIT le CSV (auto-save)
  observeEvent(input$rang, {
    op <- req(input$operateur)
    choix <- req(input$rang)
    # sécurise : forcer choix dans la liste autorisée
    if (!choix %in% rang_choices) return(NULL)
    
    m <- mapping()
    m$RANG_BOSS_CANONIQUE[m$OPERATEUR == op] <- choix
    mapping(m)
    
    # auto-save
    write_csv(m %>% select(OPERATEUR, RANG_BOSS_CANONIQUE), map_path)
    showNotification(paste0("Enregistré: ", op, " → ", choix), type = "message", duration = 1.5)
  }, ignoreInit = TRUE)
  
  output$mapTable <- renderTable({
    mapping() %>%
      select(OPERATEUR, RANG_BOSS_CANONIQUE, PROPOSITION)
  })
  
  output$dl_map <- downloadHandler(
    filename = function() paste0("operateur_rang_map_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"),
    content = function(file) {
      readr::write_csv(mapping() %>% select(OPERATEUR, RANG_BOSS_CANONIQUE), file)
    }
  )
}

shinyApp(ui, server)
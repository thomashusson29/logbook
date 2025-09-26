


# install.packages(c("shiny","miniUI","dplyr"))
library(shiny)
library(miniUI)
library(dplyr)

run_label_operateurs <- function(noms_operateurs) {
  noms <- sort(unique(noms_operateurs[!is.na(noms_operateurs)]))
  ui <- miniPage(
    gadgetTitleBar("Classer les seniors : Homme / Femme / NA"),
    miniContentPanel(
      fillCol(
        flex = c(1, NA),
        div(
          tags$p("Coche le sexe pour chaque nom (par défaut : NA)."),
          actionButton("set_all_h", "Tout mettre en Homme"),
          actionButton("set_all_f", "Tout mettre en Femme"),
          actionButton("set_all_na", "Tout mettre en NA")
        ),
        div(
          style = "height: 70vh; overflow:auto; padding: 8px; border: 1px solid #ddd;",
          do.call(tagList, lapply(seq_along(noms), function(i){
            nm <- noms[i]
            inputId <- paste0("g_", i)
            fluidRow(
              column(5, strong(nm)),
              column(7, radioButtons(inputId, NULL,
                                     choices = c("Homme","Femme","NA"),
                                     selected = "NA", inline = TRUE))
            )
          }))
        )
      )
    )
  )

  server <- function(input, output, session){
    observeEvent(input$set_all_h, {
      for(i in seq_along(noms)) updateRadioButtons(session, paste0("g_", i), selected = "Homme")
    })
    observeEvent(input$set_all_f, {
      for(i in seq_along(noms)) updateRadioButtons(session, paste0("g_", i), selected = "Femme")
    })
    observeEvent(input$set_all_na, {
      for(i in seq_along(noms)) updateRadioButtons(session, paste0("g_", i), selected = "NA")
    })

    observeEvent(input$done, {
      sexes <- vapply(seq_along(noms), function(i){
        val <- input[[paste0("g_", i)]]
        if (is.null(val)) "NA" else val
      }, character(1))
      mapping <- tibble(OPERATEUR = noms,
                        sexe_operateur = ifelse(sexes=="NA", NA_character_, sexes))
      stopApp(mapping)
    })
    observeEvent(input$cancel, { stopApp(NULL) })
  }

  runGadget(ui, server, viewer = dialogViewer("Label OPERATEUR", width = 700, height = 800))
}




# 1) Lancer le gadget et récupérer le mapping
mapping_operateur <- run_label_operateurs(df$OPERATEUR)

# 2) Joindre à df pour créer la colonne `sexe_operateur`
df <- df %>%
  left_join(mapping_operateur, by = "OPERATEUR")

saveRDS(mapping_operateur, "mapping_operateur.rds")

mapping_operateur <- readRDS("mapping_operateur.rds")

df <- df %>%
  left_join(mapping_operateur, by = "OPERATEUR")

setdiff(unique(df$OPERATEUR), mapping_operateur$OPERATEUR)



# 3) (Optionnel) Récapitulatif des seniors uniques par sexe
df %>%
  select(OPERATEUR, sexe_operateur) %>%
  distinct() %>%
  count(sexe_operateur)




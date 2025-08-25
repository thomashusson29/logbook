# Sauvegarde du dataframe dans les deux applications Shiny
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/appcarte/logbook_data.rds")
saveRDS(df, file = "/Users/thomashusson/Documents/R/Logbook/app1/logbook_data.rds")

saveRDS(df, "logbook_data.rds")

#lancements apps
# Configuration du compte (à faire une fois)
rsconnect::setAccountInfo(name='thomas-husson', token='F86928AE3B04B208C12CFF5F5324B05F', secret='E9teWbmpEpRdaNFdP5gJYZKnNJDh8nOJIcM0XtXG')

# Déploiement suivi logbook
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/appinternespourcentages",
  appName = "SuiviLogbook",
  launch.browser = TRUE
)

# Déploiement carte-logbook-v1
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/appcarte",
  appName = "CarteLogbook",
  launch.browser = TRUE
)

# Déploiement calculateur-logbook-v1
rsconnect::deployApp(
  appDir = "/Users/thomashusson/Documents/R/Logbook/app1",
  appName = "CalculateurLogbook",
  launch.browser = TRUE
)



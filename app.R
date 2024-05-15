library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(DT)

# UI setup
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
            body {
                
                background-color: #e6f7ff; /* Bleu très pâle */
                color: #333; /* Texte foncé pour un bon contraste */
                font-family: 'Arial', sans-serif;
            }
            .blue-background {
                background-color: #337ab7; /* Couleur bleue */
                color: white; /* Texte en blanc */
                padding: 10px;
                border-radius: 5px; /* Bords arrondis */
            }
            .action-button {
                background-color: #4CAF50; /* Vert */
                color: white;
                border: none;
                padding: 10px 20px;
                text-align: center;
                display: inline-block;
                font-size: 16px;
                margin: 4px 2px;
                cursor: pointer;
                border-radius: 12px;
            }
            .nav-tabs > li > a {
                border: 1px solid #ddd;
                background-color: #f8f8f8;
                color: #555;
            }
            .nav-tabs > li.active > a {
                background-color: #fff;
                border-bottom-color: transparent;
            }
            @keyframes fadeIn {
                from { opacity: 0; }
                to { opacity: 1; }
            }
            .content {
                animation: fadeIn 1s;
            }
            .sidebarPanel {
                background-color: #f8f8f9;
                border-radius: 5px;
                padding: 20px;
                margin: 10px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            .shiny-input-container {
                margin-bottom: 15px;
            }
            .form-group label {
                font-weight: bold;
                color: #333;
            }
            input[type= 'text'],input[type='number'],select{
                border: 2px solid #ccc;
                border-radius: 4px;
                font-size: 16px;
                padding: 5px 10px;
                width: 100%;
            }
        "))
  ),
  div(class = "blue-background", 
      titlePanel("Tendances Démographiques à Mayotte en 2019")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("ageRange", "Sélectionnez la plage d'âge des parents:",
                  min = 15, max = 50, value = c(20, 40)),
      selectInput("departement", "Choisissez un département:", choices = NULL),
      selectInput("analysisType", "Sélectionnez une analyse à réaliser:", 
                  choices = c("Analyse Temporelle", "Distribution Géographique", 
                              "Analyse Démographique",  
                              "Prédiction des Tendances", "Diversité Culturelle")),
      conditionalPanel(
        condition = "input.analysisType === 'Prédiction des Tendances'",
        numericInput("futureMonths", "Nombre de mois à prédire:", min = 1, max = 12, value = 3)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualisation", plotOutput("mainPlot")),
        tabPanel("Carte", leafletOutput("mapPlot")),
        tabPanel("Résumé des Données", dataTableOutput("dataSummary"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session){
  # Chargement des données
  data <- reactive({
    df <- read.csv("FD_NAIS_2019.csv", sep = ";", stringsAsFactors = FALSE)
    
    # Ajout des données synthétiques
    n <- nrow(df)
    df$LONGITUDE <- runif(n, min = 45.1, max = 45.35)
    df$LATITUDE <- runif(n, min = -12.9, max = -12.65)
    df$ETHNICITE <- sample(c("Mahorais", "Comorien", "Métropolitain", "Autre"), n, replace = TRUE)
    
    # Mise à jour des choix de département dès le démarrage de l'app
    updateSelectInput(session, "departement", choices = unique(df$DEPDOM), selected = unique(df$DEPDOM)[1])
    
    df
  })
  
  filtered_data <- reactive({
    data() %>%
      filter(AGEMERE >= input$ageRange[1], AGEMERE <= input$ageRange[2],
             DEPDOM == input$departement)
  })
  
  output$mainPlot <- renderPlot({
    req(input$analysisType)
    df <- filtered_data()
    switch(input$analysisType,
           "Analyse Temporelle" = {
             ggplot(df, aes(x = as.factor(MNAIS))) +
               geom_histogram(stat = "count", fill = "steelblue") +
               labs(title = "Répartition des naissances par mois", x = "Mois", y = "Nombre de naissances")
           },
           "Distribution Géographique" = {
             ggplot(df, aes(x = LONGITUDE, y = LATITUDE, color = DEPDOM)) +
               geom_point() +
               labs(title = "Répartition géographique des naissances", x = "Longitude", y = "Latitude")
           },
           "Analyse Démographique" = {
             ggplot(df, aes(x = AGEMERE)) +
               geom_histogram(bins = 30, fill = "lightgreen") +
               labs(title = "Distribution de l'âge des mères", x = "Âge", y = "Fréquence")
           },
           "Prédiction des Tendances" = {
             df$MNAIS <- as.numeric(as.character(df$MNAIS))
             model <- lm(AGEMERE ~ MNAIS, data = df)
             pred <- predict(model, newdata = data.frame(MNAIS = 1:12), interval = "confidence")
             data_pred <- data.frame(Mois = 1:12, Fit = pred[, "fit"], lwr = pred[, "lwr"], upr = pred[, "upr"])
             ggplot(data_pred, aes(x = Mois, y = Fit)) +
               geom_line() +
               geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
               scale_x_continuous(breaks = 1:12, labels = month.abb) +
               labs(title = "Prédictions des Tendances de Naissance par Mois", x = "Mois", y = "Âge Prévu des Mères")
           },
           "Diversité Culturelle" = {
             ggplot(df, aes(x = ETHNICITE, fill =ETHNICITE)) +
               geom_bar() +
               labs(title = "Diversité Culturelle et Ethnique à Mayotte", x = "ETHNICITE", y = "Nombre de Naissances")
           }
    )
  })
  output$mapPlot <- renderPlot({
    df <- filtered_data()
    # Assurez-vous que df contient des données valides
    if (nrow(df) > 0){
      map_data <- map_data("world", region = "france")  # Modifier selon le besoin
      gg <- ggplot(data = map_data, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "black") +
        geom_point(data = df, aes(x = LONGITUDE, y = LATITUDE, color = DEPDOM), size = 3) +
        coord_fixed(1.3) +
        labs(title = "Distribution Géographique des Naissances")
      print(gg)
    }
    
  })
  
  output$mapPlot <- renderLeaflet({
    df <- filtered_data()
    leaflet(df) %>%
      addTiles() %>%
      addCircles(lng = ~LONGITUDE, lat = ~LATITUDE, weight = 1,
                 radius = 500, color = '#ffa500', fillColor = '#ffa500',
                 fillOpacity = 0.8, popup = ~paste("Âge Mère: ", AGEMERE, "<br>", "Département: ", DEPDOM))
  })
  
}
# Run the application
shinyApp(ui, server)



library(magrittr)
library(shiny)
library(lubridate)
devtools::load_all("../rcoleo/")

server <- function(input, output, session) {

# Simple fonction pour mettre une majuscule
firstup <- function(x) {
   substr(x, 1, 1) <- toupper(substr(x, 1, 1))
   return(x)
}

withProgress(message = "Retrait des données depuis le serveur", value = NULL, {
  # On retire les cellules (classe sf) depuis l'API
  cells <- rcoleo::sf_cells()

  # On retire les observations (classe df) depuis l'API
  all_obs <- as.data.frame(rcoleo::get_obs()) %>%
  select(cell_code, date_obs, type, taxa = obs_species.taxa_name, var = obs_species.variable, val = obs_species.value)
  all_obs$date_obs %<>% as.character %>% as.Date %>% year
  all_obs$count <- as.numeric(apply(all_obs, 1, function(x){ ifelse(x["var"] == "recouvrement", 1, x["val"])}))

  # On compte le nombre d'observation/nombre espece par type, année et cellule
  obs_cells <- group_by(all_obs, cell_code, date_obs, type) %>% summarise(n = sum(count))
  sp_cells <-  select(all_obs, cell_code, date_obs, type, taxa) %>% distinct() %>% group_by(cell_code, date_obs, type) %>%
  summarise(n = n())

})

##### Prepare Data based on input #####
data_map <- reactive({
  if(input$aggType == "by_obs"){
    df <- obs_cells
    legend <- "Nombre d'observations"
  } else if(input$aggType == "by_sp"){
    df <- sp_cells
    legend <- "Nombre d'espèces"
  }

  return(list(data=merge(cells,df,by="cell_code",all=TRUE), legend=legend))
})

##### OUTPUTS #####

output$yearControl <- renderUI({
    selectInput("year", "Choisissez une année:", unique(obs_cells$date_obs))
})

output$typeControl <- renderUI({
    selectInput("type", "Choisissez un type de campagne:", c("Toutes",firstup(unique(as.character(obs_cells$type)))))
})


# MAP
output$cells_map <- renderLeaflet({

  pal <- colorNumeric(
    palette = "YlOrBr",
    domain = data_map()$n
  )

  leaflet(data_map()$data) %>%
    addProviderTiles(providers$CartoDB.Positron,
      options = providerTileOptions(noWrap = TRUE)
    ) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.5, fillOpacity = 0.8,
    color = ~rev(pal(n))) %>%
    addLegend("bottomright", pal = pal, values = ~n,
      title = data_map()$legend,
      opacity = 1
    )

})

}

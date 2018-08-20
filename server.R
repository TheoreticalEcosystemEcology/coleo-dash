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

  if(!is.null(input$year)){
      df <- subset(df, date_obs == input$year)
  }

  if(!is.null(input$type)){
    if(input$type != "Toutes"){
      df <- subset(df, type == tolower(input$type))
    }
  }

  map_data <- merge(cells,df,by="cell_code",all=TRUE)
  map_data[which(is.na(map_data$n)),"n"] <- 0

  return(list(data=map_data, legend=legend))
})

##### OUTPUTS #####

output$yearControl <- renderUI({
    selectInput("year", "Choisissez une année:", unique(obs_cells$date_obs))
})

output$typeControl <- renderUI({
    selectInput("type", "Choisissez un type de campagne:", c("Toutes",firstup(unique(as.character(obs_cells$type)))))
})

# Export shapefile

output$download_shp <- downloadHandler(
    filename = "cells.zip",
    content = function(file) {
        # create a temp folder for shp files
        temp_shp <- tempdir()
        # write shp files
        sf::st_write(data_map()$data, paste0(temp_shp,"/cells.shp"))
        # zip all the shp files
        zip_file <- file.path(temp_shp, "cells.zip")
        shp_files <- list.files(temp_shp,
                                "cells",
                                full.names = TRUE)
        print(shp_files)
        # the following zip method works for me in linux but substitute with whatever method working in your OS
        zip_command <- paste("zip -j",
                             zip_file,
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
    }
)

# MAP
output$cells_map <- renderLeaflet({

  pal <- colorNumeric(
    palette = "GnBu",
    domain = data_map()$data$n
  )

  popup <- paste0("<strong>Code de la cellule: </strong>",
                  data_map()$data$cell_code,
                  "</br><strong>Nom de la cellule: </strong>",
                  data_map()$data$name,
                  "</br><strong>",
                  data_map()$legend,
                  ": </strong>",
                  data_map()$data$n)

  leaflet(data_map()$data) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
    addProviderTiles("Stamen.TonerLite",
                     group = "Toner") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    addProviderTiles("CartoDB.Positron",     group = "CartoDB") %>%
    addLayersControl(baseGroups = c("Mapnik", "Toner", "OSM", "Topo", "CartoDB"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.8, color = "#323232",
    fillColor = ~pal(n), popup = popup, weight = 2) %>%
    addLegend("bottomright", pal = pal, values = ~n,
      title = data_map()$legend,
      opacity = 1
    )

})
}

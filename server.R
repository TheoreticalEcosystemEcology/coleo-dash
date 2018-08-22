library(magrittr)
library(shiny)
library(lubridate)
library(plotly)
library(RColorBrewer)
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
  select(cell_code,site_code, date_obs, type, taxa = obs_species.taxa_name, var = obs_species.variable, val = obs_species.value)
  all_obs$date_obs %<>% as.character %>% as.Date %>% year
  all_obs$count <- as.numeric(apply(all_obs, 1, function(x){ ifelse(x["var"] == "recouvrement", 1, x["val"])}))

  # CELLULES: On compte le nombre d'observation/nombre espece par type, année et cellule
  obs_cells <- group_by(all_obs, cell_code, date_obs, type) %>% summarise(n = sum(count))
  sp_cells <-  select(all_obs, cell_code, date_obs, type, taxa) %>% distinct() %>% group_by(cell_code, date_obs, type) %>%
  summarise(n = n())

  # CAMPAGNES
  sites <- select(rcoleo::sf_sites(), site_code, off_station_code_id, type_milieu = type, geometry, site_id = id)
  campaigns <- as.data.frame(rcoleo::get_campaigns())
  sp_sites <- merge(sites, campaigns, by = "site_id")

  # On prépare les jeux de données pour chacun des types de campagnes
  microfaunes <- subset(all_obs, type == "microfaunes")
  # comm_df <- microfaunes %>% filter(date_obs == 2016) %>% select(site_code,taxa,count) %>% group_by(site_code, taxa) %>% summarise(count=sum(count))
  # comm_df <- reshape2::dcast(microfaunes, site_code ~ taxa, value.var="count", fill = 0, fun.aggregate=sum)
  # row.names(comm_df) <- comm_df[,1]
  # comm_df <- as.matrix(comm_df[,-1])

  # div <- vegan::diversity(comm_df)
  # beta <- ncol(comm_df)/mean(vegan::specnumber(comm_df)) - 1
  # comm.hel <- vegan::decostand(comm_df, method="hellinger")
  # test <- adespatial::LCBD.comp(dist(comm.hel))
  #
  # comm.hel.mean = matrix(colMeans(comm.hel),nrow = nrow(comm.hel), ncol = ncol(comm.hel), byrow = TRUE)
  # comm2 = (comm.hel - comm.hel.mean)^2
  # SStot = sum(comm2)
  # SSsp = colSums(comm2)
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

  if(!is.null(input$year_cells)){
      df <- subset(df, date_obs == input$year_cells)
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
    selectInput("year_cells", "Choisissez une année:", unique(obs_cells$date_obs))
})

output$yearControl_micro <- renderUI({
    selectInput("year_micro", "Choisissez une année:", unique(obs_cells$date_obs))
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
    palette = "Spectral",
    domain = data_map()$data$n,
    reverse = TRUE
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
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    addLayersControl(baseGroups = c("Mapnik", "Toner", "Topo", "CartoDB"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addPolygons(stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.8, color = "#323232",
    fillColor = ~pal(n), popup = popup, weight = 2) %>%
    addLegend("bottomright", pal = pal, values = ~n,
      title = data_map()$legend,
      opacity = 1
    )

})

# MICROFAUNES
output$micro_carto <- renderLeaflet({

  data <- microfaunes %>% filter(date_obs==input$year_micro)
  data %<>% select(site_code,taxa,count) %>% group_by(site_code, taxa) %>% summarise(count=sum(count))
  comm_df <- reshape2::dcast(data, site_code ~ taxa, value.var="count", fill = 0, fun.aggregate=sum)
  row.names(comm_df) <- comm_df[,1]
  comm_df <- as.matrix(comm_df[,-1])

  rich_sp <- vegan::specnumber(comm_df)
  eveness <- vegan::diversity(comm_df)/log(vegan::specnumber(comm_df))
  geom_df <- data.frame(site_code = names(eveness), eveness, rich_sp)
  geom_sf <- merge(sites, geom_df, by=c("site_code"))

  pal <- colorNumeric(
    palette = "Spectral",
    domain = geom_sf$rich_sp,
    reverse = TRUE
  )

  popup <- paste0("<strong>Code du site: </strong>",
                  geom_df$site_code,
                  "</br><strong>Nombre d'espèces: </strong>",
                  geom_df$rich_sp,
                  "</br><strong>Régularité des espèces: </strong>",
                  round(geom_df$eveness, 2)
                )

  leaflet(geom_sf) %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
    addProviderTiles("Stamen.TonerLite",
                     group = "Toner") %>%
    addProviderTiles("Esri.WorldTopoMap",
                     group = "Topo") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    addLayersControl(baseGroups = c("Mapnik", "Toner", "Topo", "CartoDB"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    addCircleMarkers(
         color = ~pal(rich_sp),
         stroke = TRUE, fillOpacity = 0.8,
         popup = popup
       ) %>%
    addLegend("bottomright", pal = pal, values = ~rich_sp,
      title = "Richesse spécifique",
      opacity = 1
    )



})


# MAP
output$micro_compo <- renderPlotly({

  data <- microfaunes %>% filter(date_obs==input$year_micro) %>%
    select(taxa, count, site_code) %>%
    group_by(taxa, site_code) %>%
    summarise(count=sum(count)) %>%
    group_by(site_code) %>% mutate(n_tot_obs = sum(count)) %>% mutate(obs_rel = count/n_tot_obs)

  data <- reshape2::dcast(data, site_code ~ taxa, value.var="obs_rel")

  colors = colorRampPalette(brewer.pal(9,"Spectral"))(ncol(data)-1)

  p <- plot_ly(data, type = 'bar')
  for(i in 2:ncol(data)){
   p %<>% add_trace(x = ~site_code, y = data[,i], name = names(data)[i], marker = list(color = colors[i-1]))
  }
  p %<>% layout(yaxis = list(title = 'Abondance relative'), barmode = 'stack')

})


# output$camp_map <- renderLeaflet({

#   getColor <- function(quakes) {
#     sapply(quakes$mag, function(mag) {
#     if(mag <= 4) {
#       "green"
#     } else if(mag <= 5) {
#       "orange"
#     } else {
#       "red"
#     } })
#   }
#
# icons <- awesomeIcons(
#   icon = 'ios-close',
#   iconColor = 'black',
#   library = 'ion',
#   markerColor = getColor(df.20)
# )
#
#
#
#   pal <- colorNumeric(
#     palette = "GnBu",
#     domain = data_map()$data$n
#   )
#
#   popup <- paste0("<strong>Code de la cellule: </strong>",
#                   data_map()$data$cell_code,
#                   "</br><strong>Nom de la cellule: </strong>",
#                   data_map()$data$name,
#                   "</br><strong>",
#                   data_map()$legend,
#                   ": </strong>",
#                   data_map()$data$n)
#
#   leaflet(data_map()$data) %>%
#     addProviderTiles("OpenStreetMap.Mapnik", group = "Mapnik") %>%
#     addProviderTiles("Stamen.TonerLite",
#                      group = "Toner") %>%
#     addProviderTiles("Esri.WorldTopoMap",
#                      group = "Topo") %>%
#     addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
#     addLayersControl(baseGroups = c("Mapnik", "Toner", "Topo", "CartoDB"),
#                      options = layersControlOptions(collapsed = TRUE)) %>%
#     addPolygons(stroke = TRUE, smoothFactor = 0.5, fillOpacity = 0.8,
#     fillColor = ~pal(n), popup = popup, weight = 2) %>%
#     addLegend("bottomright", pal = pal, values = ~n,
#       title = data_map()$legend,
#       opacity = 1
#     )
#
# })

}

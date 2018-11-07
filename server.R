library(magrittr)
library(shiny)
library(lubridate)
library(plotly)
library(RColorBrewer)
library(vegan)
library(rcoleo)
source("./utils.R")

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

  # On prépare les jeux de données pour chacun des types de campagnes
  microfaunes <- subset(all_obs, type == "insectes_sol")
  microfaunes %<>% filter(taxa != "inconnu")

  papillons <- subset(all_obs, type == "papilionidés")
  papillons %<>% filter(taxa != "inconnu")

  odonates <- subset(all_obs, type == "odonates")
  odonates %<>% filter(taxa != "inconnu")

  vegetation <- subset(all_obs, type == "végétation")
  vegetation %<>% filter(taxa != "inconnu")


})


###########################
##### GENERIC OUTPUTS #####
###########################

output$yearControl <- renderUI({
    selectInput("year_cells", "Choisissez une année:", unique(obs_cells$date_obs))
})

output$typeControl <- renderUI({
    selectInput("type", "Choisissez un type de campagne:", c("Toutes",firstup(unique(as.character(obs_cells$type)))))
})

output$yearControl_micro <- renderUI({
    selectInput("year_micro", "Choisissez une année:", unique(microfaunes$date_obs))
})

output$yearControl_papi <- renderUI({
    selectInput("year_papi", "Choisissez une année:", unique(papillons$date_obs))
})

output$yearControl_odo <- renderUI({
    selectInput("year_odo", "Choisissez une année:", unique(odonates$date_obs))
})

output$yearControl_veg <- renderUI({
    selectInput("year_veg", "Choisissez une année:", unique(vegetation$date_obs))
})

###########################
#####    CELLS TAB    #####
###########################

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

  names(st_geometry(map_data)) <- NULL

  return(list(data=map_data, legend=legend))
})

###########

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
    fillColor = ~pal(n), weight = 2, popup = popup) %>%
    addLegend("bottomright", pal = pal, values = ~n,
      title = data_map()$legend,
      opacity = 1
    )

})


#########

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

###########################
##### MICROFAUNES TAB  ####
###########################

output$micro_carto <- renderLeaflet({

  req(microfaunes, sites, input$year_micro)


  geom_sf <- compute_desc_comm(microfaunes, sites, input$year_micro)

  pal <- colorNumeric(
    palette = "Spectral",
    domain = geom_sf$rich_sp,
    reverse = TRUE
  )

  popup <- paste0("<strong>Code du site: </strong>",
                  geom_sf$site_code,
                  "</br><strong>Nombre d'espèces: </strong>",
                  geom_sf$rich_sp,
                  "</br><strong>Régularité des espèces: </strong>",
                  round(geom_sf$eveness, 2),
                  "</br><strong>Indice de Shanon-Weaver: </strong>",
                  round(geom_sf$div, 2)
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

##########

output$micro_sp_beta <- renderPlotly({

  req(microfaunes, input$year_micro)

  SCBD <- compute_beta_scbd(microfaunes, input$year_micro)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(SCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(SCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(SCBD)){
   p %<>% add_trace(x = SCBD[i,"val"], name = SCBD[i,"sp"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$beta_micro <- renderInfoBox({

  req(microfaunes, input$year_micro)

  beta <- compute_beta(microfaunes, input$year_micro)

  valueBox(round(beta,3),"beta-diversité", icon = icon("bug"), color = "green")
})

##########

output$micro_sites_beta <- renderPlotly({

  req(microfaunes, input$year_micro)

  LCBD <- compute_beta_lcbd(microfaunes, input$year_micro)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(LCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(LCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(LCBD)){
   p %<>% add_trace(x = LCBD[i,"val"], name = LCBD[i,"site"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$micro_compo <- renderPlotly({

req(microfaunes, input$year_micro)

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


###########################
##### PAPILLONS TAB  ####
###########################

output$papi_carto <- renderLeaflet({

req(papillons, sites, input$year_papi)

  geom_sf <- compute_desc_comm(papillons, sites, input$year_papi)

  pal <- colorNumeric(
    palette = "Spectral",
    domain = geom_sf$rich_sp,
    reverse = TRUE
  )

  popup <- paste0("<strong>Code du site: </strong>",
                  geom_sf$site_code,
                  "</br><strong>Nombre d'espèces: </strong>",
                  geom_sf$rich_sp,
                  "</br><strong>Régularité des espèces: </strong>",
                  round(geom_sf$eveness, 2),
                  "</br><strong>Indice de Shanon-Weaver: </strong>",
                  round(geom_sf$div, 2)
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

##########

output$papi_sp_beta <- renderPlotly({


  req(papillons, input$year_papi)


  SCBD <- compute_beta_scbd(papillons, input$year_papi)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(SCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(SCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(SCBD)){
   p %<>% add_trace(x = SCBD[i,"val"], name = SCBD[i,"sp"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$beta_papi <- renderInfoBox({

  req(papillons, input$year_papi)

  beta <- compute_beta(papillons, input$year_papi)

  valueBox(round(beta,3),"beta-diversité", icon = icon("bug"), color = "green")
})

##########

output$papi_sites_beta <- renderPlotly({

  req(papillons, input$year_papi)

  LCBD <- compute_beta_lcbd(papillons, input$year_papi)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(LCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(LCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(LCBD)){
   p %<>% add_trace(x = LCBD[i,"val"], name = LCBD[i,"site"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$papi_compo <- renderPlotly({

  req(papillons, input$year_papi)

  data <- papillons %>% filter(date_obs==input$year_papi) %>%
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


###########################
##### ODONATES TAB  ####
###########################

output$odo_carto <- renderLeaflet({

  req(odonates, sites, input$year_odo)

  geom_sf <- compute_desc_comm(odonates, sites, input$year_odo)

  pal <- colorNumeric(
    palette = "Spectral",
    domain = geom_sf$rich_sp,
    reverse = TRUE
  )

  popup <- paste0("<strong>Code du site: </strong>",
                  geom_sf$site_code,
                  "</br><strong>Nombre d'espèces: </strong>",
                  geom_sf$rich_sp,
                  "</br><strong>Régularité des espèces: </strong>",
                  round(geom_sf$eveness, 2),
                  "</br><strong>Indice de Shanon-Weaver: </strong>",
                  round(geom_sf$div, 2)
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

##########

output$odo_sp_beta <- renderPlotly({

  req(odonates, input$year_odo)

  SCBD <- compute_beta_scbd(odonates, input$year_odo)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(SCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(SCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(SCBD)){
   p %<>% add_trace(x = SCBD[i,"val"], name = SCBD[i,"sp"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$beta_odo <- renderInfoBox({


  req(odonates, input$year_odo)

  beta <- compute_beta(odonates, input$year_odo)

  valueBox(round(beta,3),"beta-diversité", icon = icon("bug"), color = "green")
})

##########

output$odo_sites_beta <- renderPlotly({

  req(odonates, input$year_odo)

  LCBD <- compute_beta_lcbd(odonates, input$year_odo)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(LCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(LCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(LCBD)){
   p %<>% add_trace(x = LCBD[i,"val"], name = LCBD[i,"site"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$odo_compo <- renderPlotly({

  req(odonates, input$year_odo)

  data <- odonates %>% filter(date_obs==input$year_odo) %>%
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


###########################
##### VEGETATION TAB  ####
###########################

output$veg_carto <- renderLeaflet({

  req(vegetation, sites, input$year_veg)

  geom_sf <- compute_desc_comm(vegetation, sites, input$year_veg)

  pal <- colorNumeric(
    palette = "Spectral",
    domain = geom_sf$rich_sp,
    reverse = TRUE
  )

  popup <- paste0("<strong>Code du site: </strong>",
                  geom_sf$site_code,
                  "</br><strong>Nombre d'espèces: </strong>",
                  geom_sf$rich_sp,
                  "</br><strong>Régularité des espèces: </strong>",
                  round(geom_sf$eveness, 2),
                  "</br><strong>Indice de Shanon-Weaver: </strong>",
                  round(geom_sf$div, 2)
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

##########

output$veg_sp_beta <- renderPlotly({


  req(vegetation, input$year_veg)

  SCBD <- compute_beta_scbd(vegetation, input$year_veg)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(SCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(SCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(SCBD)){
   p %<>% add_trace(x = SCBD[i,"val"], name = SCBD[i,"sp"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$beta_veg <- renderInfoBox({

  req(vegetation, input$year_veg)

  beta <- compute_beta(vegetation, input$year_veg)

  valueBox(round(beta,3),"beta-diversité", icon = icon("bug"), color = "green")
})

##########

output$veg_sites_beta <- renderPlotly({

  req(vegetation, input$year_veg)

  LCBD <- compute_beta_lcbd(vegetation, input$year_veg)

  colors = colorRampPalette(rev(brewer.pal(9,"YlGnBu")))(nrow(LCBD))

  m <- list(
    l = 30,
    r = 30,
    b = 30,
    t = 30,
    pad = 4
  )

  p <- plot_ly(LCBD, type = 'bar', orientation = 'h', showlegend = FALSE)
  for(i in 1:nrow(LCBD)){
   p %<>% add_trace(x = LCBD[i,"val"], name = LCBD[i,"site"], marker = list(color = colors[i]))
  }
  p %<>% layout(barmode = 'stack',
          margin = m,
          xaxis = list(
            title = "",
            showline = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            range = c(0,1)
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE,
            showline = FALSE,
            showticklabels = FALSE,
            showgrid = FALSE))
})

##########

output$veg_compo <- renderPlotly({

  req(vegetation, input$year_veg)

  data <- vegetation %>% filter(date_obs==input$year_veg) %>%
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

}

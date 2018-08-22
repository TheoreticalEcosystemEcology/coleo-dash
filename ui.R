library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)


ui <- dashboardPage(
  dashboardHeader(title = "Coléo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cellules", tabName = "cellules", icon = icon("map")),
      menuItem("Campagnes", tabName = "campagnes", icon = icon("map")),
      menuItem("Papillons", tabName = "papillons", icon = icon("dashboard")),
      menuItem("Microfaunes", tabName = "microfaunes", icon = icon("dashboard")),
      menuItem("Odonates", tabName = "odonnate", icon = icon("dashboard")),
      menuItem("Végétation", tabName = "vegetation", icon = icon("dashboard"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(type = "text/css", "#cells_map {height: calc(75vh - 80px) !important;}"),
    tabItems(
      ######################
      ###### CELLULES ######
      ######################
      tabItem(tabName = "cellules",
        fluidRow(
          h2("Cartographie des cellules", style="margin:15px;")
        ),
        fluidRow(
          box(width = 4,
            status = "primary",
            uiOutput("typeControl")
          ),
          box(width = 4,
            status = "primary",
            uiOutput("yearControl")
          ),
          box(width = 4,
            status = "primary",
            selectInput("aggType", "Choisissez un type d'aggrégation:", c("Nombre d'observations" = "by_obs", "Richesse spécifique" = "by_sp"))
          )
        ),
        fluidRow(
          column(10, leafletOutput("cells_map")),
          column(2,
            div(downloadButton('download_shp', 'Exporter le shapefile'), style="width:140px;margin:10px")
          )
        )
      ),
      #######################
      ###### CAMPAIGNS ######
      #######################
      # tabItem(tabName = "campagnes",
      #   fluidRow(
      #     h2("Cartographie des campagnes", style="margin:15px;")
      #   ),
      #   fluidRow(
      #     box(width = 4,
      #       status = "primary",
      #       uiOutput("typeControl")
      #     ),
      #     box(width = 4,
      #       status = "primary",
      #       uiOutput("yearControl")
      #     ),
      #     box(width = 4,
      #       status = "primary",
      #       selectInput("aggType", "Choisissez un type d'aggrégation:", c("Nombre d'observations" = "by_obs", "Nombre d'espèces" = "by_sp"))
      #     )
      #   ),
      #   fluidRow(
      #     column(10, leafletOutput("camp_map")),
      #     column(2,
      #       div(downloadButton('download_shp', 'Exporter le shapefile'), style="width:140px;margin:10px")
      #     )
      #   )
      # )
      #######################
      ###### MICROFAUNES ####
      #######################
      tabItem(tabName = "microfaunes",
        fluidRow(
          h2("Analyse sur la microfaune", style="margin:15px;")
        ),
        fluidRow(
          box(width = 4,
            status = "primary",
            uiOutput("yearControl_micro")
          )
        ),
        fluidRow(
          box(width = 6, status = "primary",
          ),
          box(width = 6, status = "primary",
          )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautées", align="center", style="font-weight:700;"),
            plotlyOutput("micro_compo")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            leafletOutput("micro_carto")
          )
        )
      )
    )
  )
)

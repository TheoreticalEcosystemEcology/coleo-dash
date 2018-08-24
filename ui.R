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
      menuItem("Papillons", tabName = "papillons", icon = icon("dashboard")),
      menuItem("Microfaunes", tabName = "microfaunes", icon = icon("dashboard")),
      menuItem("Odonates", tabName = "odonates", icon = icon("dashboard")),
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
      ###### Papillons ####
      #######################
      tabItem(tabName = "papillons",
        fluidRow(
          h2("Analyse sur les papilionidés", style="margin:15px;")
        ),
        fluidRow(
        column(4,
          box(width = 12,
            status = "primary",
            uiOutput("yearControl_papi")
          ),
          valueBoxOutput("beta_papi", width = 12)
        ),
        column(8,
          box(width = 12,
            status = "primary",
            h4("Contribution des espèces à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("papi_sp_beta", height = "90px"),
            h4("Contribution des sites à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("papi_sites_beta", height = "90px")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautées", align="center", style="font-weight:700;"),
            plotlyOutput("papi_compo")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            leafletOutput("papi_carto")
          )
        )
      ),
      #######################
      ###### Microfaunes ####
      #######################
      tabItem(tabName = "microfaunes",
        fluidRow(
          h2("Analyse sur la microfaune", style="margin:15px;")
        ),
        fluidRow(
        column(4,
          box(width = 12,
            status = "primary",
            uiOutput("yearControl_micro")
          ),
          valueBoxOutput("beta_micro", width = 12)
        ),
        column(8,
          box(width = 12,
            status = "primary",
            h4("Contribution des espèces à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("micro_sp_beta", height = "90px"),
            h4("Contribution des sites à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("micro_sites_beta", height = "90px")
          )
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
      ),
      #######################
      ###### Microfaunes ####
      #######################
      tabItem(tabName = "odonates",
        fluidRow(
          h2("Analyse sur les odonates", style="margin:15px;")
        ),
        fluidRow(
        column(4,
          box(width = 12,
            status = "primary",
            uiOutput("yearControl_odo")
          ),
          valueBoxOutput("beta_odo", width = 12)
        ),
        column(8,
          box(width = 12,
            status = "primary",
            h4("Contribution des espèces à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("odo_sp_beta", height = "90px"),
            h4("Contribution des sites à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("odo_sites_beta", height = "90px")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautées", align="center", style="font-weight:700;"),
            plotlyOutput("odo_compo")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            leafletOutput("odo_carto")
          )
        )
      ),
      #######################
      ###### Vegetation ####
      #######################
      tabItem(tabName = "vegetation",
        fluidRow(
          h2("Analyse sur la végétation", style="margin:15px;")
        ),
        fluidRow(
        column(4,
          box(width = 12,
            status = "primary",
            uiOutput("yearControl_veg")
          ),
          valueBoxOutput("beta_veg", width = 12)
        ),
        column(8,
          box(width = 12,
            status = "primary",
            h4("Contribution des espèces à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("veg_sp_beta", height = "90px"),
            h4("Contribution des sites à la béta-diversité (%)", align="center", style="font-weight:700;"),
            plotlyOutput("veg_sites_beta", height = "90px")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautées", align="center", style="font-weight:700;"),
            plotlyOutput("veg_compo")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            leafletOutput("veg_carto")
          )
        )
      )
    )
  )
)

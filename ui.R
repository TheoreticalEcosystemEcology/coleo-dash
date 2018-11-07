library(shinydashboard)
library(shinyWidgets)
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
      menuItem("Insectes du sol", tabName = "insectes_sol", icon = icon("dashboard")),
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
          column(10, box(
            width = 12,
            status = "primary",
            addSpinner(leafletOutput("cells_map"),spin = "circle", color = "#2A6497FF")
          )),
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
            h4("Contribution des espèces à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("papi_sp_beta", height = "90px"),spin = "circle", color = "#2A6497FF"),
            h4("Contribution des sites à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("papi_sites_beta", height = "90px"),spin = "circle", color = "#2A6497FF")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautés", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("papi_compo"),spin = "circle", color = "#2A6497FF")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            addSpinner(leafletOutput("papi_carto"),spin = "circle", color = "#2A6497FF")
          )
        )
      ),
      #######################
      ###### Microfaunes ####
      #######################
      tabItem(tabName = "insectes_sol",
        fluidRow(
          h2("Analyse sur les insectes du sol", style="margin:15px;")
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
            h4("Contribution des espèces à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("micro_sp_beta", height = "90px"),spin = "circle", color = "#2A6497FF"),
            h4("Contribution des sites à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("micro_sites_beta", height = "90px"),spin = "circle", color = "#2A6497FF")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautés", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("micro_compo"),spin = "circle", color = "#2A6497FF")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            addSpinner(leafletOutput("micro_carto"),spin = "circle", color = "#2A6497FF")
          )
        )
      ),
      #######################
      ###### Odonates ####
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
            h4("Contribution des espèces à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("odo_sp_beta", height = "90px"),spin = "circle", color = "#2A6497FF"),
            h4("Contribution des sites à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("odo_sites_beta", height = "90px"),spin = "circle", color = "#2A6497FF")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautés", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("odo_compo"),spin = "circle", color = "#2A6497FF")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            addSpinner(leafletOutput("odo_carto"),spin = "circle", color = "#2A6497FF")
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
            h4("Contribution des espèces à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("veg_sp_beta", height = "90px"),spin = "circle", color = "#2A6497FF"),
            h4("Contribution des sites à la beta-diversité (%)", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("veg_sites_beta", height = "90px"),spin = "circle", color = "#2A6497FF")
          )
        )
        ),
        fluidRow(
          box(width = 6, status = "primary",
            h4("Composition des communautés", align="center", style="font-weight:700;"),
            addSpinner(plotlyOutput("veg_compo"),spin = "circle", color = "#2A6497FF")
          ),
          box(width = 6, status = "primary",
            h4("Richesse spécifique", align="center", style="font-weight:700;"),
            addSpinner(leafletOutput("veg_carto"),spin = "circle", color = "#2A6497FF")
          )
        )
      )
    )
  )
)

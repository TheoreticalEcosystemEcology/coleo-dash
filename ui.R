library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)


ui <- dashboardPage(
  dashboardHeader(title = "Tableau de bord"),
  dashboardSidebar(
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    sidebarMenu(
      menuItem("Maps", tabName = "map", icon = icon("map")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Trends", tabName = "trends", icon = icon("signal"))
    )),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "map",
        fluidRow(
          box(width = 4,
            selectInput("aggType", "Type d'aggrégation:", c("Nombre d'observations" = "by_obs", "Nombre d'espèces" = "by_sp") ,)
          ),
          box(width = 4,
            uiOutput("yearControl")
          ),
          box(width = 4,
            uiOutput("typeControl")
          )
        ),
        fluidRow(
          box(width = 12,
            leafletOutput("cells_map", height=500)
          )
        )
      ),
      # First tab content
      tabItem(tabName = "dashboard",
        fluidRow(
          box(),
          box()
        )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
        h1("Widgets tab content"),
        fluidRow(
          column(width = 4,
            box(
              title = "Box title", width = NULL, status = "primary",
              "Box content"
            ),
            box(
              title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
              "Box content"
            ),
            box(
              width = NULL, background = "black",
              "A box with a solid black background"
            )
          ),

          column(width = 4,
            box(
              status = "warning", width = NULL,
              "Box content"
            ),
            box(
              title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
              "Box content"
            ),
            box(
              title = "Title 5", width = NULL, background = "light-blue",
              "A box with a solid light-blue background"
            )
          ),

          column(width = 4,
            box(
              title = "Title 2", width = NULL, solidHeader = TRUE,
              "Box content"
            ),
            box(
              title = "Title 6", width = NULL, background = "maroon",
              "A box with a solid maroon background"
            )
          )
        )
      ),
      # Second tab content
      tabItem(tabName = "trends",
        h2("Trends tab content"),
        fluidRow(
              # A static valueBox
              valueBox(10 * 2, "New Orders", icon = icon("credit-card")),

              # Dynamic valueBoxes
              valueBoxOutput("progressBox"),

              valueBoxOutput("approvalBox")
            ),
            fluidRow(
              # Clicking this will increment the progress amount
              box(width = 4, actionButton("count", "Increment progress"))
        )
      )
    )
  )
)

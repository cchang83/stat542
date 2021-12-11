library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')


body <- dashboardBody(includeCSS("css/movies.css"),
                      tabItems(
                        tabItem(tabName = "userRatingRecommendations",
                                fluidRow(
                                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                      div(class = "rateitems",
                                          uiOutput('ratings')
                                      )
                                  )
                                ),
                                fluidRow(
                                  useShinyjs(),
                                  box(
                                    width = 12, status = "info", solidHeader = TRUE,
                                    title = "Step 2: Discover movies you might like",
                                    br(),
                                    withBusyIndicatorUI(
                                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                    ),
                                    br(),
                                    tableOutput("results")
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "genreRecommendations",
                                fluidRow(
                                  titlePanel("Top 20 recommendations from genres of your choice  "),
                                ),
                                
                                selectInput("genre", "Genre:", 
                                            choices = c("Action", "Comedy", "Romance",
                                                        "Drama", "Thriller", "Horror",
                                                        "Adventure", "Sci-Fi", "Children", "Crime",
                                                        "War", "Documentary", "Musical", "Mystery", "Animation", "Western", "Fantasy", "Film-Noir"), selected = "Action"),
                                hr(),
                                
                                
                                fluidRow(
                                  useShinyjs(),
                                  
                                  withBusyIndicatorUI(
                                    actionButton("button", "Click here to get your recommendations", class = "btn-warning")
                                  ),
                                  br(),
                                  tableOutput("genreresults")
                                  
                                )
                        )
                      )
)


shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Recommendations by genre", tabName = "genreRecommendations"),
        menuItem("Recommendations by user ratings", tabName = "userRatingRecommendations")
      )
    ),
    body
  )
)
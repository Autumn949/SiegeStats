#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(odbc)
library(DBI)
library(dplyr)
library(shinyjs)

# Define UI for application that draws a histogram
UI <- function(id) {
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Game Selector", tabName = "gameselector", icon = icon("th")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Database Management", tabName = "dbman", icon = icon("th"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "gameselector",
          fluidRow(
            box(
              uiOutput("selectgames")
            ),
            box(
              dateInput("filterdatestart", label="Start Date"),
              dateInput("filterdateend", label = "End Date")
            ),
            box(
              selectizeInput("filterplayerdropdown", choices = NULL, label="Filter by Player...")
              
            ),
            box(
              selectizeInput("filtermapdropdown", choices = NULL, label="Select Map To Filter By...")
            ),
            box(
              selectizeInput("filterbanonedropdown", choices = NULL, label="Select Ban One DEFENCE To Filter By..."),
              selectizeInput("filterbantwodropdown", choices = NULL, label="Select Ban Two DEFENCE To Filter By..."),
              selectizeInput("filterbanthreepdropdown", choices = NULL, label="Select Ban One ATTACK To Filter By..."),
              selectizeInput("filterbanfourpdropdown", choices = NULL, label="Select Ban Two ATTACK To Filter By...")
            ),
            box(
             selectizeInput("filtermethod", choices = c("Manual"= "manual","Filter" = "filter"),selected = "manual", label="Select Filter Method...")
            )
          )
        ),
        tabItem(
          tabName = "dashboard",
          fluidRow(
            box(
              tableOutput("gamedata"),
              textOutput("gamesselected")
            )
          )
        ),

        tabItem(
          tabName = "dbman",
          fluidRow(
            box(
              tableOutput("dbman_match_names")
            ),
            box(
              selectizeInput("dbman_gameselect", choices = NULL, label = "Select Game"),
              actionButton("dbman_displaymatchinfo", label = "UPDATE MATCH"),
              tableOutput("dbman_match_info"),
              tableOutput("dbman_players"),
              fileInput("dbman_fileinput", "Choose NEWSHEET XLXM File",
                multiple = TRUE,
                accept = c(".xlsm")
              ),
              actionButton("dbman_uploadtodatabase", "UPLOAD FILE"),
              actionButton("dbman_deletegame", "DELETE GAME")
            )
          )
        )
      )
    )
  )
}

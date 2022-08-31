#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinycssloaders)
library(plotly)
library(shinydashboardPlus)
library(shinydashboard)
library(odbc)
library(DBI)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(rjson)
library(stringr)
library(collections)
library(fresh)
library(DT)
########################### THEME

### creating custom theme object
###########################
# Define UI for application that draws a histogram
UI <- function(id) {
  dashboardPage(
    dashboardHeader(title = "Statistikill"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Game Selector", tabName = "gameselector", icon = icon("th")),
        menuItemOutput("dashboard"),
        menuItemOutput("mapdashboard"),
        menuItem("Database Management", tabName = "dbman", icon = icon("th"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "mapdashboard", default = "graphs",
          fluidRow(
            tabBox(width = 12, title = "Map Stats", selected = "graphs", tabPanel(
              "rawdata",
              width = 12,
              dataTableOutput("mapstatstable"),
              dataTableOutput("sitepermap")
            ), tabPanel(
              class = "overflowhide",
              "graphs",
              fluidRow(width=12,
                div(class = "overflowhide", width = 9, uiOutput("mapselectedimg")),
                br(),
                userBox(background="orange",
                  title = userDescription("Attack", subtitle = "WR: 50% WINS: 6 ROUNDS: 12", type=2,image = "images/testimg.jpeg"), box(
                    width = 12, uiOutput("mapinfositeaa", width = 12),
                    uiOutput("mapinfositeba", width = 12),
                    uiOutput("mapinfositeca", width = 12),
                    uiOutput("mapinfositeda", width = 12)
                  ),
                  width = 12
                ),
                userBox(width=12,background="blue",
                  title = userDescription("Defence", subtitle = "WR: 50% WINS: 6 ROUNDS: 12", type=2,image = "images/testimg.jpeg"), box(
                    width=12,
                    uiOutput("mapinfositead",width=12),
                    uiOutput("mapinfositebd",width=12),
                    uiOutput("mapinfositecd",width=12),
                    uiOutput("mapinfositedd",width=12)
                    
                    
                    
                  )
                ),
                box(width=12,
                box(width=6,
                selectizeInput("mapslist", choices=NULL, label="Pick A Map")
              ),box(width=6, actionButton("updatemappick", "Update Map Selection")))
              )
            ), tabPanel("overallmap",
              actionButton("updatemapstats", "Update Map Stats")
            )),
            
          )
        ),
        tabItem(
          tabName = "gameselector",
          fluidRow(
            box(
              uiOutput("selectgames")
            ),
            box(
              checkboxInput("datefilterenable", "Enable Date Filter"),
              dateInput("filterdatestart", label = "Start Date"),
              dateInput("filterdateend", label = "End Date")
            ),
            box(
              selectizeInput("filterplayerdropdown", choices = NULL, label = "Filter by Player...")
            ),
            box(
              selectizeInput("filtermapdropdown", choices = NULL, label = "Select Map To Filter By...")
            ),
            box(
              selectizeInput("filterbanonedropdown", choices = NULL, label = "Select Ban One DEFENCE To Filter By..."),
              selectizeInput("filterbantwodropdown", choices = NULL, label = "Select Ban Two DEFENCE To Filter By..."),
              selectizeInput("filterbanthreepdropdown", choices = NULL, label = "Select Ban One ATTACK To Filter By..."),
              selectizeInput("filterbanfourpdropdown", choices = NULL, label = "Select Ban Two ATTACK To Filter By...")
            ),
            box(
              selectizeInput("filtermethod", choices = c("Manual" = "manual", "Filter" = "filter"), selected = "manual", label = "Select Filter Method...")
            ),
            box(
              textOutput("namedata")
            )
          )
        ),
        tabItem(
          tabName = "dashboard",
          fluidRow(
            box(
              width = 12,
              tabBox(
                title = "KD Graphs", width = "12 col-lg-3",
                tabPanel(
                  id = "kdchartsmap", title = "KD by Map",
                  plotOutput("kdbymapchart"),
                  tableOutput("kdbymaptable")
                ),
                tabPanel(
                  div(
                    style = "overflow-x: scroll",
                    uiOutput("kdbyopcharts"),
                    dataTableOutput("kdbyoptable")
                  ),
                  id = "kdchartsop",
                  title = "KD By Op",
                )
              ),
              box(
                actionButton("updategraphs", "Update Graphs", disabled = TRUE)
              )
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

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plotly)
library(shinydashboard)
library(odbc)
library(DBI)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(rjson)
library(stringr)
library(collections)
library(dashboardthemes)
library(DT)
########################### THEME

### creating custom theme object
customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial",
  appFontColor = "rgb(0,0,0)",
  primaryFontColor = "rgb(0,0,0)",
  infoFontColor = "rgb(0,0,0)",
  successFontColor = "rgb(0,0,0)",
  warningFontColor = "rgb(0,0,0)",
  dangerFontColor = "rgb(0,0,0)",
  bodyBackColor = "rgb(248,248,248)"

  ### header
  , logoBackColor = "rgb(23,103,124)",
  headerButtonBackColor = "rgb(238,238,238)",
  headerButtonIconColor = "rgb(75,75,75)",
  headerButtonBackColorHover = "rgb(210,210,210)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = "rgb(238,238,238)",
  headerBoxShadowColor = "#aaaaaa",
  headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  , sidebarBackColor = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(20,97,117)",
    colorMiddle = "rgb(56,161,187)",
    colorEnd = "rgb(3,22,56)",
    colorStartPos = 0,
    colorMiddlePos = 50,
    colorEndPos = 100
  ),
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  sidebarShadowRadius = "3px 5px 5px",
  sidebarShadowColor = "#aaaaaa",
  sidebarUserTextColor = "rgb(255,255,255)",
  sidebarSearchBackColor = "rgb(55,72,80)",
  sidebarSearchIconColor = "rgb(153,153,153)",
  sidebarSearchBorderColor = "rgb(55,72,80)",
  sidebarTabTextColor = "rgb(255,255,255)",
  sidebarTabTextSize = 13,
  sidebarTabBorderStyle = "none none solid none",
  sidebarTabBorderColor = "rgb(35,106,135)",
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = "rgb(0,0,0)",
  sidebarTabRadiusSelected = "0px 20px 20px 0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgba(44,222,235,1)",
    colorMiddle = "rgba(44,222,235,1)",
    colorEnd = "rgba(0,255,213,1)",
    colorStartPos = 0,
    colorMiddlePos = 30,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "rgb(50,50,50)",
  sidebarTabBorderStyleHover = "none none solid none",
  sidebarTabBorderColorHover = "rgb(75,126,151)",
  sidebarTabBorderWidthHover = 1,
  sidebarTabRadiusHover = "0px 20px 20px 0px"

  ### boxes
  , boxBackColor = "rgb(255,255,255)",
  boxBorderRadius = 5,
  boxShadowSize = "0px 1px 1px",
  boxShadowColor = "rgba(0,0,0,.1)",
  boxTitleSize = 16,
  boxDefaultColor = "rgb(210,214,220)",
  boxPrimaryColor = "rgba(44,222,235,1)",
  boxInfoColor = "rgb(210,214,220)",
  boxSuccessColor = "rgba(0,255,213,1)",
  boxWarningColor = "rgb(244,156,104)",
  boxDangerColor = "rgb(255,88,55)",
  tabBoxTabColor = "rgb(255,255,255)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(0,0,0)",
  tabBoxTabTextColorSelected = "rgb(0,0,0)",
  tabBoxBackColor = "rgb(255,255,255)",
  tabBoxHighlightColor = "rgba(44,222,235,1)",
  tabBoxBorderRadius = 5

  ### inputs
  , buttonBackColor = "rgb(245,245,245)",
  buttonTextColor = "rgb(0,0,0)",
  buttonBorderColor = "rgb(200,200,200)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(235,235,235)",
  buttonTextColorHover = "rgb(100,100,100)",
  buttonBorderColorHover = "rgb(200,200,200)",
  textboxBackColor = "rgb(255,255,255)",
  textboxBorderColor = "rgb(200,200,200)",
  textboxBorderRadius = 5,
  textboxBackColorSelect = "rgb(245,245,245)",
  textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  , tableBackColor = "rgb(255,255,255)",
  tableBorderColor = "rgb(240,240,240)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)
###########################
# Define UI for application that draws a histogram
UI <- function(id) {
  dashboardPage(
    dashboardHeader(title = "Statistikill"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Game Selector", tabName = "gameselector", icon = icon("th")),
        menuItem("Player Stats Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Map Stats Dashboard", tabName = "mapdashboard", icon = icon("dashboard")),
        menuItem("Database Management", tabName = "dbman", icon = icon("th"))
      )
    ),
    dashboardBody(
      customTheme,
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "mapdashboard", default = "graphs",
          fluidRow(
            tabBox(width = 12, title = "Map Stats", selected = "graphs", tabPanel(
              "rawdata",
              box(
                width = 12,
                dataTableOutput("mapstatstable"),
                dataTableOutput("sitepermap")
              ),
            ), tabPanel(
              "graphs",
              box(
                width = 12,
                fluidRow(
                  box(box(width=12, height = 4, 
                   span(span(textOutput("wrsite1"),style = "color:red;font-size:large;")), span(textOutput("rp1"), style = "color:red;font-size:large;"),
                    span(textOutput("opper1"), style = "color:red;font-size:large;"), span(textOutput("conper1"), style = "color:red;font-size:large;")
                    )),
                  valueBox(subtitle = dataTableOutput("site2WR"), value = "SITEB", icon = icon("credit-card"))
                ),
                fluidRow(
                  valueBox(subtitle = dataTableOutput("site3WR"), value = "SITEC", icon = icon("credit-card")),
                  valueBox(subtitle = dataTableOutput("site4WR"), value = "SITED", icon = icon("credit-card"))
                )
              )
            )),
            box(
              actionButton("updatemapstats", "Update Map Stats")
            )
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
                title = "KD Graphs",width="12 col-lg-3",
                tabPanel(
                  id = "kdchartsmap", title = "KD by Map",
                  plotOutput("kdbymapchart"),
                  tableOutput("kdbymaptable")
                ),
                tabPanel(
                  div(
                    style = "overflow-x: scroll",
                    withSpinner(uiOutput("kdbyopcharts")),
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

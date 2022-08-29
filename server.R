#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#
# TODO: ADD NEW FILTER OPTIONS SUPPORT
# TODO: ADD N=ROUNDS TO GRAPHS
#

#
# FUNCTIONS

################################ MISC###############################
dynamicplayerstats <- function(playernames, gamescheckbox) {
  #
  # TODO:dynamically fetch stats for players in selected games for the selected games
  #
  playerdfs <- ordered_dict()
  for (player in playernames) {
    selplayerquery <- paste("SELECT * FROM ", player, " WHERE MATCHID='", gamescheckbox, "'", sep = "")
    selplayerdf <- dbGetQuery(con, selplayerquery)
    playerdfs$set(player, selplayerdf)
  }
  return(playerdfs)
}
updateclient <- function(input, output, session) {
  metadata <<- dbReadTable(con, "METADATA")
  gamesdata <<- dbReadTable(con, "MATCHINFO")
  choice <- metadata$MATCHID
  updateCheckboxGroupInput("gamescheckbox", session = session, choices = choice, selected = choice[1])
}
################################ DBMAN###############################
dbman_update <- function(input, output, session) {
  dbman_metadata <<- dbReadTable(con, "METADATA")
  dbman_matchinfo <<- dbReadTable(con, "MATCHINFO")
  output$dbman_match_names <- renderTable(dbman_metadata$MATCHID)
  # update table
  updateSelectizeInput(session, "dbman_gameselect", label = "TEST", choices = dbman_metadata$MATCHID, server = TRUE)
  updateclient(input, output, session)
}

dbman_pullgamedata <- function(input, output, session) {
  dbman_selectedmatchinfo <<- filter(dbman_matchinfo, dbman_matchinfo$MATCHID == input$dbman_gameselect)
  dbman_selectedmetadata <<- filter(dbman_metadata, dbman_metadata$MATCHID == input$dbman_gameselect)
}

dbman_pushfile <- function(input, output, session) {
  disable("dbman_fileinput")
  removeModal()
  dbman_uploadmatchmetadata <- readxl::read_excel(input$dbman_fileinput$datapath, "MATCHMETADATA")
  dbman_uploadmatchinfo <- readxl::read_excel(input$dbman_fileinput$datapath, "MATCHINFO")
  dbman_uploadplayer1pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER1PULL")
  dbman_uploadplayer2pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER2PULL")
  dbman_uploadplayer3pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER3PULL")
  dbman_uploadplayer4pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER4PULL")
  dbman_uploadplayer5pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER5PULL")


  dbWriteTable(con, "METADATA", dbman_uploadmatchmetadata, append = TRUE)
  dbWriteTable(con, "MATCHINFO", dbman_uploadmatchinfo, append = TRUE)
  dbWriteTable(con, dbman_uploadplayer1pull$PLAYERNAME[1], dbman_uploadplayer1pull, append = TRUE)
  dbWriteTable(con, dbman_uploadplayer2pull$PLAYERNAME[1], dbman_uploadplayer2pull, append = TRUE)
  dbWriteTable(con, dbman_uploadplayer3pull$PLAYERNAME[1], dbman_uploadplayer3pull, append = TRUE)
  dbWriteTable(con, dbman_uploadplayer4pull$PLAYERNAME[1], dbman_uploadplayer4pull, append = TRUE)
  dbWriteTable(con, dbman_uploadplayer5pull$PLAYERNAME[1], dbman_uploadplayer5pull, append = TRUE)

  dbman_update(input, output, session)
  reset("dbman_fileinput")
  removeModal()
  enable("dbman_fileinput")
  showModal(modalDialog(
    title = "Success",
    "UPLOADED FILE",
    footer = tagList(modalButton("OK"))
  ))
}
dbman_server <- function(id, input, output, session) {
  # generate database connection and update metadata object
  dbman_update(input, output, session)




  # PULL GAME DATA
  observeEvent(input$dbman_displaymatchinfo, {
    dbman_pullgamedata(input, output, session)
    output$dbman_match_info <- renderTable(dbman_selectedmatchinfo)
    output$dbman_players <- renderTable(dbman_selectedmetadata[, c("P1", "P2", "P3", "P4", "P5")])
  })

  # UPLOAD FILE
  observeEvent(input$dbman_uploadtodatabase, {
    if (is.null(input$dbman_fileinput)) {
      showModal(modalDialog(
        title = "No File Error",
        "NO FILE PROVIDED",
        footer = tagList(
          modalButton("OK")
        )
      ))
    } else {
      showModal(modalDialog(
        title = "Confirm Upload",
        "Please confirm you wish to upload the file provided",
        footer = tagList(
          actionButton("dbman_checkdupe", "Upload"),
          modalButton("Cancel")
        )
      ))
    }
  })
  observeEvent(input$dbman_checkdupe, {
    # CHECK IF FILE WITH NAME ALREADY EXISTS
    dbman_filename <- sub(".xlsm", "", basename(input$dbman_fileinput$name))
    removeModal()
    if (dbman_filename %in% dbman_metadata$MATCHID) {
      showModal(modalDialog(
        title = "Confirm Upload",
        "Please confirm you wish to upload the file provided",
        footer = tagList(
          actionButton("dbman_confirmupload", "YES"),
          modalButton("NO")
        )
      ))
    } else {
      dbman_pushfile(input, output, session)
    }
  })

  observeEvent(input$dbman_confirmupload, {
    dbman_pushfile(input, output, session)
  })

  # DELETE FILE
  observeEvent(input$dbman_deletegame, {
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Please confirm you wish to delete game:", input$dbman_gameselect),
      footer = tagList(
        actionButton("dbman_confirmdelete", "Delete"),
        modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$dbman_confirmdelete, {
    dbman_deletematch(input, output, session)
    removeModal()
    showModal(modalDialog(
      title = "Success",
      paste("Deleted: ", input$dbman_gameselect),
      footer = tagList(modalButton("OK"))
    ))
  })
}



dbman_deletematch <- function(input, output, session) {
  #
  dbman_SQLquery <- paste("SELECT * FROM METADATA WHERE MATCHID='", input$dbman_gameselect, "'", sep = "")
  dbman_playernames <- dbGetQuery(con, dbman_SQLquery)[1, ]
  dbExecute(con, paste("DELETE FROM METADATA WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM MATCHINFO WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM ", dbman_playernames$P1, " WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM ", dbman_playernames$P2, " WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM ", dbman_playernames$P3, " WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM ", dbman_playernames$P4, " WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))
  dbExecute(con, paste("DELETE FROM ", dbman_playernames$P5, " WHERE [MATCHID]='", input$dbman_gameselect, "'", sep = ""))

  dbman_update(input, output, session)
}



######################################################################################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # creates list of games to be pulled for stats
  gameslist <<- reactiveValues()
  config <- fromJSON(file = "config.json")
  # database connection intialized globally
  con <<- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = config$server, database = config$database, user = config$user, password = config$password)
  metadata <<- dbReadTable(con, "METADATA")
  gamesdata <<- dbReadTable(con, "MATCHINFO")
  output$selectgames <- renderUI({
    choice <- metadata$MATCHID
    checkboxGroupInput("gamescheckbox", "Select Games To Load", choices = choice, selected = choice[1])
  })

  # makes sure that at least one game is selected
  observeEvent(input$gamescheckbox, {
    if (length(input$gamescheckbox) == 1) {
      shinyjs::disable(selector = paste("#gamescheckbox input[value='", input$gamescheckbox[1], "']", sep = ""))
    } else {
      enable("gamescheckbox")
    }

    gameslist$gamenames <- input$gamescheckbox
    gameslist$gamesselected <- filter(metadata, MATCHID %in% gameslist$gamenames)
    # PLAYERLIST
    gameslist$playernames <- levels(factor(unlist(as.list(gameslist$gamesselected[, c("P1", "P2", "P3", "P4", "P5")]))))
    gameslist$playersseldict <- dynamicplayerstats(gameslist$playernames, toString(input$gamescheckbox))
  })



  # renders table of matchinfo for selected games
  output$gameslist <- renderTable(
    gameslist$gamesselected
  )

  output$namedata <- renderText(gameslist$playernames)


  # executes dbman server
  dbman_server("dbman", input, output, session)

  # renders text for list of games selected in game selector
  output$gamesselected <- renderText(gameslist$gamenames)
}

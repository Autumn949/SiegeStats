#
# RAINBOW 6 STATS APP
# MADE BY @AutumnStats
# 
#

#
# TODO: ADD NEW FILTER OPTIONS SUPPORT
# TODO: LABEL CHARTS
# TODO: Overall map stats
# TODO: Check phone notes for more
#

#
# FUNCTIONS

################################ STATS FETCH DYNAMIC###############################
dynamicquery <- function(input, output, session, mapslist) {
  mapslist<- unlist(mapslist)
  # generates match select query

  #
  # TODO: ADD FILTER OPTIONS
  #
  str <- ""
  for (i in 1:length(mapslist)) {
    if (i > 1) {
      str <- paste("( ",str, " OR MATCHID='", mapslist[i], "')", sep = "")
    } else {
      str <- paste("MATCHID='", mapslist[1], "'", sep = "")
    }
  }
  return(str)
}


dynamicplayerstats <- function(playernames, input, output, session) {
  #
  # FETCHS PLAYER STATS DYNAMIC
  #
  playerdfs <- ordered_dict()

  str <- dynamicquery(input, output, session, gameslist$gamesselected$MATCHID)
 
  
  for (player in playernames) {
    selplayerquery <- paste("SELECT * FROM ", player, " WHERE ", str, sep = "")
    print(paste("DEBUG: SQL GET QUERY:", selplayerquery))
    selplayerdf <- dbGetQuery(con, selplayerquery)
    playerdfs$set(player, selplayerdf)
  }
  shinyjs::enable("updategraphs")
  return(playerdfs)
}
genplayerquery<- function(input,output,session){
  if(!(input$filterplayerdropdown=="No Player")){
    strplayer<- paste0("'",input$filterplayerdropdown,"'")
    return(paste0("(P1=",strplayer,"OR P2=",strplayer," OR P3=",strplayer," OR P4=",strplayer," OR P5=",strplayer, ")"))
  }
  return(NULL)
}
genselectmapquery<- function(input,output,session,flag){
  if(!is.null(flag)){if(!(input$filtermapdropdown=="No Map")){
    strmap<-paste0("'",input$filtermapdropdown,"'")
    return(paste0(" AND (MAP=",strmap,")"))
  }else{
    return(NULL)
  }
  }else{
    if(!(input$filtermapdropdown=="No Map")){
      strmap<-paste0("'",input$filtermapdropdown,"'")
      return(paste0(" (MAP=",strmap,")"))
    }else{
      return(NULL)
    }
  }
}
filterquery<- function(input,output,session){
  playerqueryval<-genplayerquery(input,output,session)
  mapqueryval<-genselectmapquery(input,output,session, playerqueryval)
  if(!(is.null(playerqueryval) & is.null(mapqueryval))){
  query <- paste0("SELECT MATCHID FROM METADATA WHERE ",playerqueryval,mapqueryval)
  }else{
    query<-"SELECT MATCHID FROM METADATA"
  }
  
  return(dbGetQuery(con,query)$MATCHID)
}
dynamicmapstats <- function(input, output, session) {
  # FETCHS MAP STATS DYNAMIC
  query<- paste0("SELECT * FROM MATCHINFO WHERE (", dynamicquery(input,output,session,gameslist$gamesselected$MATCHID), ")")

  print(paste0("DEBUG: ", query))
  maps <- dbGetQuery(con, query)
  return(maps)
}

################################## UPDATE CLIENT AND CHARTS

updatedynamic <- function(input, output, session) {
  gameslist$playernames <- levels(factor(unlist(as.list(gameslist$gamesselected[, c("P1", "P2", "P3", "P4", "P5")]))))
  gameslist$playersseldict <- dynamicplayerstats(gameslist$playernames, input, output, session)
  gameslist$mapstats <- dynamicmapstats(input, output, session)
}
updateclient <- function(input, output, session) {
  # UPDATES CLIENT DATA
  
  metadata <<- dbReadTable(con, "METADATA")
  gamesdata <<- dbReadTable(con, "MATCHINFO")
  choice <- metadata$MATCHID
  updateCheckboxGroupInput("gamescheckbox", session = session, choices = choice, selected = choice[1])
  updateSelectizeInput(session,"filterplayerdropdown",choices=append(levels(factor(unlist(as.list(metadata[, c("P1", "P2", "P3", "P4", "P5")])))),"No Player"), selected="No Player")
  }

updatemapinfo <- function(input, output, session) {
  MapData <- mapstatscalc(input, output, session)
  output$sitepermap <- renderDataTable({
    datatable(MapData, options = list(
      pageLength = 50, scrollX = "400px"
    ), filter = "top")
  })
  gameslist$selectedmapdata <- MapData
  updateinfobox(input, output, session)
}

updatecharts <- function(input, output, session) {
  kdbyopdata <- kdchartcalc(input, output, session)
  kdbymapdata <- mapchartcalc(input,output,session)
  kdbymapdata$KDR<- round(as.double(kdbymapdata$KDR),2)
  kdbyopdata$KDR <- round(as.double(kdbyopdata$KDR),2)
  print(kdbymapdata)
  output$kdbyoptable <- renderDataTable({
    datatable(kdbyopdata)
  })
  output$kdbymaptable <- renderDataTable({
    datatable(kdbymapdata)
  })
  output$kdbymapcharts <- renderUI({
    
    # USEFULCODE
    # GENERATES DYNAMIC LIST OF PLOTS BASED ON UNIQUE PLAYER NAMES
    #
    
    unique <- unique(kdbymapdata$Player)
    unique<- unique[! unique %in% c("0")]
    plot_output_list <- lapply(1:length(unique), function(i) {
      plotname <- paste0("map",unique[i])
      box(width=12,
          plotlyOutput(plotname, width = "600px", height = "300px")
      )
    })
    # convert the list to a tagList - this is necessary for the list of
    # items to display properly
    do.call(tagList, plot_output_list)
    

  })
    for (i in 1:length(unique(kdbymapdata$Player))) {
      
      # NEEDED LOCAL TO MAKE RETURN PLOT FOR EACH PLAYER VERSUS ALL PLOTS BEING LAST PLAYER
      local({
        psel <- unique(kdbymapdata$Player)[i]
        psel<- psel[! psel %in% c("0")]
        output[[paste0("map",psel)]] <- renderPlotly({
          g <- ggplot(filter(kdbymapdata, kdbymapdata$Player == psel), aes(x = Map, y = KDR,text=paste("Kills:",Kills,"\nDeaths:",Deaths)) )+ labs(y="KDR",x="Operator")+geom_text(aes(label = Rounds), vjust = 1.5, position = position_dodge(width = 1),  colour = "blue") +
            geom_bar(aes(fill=Side),stat = "identity",position="dodge") +scale_y_continuous(limits=c(0,1+max(as.integer(filter(kdbymapdata,Player==psel)$KDR))),breaks = round(0:(4+ceiling(max(as.integer(filter(kdbymapdata,Player==psel)$KDR)))*4),2)/4)+labs(title = psel)
          g <- ggplotly(g)
        print( round(0:(4+ceiling(max(as.integer(filter(kdbymapdata,Player==psel)$KDR)))*4),2)/4)
          g <- layout(g, legend=list(font=list(family = "sans-serif",
            size = 12,
            color = "#000"),title=list(text="<b> Map </b>"),  bgcolor = "#E2E2E2",
            bordercolor = "#FFFFFF"
            
            
          ))
          dev.off()
          g
        })
        
      })
  output$kdbyopcharts <- renderUI({
    # USEFULCODE
    # GENERATES DYNAMIC LIST OF PLOTS BASED ON UNIQUE PLAYER NAMES
    #

    unique <- unique(kdbyopdata$Player)
    plot_output_list <- lapply(1:length(unique), function(i) {
      plotname <- paste0("op",unique[i])
      box(width=12,
      plotlyOutput(plotname, width = "600px", height = "300px")
      )
    })
    # convert the list to a tagList - this is necessary for the list of
    # items to display properly
    do.call(tagList, plot_output_list)
  })
  for (i in 1:length(unique(kdbyopdata$Player))) {
    # NEEDED LOCAL TO MAKE RETURN PLOT FOR EACH PLAYER VERSUS ALL PLOTS BEING LAST PLAYER
    local({
      psel <- unique(kdbyopdata$Player)[i]
      output[[paste0("op",psel)]] <- renderPlotly({
        g <- ggplot(filter(kdbyopdata, kdbyopdata$Player == psel), aes(fill=Operator,x = Operator, y = KDR,text=paste("Kills:",Kills,"\nDeaths:",Deaths)), )+geom_text(aes(label = Rounds), vjust = 1.5, position = position_dodge(width = 1),  colour = "blue")+scale_y_continuous(limits=c(0,1+max(as.integer(filter(kdbyopdata,Player==psel)$KDR))),breaks = round(0:(4+ceiling(max(as.integer(filter(kdbyopdata,Player==psel)$KDR)))*4),2)/4)+labs(title = psel)+geom_bar(stat = "identity") +
          labs(title = psel)
        g <- ggplotly(g)
        g <- layout(g, showlegend=FALSE)
        dev.off()
        g
      })
    })
  }
  
  }
}
mapstatscalc <- function(input, output, session) {
  MapDataTable <- data.frame(matrix(ncol = 10, nrow = 0))
  
    colnames(MapDataTable) <- c("Map", "Side", "Site", "OpeningPicks", "OpeningPickWins", "Wins", "Rounds", "AvgRoundTime", "AvgPlantTime", "FiveVThreesThrown")
    currentmapgamenames <- filter(metadata, MATCHID %in% gameslist$gamesselected$MATCHID)$MAP
    for (map in unique(currentmapgamenames)) {
     # ONCE HAS MAP NAME FILTERS MATCH NAMES OF THAT MAP
      matchnames <- as.list(filter(metadata, MAP == map)$MATCHID)
      currentmaprounds <- filter(gameslist$mapstats, MATCHID %in% matchnames)
      # FILTERS TO JUST THAT MAP
      mapname <- currentmaprounds$MAP[1]
      for (site in unique(currentmaprounds$SITE)) {
      # FILTERS TO BOMBSITE
      for (side in unique(filter(currentmaprounds, SITE == site)$SIDE)) {
        # FILTERS TO SIDE
        roundswon <- 0
        rounds <- 0
        openingpickwins <- 0
        openingpicks <- 0
        fivevthreesthrown <- 0
        for (i in 1:nrow(filter(filter(currentmaprounds, SITE == site), SIDE == side))) {
          round <- filter(filter(currentmaprounds, SITE == site), SIDE == side)[i, ]
          if (round$OUTCOME == "Won") {
            roundswon <- roundswon + 1
          }
          if (round$OPENINGPICK == TRUE) {
            openingpicks <- openingpicks + 1
            if (round$OUTCOME == "Won") {
              openingpickwins <- openingpickwins + 1
            }
          }
          if (round[1, "5V3THROWN"] == TRUE) {
            fivevthreesthrown <- fivevthreesthrown + 1
          }

          rounds <- rounds + 1
        }

        MapDataTable[nrow(MapDataTable) + 1, ] <- c(map, side, site, openingpicks, openingpickwins, roundswon, rounds, mean(as.integer(filter(filter(currentmaprounds, SITE == site), SIDE == side)$ROUNDLENGTH)), mean(as.integer(filter(filter(filter(currentmaprounds, SITE == site), SIDE == side), !as.numeric(PLANTTIME) == 0)$PLANTTIME)), fivevthreesthrown)
      }
    }
  }


  return(MapDataTable)
}
updateinfobox <- function(input, output, session) {
  output$mapselectedimg <- renderUI({
    div(
      class = "mapimg",
      tags$img(height = 80, width = "100%", class = "mapimgfile", src = paste0("images/", input$mapslist, ".jpg"))
    )
  })
  mapdata <- filter(gameslist$selectedmapdata, Map == input$mapslist)
  atk <- filter(mapdata, Side == "Attack")
  def <- filter(mapdata, Side == "Defense")

  output$atkstats <- renderText(paste0("WR: ", (sum(as.integer(atk$Wins)) / sum(as.integer(atk$Rounds))), " | Wins: ", sum(as.integer(atk$Wins)), " | Rounds: ", sum(as.integer(atk$Rounds))))
  output$defstats <- renderText(paste0("WR: ", (sum(as.integer(def$Wins)) / sum(as.integer(def$Rounds))), " | Wins: ", sum(as.integer(def$Wins)), " | Rounds: ", sum(as.integer(def$Rounds))))
  output$mapinfositeda <- renderUI({
    active <- filter(atk, Site == toString(sitenames[4, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[4, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ), br(),
        actionButton("sitegraphsatka", label = "View Charts")
      )
    )
  })

  output$mapinfositeca <- renderUI({
    active <- filter(atk, Site == toString(sitenames[3, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[3, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ), br(),
        actionButton("sitegraphsatkb", label = "View Charts")
      )
    )
  })
  output$mapinfositeba <- renderUI({
    active <- filter(atk, Site == toString(sitenames[2, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[2, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ),
        actionButton("sitegraphsatkc", label = "View Charts")
      )
    )
  })
  output$mapinfositeaa <- renderUI({
    active <- filter(atk, Site == toString(sitenames[1, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[1, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ),
        actionButton("sitegraphsatkd", label = "View Charts")
      )
    )
  })
  # DEFENSE
  output$mapinfositedd <- renderUI({
    active <- filter(def, Site == toString(sitenames[4, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[4, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ),
        actionButton("sitegraphsdefa", label = "View Charts")
      )
    )
  })
  output$mapinfositecd <- renderUI({
    active <- filter(def, Site == toString(sitenames[3, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[3, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ),
        actionButton("sitegraphsdefb", label = "View Charts")
      )
    )
  })
  output$mapinfositebd <- renderUI({
    active <- filter(def, Site == toString(sitenames[2, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[2, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ), actionButton("sitegraphsdefc", label = "View Charts")
      )
    )
  })
  output$mapinfositead <- renderUI({
    active <- filter(def, Site == toString(sitenames[1, mapdata$Map[1]]))
    box(
      color = "maroon",
      div(class = "sitenametext", sitenames[1, mapdata$Map[1]]), div(
        class = "mapinfo",
        sitestring(active), box(
          title = "Win Rate Info",
          width = 12,
          collapsible = T,
          class = "collapsed-box", div(
            class = "percentbox", progressBar(100 * isnullconzero((as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"])))),
            div(div(class = "progressbarlabel", paste0(label_percent()(isnullconzero(as.integer(active[1, "Wins"]) / as.integer(active[1, "Rounds"]))), " Winrate")), br(), )
          )
        ), actionButton("sitegraphsdefd", label = "View Charts")
      )
    )
  })
  
  ##FIXED: FIX OPENING ON BUTTON PRESS AFTER DISPLAY
  observeEvent(input$sitegraphsatkd, {
    genmapgraphs(1,"Attack",mapdata)
    
    
  })

  observeEvent(input$sitegraphsdefd, {
    genmapgraphs(1,"Defense",mapdata)
    
    })
  observeEvent(input$sitegraphsatkc, {
    genmapgraphs(2,"Attack",mapdata)
    
  })
  observeEvent(input$sitegraphsdefc, {
    genmapgraphs(2,"Defense",mapdata)
    
  })
  observeEvent(input$sitegraphsatkb, {
    genmapgraphs(3,"Attack",mapdata)
    
  })
  observeEvent(input$sitegraphsdefb, {
    genmapgraphs(3,"Defense",mapdata)
    
  })
  observeEvent(input$sitegraphsatka, {
    genmapgraphs(4,"Attack",mapdata)
    
  })
  observeEvent(input$sitegraphsdefa, {
    genmapgraphs(4,"Defense",mapdata)
    
  })
  
}

isnullconzero <- function(value) {
  if (is.null(value) | is.na(value)) {
    return(0)
  } else {
    return(value)
  }
}

genmapgraphs<- function(siten,side,mapdata){
  #FILTER REQUIRES TOSTRING
  if(updateflagmapcharts==2){
    updateflagmapcharts<<-1
  }else{
    planttimedata<-filter(filter(filter(gameslist$mapstats, MATCHID %in% as.list(filter(metadata, MAP == mapdata$Map[1])$MATCHID)), SITE==toString(sitenames[siten, mapdata$Map[1]])), !PLANTTIME==0)
    roundtimedata<-filter(filter(gameslist$mapstats, MATCHID %in% as.list(filter(metadata, MAP == mapdata$Map[1])$MATCHID)), SITE==toString(sitenames[siten, mapdata$Map[1]]))
    showModal(modalDialog(title=paste0("Site Graphs: ", sitenames[siten, mapdata$Map[1]]),
                                    
      tabBox(width=12,
        tabPanel(
          
          #TODO: GET FACTOR LEVEL WITH MAX NUMBER OF VALUES IN IT AND SET AS GRAPH LIMIT
      renderPlotly(ggplot(filter(planttimedata, SIDE==side),aes(PLANTTIME))+geom_histogram(breaks=c((1:36)*5))+labs(x="Plant Time",y="Count")), title = "Plant Time Binned"),
      tabPanel(
      renderPlotly(ggplot(filter(roundtimedata, SIDE==side),aes(ROUNDLENGTH))+geom_histogram(breaks=c((1:45)*5))+labs(x="Round Time", y="Count")),title="Round Time"
      ),
    
    tabPanel("Test Panel", renderText("Test"))
      )))}
}
sitestring <- function(active) {
  return(paste0("Wins: ", active[1, "Wins"], " | Rounds Played: ", active[1, "Rounds"], " | Opening Picks: ", active[1, "OpeningPicks"], " | Opening Pick Rate: ", label_percent()(as.integer(active[1, "OpeningPicks"]) / as.integer(active[1, "Rounds"])), " | AVG Round Time :", as.integer(active[1, "AvgRoundTime"]), " | Avg Plant Time: ", as.integer(active[1, "AvgPlantTime"]), " | FiveVThrees Thrown: ", as.integer(active[1, "FiveVThreesThrown"])))
}

kdchartcalc <- function(input, output, session) {
  KDTable <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(KDTable) <- c("Player", "Operator", "Kills", "Deaths", "KDR", "Rounds")
  print(paste("DEBUG: KEYS FOR PLAYER DICTIONARY:", gameslist$playersseldict$keys()))
  for (key in gameslist$playersseldict$keys()) {
    activeplayerdf <- gameslist$playersseldict$get(key)
    for (op in unique(activeplayerdf$OPERATOR)) {
      kills <- 0
      deaths <- 0
      rounds <- 0
      temp <- filter(activeplayerdf, activeplayerdf$OPERATOR == op)
      for (i in 1:nrow(temp)) {
        row <- temp[i, ]

        kills <- kills + row$KILLS
        if (row$TOD > 0) {
          deaths <- deaths + 1
        }
        rounds <- rounds + 1
      }
      kd <- 0
      if (kills == 0) {
        kd <- 0
      } else if (deaths == 0) {
        kd <- kills
      } else {
        kd <- round((kills / deaths), digits = 4)
      }


      KDTable[nrow(KDTable) + 1, ] <- c(activeplayerdf$PLAYERNAME[1], op, kills, deaths, kd, rounds)
    }
  }
  return(KDTable)
}

mapchartcalc <- function(input, output, session) {
  Bindtotal<- data.frame(matrix(ncol=7,nrow=0))
  colnames(Bindtotal) <- c("Player", "Map", "Side","Kills", "Deaths", "KDR", "Rounds")
  KDTable <- data.frame(matrix(data=sapply(1:128,function(x) 0),ncol = 7, nrow = 18))
  colnames(KDTable) <- c("Player", "Map", "Side","Kills", "Deaths", "KDR", "Rounds")
  row.names(KDTable)<- append(sapply(mapnames$MAPS, paste0,"Attack"), sapply(mapnames$MAPS,paste0,"Defense"))
  print(KDTable)
  print(paste("DEBUG: KEYS FOR PLAYER DICTIONARY:", gameslist$playersseldict$keys()))
  for (key in gameslist$playersseldict$keys()) {
    activeplayerdf <- gameslist$playersseldict$get(key)
  for(map in mapnames$MAPS){
    print(map)
    filtermapnames <- filter(gameslist$gamesselected, MAP==map)
    if(!nrow(filtermapnames)==0){
    for (i in 1:nrow(filtermapnames)) {
      match<- filter(activeplayerdf, MATCHID==filtermapnames[i,"MATCHID"])
      for(j in 1:nrow(match)){
        row <- match[j,]
        if(row$TOD>0){
          deaths<-1
        }else{
          deaths<-0
        }
        print(row$SIDE)
        if(row$SIDE=="Attack"){
          
          KDTable[paste0(map,"Attack"),]<-append(c(key,map,"Attack"),(as.vector(as.integer(KDTable[paste0(map,"Attack"),c("Kills","Deaths","KDR","Rounds")]))+c(row$KILLS,deaths,0,1)))
          
        }else{
          KDTable[paste0(map,"Defense"),]<-append(c(key,map,"Defense"),(as.vector(as.integer(KDTable[paste0(map,"Defense"),c("Kills","Deaths","KDR","Rounds")]))+c(row$KILLS,deaths,0,1)))
        }
        
      }

      KDTable[paste0(map,"Defense"),6]<-(as.integer(KDTable[paste0(map,"Defense"),4])/as.integer(KDTable[paste0(map,"Defense"),5]))
      
      
      KDTable[paste0(map,"Attack"),6]<-(as.integer(KDTable[paste0(map,"Attack"),4])/as.integer(KDTable[paste0(map,"Attack"),5]))
      
      
    }
    
    }
  }
    Bindtotal<-rbind(Bindtotal,KDTable)
    KDTable <- data.frame(matrix(data=sapply(1:128,function(x) 0),ncol = 7, nrow = 18))
    colnames(KDTable) <- c("Player", "Map", "Side","Kills", "Deaths", "KDR", "Rounds")
    row.names(KDTable)<- append(sapply(mapnames$MAPS, paste0,"Attack"), sapply(mapnames$MAPS,paste0,"Defense"))}
  return(Bindtotal)
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
  type <- c(MATCHID = "varchar(20)", ROUND = "int", SIDE = "varchar(20)", PLAYERNAME = "varchar(20)", OPERATOR = "varchar(20)", TOD = "int", KILLER = "varchar(20)", DEATHREFRAGGED = "varchar(20)", DEATHTRADED = "varchar(20)", UTILDEATH = "bit", DEATHLOC = "varchar(20)", OPENINGDEATH = "bit", ENTRYDEATH = "bit", ROUND_1 = "int", KILLS = "int", HSKILLS = "int", UTILKILLS = "int", EXIT = "int", OPENINGKILL = "bit", ENTRYKILL = "bit", ROUND_2 = "int", OBJECTIVE = "bit", KOST = "bit", "1VX" = "bit", "1VXCLUTCH" = "bit", K1LOC = "varchar(20)", UKILL = "bit", K1PLAYER = "varchar(20)", K1TIME = "int", ZPING = "bit", HS = "bit", REFRAG = "varchar(20)", K2LOC = "varchar(20)", UKILL2 = "bit", K2PLAYER = "varchar(20)", K2TIME = "int", ZPING2 = "bit", HS2 = "bit", REFRAG2 = "varchar(20)", K3LOC = "varchar(20)", UKILL3 = "bit", K3PLAYER = "varchar(20)", K3TIME = "int", ZPING3 = "bit", HS3 = "bit", REFRAG3 = "varchar(20)", K4LOC = "varchar(20)", UKILL4 = "bit", K4PLAYER = "varchar(20)", K4TIME = "int", ZPING4 = "bit", HS4 = "bit", REFRAG4 = "varchar(20)", K5LOC = "varchar(20)", UKILL5 = "bit", K5PLAYER = "varchar(20)", K5TIME = "int", ZPING5 = "bit", HS5 = "bit", REFRAG5 = "varchar(20)", ROUND_3 = "int", ZPINGKILLS = "int", ZPINGASSIST = "int", DKP = "varchar(20)", DKR = "varchar(20)", DDP = "varchar(20)", POCKET = "varchar(20)", D1DT = "varchar(20)", D2DT = "varchar(20)")
  dbman_uploadmatchmetadata <- readxl::read_excel(input$dbman_fileinput$datapath, "MATCHMETADATA")
  dbman_uploadmatchinfo <- readxl::read_excel(input$dbman_fileinput$datapath, "MATCHINFO")
  dbman_uploadplayer1pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER1PULL")
  dbman_uploadplayer2pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER2PULL")
  dbman_uploadplayer3pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER3PULL")
  dbman_uploadplayer4pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER4PULL")
  dbman_uploadplayer5pull <- readxl::read_excel(input$dbman_fileinput$datapath, "PLAYER5PULL")


  dbWriteTable(con, "METADATA", dbman_uploadmatchmetadata, append = TRUE)
  dbWriteTable(con, "MATCHINFO", dbman_uploadmatchinfo, append = TRUE)
  if (dbExistsTable(con, dbman_uploadplayer1pull$PLAYERNAME[1])) {
    dbWriteTable(con, dbman_uploadplayer1pull$PLAYERNAME[1], dbman_uploadplayer1pull, append = TRUE)
  } else {
    dbWriteTable(con, dbman_uploadplayer1pull$PLAYERNAME[1], dbman_uploadplayer1pull, overwrite = TRUE, field.type = type)
  }
  if (dbExistsTable(con, dbman_uploadplayer2pull$PLAYERNAME[1])) {
    dbWriteTable(con, dbman_uploadplayer2pull$PLAYERNAME[1], dbman_uploadplayer2pull, append = TRUE)
  } else {
    dbWriteTable(con, dbman_uploadplayer2pull$PLAYERNAME[1], dbman_uploadplayer2pull, overwrite = TRUE, field.type = type)
  }
  if (dbExistsTable(con, dbman_uploadplayer3pull$PLAYERNAME[1])) {
    dbWriteTable(con, dbman_uploadplayer3pull$PLAYERNAME[1], dbman_uploadplayer3pull, append = TRUE)
  } else {
    dbWriteTable(con, dbman_uploadplayer3pull$PLAYERNAME[1], dbman_uploadplayer3pull, overwrite = TRUE, field.type = type)
  }
  if (dbExistsTable(con, dbman_uploadplayer4pull$PLAYERNAME[1])) {
    dbWriteTable(con, dbman_uploadplayer4pull$PLAYERNAME[1], dbman_uploadplayer4pull, append = TRUE)
  } else {
    dbWriteTable(con, dbman_uploadplayer4pull$PLAYERNAME[1], dbman_uploadplayer4pull, overwrite = TRUE, field.type = type)
  }
  if (dbExistsTable(con, dbman_uploadplayer5pull$PLAYERNAME[1])) {
    dbWriteTable(con, dbman_uploadplayer5pull$PLAYERNAME[1], dbman_uploadplayer5pull, append = TRUE)
  } else {
    dbWriteTable(con, dbman_uploadplayer5pull$PLAYERNAME[1], dbman_uploadplayer5pull, overwrite = TRUE, field.type = type)
  }
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
  
  output$atkstats <- renderText("Select A Map")
  output$defstats <- renderText("Select A Map")
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
  updateflagmapcharts<<-0
  sitenames <<- readxl::read_excel("Ref/sitenames.xlsx")
  mapnames<<- read.csv("Ref/maps.csv")

  # makes sure that at least one game is selected
  observeEvent(input$gamescheckbox, {
    if (length(input$gamescheckbox) == 1) {
      shinyjs::disable(selector = paste("#gamescheckbox input[value='", input$gamescheckbox[1], "']", sep = ""))
    } else {
      enable("gamescheckbox")
    }

    gameslist$gamenames <- input$gamescheckbox
    
    # UPDATE DYNAMIC
    
  })
  
  observeEvent(input$updatedatafetch,{
    
    if(input$filtermethod=="Manual"){
      gameslist$gamesselected <- filter(metadata, MATCHID %in% gameslist$gamenames)
    }else{
      gameslist$gamesselected <- filter(metadata, MATCHID %in% filterquery(input,output,session))
    }
               updatedynamic(input, output, session)
               output$namedata <- renderText({
                 updateSelectizeInput(session, "mapslist", choices = unique(filter(metadata, MATCHID %in% gameslist$gamesselected$MATCHID)$MAP), selected = unique(filter(metadata, MATCHID %in% gameslist$gamesselected$MATCHID)$MAP)[1])
                 output$dashboard <- renderMenu(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")))
                 output$mapdashboard <- renderMenu(menuItem("Map Dashboard", tabName = "mapdashboard", icon = icon("dashboard")))
                 gameslist$playernames
               })
               
               })

  observeEvent(input$updategraphs, {
    updatecharts(input, output, session)
  })
  observeEvent(input$updatemapstats, {
    if(updateflagmapcharts==1){
      updateflagmapcharts<<-2
    }
    if(updateflagmapcharts==0){
      updateflagmapcharts<<-1
    }
    updatemapinfo(input, output, session)
  })
  observeEvent(input$updatemappick, {
    if(updateflagmapcharts==1){
      updateflagmapcharts<<-2
    }
    if(updateflagmapcharts==0){
      updateflagmapcharts<<-1
    }
    updatemapinfo(input, output, session)
  })
  # renders table of matchinfo for selected games
  output$gameslist <- renderTable(
    gameslist$gamesselected
  )

  output$mapstatstable <- renderDataTable(datatable(gameslist$mapstats, options = list(
    pageLength = 50, scrollX = "400px"
  ), filter = "top"))
  # executes dbman server
  dbman_server("dbman", input, output, session)

  # renders text for list of games selected in game selector
  output$gamesselected <- renderText(gameslist$gamenames)
}

library(ggplot2)
library(DBI)
library(reticulate)

#CONNECT TO DATABASE
#A WHOLE FUCKING HOUR TO FIGURE THIS OUT
con <- DBI::dbConnect(odbc::odbc(),Driver = "SQL Server",Server = "DESKTOP-PRASB39\\TESTINSTANCE", database= "TutorialDB")
dbListTables(con)
#dbWriteTable(con, "mtcars", mtcars)
#FETCHDATA
#siegedata <- tbl(con, "odc")
#SOLIDIFY DATA
#siegedata<-siegedata %>% collect()
siegedata <- dbGetQuery(con, "SELECT * from PULLP1 WHERE GAMEID='5_30 Theme'")
#dbGetQuery(con, "DROP TABLE mtcars")
dbDisconnect(con)
#GRAPH
#CUTOFF VALUES NEED TO BE 1 OVER FOR GOD KNOWS WHY
ChartA <- ggplot(subset(siegedata,GAMEID == "5_30 Theme"), aes(x=as.numeric(Alpha), y = as.numeric(KILLS))) + geom_bar(stat="identity")+scale_x_continuous(breaks=c(1:15),limits=c(0,16))+scale_y_continuous(breaks=c(1:5),limits=c(0,5))
ChartA

obj<- readxl::read_xlsx("C:/Users/Autumn/Desktop/test.xlsx")

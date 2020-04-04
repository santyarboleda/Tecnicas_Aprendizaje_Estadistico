#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plyr)
library(ggplot2)
library(Utiltae)
library(lazyeval)
library(rgdal)
library(leaflet)
library('sqldf')


#En este trabajo se abordara el problema de agrupar los barrios de Medellin de acuerdo a distintas dimensiones y
#analizar espacialmente las agrupaciones.

barrios_med <- readOGR("Barrio_Vereda/Barrio_Vereda.shp",layer="Barrio_Vereda")

nombres_barrios <- iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")





ECV <- read.csv("encuesta_calidad_vida.csv", header = TRUE, sep=";", encoding = "UTF-8")

ECV <- setNames(ECV,set_dataSet_names(names(ECV)))

ECV_MOVILIDAD<- ECV[,c("encuesta","comuna","barrio","estrato","p_83","p_84","p_212","p_213","p_214","p_318","p_321","p_322")]

ECV_MOVILIDAD_DEP <- subset(ECV_MOVILIDAD,p_83 != 'NULL' & p_84 != 'NULL' & p_212 != 'NULL' & p_213 != 'NULL' & p_214 != 'NULL' & p_318 != 'NULL' & p_321 != 'NULL' & p_322 != 'NULL')
nrow(ECV_MOVILIDAD_DEP)


opciones_comuna<-c("", sort(as.character(unique(ECV_MOVILIDAD_DEP$comuna))),"TODAS")


ECV_MOVILIDAD_P83 <- get_indicadores_ei(ECV_MOVILIDAD_DEP,'p_83')
CONS_P83 <- sqldf("SELECT comuna, barrio, CAST (SUM(totalRespuestaE) AS REAL) / TotalB *100 AS P83_IND
                  FROM ECV_MOVILIDAD_P83  
                  WHERE p_83 = '2' 
                  GROUP BY comuna, barrio")



ECV_MOVILIDAD_P84 <- get_indicadores_ei(ECV_MOVILIDAD_DEP,'p_84')
CONS_P84 <- sqldf("SELECT comuna, barrio, CAST (SUM(totalRespuestaE) AS REAL) / TotalB *100 AS P84_IND
                  FROM ECV_MOVILIDAD_P84  
                  WHERE p_84 = '4' OR p_84 = '5' 
                  GROUP BY comuna, barrio")


ECV_MOVILIDAD_P212 <- get_indicadores_ec(ECV_MOVILIDAD_DEP,'p_212')
CONS_P212 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_212) as real)*(totalRespuestaE))/TotalB AS P212_IND, SUM(cast ((p_212) as real)*(totalRespuestaE)) AS CANT_V
                  FROM ECV_MOVILIDAD_P212  
                  WHERE p_212 > 0 
                  GROUP BY comuna, barrio")


ECV_MOVILIDAD_P213 <- get_indicadores_ec(ECV_MOVILIDAD_DEP,'p_213')
CONS_P213 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_213) as real)*(totalRespuestaE))/TotalB AS P213_IND 
                  FROM ECV_MOVILIDAD_P213  
                  WHERE p_213 > 0
                  GROUP BY comuna, barrio")

ECV_MOVILIDAD_P214 <- get_indicadores_ec(ECV_MOVILIDAD_DEP,'p_214')
CONS_P214 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_214) as real)*(totalRespuestaE))/TotalB AS P214_IND 
                  FROM ECV_MOVILIDAD_P214  
                  WHERE p_214 > 0
                  GROUP BY comuna, barrio")

ECV_MOVILIDAD_P318 <- get_indicadores_ei(ECV_MOVILIDAD_DEP,'p_318')
CONS_P318 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_318) as real)*(totalRespuestaE)) / sum(totalRespuestaE) AS P318_IND 
                  FROM ECV_MOVILIDAD_P318  
                  WHERE p_318 > 0
                  GROUP BY comuna, barrio")

ECV_MOVILIDAD_P321 <- get_indicadores_ei(ECV_MOVILIDAD_DEP,'p_321')
CONS_P321 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_321) as real)*(totalRespuestaE)) / sum(totalRespuestaE) AS P321_IND 
                  FROM ECV_MOVILIDAD_P321  
                  WHERE p_321 > 0
                  GROUP BY comuna, barrio")

ECV_MOVILIDAD_P322 <- get_indicadores_ei(ECV_MOVILIDAD_DEP,'p_322')
CONS_P322 <- sqldf("SELECT comuna, barrio, SUM(cast ((p_322) as real)*(totalRespuestaE)) / sum(totalRespuestaE) AS P322_IND 
                  FROM ECV_MOVILIDAD_P322  
                  WHERE p_322 > 0
                  GROUP BY comuna, barrio")

ECV_MOVILIDAD_FINAL <- data.frame(unique(ECV_MOVILIDAD[,c("comuna", "barrio")]))
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P83, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P84, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P212, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P213, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P214, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P318, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P321, by = c("comuna", "barrio"), all.x = TRUE)
ECV_MOVILIDAD_FINAL <- merge(ECV_MOVILIDAD_FINAL, CONS_P322, by = c("comuna", "barrio"), all.x = TRUE)


ECV_MOVILIDAD_FINAL[is.na(ECV_MOVILIDAD_FINAL)] <- 0
ECV_MOVILIDAD_FINAL$barrio <- paste(ECV_MOVILIDAD_FINAL$comuna, ECV_MOVILIDAD_FINAL$barrio, sep = "-")
ECV_MOVILIDAD_FINAL$comuna <- NULL


ECV_MOVILIDAD_SCALE <-  tibble::column_to_rownames(ECV_MOVILIDAD_FINAL, var = ("barrio"))
ECV_MOVILIDAD_SCALE <-  scale(ECV_MOVILIDAD_SCALE)


set.seed(42)
kmeans_model <- kmeans(ECV_MOVILIDAD_SCALE, 4, nstart = 50)

df_member <- cbind(ECV_MOVILIDAD_FINAL, cluster = kmeans_model$cluster)

factpal <- colorFactor(topo.colors(4), df_member$cluster)





# Cabecera
header <- dashboardHeader(title = "Encuesta Calidad de Vida Medellin 2019", titleWidth = 450)



# Menu

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Vivienda y Servicios Publicos", icon = icon("home"), tabName = "vivienda",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Educacion", icon = icon("book-open"), tabName = "widgets",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Empleo", icon = icon("building"), tabName = "widgets",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Movilidad", icon = icon("car"), tabName = "movilidad",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Salud", icon = icon("stethoscope"), tabName = "widgets",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Seguridad Ciudadana", icon = icon("user-secret"), tabName = "widgets",
                 badgeLabel = "", badgeColor = "green"),
        menuItem("Nutricion", icon = icon("utensils"), tabName = "nutricion",
                 badgeLabel = "", badgeColor = "green")
    )
    
    
    
    
)








body <- dashboardBody(
    tabItems(
        
        ### Tab que corresponde al menuItem "Pagina Inicial"
        tabItem(tabName = "nutricion",
                fluidRow(
                    infoBox("New Orders", 10 * 2, icon = icon("credit-card")),
                    column(width = 4,
                           #actionButton("calcular","Calcular")
                    ),
                    column(width = 12,
                           box(title = "Mapa", background = "green", width = NULL, solidHeader = TRUE, "Box content"),
                          # box(title = "Georeferenciacion",leafletOutput("graficarMapa"), height = 600)
                          #box(plotOutput("plot1", height = 600, width = "100%",)),
                    ),
                    
                
                )
                
            ),
    
        
        
        tabItem(tabName = "movilidad",
                fluidRow(
                    valueBox("Auto", 0, icon = icon("car"), color = "red"),
                    valueBox("Moto", 10 * 2, icon = icon("motorcycle"), color = "purple"),
                    valueBox("Bici", 10 * 2, icon = icon("bicycle"), color = "green")
                    
                ), 
                
                column(width = 4,
                       selectInput("comuna","Comuna",choices =  opciones_comuna, selected='POPULAR'),
                       actionButton("calcular","Calcular")
                ),
                
                
                
                #column(width = 12,
                       #box(title = "Georeferenciacion 2",leafletOutput("graficarMapa_2"), height = 600, width = "100%")
                       #box(plotOutput("plot1", height = 600, width = "100%",)),
                #),
                
               
                column(width = 12,
                       #box(title = "Georeferenciacion 2",leafletOutput("graficarMapa_2"), height = 600, width = "100%")
                       box(plotOutput("plot1", height = 300, width = 1600)),
                ),     
        )
        
        
        
        
        
        
        
        
        )

)







ui <- dashboardPage(header, sidebar, body, skin = "green")






server <- function(input, output) {
    output$plot1 <- renderPlot({
        
        #barplot(CONS_P83$P83_IND)
        datos_212<-(subset(CONS_P212,subset = (CONS_P212$comuna==input$comuna)))
        #barplot(datos_212$P212_IND, col="darkblue")
        barplot(datos_212$P212_IND, col="darkblue", names.arg = datos_212$barrio, xlab="Barrio", 
                ylab="Vehiculos", main="Promedio de Vehiculos por Hogar")
        #data <- histdata[seq_len(input$slider)]
        #hist(data)
        #axis(
         #    side = 1,
          #   at = datos_212$P212_IND,
           #  labels = datos_212$barrio
            # )
    })
    
    
    output$graficarMapa<-renderLeaflet({
        data_mapa()
    })
    
    output$graficarMapa_2<-renderLeaflet({
        data_mapa_2()
    })
    
    data_mapa<-eventReactive(input$calcular, {
        mapa<-leaflet()
        mapa<-addTiles(mapa)
        mapa<-addProviderTiles(mapa,provider <- "OpenStreetMap.Mapnik")
        mapa<-addMarkers(mapa,lng=-75.5926108,lat=6.2746986,clusterOptions = markerClusterOptions())
        mapa
        
        
    }) 
    
    
    data_mapa_2<-eventReactive(input$calcular, {
        m <-leaflet(barrios_med)
        colores=sample(x=c("orange","green","yellow"),size=length(df_member$barrio),replace=TRUE)
        m <- addPolygons(m,popup=df_member$barrio, color=colores)
        m <- addTiles(m)
        m
        
        
    }) 
   
}

shinyApp(ui, server)


# Run the application 
#shinyApp(ui = ui, server = server)

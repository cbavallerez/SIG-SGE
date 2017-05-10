library(leaflet)
library(RMySQL)
library(DBI)
library(htmltools)
library(rgdal)
library(dbConnect)
MySQL(max.con=100, fetch.default.rec=1000)
#la funcion ubicacion establecimiento recibe como parametro txtregion que ...
#contiene le numero de region seleccionado por el input regiones
conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
ubicacion_establecimiento <- function(txtregion) {
  
  #la consulta extra el RBD, Langitud y latitud de todos los establecimientos de la region 
  my_query <- 'SELECT RBD, LATITUD, LONGITUD FROM ESTABLECIMIENTOS WHERE COD_REG_RBD = TXTREGION'
  my_query <- sub("TXTREGION",txtregion,my_query)
  ubicacion <- dbGetQuery(conn,my_query)
}


ubicacion_establecimiento_comuna <- function(txtcomuna) {
  
  #la consulta extra el RBD, Langitud y latitud de todos los establecimientos de la region 
  my_query <- 'SELECT RBD, LATITUD, LONGITUD FROM ESTABLECIMIENTOS WHERE COD_COM_RBD = TXTCOMUNA'
  my_query <- sub("TXTCOMUNA",txtcomuna,my_query)
  ubicacion <- dbGetQuery(conn,my_query)
}





shinyServer(function(input, output) {
  conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
  
  comunas <- function(txtregion) {
    #la consulta extra el RBD, Langitud y latitud de todos los establecimientos de la comuna 
    my_query <- 'SELECT NOM_COM, COD_COMUNA FROM COMUNAS WHERE COD_REG = TXTREGION'
    my_query <- sub("TXTREGION",txtregion,my_query)
    ubicacion <- dbGetQuery(conn,my_query)
  }
  
  #En la siguiente salida se crea un select para las comunas
  output$seleccionar_comuna = renderUI({
    RBD_establecimiento_seleccionado <- as.numeric(input$establecimiento_seleccionado) 
    choicesComunas <- setNames(as.character(comunas(input$txtregion)$COD_COMUNA), comunas(input$txtregion)$NOM_COM)
    if(is.null(RBD_establecimiento_seleccionado)){
      selectInput("txtcomuna", label = h4("Seleccionar Comuna"), choices = choicesComunas
      )
    }else{
      #selectInput("txtcomuna", label = h4("Seleccionar Comuna"), choices = choicesComunas, selected = as.numeric(establecimiento_seleccionado(RBD_establecimiento_seleccionado)$COD_COM_RBD) 
      #)
    }
    
    
    
  })
  #En la siguiente salida se envia a la UI.R un input del tipo numerico que servira pa seleecionar un establecimiento 
  output$inputRBD = renderUI({
    numericInput("establecimiento_seleccionado", "RBD:",  5666)
  })
  output$salida_ver_shapefile<-renderPrint({
    if (input$estab_for_region == FALSE) {
      input$txtcomuna
    } else {
      input$txtcomuna
    }
  })
  
  output$salida_estab_for_region <- renderText({ input$estab_for_region })

  
  output$establecimiento_datos = renderUI({
    
    
    txtregion <-input$txtregion
    RBD_establecimiento_seleccionado <- as.numeric(input$establecimiento_seleccionado) 
    list(
      tags$p("Nombre: ",establecimiento_seleccionado(RBD_establecimiento_seleccionado)$NOM_RBD),
      tags$p("Longitud: ",establecimiento_seleccionado(RBD_establecimiento_seleccionado)$LONGITUD),
      tags$p("Latitud: ",establecimiento_seleccionado(RBD_establecimiento_seleccionado)$LATITUD),
      tags$p("Matricula: ",matricula_establecimiento(RBD_establecimiento_seleccionado)),
      tags$p("Comuna: ",establecimiento_seleccionado(RBD_establecimiento_seleccionado)$NOM_COM_RBD),
      tags$p("Cantidad de Alumnos SEP en la escuela: ",sep_establecimiento(RBD_establecimiento_seleccionado)),
      tags$p("pi -> Proporcion SEP/ESC: ",matricula_establecimiento(RBD_establecimiento_seleccionado)),
      tags$p("Indice Seg: ",matricula_establecimiento(RBD_establecimiento_seleccionado))
      
      
      
    )
  })
  #En la siguiente salida se crea un select para las comunas
  output$seleccionar_comuna = renderUI({
    RBD_establecimiento_seleccionado <- as.numeric(input$establecimiento_seleccionado) 
    choicesComunas <- setNames(as.character(comunas(input$txtregion)$COD_COMUNA), comunas(input$txtregion)$NOM_COM)
    selectInput("txtcomuna", label = h4("Seleccionar Comuna"), choices = choicesComunas, selected = as.numeric(establecimiento_seleccionado(RBD_establecimiento_seleccionado)$COD_COM_RBD) 
    )
    
    
  })

  
  #Envia informacion sobre los establecimientos a una tabla 
  output$DatosEstablecimiento = renderDataTable({
    #ubicacion_establecimiento(input$txtregion)
  })
  
  
  
  #Toma informacion del establecimiento seleccionado en el input RBD
  establecimiento_seleccionado <- function(RBD_establecimiento_seleccionado) {
    
    RBD_establecimiento_seleccionado <- as.numeric(input$establecimiento_seleccionado) 
    conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
    
    my_query <- 'SELECT NOM_RBD, LATITUD, LONGITUD, COD_COM_RBD, NOM_COM_RBD FROM ESTABLECIMIENTOS WHERE AGNO = 2015 AND RBD = RBD_SELECCIONADO'
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    ubicacion <- dbGetQuery(conn,my_query)
    #cons<-dbListConnections(MySQL()) for(con in cons) dbDisconnect(con)
    
  }
  
  #Esta funciona recibe el RBD del establecimiento para almacenar el conteo de alumnos inscritos en ese establecimiento   
  
  matricula_establecimiento <- function(RBD_establecimiento_seleccionado) {
    conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
    
    my_query <- 'SELECT count(*) FROM ALUMNOS WHERE AGNO = AÑO_SELECCIONADO AND RBD = RBD_SELECCIONADO'
    my_query <- sub("AÑO_SELECCIONADO",input$AGNO,my_query)
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    matricula <- dbGetQuery(conn,my_query)
    #cons<-dbListConnections(MySQL()) for(con in cons) dbDisconnect(con)
    
  }
  
  #Esta funcion Recibe el RBD del establecimiento y almacena el conteo de alumnos vulnerables  
  sep_establecimiento <- function(RBD_establecimiento_seleccionado) {
    conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
    on.exit(DBI::dbDisconnect(conn))
    my_query <- 'SELECT count(*) FROM ALUMNOS_SEP WHERE AGNO = AÑO_SELECCIONADO AND RBD = RBD_SELECCIONADO'
    my_query <- sub("AÑO_SELECCIONADO",input$AGNO,my_query)
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    ubicacion <- dbGetQuery(conn,my_query)
    #cons<-dbListConnections(MySQL()) for(con in cons) dbDisconnect(con)
    
  }
  
  
  
  
  ######################################    MAPA ############################################
  output$oid2<-renderPrint({
    if (x()>50) {
      "You entered a number greater than 10"
    } else {
      "You entered a number less than or equal to 10"
    }
  })
  
  
  #  Se declara la salida mimapa almacenando un renderleaflet
  output$mimapa <- renderLeaflet({ 
    
    
    #Se inicia la conexion con la base de datos
    conn <- dbConnect(MySQL(), user="root", host="localhost", password="03121991-0oK", dbname="mydb", port=3306)
    
    #la consulta extrae contiene la longitud y latitud de la region seleccionada en el input$txtregion
    query_posicionregion <- 'SELECT LONGITUD, LATITUD FROM `REGIONES` WHERE `ID` = TXTREGION'
    query_posicionregion <- sub("TXTREGION",input$txtregion,query_posicionregion)
    #   pasteregion contiene la longitud y latitud de la region seleccionada en el input$txtregion
    posicionregion <- dbGetQuery(conn,query_posicionregion)
    
    
    
    
    
    #   Se cargan los establecimientos de la region seleccionada
    ubicacion_RBD <- as.character(ubicacion_establecimiento(input$txtregion)$RBD)
    
    
    
    #Se carga la funcion leaflet con el argumento
    leaflet() %>% addTiles()%>%setView(lng = posicionregion$LONGITUD, lat = posicionregion$LATITUD, zoom = 8)
    #cons<-dbListConnections(MySQL()) for(con in cons) dbDisconnect(con)
  })

    observe({
      
      if (input$estab_for_region == TRUE) {
        txtregion <-input$txtregion
        
        
        leafletProxy("mimapa", data = ubicacion_establecimiento(txtregion)) %>%
          clearMarkers() %>%
          addCircleMarkers(lat =  ~LATITUD, lng =  ~LONGITUD, weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD
          )
      } else {
        
        
        if(is.null(input$txtcomuna)){
          txtcomuna <- "9119"
        } else{
          txtcomuna <-input$txtcomuna
          
       
        }
        
        leafletProxy("mimapa", data = ubicacion_establecimiento_comuna(txtcomuna)) %>%
          clearMarkers() %>%
          addCircleMarkers(lat =  ~LATITUD, lng =  ~LONGITUD, weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD
          )
      }
      

    })

  
  observeEvent(input$mimapa_marker_click, { # update the map markers and view on map clicks
    p <- input$mimapa_marker_click
    proxy <- leafletProxy("mimapa")
    if(p$id=="Selected"){
      proxy %>% removeMarker(layerId="Selected")
    } else {
      proxy %>% setView(lng=p$lng, lat=p$lat, 15)
      output$inputRBD = renderUI({
        numericInput("establecimiento_seleccionado", "RBD:", p$id  )
      })
      
    }
  })
  
  
  
  states <- readOGR("www/division_comunal/division_comunal.shp",
                    layer = "division_comunal", GDAL1_integer64_policy = TRUE)
  
  neStates <- subset(states, states$COD_COMUNA %in% c(
    "9112"
  ))
  
  leaflet(neStates) %>%
    addPolygons(color = "#444444", weight = 3, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5
    )
  
  
  
  
  
  
  
  
  ################################# FIN MAPA #########################################################
  
})
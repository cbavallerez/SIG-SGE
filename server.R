library(leaflet)
library(RMySQL)
library(DBI)
library(htmltools)
library(rgdal)
library(dbConnect)

#la funcion ubicacion establecimiento recibe como parametro txtregion que ...
#contiene le numero de region seleccionado por el input regiones
ubicacion_establecimiento <- function(txtregion) {
  on.exit(dbDisconnect(conn))
  
  conn <- dbConnect(MySQL(), user="SIG-SGE", host="127.0.0.1", password="SiFNobafwSulPVGT", dbname="mydb")
  
  #la consulta extra el RBD, Langitud y latitud de todos los establecimientos de la region 
  my_query <- 'SELECT RBD, LATITUD, LONGITUD FROM ESTABLECIMIENTOS WHERE COD_REG_RBD = TXTREGION'
  my_query <- sub("TXTREGION",txtregion,my_query)
  ubicacion <- dbGetQuery(conn,my_query)
}


shinyServer(function(input, output) {

  
  #En la siguiente salida se envia a la UI.R un input del tipo numerico que servira pa seleecionar un establecimiento 
  output$inputRBD = renderUI({
    numericInput("establecimiento_seleccionado", "RBD:",  5666)
  })
  #En la siguiente salida se crea un select para las comunas
  output$seleccionar_comuna = renderUI({
    
    selectInput("txtcomuna", label = h4("Seleccionar Comuna"), 
                
                choices = c("Seleccione una comuna"=  0,
                            "Región de sss"=  1,
                            "Región de Antofagasta"= 2,
                            "Región de Atacama" = 3,
                            "Región de Coquimbo" = 4,
                            "Región de Valparaíso" = 5,
                            "Región del Libertador Gral. Bernardo O’Higgins" = 6,
                            "Región del Maule" = 7,
                            "Región del Biobío" = 8,
                            "Región de la Araucanía" = 9,
                            "Región de Aysén del Gral. Carlos Ibáñez del Campo"= 10,
                            "Región de Magallanes y de la Antártica Chilena"= 11,
                            "Región de Metropolitana de Santiago"= 12,
                            "Región de Los Ríos"= 13,
                            "Región de Tarapacá" =14,
                            "Región de Arica y Parinacota" = 15), 
                selected = 0
    )
  })
  
  
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
  
  #Envia informacion sobre los establecimientos a una tabla 
  output$DatosEstablecimiento = renderDataTable({
    ubicacion_establecimiento(input$txtregion)
  })
  
  
  
  #Toma informacion del establecimiento seleccionado en el input RBD
  establecimiento_seleccionado <- function(RBD_establecimiento_seleccionado) {
    
    conn <- dbConnect(MySQL(), user="root", host="127.0.0.1", password="03121991-oK", dbname="mydb")
    
    my_query <- 'SELECT NOM_RBD, LATITUD, LONGITUD, COD_COM_RBD, NOM_COM_RBD FROM ESTABLECIMIENTOS WHERE AGNO = 2015 AND RBD = RBD_SELECCIONADO'
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    ubicacion <- dbGetQuery(conn,my_query)
  
     }
  
  #Esta funciona recibe el RBD del establecimiento para almacenar el conteo de alumnos inscritos en ese establecimiento   
  
  matricula_establecimiento <- function(RBD_establecimiento_seleccionado) {
    conn <- dbConnect(MySQL(), user="root", host="127.0.0.1", password="03121991-oK", dbname="mydb")

    my_query <- 'SELECT count(*) FROM ALUMNOS WHERE AGNO = AÑO_SELECCIONADO AND RBD = RBD_SELECCIONADO'
    my_query <- sub("AÑO_SELECCIONADO",input$AGNO,my_query)
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    matricula <- dbGetQuery(conn,my_query)
  }
  
  #Esta funcion Recibe el RBD del establecimiento y almacena el conteo de alumnos vulnerables  
  sep_establecimiento <- function(RBD_establecimiento_seleccionado) {
    conn <- dbConnect(MySQL(), user="root", host="127.0.0.1", password="03121991-oK", dbname="mydb")
    on.exit(DBI::dbDisconnect(conn))
    my_query <- 'SELECT count(*) FROM ALUMNOS_SEP WHERE AGNO = AÑO_SELECCIONADO AND RBD = RBD_SELECCIONADO'
    my_query <- sub("AÑO_SELECCIONADO",input$AGNO,my_query)
    my_query <- sub("RBD_SELECCIONADO",RBD_establecimiento_seleccionado,my_query)
    ubicacion <- dbGetQuery(conn,my_query)
  }
  
  
  
  
  ######################################    MAPA ############################################
  
  
  
  #  Se declara la salida mimapa almacenando un renderleaflet
  output$mimapa <- renderLeaflet({
    
    
    #Se inicia la conexion con la base de datos
    conn <- dbConnect(MySQL(), user="root", host="127.0.0.1", password="03121991-oK", dbname="mydb")
    
    #la consulta extrae contiene la longitud y latitud de la region seleccionada en el input$txtregion
    query_posicionregion <- 'SELECT LONGITUD, LATITUD FROM `REGIONES` WHERE `ID` = TXTREGION'
    query_posicionregion <- sub("TXTREGION",input$txtregion,query_posicionregion)
    #   pasteregion contiene la longitud y latitud de la region seleccionada en el input$txtregion
    posicionregion <- dbGetQuery(conn,query_posicionregion)
    
    
    
  
    #   Se cargan los establecimientos de la region seleccionada
    ubicacion_RBD <- as.character(ubicacion_establecimiento(input$txtregion)$RBD)
    
    
    
    #Se carga la funcion leaflet con el argumento
    leaflet() %>% addTiles()%>%setView(lng = posicionregion$LONGITUD, lat = posicionregion$LATITUD, zoom = 8)
    
    
  })
  observe({

    txtregion <-input$txtregion
    

    leafletProxy("mimapa", data = ubicacion_establecimiento(txtregion)) %>%
      clearShapes() %>%
      addCircleMarkers(lat =  ~LATITUD, lng =  ~LONGITUD, weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD
      )
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

  
  

  
  
  
  
  
  ################################# FIN MAPA #########################################################

})

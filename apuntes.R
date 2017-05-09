#la funcion ubicacion establecimiento recibe como parametro txtregion que ...
#contiene le numero de region seleccionado por el input regiones
ubicacion_establecimiento <- function(txtregion) {
  conn <- dbConnect(MySQL(), user="root", host="127.0.0.1", password="03121991-oK", dbname="mydb")
  
  #la consulta extra el RBD, Langitud y latitud de todos los establecimientos de la region 
  ubicacion <-  dbGetQuery(conn, "SELECT RBD, LONGITUD, LATITUD FROM ESTABLECIMIENTOS WHERE COD_REG_RBD =",txtregion,";")
  
}



#Envia informacion sobre los establecimientos a una tabla debajo del mapa
output$DatosEstablecimiento = renderDataTable({
  txtregion = input$txtregion
  ubicacion_establecimiento(txtregion)
})


########################## mapa################3


states <- readOGR("www/division_comunal/division_comunal.shp",
                  layer = "division_comunal", GDAL1_integer64_policy = TRUE)

neStates <- subset(states, states$COD_COMUNA %in% c(
  "9112"
))

leaflet(neStates) %>%
  addPolygons(color = "#444444", weight = 3, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~colorQuantile("YlOrRd", SHAPE_Area)(SHAPE_Area)
  )




observe({
  
  leafletProxy("mimapa", data = ubicacion_establecimiento(input$txtregion)) %>%
    clearShapes() %>%
    addCircleMarkers(lat = ~LATITUD, lng = ~LONGITUD, weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD)
})
observeEvent(input$mimapa_marker_click, { # update the map markers and view on map clicks
  p <- input$mimapa_marker_click
  proxy <- leafletProxy("mimapa")
  if(p$id=="Selected"){
    proxy %>% removeMarker(layerId="Selected")
  } else {
    proxy %>% setView(lng=p$lng, lat=p$lat, 15)
    output$IDDynamicInput = renderUI({
      numericInput("establecimiento_seleccionado", "RBD:", p$id  )
    })
    
  }
})

V1
observe({
  txtregion <-input$txtregion
  
  establecimientos <- ubicacion_establecimiento(input$txtregion)
  leafletProxy("mimapa", data = ubicacion_establecimiento(txtregion)) %>%
    
    addCircles(lat = ~LATITUD, lng = ~LONGITUD, weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD
    )
})

V2

observe({
  txtregion <-input$txtregion
  
  establecimientos <- ubicacion_establecimiento(input$txtregion)
  leafletProxy("mimapa", data = ubicacion_establecimiento(txtregion)) %>%
    
    addCircles(lat = 	"-72.700897", lng = "-37.805373", weight = 6, radius = 3,color="#ffa500", stroke = TRUE, fillOpacity = 0.8, layerId = ~RBD
    )
})




tags$p("Matricula: ",matricula_establecimiento(RBD_establecimiento_seleccionado)),
tags$p("Comuna: ",establecimiento_seleccionado(RBD_establecimiento_seleccionado)$NOM_COM_RBD),
tags$p("Cantidad de Alumnos SEP en la escuela: ",sep_establecimiento(RBD_establecimiento_seleccionado)),
tags$p("pi -> Proporcion SEP/ESC: ",matricula_establecimiento(RBD_establecimiento_seleccionado)),
tags$p("Indice Seg: ",matricula_establecimiento(RBD_establecimiento_seleccionado))


####################INTERFAZ DE USUARIO ·###########################3

,
tabItem("tabla",
        fluidRow(
          box(width = NULL,
              dataTableOutput('DatosEstablecimiento')
          )
          
        )
        
)







##############apunte
#S soluciono dando como typo foat a los cmapos longid y lattud


###############
interfaz 

box(width = NULL, status = "warning",
    uiOutput("routeSelect"),
    checkboxGroupInput("directions", "Show",
                       choices = c(
                         Northbound = 4,
                         Southbound = 1,
                         Eastbound = 2,
                         Westbound = 3
                       )
    )
)


tabItems(
  tabItem("mapa",
          
          leafletOutput("mimapa", width="100%",height="800px")
  ),
  tabItem("tabla",
          
          
          dataTableOutput('DatosEstablecimiento')
          
          
          
          
  )
  
)



library(magrittr)
library(htmltools)
library(shinydashboard)
library(shinycssloaders)


# set default sample site (Id=85006) and calcualte the min and max year
# yearStart <- join_camrose_pelcomb %>% dplyr::filter(ID==85006) %$% min(lubridate::year(`Date (MM/DD/YYYY)`))
# yearEnd <- join_camrose_pelcomb %>% dplyr::filter(ID==85006) %$% max(lubridate::year(`Date (MM/DD/YYYY)`))

yearStart <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% min(lubridate::year(Date))

yearEnd <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% max(lubridate::year(Date))

# UI component
siteLocationUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
       
         fluidRow(
                 column(6, 
                          wellPanel(
                                  style = "border-color: blue; height: 460px;",
                                     # p(tags$b("1. Select a sample point on the map"), 
                                     #      style = "font-size:16px; color:#4997d0"),
                                  shinycssloaders::withSpinner(leafletOutput(ns("sampleMap"),width = "100%",height = 420))
                                   # leafletOutput(ns("sampleMap"),width = "100%",height = 420)
                                    )
                       ), 
                 
                 column(6,
                        dataTableOutput(ns("table"))
                       ) 
                )
           )
    
        }

# Exploration Server
siteLocationServer <- function (input,output,session){
  
 
  
  # Render leaflet
  
  output$sampleMap <- renderLeaflet({
                          leaflet(data=metadata_Sonde_Wye) %>%
                          setView(lng = -3.06,lat = 52.06, zoom = 9) %>%
                        
                          # setMaxBounds(-5.42, 51.25, -2.08, 53.39) %>% 
                          addTiles() %>%
                          addProviderTiles("Esri.WorldTopoMap", group = "USGS USImagery") %>%
                          addProviderTiles("Esri.WorldImagery",group = "Esri WorldImagery") %>%
                          addProviderTiles("Esri.WorldShadedRelief",group = "Esri WorldShadedRelief") %>%
                          addProviderTiles(providers$Esri.WorldGrayCanvas,group =  "Esri WorldGrayCanvas") %>%
                     
                          addMarkers(~Longitude, ~Latitude,
                                     popup = ~as.character(paste(ID,Name)),
                                     layerId =~ID,
                                     # icon = pin_darkblue,
                                     label = ~as.character(paste(ID,Name)),
                                     labelOptions = labelOptions(textsize = "10px"),
                                    group = "Markers") %>% 
    
                         addPolygons(data =  simplifiedWalesBoundaries,
                                              color = "black",
                                              fillOpacity = 0,
                                              weight  = 1,
                                              popup = ~name,
                                              group = "County Boundary") %>%
                        addPolylines(data = simplifiedRiver,
                                               color = "blue",
                                               weight  = 1,
                                               popup =~RIV_NAME,
                                               group = "River"
                                                ) %>% 
                        addMiniMap(width = 150,
                                   height = 150,
                                   toggleDisplay = TRUE,
                                   autoToggleDisplay = TRUE,
                                   minimized = TRUE) %>% 

                        addLayersControl(
                               baseGroups = c("OpenStreetMap", "USGS USImagery","Esri WorldImagery",
                                              "Esri WorldShadedRelief","Esri WorldGrayCanvas"),
                               overlayGroups = c("Markers","County Boundary", "River")
                                          ) %>% 
      
                        hideGroup("County Boundary") %>%
                  
                        leafem::addMouseCoordinates() %>%
                  
                        inlmisc::AddHomeButton() %>%
                        inlmisc::AddSearchButton(group = "Markers", zoom = 15,
                                                    textPlaceholder = "Search sample sites...")
                     
                })
  
             
  # create a reactive value to store the clicked station
    selectedSample <- reactiveValues(ID = 55023)
    

   # store the click station id
      observeEvent(input$sampleMap_marker_click, {
         selectedSample$ID <-  input$sampleMap_marker_click$id
        }
      )
 
  # # Render table    
          output$table <- renderDataTable({
            # selSampleID <- selectedSample$ID
            metadata_Sonde_Wye %>%
              #
              dplyr::select(.,c(1:4,7)) %>%
              dplyr::mutate(flag=ifelse(ID==selectedSample$ID,"TRUE","FALSE")) %>%
              dplyr::arrange(desc(flag)) %>%
              dplyr::select(.,c(1:5)) %>% 
              # dplyr::select(.,c(1:4)) %>%
              # dplyr::mutate(ranking = rank(desc(`catchment-area`))) %>%

              datatable(extension = "Scroller",
                        options = list(deferRender=TRUE,
                                       scrollY=360,
                                       scrollX = TRUE,
                                       scroller=TRUE,
                                       autoWidth = FALSE,
                                       # scrollCollapse=TRUE,
                                       columnDefs = list(list(width = '100px', targets = c(0,2)))

                        )) %>%
              formatStyle('ID',target = 'row', 
                          backgroundColor = styleEqual(selectedSample$ID, 'lightskyblue'),
                          `font-size` = '13px')
          })

    }

shinyApp(siteLocationUI,siteLocationServer)


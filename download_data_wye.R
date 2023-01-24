
# library(magrittr)
# library(htmltools)
# library(shinydashboard)
library(lubridate)

# set default sample site (Id=85006) and calcualte the min and max year
yearStart <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% min(lubridate::year(Date))

yearEnd <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% max(lubridate::year(Date))

# UI component
dataDownloadUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
         
         tags$div(
           "The Wye Sondes data can be downloaded in 3 ways: 1) download all the data. 2) download 
            selected sites for all the parameters or 3) download selected site(s) and parameter(s)"
                 ),
         tags$br(),
     #     tags$head(
     #       tags$style(HTML("
     #       body {  background-color: white;}
     #       .item {
     #   background: #2196f3;
     #   color: red;
     # }
     # .selectize-dropdown-content .active {
     #   background: #2196f3 !important;
     #   color: white !important;
     # 
     #       #sampleSelection+ div>.selectize-input {
     #                        margin-top: -15px;
     #                        border: 1px solid;
     #                        height: 55px;
     #                      }
     #       #sampleSelection+ div>.selectize-dropdown {
     #                        width: auto !important;
     #                        color: blue;
     #                        background-color: coral;
     #                      } "
     #                    )
     #                  )
     #               ),
     #     
         fluidRow(
           
           column(width = 5,
                  wellPanel(style = "border-color: blue",
                    
                      div(style = "margin-top: -10px",
                            p(tags$b("Download all the Sondes data "),
                                 style = "font-size:15px; color:#4997d0;"
                               ),
                        
                            downloadButton(ns("downloadAllData"),
                                            "Click this box and wait for the downloading to complete"
                                           ,
                                             style= "color: #3498DB ;
                                                     background-color: #CCCCFF;
                                                     border: 1px solid;
                                                    "
                                           )
                              ),
                      tags$br(),
                        )
                  ),
           column(width = 7,
                  
                  # WellPanel for downloading selected sites
                  
                  wellPanel(style = "border-color: blue",
                            
                   div(style = "margin-top: -10px",
                         p(tags$b("Select one or multiple sites for downloading data "),
                                  style = "font-size:15px;
                                          color:#4997d0;"
                                        
                                )),
                   fluidRow(
                    column(width = 5,
                        div(selectizeInput(ns("sampleSelection"),
                                     label = "",
                                     choice = NULL,
                                     # choices =metadata_Sonde_Wye$Name,
                                     multiple = TRUE,
                                     selected = "Redbrook",
                                     options = list(placeholder = 'select sample site(s)')
                                    ),
                            style ="margin-top: -20px;"
                          )
                        ),
                      
                    column(width = 2,
                          downloadButton(ns("downloadSelectedSiteData"),
                                     "Click this box for downloading data",
                                     style= "color: #3498DB ; 
                                            background-color: #CCCCFF;
                                            border: 1px solid;
                                           "   
                                    )
                        )
                      )
                  )
                  )
           ),
           
               
         
   # WellPanel for downloading all the data    
    
    tags$hr(style="color:gray;background-color:gray; border-width:0; height:2px;"),

     p(tags$b("Select one or multiple sites and individual or multiple parameters for downloading data "), style = "font-size:18px;color:#4997d0;"
       ),

         fluidRow(
           column(width = 3,
                  p(tags$b("1. Choose sample site(s) from the list"), style = "font-size:14px;color:#4997d0",width = "100%"),
                  div(style = "margin-top: -20px"),
                  selectizeInput(ns("sampleSelection2"),
                                   label = "",
                                   choice = NULL,
                                   # choices =metadata_Sonde$Name,
                                   multiple = TRUE,
                                   selected = "Redbrook",
                                   options = list(placeholder = 'select sample site(s)'))
                ),

           column(width = 3,
                 p(tags$b("2. Choose element(s) from the list"), style = "font-size:14px;color:#4997d0",width = "100%"),
                 div(style = "margin-top: -20px"),
                 uiOutput(ns("paraSelection"))
                ),

           column(width = 6,
                 p(tags$b("3. Download data"), style = "font-size:14px;color:#4997d0",width = "100%"),
                 # div(style = "margin-top: -10px"),
                 div(style = "margin-top: 2px",
                 downloadButton(ns("downloadSelectedSiteParaData"),
                                "Click this box for downloading data",
                                style= "color: #3498DB ; background-color: #CCCCFF; border-color: black"
                               )
                 )
                )
            )
         )    # FluidPage
        }

# Exploration Server

dataDownloadServer <- function (input,output,session){

  # create a reactive value to store the clicked station
    selectedSample <- reactiveValues(ID = 55023)

  # update the SelectizeInput
    updateSelectizeInput(session, "sampleSelection", choices = metadata_Sonde_Wye$Name, server = TRUE)
    updateSelectizeInput(session, "sampleSelection2", choices = metadata_Sonde_Wye$Name, server = TRUE)
    
   # store the click station id
      # observeEvent(input$sampleMap_marker_click, {
      #    selectedSample$ID <-  input$sampleMap_marker_click$id
      # 
      #  # update slider input min, max and range value for year
      #    yearStart <- join_redbrook_glasbury %>%
      #    dplyr::filter(ID==selectedSample$ID) %$% min(lubridate::year(Date))
      # 
      #    yearEnd <- join_redbrook_glasbury %>%
      #    dplyr::filter(ID==selectedSample$ID) %$% max(lubridate::year(Date))
      # 
      #    yearValueRange <- c(yearStart,yearEnd)
      # 
      #    updateSliderInput(session, "yearRange",
      #                   min = yearStart,
      #                   max = yearEnd,
      #                   value = yearValueRange)
      #   })

  # filteredData <- reactive({
  #                 filterd <- join_redbrook_glasbury %>%
  #                   dplyr::filter(ID==selectedSample$ID)
  #                  })

  filteredDataDropBox <- reactive({

              filterd2 <- join_redbrook_glasbury %>%
              dplyr::filter(Name %in% input$sampleSelection)
         })

        filteredDataDropBox2 <- reactive({

          filterd3 <- join_redbrook_glasbury %>%
            dplyr::filter(Name %in% input$sampleSelection2 ) %>%
            dplyr::select("Name",!!!input$variable)
        })

        output$paraSelection <- renderUI({
            # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
          varSelectInput(session$ns("variable"),
                           label = "",
                           # data = join_camrose_pelcomb %>% dplyr::filter(ID==selectedSample$ID),
                           data = select_if(join_redbrook_glasbury,is.numeric) %>%
                                  select(-"ID"),
                           multiple = TRUE,
 #                          data = filteredDataDropBox(),
                           selected = "pH")

                  })
      
          # update the SelectizeInput

          updateSelectizeInput(session, inputId = "sampleSelection", choices = metadata_Sonde_Wye$Name, server = FALSE)

# Download all the data
          
          data <- join_redbrook_glasbury
          
          output$downloadAllData <- downloadHandler(
            
            filename = function() {
              paste("NRW Sondes Wye all data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(data,file)
              # write.csv(data, file)
            }
          )
          
# Download selected sites
          
        output$downloadSelectedSiteData <- downloadHandler(
            
            filename = function() {
              paste("NRW Sondes Wye selected site(s) data-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              write.csv(filteredDataDropBox(),file)
              # write.csv(data, file)
            }
          )
      
# Download selected sites and parameters
        
        output$downloadSelectedSiteParaData <- downloadHandler(
          
          filename = function() {
            paste("NRW Sondes Wye selected site and parameter data-", Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(filteredDataDropBox2(),file)
            # write.csv(data, file)
          }
        )     
        
        
        
       }
          
shinyApp(dataDownloadUI,dataDownloadServer)


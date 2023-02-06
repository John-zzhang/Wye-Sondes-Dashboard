
library(magrittr)
library(htmltools)
library(shinydashboard)
library(lubridate)

# set default sample site (Id=85006) and calcualte the min and max year
dateStart <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% min(Date)

dateEnd <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% max(Date)

# UI component
siteComparisonUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
       
         fluidRow(
           column(width = 3,
                  p(tags$b("1. Choose sample site(s) from the list"), style = "font-size:14px;color:#4997d0",width = "100%"),
                  div(style = "margin-top: -20px"),
                  selectizeInput(ns("sampleSelection"),
                                   label = "",
                                   # choice = NULL,
                                   choices =metadata_Sonde_Wye$Name,
                                   multiple = TRUE,
                                   selected = "Redbrook"
                                   # options = list(placeholder = 'Glasbury')
                                 # ,
                                   # options = list(placeholder = 'Click here for the drop-down menu')
                                )
                               ),
           
           column(width = 3,
                 p(tags$b("2. Choose an element from the list"), style = "font-size:14px;color:#4997d0",width = "100%"),
                 div(style = "margin-top: -20px"),
                 uiOutput(ns("paraSelection"))
                ),
               
           column(width = 6,
                 p(tags$b("3. Choose a start and an end date"), style = "font-size:14px;color:#4997d0",width = "100%"),
                 div(style = "margin-top: -10px"),
                 sliderInput(ns("dateRange"),
                            # label="3. Choose a start and end year:",
                            label = NULL,
                            min = dateStart,
                            max = dateEnd,
                            value=c(dateStart,dateEnd),
                            width = '600px',
                            animate = TRUE,
                            step = 1)
                            # ,
                            # sep = "" 
                            # )
                )
         ),
         
         plotlyOutput(ns("plot"))
      
         )
        }

# Exploration Server
siteComparisonServer <- function (input,output,session){
  
  # create a reactive value to store the clicked station
    selectedSample <- reactiveValues(ID = 55023)
    
  # update the SelectizeInput
    # updateSelectizeInput(session, "sampleSelection", choices = metadata_Sonde_Wye$Name, selected = "Redbrook", server = TRUE)

   # store the click station id
      # observeEvent(input$sampleMap_marker_click, {
      #    selectedSample$ID <-  input$sampleMap_marker_click$id
      # 
      #  # update slider input min, max and range value for year
      #    dateStart <- join_redbrook_glasbury %>%
      #    dplyr::filter(ID==selectedSample$ID) %$% min(Date)
      # 
      #    dateEnd <- join_redbrook_glasbury %>%
      #    dplyr::filter(ID==selectedSample$ID) %$% max(Date)
      # 
      #    yearValueRange <- c(dateStart,dateEnd)
      # 
      #    updateSliderInput(session, "dateRange",
      #                   min = dateStart,
      #                   max = daterEnd,
      #                   value = yearValueRange,
      #                   step = 1)
      #   })  
    
        filteredData <- reactive({
                        filterd <- join_redbrook_glasbury %>%
                          dplyr::filter(ID==selectedSample$ID)
                         })
        
        filteredDataDropBox <- reactive({
         req(input$sampleSelection)
                    filterd2 <- join_redbrook_glasbury %>%
                    dplyr::filter(Name %in% input$sampleSelection)
        })
        
        # filteredDataDropBox2 <- reactive({
        #   
        #   filterd3 <- join_redbrook_glasbury %>%
        #     dplyr::filter(Name %in% input$sampleSelection2)
        # })
     
        output$paraSelection <- renderUI({
            # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
          varSelectInput(session$ns("variable"),  
                           label = "",
                           # data = join_camrose_pelcomb %>% dplyr::filter(ID==selectedSample$ID),
                           data = select_if(join_redbrook_glasbury,is.numeric) %>% 
                                  select(-"ID"),
 #                          data = filteredDataDropBox(),
                           selected = "pH")
            
                  })
        
 #        output$paraSelection2 <- renderUI({
 #          # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
 #          varSelectInput(session$ns("variable2"),  
 #                         label = "",
 #                         # data = join_camrose_pelcomb %>% dplyr::filter(ID==selectedSample$ID),
 #                      
 #                         data = select_if(join_redbrook_glasbury,is.numeric) %>% 
 #                           select(-"ID"),
 #                         
 # #                      data = filteredDataDropBox2(),
 #                         multiple = TRUE,
 #                         selected = "pH")
 #                         # options = list(placeholder = 'select variable(s)')
 #                     
 #        })
       
   
          # update the SelectizeInput
          
          # updateSelectizeInput(session, inputId = "sampleSelection", choices = metadata_Sonde_Wye$Name, server = FALSE)
          # 
          
 # Render plotly depending of the selected parameter

        output$plot <- renderPlotly({
          
          # if(is.null(input$sampleSelection)){return()}

           req(input$sampleSelection,input$variable)

      
            sampleData <- filteredDataDropBox() %>% 
              # join_camrose_pelcomb %>%
              dplyr::filter(Date >=input$dateRange[1] & Date<=input$dateRange[2]) %>%
              group_by(Name) 
            
            plot_ly(data = sampleData, 
                    x = ~lubridate::ymd_hms(Datetime),
                    y = ~ sampleData[[as.name(input$variable)]], 
                    type = "scatter", 
                    mode = "lines",
                    line = list(width = 1.2),
                    color = ~Name ) %>% 
            
            # 
               layout(
            title= list(text = paste0("Time series plot of ",
                                            input$variable),
       
                               font = list(size = 15,
                                           face = "bold")
                               ),
            # 
                  xaxis = list(title = "Date-Time",
                               rangeslider = list(visible = T,thickness=0.1),
                               rangeselector=list(buttons=list(
                                                             list(step = "all"),
                                                             list(count=3, label="3months", step="month", stepmode="backward"),
                                                             list(count=2, label="2m", step="month", stepmode="backward"),
                                                             list(count=1, label="1m", step="month", stepmode="todate")
                                                          )
                                              ),
                                font = list(
                                            family = "Courier New, monospace",
                                            size = 12,
                                            color = "RebeccaPurple")
                                ),

                  yaxis = list(title = as.character(input$variable),
                                font = list(
                                            family = "Courier New, monospace",
                                            size = 12,
                                            color = "RebeccaPurple")
                                ),
                
               
                  plot_bgcolor='#e5ecf6', 
                  paper_bgcolor = "rgba(0, 0, 0, 0)",
                  fig_bgcolor   = "rgba(0, 0, 0, 0)"
                 )
        })
         
        
        output$plot2 <- renderPlotly({
          
          req(input$variable2,input$sampleSelection2)
          
          
          n <- length(input$variable2)
         
          plot_list = vector("list", n)
         
          sampleData2 <- filteredDataDropBox2() %>%
                       dplyr::filter(Date>=input$dateRange[1] & Date<=input$dateRange[2])

          for (i in 1:n)
          {
        
            plot_list[[i]] <- plotly_build(plot_ly(data = sampleData2, x = ~lubridate::ymd_hms(Datetime),
                 y = ~ sampleData2[[as.name(input$variable2[[i]])]], name = input$variable2[[i]], type = "scatter", mode = "lines",evaluate = TRUE) %>% 
             
                layout(
                
                   xaxis = list(title = "Date-Time",
                              font = list(
                                family = "Courier New, monospace",
                                size = 12,
                                color = "RebeccaPurple")
                 ),
                 
                   yaxis = list(title = as.character(input$variable2[[i]]),
                              font = list(
                                family = "Courier New, monospace",
                                size = 12,
                                color = "RebeccaPurple")
                 ),
                 
              
                 plot_bgcolor='#e5ecf6',
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)"
            ) 
            
            )     
 
          }
          
       
         subplot(plot_list,nrows= (as.integer(n/3)+1),shareX = TRUE,titleY = TRUE,margin = 0.05) %>% 
           layout(title = list(text = paste0("Sample site _ ", input$sampleSelection2),
                          font = list(
                            # family = "Courier New, monospace",
                                       size = 18,
                                       face = "bold",
                                       color = "RebeccaPurple")))
         
       
        })
    }

shinyApp(siteComparisonUI,siteComparisonServer)


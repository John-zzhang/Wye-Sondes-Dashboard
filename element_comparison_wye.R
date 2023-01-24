
library(magrittr)
library(htmltools)
library(shinydashboard)


# set default sample site (Id=85006) and calcualte the min and max date
dateStart <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% min(Date)
dateEnd <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% max(Date)

# UI component
elementComparisonUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
       
        fluidRow(
           
           column(width = 3,
                  p(tags$b("1. Choose a sample site from the list"), style = "font-size:14px;color:#4997d0;"),
                  div(style = "margin-top: -20px"),
                  # wellPanel(
                  #   style = "border-color: blue",
                    selectizeInput(ns("sampleSelection2"),label = "", 
                                   choice = NULL,
                                   # choices =metadata_Sonde$Name,
                                   # multiple = TRUE,
                                   selected = "Redbrook",
                                   options = list(placeholder = 'select sample site(s)'))
                 ),
           
           column(width = 3,
                  p(tags$b("2. Choose element(s) from the list"), style = "font-size:14px;color:#4997d0;"),
                  div(style = "margin-top: -20px"),
                  # wellPanel(
                  #   style = "border-color: blue",
                    # selectizeInput(ns("sampleSelection"),label = "1. Please select a sample point", choices =metadata_Sonde$df_Name,selected = "camrose_knock"),
                    uiOutput(ns("paraSelection2"))
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
                  )
           
           ),
       
         plotlyOutput(ns("plot2"))
        
           )
  
        }

# Exploration Server
elementComparisonServer <- function (input,output,session){
  
   # create a reactive value to store the clicked station
    selectedSample <- reactiveValues(ID = 55023)
    
  # update the SelectizeInput
    updateSelectizeInput(session, "sampleSelection", choices = metadata_Sonde_Wye$Name, server = TRUE)
    
    # update the SelectizeInput2
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
    
        filteredData <- reactive({
                        filterd <- join_redbrook_glasbury %>%
                          dplyr::filter(ID==selectedSample$ID)
                         })
        
        # filteredDataDropBox <- reactive({
        # 
        #             filterd2 <- join_redbrook_glasbury %>%
        #             dplyr::filter(Name %in% input$sampleSelection)
        # })
        
        filteredDataDropBox2 <- reactive({
          
          filterd3 <- join_redbrook_glasbury %>%
            dplyr::filter(Name %in% input$sampleSelection2)
        })
     
       output$paraSelection2 <- renderUI({
          # varSelectInput(session$ns("variable"), label = "2. Please select a variable", data = get(input$sampleSelection),selected = "pH")
          varSelectInput(session$ns("variable2"),  
                         label = "",
                         # data = join_redbrook_glasbury %>% dplyr::filter(ID==selectedSample$ID),
        
                         data = select_if(join_redbrook_glasbury,is.numeric) %>% 
                           select(-"ID"),
                     
#                        data = filteredDataDropBox2(),
                         multiple = TRUE,
                         selected = "pH")
                         # options = list(placeholder = 'select variable(s)')
                     
        })
       
        # update the SelectizeInput
          
          # updateSelectizeInput(session, inputId = "sampleSelection", choices = metadata_Sonde_Wye$Name, server = FALSE)
          # 
         output$plot2 <- renderPlotly({
          
          req(input$sampleSelection2,input$variable2)
          
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
                                size = 15,
                                color = "RebeccaPurple")
                 ),
                 
                   yaxis = list(title = as.character(input$variable2[[i]]),
                              font = list(
                                family = "Courier New, monospace",
                                size = 11,
                                color = "RebeccaPurple")
                 ),
                 
              
                 plot_bgcolor='#e5ecf6',
                 paper_bgcolor = "rgba(0, 0, 0, 0)",
                 fig_bgcolor   = "rgba(0, 0, 0, 0)"
            ) 
            
            )     
 
          }
          
       
         subplot(plot_list,nrows= (as.integer(n/3)+1),shareX = TRUE,titleY = TRUE,margin = 0.05) %>%
         # subplot(plot_list,nrows= n,shareX = TRUE,titleY = TRUE,margin = 0.05) %>% 
           layout(title = list(text = paste0("Sample site _ ", input$sampleSelection2),
                          font = list(
                            # family = "Courier New, monospace",
                                       size = 18,
                                       face = "bold",
                                       color = "RebeccaPurple")))
         
       
        })
    }

shinyApp(elementComparisonUI,elementComparisonServer)


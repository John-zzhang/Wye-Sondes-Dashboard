
library(magrittr)
library(htmltools)
library(shinydashboard)
library(RColorBrewer)


# set default sample site (Id=85006) and calcualte the min and max date
dateStart <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% min(Date)
dateEnd <- join_redbrook_glasbury %>% dplyr::filter(ID==55023) %$% max(Date)

colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

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
       
       basePlot <- reactive({
         plot_ly(join_redbrook_glasbury, type = 'scatter', mode = 'lines') %>%
           layout(title = list(text = paste0("Sample Site - ",input$sampleSelection2),
                               font = list(size = 14, face = "bold")),
                  xaxis = list(title = list(text = "Date-Time",
                                            font = list(size = 12)),
                               tickfont = list(size = 11),
                               rangeslider = list(visible = T,thickness=0.1),
                               rangeselector=list(buttons=list(
                                                           list(step = "all"),
                                                           list(count=3, label="3months", step="month", stepmode="backward"),
                                                           list(count=2, label="2m", step="month", stepmode="backward"),
                                                           list(count=1, label="1m", step="month", stepmode="todate")
                                                           )
                                                  ),
                               domain = c(0.10,0.85)), 
                  yaxis = list(title = list (text = as.character(input$variable2[[1]]), 
                                             font=list(size = 11,color = colors[1]),
                                             standoff=8),
                               color = colors[1],
                               tickfont = list(size = 10), color = colors[1]),
                
                  # showlegend = FALSE,
                  legend = list(font = list(size = 11))

                )
             })
         
         output$plot2 <- renderPlotly({
          
          req(input$sampleSelection2,input$variable2)
          
          # n <- length(input$variable2)
          # 
          # 
          # 
          # plot_list = vector("list", n)
          
          sampleData2 <- filteredDataDropBox2() %>%
                       dplyr::filter(Date>=input$dateRange[1] & Date<=input$dateRange[2])
# 
          p <- basePlot()
          
          # y2 <- list(
          #       overlaying = "y",
          #       side = "left",
          #       anchor="free",
          #       position=0.85,
          #       tickfont = list(color = "#ff7f0e"),
          #       titlefont = list(color = "#ff7f0e"),
          #       title = "test2"
          #          )
          # y3 <- list(
          #   overlaying = "y",
          #   side = "left",
          #   anchor="free",
          #   position=0.85,
          #   tickfont = list(color = "#d62728"),
          #   titlefont = list(color = "#d62728"),
          #   title = "test3"
          # )
          
          # p <- plot_ly(data = sampleData2, 
          #                   x = ~lubridate::ymd_hms(Datetime),
          #                   y = ~sampleData2[[as.name(input$variable2[[1]])]],
          #                   name = input$variable2[[1]],
          #                   mode = "lines",
          #                   type = "scatter",
          #                   line = list(width = 0.9)
          #                ) %>% 
          #       layout (
          #         title = list(text = paste0("Sample site _ ", input$sampleSelection2)),
          #         xaxis = list(title = 'Date-Time'),
          #         yaxis = list(title = as.character(input$variable2[[1]]))
          #       )
          # if (n>=2)
          # (
           for (i in seq_along(input$variable2))
           {
             variable <- input$variable2[[i]]  
             p <- p %>% add_trace(data = sampleData2, 
                                        x = ~lubridate::ymd_hms(Datetime),
                                        y = as.formula(paste0("~", "`",variable,"`")),
                                         # y = paste0("~", sampleData2[[as.name(variable)]]),
                                        name = variable,
                                        # y = ~sampleData2[[as.name(input$variable2[[i]])]],
                                        # # name = input$variable2[[i]],
                                        mode = "lines",
                                        type = "scatter",
                                        line = list(width = 0.9),
                                        # color = ~Name,
                                        # color = as.formula(paste0("~", "`",variable,"`")),
                                        # line = list(width = 0.9, color = colors[i]),
                                        yaxis = paste0("y",i)
                                       )
            
             side  <-  ifelse((i %% 2) == 0,"right","left")
            
             if (side == "right")
             {position <- 0.85 + (i - 1) * 0.03}
             else if (side == "left")
             { position <- 0.10- (i-1) * 0.025}
              
             layoutArgs <- list(p, list(title = list(text = as.character(variable),
                                                     font = list(size = 11),
                                                     automargin=T,
                                                     standoff=7), 
                                        side = side, 
                                        color = colors[i+1],
                                        overlaying = "y", 
                                        anchor = 'free',
                                        position = position,
                                        tickfont = list(size = 10), color = colors[i+1]))
             names(layoutArgs) <- c("p", paste0("yaxis", i))

             p<- do.call(layout, layoutArgs)

           }
          # )
           
           p <- p %>% config(scroolZoom = T, displaylogo = F) %>% 
             layout(colorway = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"))
           p
               
          
                                          
                                    
               # fig <- fig %>% 
               #   layout(
               #       yaxis2 = y2
               #       )
               # }   
         
          # fig <- fig %>% config(scrollZoom = TRUE, displaylogo = FALSE)
          # fig
          # )
          # fig
        })
    }

shinyApp(elementComparisonUI,elementComparisonServer)


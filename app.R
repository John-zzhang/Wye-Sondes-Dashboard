### SERVER + UI SCRIPT 
### for river flow data app
#~~~~~~~~~~~~~~~~~~~~~~~~~#

## written by - 
## date -   

# Load essential packages ####
# library(shiny)
# library(magrittr)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(shinyWidgets)
# library(shinyjs)
# library(dplyr)
# library(shinyWidgets)

library(leaflet)
library(plotly)
library(DT)
library(dplyr)
library(rlang)


#~~~~~~~~~~~~~~~~~~~~~~~~~#

## Load data ####
# source("../get_flow_data.R")
# source("C:/Users/Zhong.Zhang/Documents/Monitoring Analysis/Work/2021/Accelerator Programme/R code/Extract data from RNRFA_25-09-21.R")


## Load modules ####
source("./introduction_wye.R")
source("./site_location_wye.R")
source("./site_comparison_wye.R")
source("./element_comparison_wye.R")
source("./download_data_wye.R")

####~~~~~~~~~~ UI ~~~~~~~~~~~####

ui <- fluidPage(
  
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "leaflet_legend.css")
  ),

  theme = shinythemes::shinytheme("cerulean"),
  # shinythemes::themeSelector(),
  # theme = "bootstrap_Lumen.css",
 
  # tags$body(style = "background-color: #33475b"),
  tags$title("NRW Wye Sondes Water Quality Dashboard"),
  # shinyWidgets::useShinydashboardPlus(),
  shinyjs::useShinyjs(),
  ## customising app header
  shiny::fluidRow(
    shiny::column(
      width = 10,
      h4(shiny::strong("NRW Wye Sondes Water Quality Dashboard"))
    )
  ),
  
 
    # tags$style(HTML(".navbar-default {
    # background-color: #6381EC;)
    #                 }")),

  navbarPage(
   
    # title = tags$p("Flow data dashboard"),
    # 
    title = div(tags$a(img(src="NRW_Logo.png", height=40), href= "https://naturalresources.wales/?lang=en"),
                style = "position: relative; top: -10px;"),
            
    windowTitle = "NRW Wye Sondes Water Quality Dashboard", #title for browser tab
    
    # theme = bslib::bs_theme(bootswatch = "slate"),
    # icon = icon("home"),
    # tags$style(".navbar-default .navbar-brand {color:#99ccff;font-size: 22px;}"),
    # 

    # navbarMenu(""),
    tabPanel("Introduction",icon = icon("table"),
             introductionUI("introduction")),
    tabPanel("Site Location",
             icon = icon("home",lib = "glyphicon"),
             siteLocationUI("site_location")),
    tabPanel("Site Comparison", 
             icon = icon("table"),
             # tags$style(HTML("
             #   li a {
             #   font-size: 15px;
             #   background: blue ;
             #   margin: 5px;
             #   color: red ;
             #   }
             #   ")),
             siteComparisonUI("site_comparison")),
    tabPanel("Element Comparison", 
                      icon = icon("table"),
                      # tags$style(HTML("
                      #   li a {
                      #   font-size: 15px;
                      #   background: blue ;
                      #   margin: 5px;
                      #   color: red ;
                      #   }
                      #   ")),
             elementComparisonUI("element_comparison")
             ),
    tabPanel("Data Download",
             icon = icon("download"),
             # tags$style(HTML("
             #   li a {
             #   font-size: 15px;
             #   background: blue ;
             #   margin: 5px;
             #   color: red ;
             #   }
             #   ")),
             dataDownloadUI("data_download")
    )
    # tabPanel("Data Comparison", icon = icon("signal"),comparisonUI("comparison")),
    # # tabPanel("Flow Duration Curve", comparisonUI("durationCurve")),
    # tabPanel("Long Term Trend Analysis",icon = icon("chart-area"), analysisUI("analysis"))
    # 
    # tags$style("
    #     .navbar-default .navbar-brand {color:black;}
    #                 .navbar-default .navbar-brand:hover {color:white;}
    #                 .navbar { background-color:red;}
    #                 .navbar-default .navbar-nav > li > a {color:white;}
    #                 .navbar-default .navbar-nav > .active > a,
    #                 .navbar-default .navbar-nav > .active > a:focus,
    #                 .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:white;}
    #                 .navbar-default .navbar-nav > li > a:hover {color:white;background-color:red;text-decoration}
    #                 ")
    )
    
  
)

####~~~~~~~~~~ SERVER ~~~~~~~~~~~####

server <- function(input, output,session) {
 
  shiny::callModule(introductionServer,"introduction")
  shiny::callModule(siteLocationServer, "site_location")
  shiny::callModule(siteComparisonServer, "site_comparison")
  shiny::callModule(elementComparisonServer, "element_comparison")
  shiny::callModule(dataDownloadServer,"data_download")
  # shiny::callModule(analysisServer , "analysis")
  # shiny::callModule(comparisonServer , "comparison")
  # shiny::callModule(durationCurveServer , "durationCurve")
  # explorationServer("exploration")
  # longTermTrendsServer("longTermTrends")
  # Tab3Server("tab3")
  
}

app <- shinyApp(ui = ui, server = server)

library(magrittr)
library(htmltools)
library(shinydashboard)


# UI component
introductionUI <- function(id){
     ns <- NS(id)
    
     fluidPage(
         shinyWidgets::useShinydashboard(),
         div(style = "margin-top: -20px"),
         h3("River Wye Water Quality Monitoring"),
         tags$div(
           "The River Wye is a designated Special Area of Conservation (SAC).
               Natural Resources Wales (NRW) deployed multi-parameter Sondes at six sites to help
               fulfil its statutory duties. The calibrated automatic monitors (Sondes) record conditions in the river at each location every 15 minutes 
               for the following parameters:",
           tags$b("Temperature, conductivity, dissolved oxygen, pH, turbidity, total algae
              (including chlorophyll) and nitrate.")
         ),
         
         h3("The Sondes Water Quality Dashboard"),
         tags$div(
           "This dashboard has been developed for users to explore the data collected by the sondes.
               It consists of 5 tabs:",
           tags$br(),
           "1. This introduction",
           tags$br(),
           "2. Site Locations and their attributes.",
           tags$br(),
           "3. Site Comparison -  displaying time-series data of one parameter at one site
                  or multiple sites for comparison.",
           tags$br(),
           "4. Element Comparison - displaying time-series data of multiple-parameters 
                 at one site for comparison.",
           tags$br(),
           "5. Data download - downloading either all NRW Sondes data or users's selected sites."
         ),
         tags$br(),
         tags$div(
           "This dashboard is still in its development phase. We publish the dashboard
             because the information might be useful to others involved in catchment management.
             We would welcome ",
           a(href="mailto:reporting@cyfoethnaturiolcymru.gov.uk","feedback"),
           " on its use.",
         ),
         tags$br(),
         tags$div(
           "Every effort is made to ensure the accuracy of the information provided, including data cleansing
            to remove obviously erroneous data. The data is presented as recorded by the sondes
            and is only an indication of local conditions. Neither Natural Resources Wales, nor its employees or agents can be held 
            responsible for any inaccuracies or omissions, whether caused by negligence 
            or otherwise."
         ),
         
         h3("Terms and Conditions of Use"),
         
         tags$div(
           "This data is provided under the terms of ",
           tags$a("Open Government Licence (OGL)", href= "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
           ".",
           "Please also include the attribution statement:",
           HTML("&quot"),
           
           "Contains Natural Resources Wales information",
           HTML("&#169"),
           "Natural Resources Wales and database right. All rights reserved.",
           HTML("&quot")
         ),
        
         tags$hr(style="border-color: purple; height:15px;"),
         
# Welsh version of the introduction text
      div(style = "margin-top: -20px"),
      h5(style="font-weight: bold","Cymraeg"),
      h3("Monitro Ansawdd ",HTML("&#68&#373&#114")," Afon Gwy"),
     
      tags$div(
               "Mae Afon Gwy yn Ardal Cadwraeth Arbennig (ACA) ddynodedig.
               Mae Cyfoeth Naturiol Cymru (CNC) wedi gosod sondiau aml-baramedr 
               mewn chwe safle  i helpu Cyfoeth Naturiol Cymru i gyflawni
               ei ddyletswyddau statudol. Mae'r sondiau'n cofnodi amodau yn yr afon ym mhob 
               lleoliad bob 15 munud ar gyfer y paramedrau canlynol: ",
         tags$b("Tymheredd, dargludedd, ocsigen tawdd, pH, tyrfedd, cyfanswm yr",
                HTML("&#97&#108&#103&#226&#117"),"(gan gynnwys cloroffyl), nitrad.")
          ),
    h3("Dangosfwrdd Ansawdd",HTML("&#68&#373&#114"),"y Sondiau"),
    tags$div(
        "TDatblygwyd y dangosfwrdd fel y gall defnyddwyr edrych ar y data a 
         gesglir gan y sondiau. Mae ganddo pum tab:",
   tags$br(),
       "1. Y cyflwyniad hwn",
   tags$br(),
       "2. Lleoliadau'r Safle a'r nodweddion.", 
   tags$br(),
       "3. Cymharu Safleoedd - sy'n dangos data cyfres amser o un paramedr 
           yn un safle neu sawl safle at ddiben cymharu.",
   tags$br(),
       "4. Cymharu Elfennau - sy'n dangos data cyfres amser o sawl paramedr 
        yn un safle at ddiben cymharu.",
        
   tags$br(),
       "5. Lawrlwytho data - lawrlwytho naill ai holl ddata Sondes CNC neu
           safleoedd dethol defnyddwyr."
       ),
   tags$br(),
   tags$div(
        "Mae'r dangosfwrdd hwn yn dal yn ei gyfnod datblygu. Caiff y data ei gyhoeddi
              oherwydd gallai'r wybodaeth fod yn ddefnyddiol i eraill sy'n ymwneud", HTML("&#226"),
              "rheoli dalgylchoedd a a byddem yn croesawu",
              a(href="mailto:reporting@cyfoethnaturiolcymru.gov.uk","adborth"), "sut i'w ddefnyddio."
    ),
tags$br(),
tags$div(
  "Gwneir pob ymdrech i sicrhau cywirdeb y wybodaeth a ddarperir, gan gynnwys
    craffu ar y data i gael gwared ar ddata sy'n amlwg yn wallus. Cyflwynir y data fel y'i
   cofnodwyd gan y sondiau ac nid yw ond yn arwydd o'r amodau lleol. Ni ellir dal Cyfoeth Naturiol Cymru,
   na'i weithwyr na'i asiantiaid yn gyfrifol am unrhyw anghywirdebau neu fylchau, 
   p'un ydynt yn ganlyniad i esgeulustod neu reswm arall."
),

h3("Telerau ac Amodau Defnyddio"),

tags$div(
  "Mae'r data hwn yn cael ei ddarparu o dan delerau'r",
  tags$a("Drwydded Llywodraeth Agored (OGL)", href= "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
  ".",
  "Dylech hefyd gynnwys y datganiad priodoli hwn:",
  HTML("&quot"),
  
  "Yn cynnwys gwybodaeth Cyfoeth Naturiol Cymru",
  HTML("&#169"),
  "Cyfoeth Naturiol Cymru a hawl cronfa ddata. Cedwir pob hawl.",
  HTML("&quot"),
  hr()
   ), 
    )
    }

# Exploration Server
introductionServer <- function (input,output,session){
 
    } 

shinyApp(introductionUI,introductionServer)


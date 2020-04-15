library(plotly)
library(ggplot2)
library(shiny)
library(shinydashboard)





CoronaDF <-  as.data.frame(
  read.csv("covid_19_clean_complete.csv")
)



## ------------------------------------------------------------------------------------
##Fix Country Names
CoronaDF$Country.Region <- as.character(CoronaDF$Country.Region)
CoronaDF$Country.Region[CoronaDF$Country.Region=="Korea, South"] <- "South Korea"
CoronaDF$Country.Region <- factor(CoronaDF$Country.Region)
Countries <- unique(CoronaDF$Country.Region)





header <- dashboardHeader(title= "Covid 19")

sidebar <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Dashboard",icon = icon("dashboard")),
    menuItem("LinkedIn",icon  = icon("fab fa-linkedin"),
             href = "https://www.linkedin.com/in/nimataghidoost")
    
  )  
  
)

frow1 <-fluidRow(
  
  
  
  valueBoxOutput("ConfirmedBox"),
  
  valueBoxOutput("DeathBox"),
  
  valueBoxOutput("RecoveredBox")
)
frow2 <- fluidRow(
  
  box(
    #Put the box and the label aside.
    tags$head(
      tags$style(type="text/css", "label{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
      
    ),
    
    title = "Top Countries Involved With Corona"
    ,solidHeader = TRUE,
    collapsible = FALSE,
    status = "danger",
    height = "500px",
    #Put the Inputs aside.
    div(style="display:flex;align-items:center",
        bootstrapPage( 
    ##Changed the boxes to be centered but I dont know how :)
          tags$head(
            tags$style(type="text/css", 
                       "label.control-label, .selectize-control.single { 
         display: table-cell; 
         text-align: center; 
         vertical-align: middle; 
      } 

      .form-group { 
        display: table-row;
      }
      .selectize-control.single div.item {
        padding-right: 15px;
      }")
          ),
          div(style="display:inline-block",numericInput("InpTopN",strong("Number of Countries:"),
                                                        10,min=1,max =20,step=1 ))
          ,
   
 
##Put Space between the inputs

          div(style = "margin-right:25px"),
          
          div(style="display:inline-block",
              selectInput("InputCase","Case Kind:",
                          c("Confirmed","Deaths","Recovered")
              )),
          
          div(style = "margin-right:25px"),
          div(style="display:inline-block",checkboxInput(
            "CheckChinaTop10",strong("Include US's Data?")))
          
        ))
    ,
    plotlyOutput("Top10",height = "400px")
    
  )
  
  ,
  
  
  
  box(
    title = "Corona On Earth Excluding US!"
    ,solidHeader = TRUE,
    collapsible = FALSE,
    status = "danger",
    height = "500px",
    htmlOutput("GeoChart",height= "200px")
  )
  
)

frow3 <- fluidRow(
  
  box(
    title = "Corona Timeline"
    ,solidHeader = TRUE,
    collapsible = FALSE,
    width = 12,
    height = "400px",
    status = "danger",
    div(style="display:flex;align-items:center",
        bootstrapPage( 
          div(style="display:inline-block",
              selectInput("InpCountry","Country:",choices = sort(Countries),selected = "US")),
          div(style = "margin-right:25px"),
          div(style="display:inline-block",
              selectInput("InputCase2","Case Kind:",
                          c("Confirmed","Deaths","Recovered")))
        )),
    
    plotlyOutput("Timeline",height= "200px")
  )
)

body <- dashboardBody(frow1,frow2,frow3)

# Define UI for application that draws a histogram
shinyUI(
  
  
  ui <- dashboardPage(header = header,sidebar = sidebar,body = body,skin = "red")
  
)

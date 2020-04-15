library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
require(shinydashboard)
library(googleVis)
library(plotly)



## ------------------------------------------------------------------------------------
shinyServer(function(input, output) {
  

 CoronaDF <-  as.data.frame(
                        read.csv("covid_19_clean_complete.csv")
                                    )
                    


## ------------------------------------------------------------------------------------
##Fix Country Names
CoronaDF$Country.Region <- as.character(CoronaDF$Country.Region)
CoronaDF$Country.Region[CoronaDF$Country.Region=="Korea, South"] <- "South Korea"
CoronaDF$Country.Region <- factor(CoronaDF$Country.Region)
Countries <- unique(CoronaDF$Country.Region)
## ------------------------------------------------------------------------------------
CoronaDF$Date <- as.Date(CoronaDF$Date,format="%m/%d/%y")
CoronaDF$DateNumeric <- as.numeric(CoronaDF$Date)
CoronaDF$PrevDateNumeric <- CoronaDF$DateNumeric-1


## ------------------------------------------------------------------------------------

MergedDF <- merge(CoronaDF,CoronaDF,
                  by.x =  c("Province.State","Country.Region","DateNumeric"),
                  by.y =  c("Province.State","Country.Region","PrevDateNumeric"))

CleanCoronaDF <- MergedDF %>% mutate(Confirmed=Confirmed.y-Confirmed.x,
                                     Deaths=Deaths.y-Deaths.x,
                                     Recovered=Recovered.y-Recovered.x)

CleanCoronaDF <- CleanCoronaDF %>% select(                 Province.State,Country.Region,Lat.y,Long.y,
                                                           Date.y,DateNumeric.y,Confirmed,Deaths,
                                                           Recovered,Confirmed.y,Deaths.y,Recovered.y)

names(CleanCoronaDF) <- c("Province.State","Country.Region","Lat","Long",
                          "Date","DateNumeric","Confirmed","Deaths",
                          "Recovered","CumConfirmed","CumDeaths","CumRecovered")

## ------------------------------------------------------------------------------------
CoronaGroupByCountry <- CleanCoronaDF %>% group_by(Country.Region) %>% summarise(sum(Confirmed),sum(Deaths),sum(Recovered),max(Date))
names(CoronaGroupByCountry) <- c("Country","Confirmed","Deaths","Recovered","Date")


CoronaGroupByCountryExcChina <- CoronaGroupByCountry[CoronaGroupByCountry$Country!="US",]
## ------------------------------------------------------------------------------------

CoronaIran <- CoronaGroupByCountry[CoronaGroupByCountry$Country=="US",]




    output$Top10 <- renderPlotly({
        
        InpTopN <- input$InpTopN
        InpCheckChinaTop10 <- input$CheckChinaTop10
        InpCase <- input$InputCase
        
if(InpCheckChinaTop10==1)
    {
       Top10Confirmed <- arrange(top_n(CoronaGroupByCountry,InpTopN,
                                      get(InpCase)),
                                         desc(  get(InpCase)))
       
    }  
else{   
       Top10Confirmed <- arrange(top_n(CoronaGroupByCountryExcChina,InpTopN,
                                       get(InpCase)),
                                        desc(  get(InpCase)))
       

       
    }
    
        ## ------------------------------------------------------------------------------------
ggplot(Top10Confirmed, aes(x= get(InpCase), y=reorder(Country,-get(InpCase)))) + 
         geom_segment(aes(x=0, 
                    xend= get(InpCase), 
                    y=reorder(Country,-get(InpCase)), 
                    yend=Country)) + 
          geom_point( size=3, color="orange") +
            labs(title=paste(InpCase,"Cases") )+ 
            xlab(InpCase)+ ylab("Country")+
            theme(axis.text.x = element_text(angle=65, vjust=0.6))+
            theme_bw() 
            #geom_text(aes(label= get(InpCase)),hjust=8, vjust=8)
    })
        ## ------------------------------------------------------------------------------------
      
    output$Timeline <- renderPlotly({
        
    InpCountry <- input$InpCountry  
    InputCase2 <- input$InputCase2
    
CoronaGroupByDate <- CleanCoronaDF[CleanCoronaDF$Country.Region==InpCountry,]%>%
                        group_by(Date,DateNumeric) %>% 
                    summarise(sum(Confirmed),sum(Deaths),sum(Recovered))



names(CoronaGroupByDate) <- c("Date","DateNumeric","Confirmed","Deaths","Recovered")
MinDateWhichHasData <- min(CoronaGroupByDate[CoronaGroupByDate$Confirmed>0,]$Date)
CoronaGroupByDate <- CoronaGroupByDate[CoronaGroupByDate$Date>=MinDateWhichHasData,]
## ------------------------------------------------------------------------------------
ggplot(CoronaGroupByDate,aes(x=Date,y=get(InputCase2)))+
   geom_line(color="black",size=1)   +
   geom_point(color="orange",size=3)+
    ylab(InputCase2)+
   #geom_text(aes(label= get(InputCase2)),vjust=1)+
    theme_bw() 
    })     
    ## ------------------------------------------------------------------------------------
    output$GeoChart <- renderGvis({ 
        
       gvisGeoChart(CoronaGroupByCountryExcChina,
                          locationvar = "Country",colorvar = "Confirmed"
                          ,options = list(width=500,height=400))
    
        

    })
    
    output$ConfirmedBox <- renderValueBox({
      valueBox(
        CoronaIran$Confirmed ,paste("Total Confirmed Cases In US Until",CoronaIran$Date), icon = icon("check-circle"),
        color = "blue"
      )
    })
    
    output$DeathBox <- renderValueBox({
      valueBox(
        CoronaIran$Deaths , paste("Total Death In US Until",CoronaIran$Date), icon = icon("bolt"),
        color = "purple"
      )
    })
    
    output$RecoveredBox <- renderValueBox({
      valueBox(
        CoronaIran$Recovered, paste("Total Recovered People In US Until",CoronaIran$Date), icon = icon("notes-medical"),
        color = "green"
      )
    })



})

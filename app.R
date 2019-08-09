# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(devtools)
#library(dashboardthemes)
#source_url("https://raw.githubusercontent.com/DrMattG/ShinyNINA/master/Shinytheme_NINA.R")
###########################
###########################
options(encoding="UTF-8")
library(httr)
library(rjstat)
##Get data from the stats agency
# url for POST
url <- "https://data.ssb.no/api/v0/en/table/03984/"
# Query, copied from API Console
# Run by highlighting all of this function and ctrl enter/r
data <- '{  "query": [    {      "code": "Region",      "selection": {        "filter": "agg_single:Fylker1972",        "values": [          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08",          "09",          "10",          "11",          "12",          "14",          "15",          "50",          "16",          "17",          "18",          "19",          "20",          "21"        ]      }    },    {      "code": "Aarsak2",      "selection": {        "filter": "item",        "values": [          "00",          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08"]}    },    {      "code": "Rovdyr",      "selection": {        "filter": "item",        "values": [          "1",          "2",          "3",          "4"]}} ],"response": {"format": "json-stat2"}}'
# post query
d.tmp <- POST(url , body = data, encode = "json", verbose())
# Get content from d.tmp as text, using fromJSONstat
dattable <- fromJSONstat(content(d.tmp, "text"))
head(dattable)
#data manipulation
dattable<-dattable %>% 
  mutate(region=case_when
         (region %in%  c("Trøndelag","Sør-Trøndelag (-2017)" , "Nord-Trøndelag (-2017)") ~ "Trøndelag",
           region %in%  c("Østfold")~"Østfold",
           region %in%  c("Akershus")~"Akershus",
           region %in%  c("Oslo")~"Oslo",
           region %in%  c("Hedmark")~"Hedmark",
           region %in%  c("Oppland")~"Oppland",
           region %in%  c("Rogaland")~"Rogaland",
           region %in%  c("Buskerud")~"Buskerud",
           region %in%  c("Vestfold") ~"Vestfold",
           region %in%  c("Telemark")~"Telemark",
           region %in%  c("Aust-Agder")~"Aust-Agder",
           region %in%  c("Vest-Agder")~"Vest-Agder",
           region %in%  c("Hordaland")~"Hordaland",
           region %in%  c("Sogn og Fjordane")~"Sogn og Fjordane",
           region %in%  c("Møre og Romsdal"  )~"Møre og Romsdal"  ,
           region %in%  c("Finnmark - Finnmárku")~"Finnmark - Finnmárku",
           region %in%  c("Troms - Romsa")~"Troms - Romsa",
           region %in%  c("Nordland")~"Nordland",
           region %in%  c("Unknown hunting county")~"Unknown hunting county"))


#Build the shiny App

title <- tags$a(href='https://www.nina.no',
                    tags$img(src='https://github.com/DrMattG/ShinyNINA/blob/master/NINA.png', height=50, width=50),
                    'Registered mortality of large carnivores, Norway', target="_blank")
#UI
ui <- dashboardPage(
    dashboardHeader(
      title = title, titleWidth=600),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "Large Mammal Dashboard", icon = icon("dashboard")),
        menuItem("Visit-us", icon = icon("send",lib='glyphicon'),
                               href = "https://www.nina.no")
        )
      ),
    dashboardBody(#theme_nina,
      tags$head(
        tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
      tabsetPanel(
        id = "tabs",
        tabPanel(
          title = "All species",
          value = "page1",
          fluidRow(box(title= "Select inputs"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,width = 6
          , selectInput("reason", "Choose the reported reason:",
          c("Total"= "Total",
          "Noxious" = "Animals felled as noxious",
          "Self-defence" = "Animals felled as self-defence",
          "Illegal" = "Animals felled illegally",
          "Vehicle" = "Animals killed by motor car",
          "Train" = "Animals killed by train",
          "Other" = "Animals killed by other causes",
          "Hunting (not Lynx)" = "Animals felled under licence hunting",
          "Quota (Lynx hunting)" = "Animals felled under quota hunting"
          )),
          selectInput("species", "Choose the focal species",
          c("Bear"="Bear", "Lynx"="Lynx", "Wolf"="Wolf",
          "Wolverine"="Wolverine")),
          selectInput("region", "Choose the region"
                       ,choices=c(
                         "Østfold" = "Østfold",
                         "Akershus"= "Akershus",
                         "Oslo"="Oslo",
                         "Hedmark"="Hedmark",
                         "Oppland"="Oppland",
                         "Buskerud"="Buskerud",
                         "Vestfold"="Vestfold",
                         "Telemark"="Telemark" ,
                         "Aust-Agder"="Aust-Agder",
                         "Vest-Agder"="Vest-Agder",
                         "Rogaland"="Rogaland",
                         "Hordaland"="Hordaland",
                         "Sogn og Fjordane"="Sogn og Fjordane",
                         "Møre og Romsdal"="Møre og Romsdal",
                         "Trøndelag"="Trøndelag",
                         "Nordland"="Nordland",
                         "Troms - Romsa"="Troms - Romsa",
                         "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                         "Unknown hunting county"="Unknown hunting county")
                       ,multiple = FALSE) 
          ),
          box(title = "Data description",
              p("This Shiny App uses an API link to Statistics Norway to display known mortalities of large carnivorous mammals in Norway by year.
                The reported reason is one of 'Total' (all reported mortalities); 'Noxious' (killed as a disease risk), 'Self-defence' (animal killed in self defence),
                'Illegal' (confirmed poaching),  'Vehicle' (road traffic accident), 'Train' (train collision),  'Other' (other unrecorded causes),
                'Hunting (not Lynx)' (licensed hunting), 'Quota' (Licensed lynx hunting). 
                
                There are data on four large carnivores; Bear, Lynx, Wolf and Wolverine. The regional data for Trøndelag, Sør-Trøndelag (pre 2017) and Nord-Trøndelag (pre 2017) has been merged in to one region 'Trøndelag' "))
          )
          ),
          fluidRow(
          box(title = "Recorded causes of mortality"
          ,status = "primary"
          ,solidHeader = TRUE
          ,collapsible = TRUE
          ,plotOutput("mortality", height = "600px")
          ),
          box(title = "Region"
              ,status = "primary"
              ,solidHeader = TRUE
              ,collapsible = TRUE
              ,plotOutput("region", height = "600px"))
                )
        )
    )
)

# create the server functions for the dashboard
server <- function(input, output) {
#some data manipulation to derive the values of boxes (take last year in the dataset)
#Make a reactive dataframe
data<-reactive({
dattable %>%
filter(reason==input$reason) %>%
filter(`type of carnivore`==input$species)
})
Rdata<-reactive({
  dattable %>%
    filter(region==input$region) %>% 
    filter(reason==input$reason) %>%
    filter(`type of carnivore`==input$species)
})
#dattable<-dattable %>%
# tidyr::separate(`interval (year)`,"-")
#creating the valueBoxOutput content
 #creating the plotOutput content
 output$mortality <- renderPlot({
 ggplot(data = data(),
 aes(x=`interval (year)`, y=value, fill=`type of carnivore`) )+
 geom_bar(position = "dodge", stat = "identity") +
 labs(x="Year", y="Number of recorded mortalities")+
 theme_classic()+
 theme(axis.text.x = element_text(angle = 90, hjust = 1))+
     ggtitle(as.character(input$reason))+
     theme(plot.title = element_text(hjust = 0.5))
 })
 output$region <- renderPlot({
   ggplot(data = Rdata(),
          aes(x=`interval (year)`, y=value, fill=`type of carnivore`) )+
     geom_bar(position = "dodge", stat = "identity") +
     labs(x="Year", y="Number of recorded mortalities")+
     theme_classic()+
     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
     ggtitle(as.character(input$region))+
     theme(plot.title = element_text(hjust = 0.5))
 })
 }
#Launch the App
shinyApp(ui, server)

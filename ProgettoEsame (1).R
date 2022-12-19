library(shiny)
library(shinydashboard)
library(plotly)
library(shinybusy)
library(lubridate)

source(paste(getwd(),"CodingR_manuele_davide.R",sep="/"))
source(paste(getwd(),"CodingR_demian_simone.R",sep="/"))
addResourcePath('img',getwd()) 

options(shiny.maxRequestSize = 30*1024^2)
options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)



caricamentoFile2<-function(in_tipo){
 
  dataTableOutput(paste("contents",in_tipo,sep=""))

}

grafici<-function(in_Tipo){
  retGrafici<- sidebarLayout(
                              sidebarPanel(
                                            outputId=paste("Pannello",in_Tipo,sep=""),
                                            uiOutput(paste("ascissa",in_Tipo,sep=""),style = "overflow-y:scroll; max-height: 250px; position:relative; align: centre"),
                                            uiOutput(paste("ordinata",in_Tipo,sep=""),style = "overflow-y:scroll; max-height: 250px; position:relative; align: centre"),
                                            actionButton(paste("do",in_Tipo,sep=""), "Click Me"),
                                            options = list("height=50px")
                              
                              ),
                              mainPanel(plotlyOutput(outputId = paste("p",in_Tipo,sep="")))
                            )
    
 

 return(retGrafici)
}

graficiAnimati<-function(in_tipo){
  if (in_tipo == "Province") {
    retGrafici<- fluidPage(uiOutput("listaRegioni"),
                           plotlyOutput("graficoAnimatoProvince",height="600px"))    
  } else if (in_tipo == "Regioni") {
    retGrafici<- fluidPage(plotlyOutput("graficoAnimatoRegioni",height="600px")) 
  } else if (in_tipo == "Nazionale") {
    retGrafici<- fluidPage(plotlyOutput("graficoAnimatoNazionale",height="600px")) 
  }

  
  return (retGrafici) 
}

TabProvince<-function(){
  
 retTab<- fluidPage(
   tabBox(
     id="TabProvince",
     width = "100%",
     tabPanel("Province" ,caricamentoFile2("Province")),
     tabPanel("Grafici",grafici("Province")),
     tabPanel("Grafici Animati",graficiAnimati("Province"))
     
   )
  
  )
 return (retTab)
}

TabRegioni<-function(){
  
  retTab<- fluidPage(
    add_busy_bar(color = "#FF0000"),
    
    tabBox( id="TabRegioni",
      width = "65%",
      tabPanel("Caricamento Regioni" ,caricamentoFile2("Regioni")),
      tabPanel("Grafici Regioni",grafici("Regioni")) ,
      tabPanel("Grafici Animati",graficiAnimati("Regioni"))
      
    )
    
    
    
    # Sidebar layout with input and output definitions ----
    
  )
  return (retTab)
}

TabNazionale<-function(){
  
  retTab<- fluidPage(
    tabBox(id="TabNazionale",
      width = "65%",
      tabPanel("Visualizzazione" ,caricamentoFile2("Nazionale")),
      tabPanel("Grafici",grafici("Nazionale")),
      tabPanel("Grafici Animati",graficiAnimati("Nazionale"))
      
    )
    
    
    
    # Sidebar layout with input and output definitions ----
    
  )
  return (retTab)
}


TabGrafico<-function(){
  
  retTab<- fluidPage(
    tabBox(id="TabGrafico",
           width = "65%",
           tabPanel("Visualizzazione" ,( 
                    fluidPage(selectInput(
                      inputId="selezioneRegioniMappe",
                      label="Regioni",
                      choices=unique(  c("ITALIA",
                                         "Lazio",
                                         "Umbria",
                                         "Marche",
                                         "Abruzzo",
                                         "Campania",
                                         "Molise",
                                         "Basilicata",
                                         "Puglia",
                                         "Calabria",
                                         "Sicilia",
                                         "Sardegna",
                                         "Marche",
                                         "Toscana",
                                         "Liguria",
                                         "Piemonte",
                                         "Lombardia",
                                         "Veneto",
                                         "Friuli Venezia Giulia",
                                         "P.A. Trento",
                                         "P.A. Bolzano",
                                         "Valle d'Aosta",
                                         "Emilia-Romagna" )
                      )),
                              plotlyOutput("graficoMappa")
                            )    
                          )
                    )
            )
    
 
  
  # Sidebar layout with input and output definitions ----
  
  )
return (retTab)
}


ui <- dashboardPage(

  
 
  dashboardHeader(title = "Dati Covid ",uiOutput("new")),
       
  dashboardSidebar(    
    sidebarMenu(
      menuItem("Province", tabName = "Province", icon = icon("th")),
      menuItem("Regioni", tabName = "Regioni", icon = icon("th")) ,
      menuItem("Nazionale", tabName = "Nazionale", icon = icon("th")),
      menuItem("Grafico Mappa", tabName = "GraficoMappa", icon = icon("th"))
    )
  ),
  body <- dashboardBody(
    busy_start_up(
      loader = tags$img(
        src = "/img/jesus-christ-homer.gif",
        width = 500
      ),
      text = "Loading...",
      mode = "auto"
    ),

 
    
    tabItems(
      tabItem(tabName = "Province",TabProvince()),
      tabItem(tabName = "Regioni",TabRegioni()) ,
      tabItem(tabName = "Nazionale",TabNazionale()),
      tabItem(tabName = "GraficoMappa",TabGrafico())
    )
  )
 
)

caricaFile<-function(in_df){
  tryCatch(
    {
      df <- read.csv(in_df$file1$datapath,
                     header = in_df$header,
                     sep = in_df$sep,
                     quote = in_df$quote)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  return(df)
}

caricaFile2<-function(in_tipo){
  if (in_tipo == "Province"){
    path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"
  } else if (in_tipo == "Regioni"){
    path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"
    
  } else {
    path = "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
  }
  
  tryCatch(
    { df <- read.csv(path,
                     header = TRUE,
                     sep = ",",
                     quote = "\"")
    
     df$data = substr(df$data, 1, 10)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  
  return(df)
}

 

server <- function(session,input, output) {
  set.seed(122)
  
  dfProvince <- reactiveValues()
  dfRegioni <- reactiveValues()
  dfNazionale <- reactiveValues()
  
  
  dfProvince$dati <- caricaFile2("Province")
  dfRegioni$dati <- caricaFile2("Regioni")
  dfNazionale$dati <- caricaFile2("Nazionale")
  
  output$messageMenu <- renderMenu({
  dropdownMenu(taskItem(value = 80, color = "red",
                        "Write documentation"
  )
  )})

  
  output$contentsProvince <- renderDataTable({

    

    
    output$graficoAnimatoProvince<-renderPlotly(graficoAnimatoProvince(dfProvince$dati,"Abruzzo"))
    
    return(dfProvince$dati)

   
  },options = list( pageLength = 5,scrollX = TRUE,scrolly=TRUE))
  

  output$contentsRegioni <- renderDataTable({
    
    
  
    

    output$graficoAnimatoRegioni<-renderPlotly(graficoAnimatoRegioni(dfRegioni$dati))
    output$graficoMappa<-renderPlotly(graficiMappaRegioni(dfRegioni$dati)) 
    
    return(dfRegioni$dati)
    
    
  },options = list( pageLength = 5,scrollX = TRUE,scrolly=TRUE))
  
  output$contentsNazionale <- renderDataTable({
    

  
    
    output$graficoAnimatoNazionale<-renderPlotly(graficoAnimatoNazionale(dfNazionale$dati))
    
    return(dfNazionale$dati)
    
    
  },options = list( pageLength = 5,scrollX = TRUE,scrolly=TRUE))
  
  observeEvent(input$doProvince, {
    output$pProvince <- renderPlotly({
      plot_ly(dfProvince$dati, x = dfProvince$dati[[input$XProvince]], y = dfProvince$dati[[input$YProvince]]) %>%
        add_lines()
    })
    
  })
  output$ascissaProvince <- renderUI({
    radioButtons("XProvince", "Seleziona Ascissa: ",
                       colnames(dfProvince$dati))

  })
  output$ordinataProvince <- renderUI({
    radioButtons("YProvince", "Seleziona Ordinata: ",
                 colnames(dfProvince$dati))

  })
  
  observeEvent(input$doRegioni, {
    output$pRegioni <- renderPlotly({
      plot_ly(dfRegioni$dati, x = dfRegioni$dati[[input$XRegioni]], y = dfRegioni$dati[[input$YRegioni]]) %>%
        add_lines()
    })
    
  })
  output$ascissaRegioni <- renderUI({
    radioButtons("XRegioni", "Seleziona Ascissa: ",
                 colnames(dfRegioni$dati))
    
  })
  output$ordinataRegioni <- renderUI({
    radioButtons("YRegioni", "Seleziona Ordinata: ",
                 colnames(dfRegioni$dati))
    
  })
  
  observeEvent(input$doNazionale, {
    output$pNazionale <- renderPlotly({
      plot_ly(dfNazionale$dati, x = dfNazionale$dati[[input$XNazionale]], y = dfNazionale$dati[[input$YNazionale]]) %>%
        add_lines()
    })
    
  })

 
  output$ascissaNazionale <- renderUI({
    radioButtons("XNazionale", "Seleziona Ascissa: ",
                 colnames(dfNazionale$dati))
    
  })
  output$ordinataNazionale <- renderUI({
    radioButtons("YNazionale", "Seleziona Ordinata: ",
                 colnames(dfNazionale$dati))
    
  })
  
  output$new<-renderText(paste("Aggiornato al",format(as.Date(max(dfProvince$dati[["data"]])),format = "%d/%m/%Y")))

  output$listaRegioni<-renderUI({selectInput(
    inputId="selezioneRegioni",
    label="Regioni",
    choices=unique(dfProvince$dati[["denominazione_regione"]])
  )
  }) 
  
# output$listaRegioniMappa<-renderUI({
# )
# }) 
  
  observeEvent(input$selezioneRegioni,{
    output$graficoAnimatoProvince<-renderPlotly(graficoAnimatoProvince(dfProvince$dati,input$selezioneRegioni))
  })
  
  observeEvent(input$selezioneRegioniMappe,{
    if (input$selezioneRegioniMappe == "ITALIA") {
      output$graficoMappa<-renderPlotly(graficiMappaRegioni(dfRegioni$dati)) 
    } else {
      output$graficoMappa<-renderPlotly(graficiMappaRegioniDettaglio(dfProvince$dati,input$selezioneRegioniMappe))
    }
    
  })
  

 
   
 
}

shinyApp(ui, server)
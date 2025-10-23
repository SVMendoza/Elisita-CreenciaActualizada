source('global.R')

ui <- fluidPage(
 theme = shinytheme("flatly"),
  useShinyjs(),
tags$div(
    style = "display: flex; align-items: center; padding: 10px; background-color: #222; color: white; margin-bottom: 20px;",
    
      tags$img(src = "logo.png", height = "80px", style = "margin-right: 15px;"),
    
    tags$h2("Elisita-CreenciaActualizada: opinión, datos, creencia actualizada", style = "margin: 0;")
  ),


  tabsetPanel(
    tabPanel("Información de persona experta", 
             TabExpUI('Entrada')
             
    ),
    tabPanel("Elicitación", 
    quantileAnalysisUI("analisis")
    ),
    tabPanel("Datos observados", 
             TabDatosCampoUI('SubirBases')
    ),
    tabPanel("Actualizar creencias",
             BayesUI("prior")
   )         
  )
)

server <- function(input, output) {
  
  ModuloDatos('Entrada')
  ParametrosParaElicitacion <-distributionSimulationExpertoServer("Entrada")
  quantileAnalysisServer("analisis", ParametrosParaElicitacion)
  
  
  distributionDatosCampoServer('SubirBases')
  
  BayesServer("prior")
}


shinyApp(ui, server)


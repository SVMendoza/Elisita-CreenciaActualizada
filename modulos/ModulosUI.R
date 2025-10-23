nombresDistr <- c("Normal", "Lognormal",
                  "Exponencial", "Gamma", "Poisson",
                  "Beta", "Binomial",
                  "Uniforme", "PERT", "Triangular")


TabDatosCampoUI <- function(id) {
  tagList(
    sidebarPanel(
      fileInput(NS(id, "MiDtEmpirico"), "Cargar datos", accept = c(".csv", ".xlsx")),
      uiOutput(NS(id, "colselec")),
      checkboxGroupInput(NS(id,"distribuciones"), "Distribuciones a comparar:", 
                         choices = nombresDistr, selected = c("Normal", "Lognormal",
                                                               "Gamma")),
      shinyWidgets::switchInput(
        inputId = NS(id, "BtnRemuestreo"),
        label = "n<30 Remuestreo",
        value = FALSE,  
        onLabel = "Sí", 
        offLabel = "No",
        labelWidth = "150px"
      ),
      actionButton(NS(id,"run"), "Ejecutar análisis", class = "btn-primary",  icon = icon("cogs"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Observados', DT::dataTableOutput(NS(id, "Salidatabla"))),
        tabPanel("Ajuste de la verosimilitud (Likelihood)",
                 plotlyOutput(NS(id,"graficoCFD")),
		DT::DTOutput(NS(id,"TestEmpiricoresultados")))
        
      )
    )
  )
}


TabExpUI<- function(id) {
  tagList(
    sidebarPanel(
      radioButtons(inputId = NS(id, "ImporFile"), label = 'Importar archivo', 
                   choices = c('Si', 'No'), selected = 'No'),
      conditionalPanel(
        paste0("input['", NS(id, 'ImporFile'), "'] == 'No'"),
        textInput(inputId = NS(id, "IDVariable"), label = "Variable", value = "Variable1"),
        textInput(inputId = NS(id, "n_expertos"), label = "Experto", value = "Experto1"),
        numericInput(inputId = NS(id, "min_expertos"), label = "Mínimo", value = 5),
        numericInput(inputId = NS(id, "mean_expertos"), label = "Media", value = 10),
        numericInput(inputId = NS(id, "max_expertos"), label = "Máximo", value = 100),
        numericInput(inputId = NS(id, "peso_expertos"), label = "Años de experiencia", value = 1),
        selectInput(inputId=NS(id, "dist_options"), label='Selecionar distribución', 
                    choices=c("Normal", "Lognormal",
                              "Exponencial", "Gamma", "Poisson",
                              "Beta", "Binomial",
                              "Uniforme", "PERT", "Triangular"), multiple=FALSE),
        fluidRow(
          column(width = 6,
                 actionButton(inputId = NS(id, "Add"), label = "Agregar info", class = "btn-success", icon = icon("plus"))
          )
        )
      ),
      conditionalPanel(
        paste0("input['", NS(id, 'ImporFile'), "'] == 'Si'"),
        fluidRow(
          column(width = 12,
                 fileInput(NS(id, 'fileExp'), "Cargar datos", accept =c(".csv", ".xlsx"))
               
                 
          )
        )
      ),
      
      selectInput(inputId = NS(id,"nivel_Variable"), "Selecciona una variable:",
                  choices = NULL),
      numericInput(inputId = NS(id,"nSim"), "Número de simulaciones", value = 10000, min = 1000),
      
      fluidRow(
        column(width = 6,
               actionButton(inputId = NS(id, "run"), label = "Ejecutar",class = "btn-primary",  icon = icon("cogs"))
        )
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tabla", DT::DTOutput(NS(id, "expertTabla"))),
        tabPanel("Simulación", plotOutput(NS(id, "gridPlot"))),
        tabPanel("Ajuste consenso", plotlyOutput(NS(id, 'plotConsensoAjust')))
      )
    )
  )
}


paramPriorUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("paramsUI"))  
}


parametros_guardados <- ParametrosPriorDist$new()


mod_priorDist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("PriorDist"), 
      label = "Selecciona la distribución:", 
      choices = c("Normal", "Lognormal", "Exponencial", "Gamma", "Beta", "Triangular", "PERT")
    ),
    uiOutput(ns("paramsUI"))
  )
}



BayesUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        mod_priorDist_ui(ns("prior")),
        selectInput(
          inputId = ns("VeroDist"),
          label = "Función de verosimilitud:",
          choices = c("Normal", "Lognormal", "Exponencial", "Gamma", "Poisson", "Beta", "Binomial", "PERT", "Triangular")
        ),
          shinyWidgets::switchInput(
          inputId = ns("BtnBayes"),
          label = "Método de estimación",
          value = TRUE,
          onLabel = "MCMC", 
          offLabel = "Analítico",
          size = "small",     
          width = "100px"
        ),
        sliderInput(inputId=ns("Itera"), 
                    label='Número de iteracciones:', min=500, max=10000, value=1000,step=100), 
        sliderInput(inputId=ns("ChainCha"), 
                    label='Número de cadenas:', min=2, max=10, value=4,step=1), 
        sliderInput(inputId=ns("ICRE"), 
                    label='intervalo de credibilidad:', min=0.80, max=0.99, value=0.95,step=0.05),
        hr(),  
        actionButton(inputId = ns("runBayes"), "Ejecutar análisis", icon = icon("cogs"),  class = 'btn-primary'),
        hr(),
      ),
      mainPanel(
        
            verbatimTextOutput(ns("DiagnosticoModel"))%>% withSpinner(type = 8, color = "#0d6efd"),
              plotOutput(ns("graficoPosterior"))
              
    )
   )
  )
}

#verbatimTextOutput(ns("debugOutput"))
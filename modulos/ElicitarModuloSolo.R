quantileAnalysisUI  <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("selected_dist"), 
                    "Distribución ajustada",
                    choices = NULL),
        #numericInput(ns("nsim"), 
         #            "N° simulaciones",
         #            value = 10000, min = 1000),
        numericInput(ns("confidence_level"), 
                     "Nivel de confianza (%)",
                     value = 95, min = 1, max = 99),
        textInput(inputId = NS(id, "IDComp"), label = "Componente", value = "Componente1"),
        
        fluidRow(
          column(6, actionButton(ns("calculate"), "Calcular", class = "btn-primary btn-block"))
        ),
       
        hr(),
        downloadButton(ns("export"), "Exportar a CSV", class = "btn-block"),
        hr(),
        fluidRow(
          column(6, actionButton(ns("clear"), "Limpiar", class = "btn-danger btn-block"))
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Resumen",
                   fluidRow(
                     column(6,
                            wellPanel(
                              h4("Estadísticas Descriptivas"),
                              verbatimTextOutput(ns("stats_summary"))
                            )
                     ),
                     column(6,
                            wellPanel(
                              h4("Intervalo de Confianza"),
                              verbatimTextOutput(ns("interval_output"))
                            )
                     )
                   ),
                   fluidRow(
                     column(6,
                   plotOutput(ns("distribution_plot")),
                     ),
                   column(6,
                   plotlyOutput(NS(id, 'plotAjuste')),
                   ))
          ),
          tabPanel("Histórico",
                   DTOutput(ns("results_table"))
          )
        )
      )
    )
  )
}


quantileAnalysisServer <- function(id, inputs_Entrada) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values para almacenar resultados
    rv <- reactiveValues(
      simulated_data = NULL,
      results_history = tibble::tibble(
        Componente=character(),
        Variable = character(),
        Fecha = character(),
        Distribucion = character(),
        Nivel_Confianza = numeric(),
        Media = numeric(),
        Mediana = numeric(),
        Limite_Inferior = numeric(),
        Limite_Superior = numeric(),
        Estado = character()
      ),
      last_calculation = NULL
    )
    
    # Función para validar datos de entrada
    validate_consenso <- function(x) {
      if(is.null(x) || length(x) == 0 || all(is.na(x))) {
        showNotification("Datos de consenso no disponibles o inválidos", type = "error")
        return(FALSE)
      }
      TRUE
    }
    
    
    
    
    # Actualizar distribuciones disponibles
    observe({
      #req(Contener$obtenerDatos("SimConsenso"))
     #x <- Contener$obtenerDatos("SimConsenso")
      #if(validate_consenso(x)) {
        available_dists <- c("Normal", "Lognormal", "Gamma", "Beta", "PERT", 
                             "Triangular", "Uniforme", "Exponencial", "Poisson")
        updateSelectInput(session, "selected_dist", choices = available_dists)
     # }
    })
    
    # Simular datos y calcular intervalos
    observeEvent(input$calculate, {
      req(Contener$obtenerDatos("SimConsenso"))
      x <- Contener$obtenerDatos("SimConsenso")
      
    
      
     # req(input$selected_dist, input$confidence_level)
      req(input$selected_dist)
      # Validaciones básicas
      if(!validate_consenso(x)) return()
      #if(input$confidence_level <= 0 | input$confidence_level >= 100) {
       # showNotification("Nivel de confianza debe estar entre 1% y 99%", type = "error")
      #  return()
      #}
      
      # Calcular percentiles
      alpha <- (100 - input$confidence_level)/200
      lower_percentile <- alpha
      upper_percentile <- 1 - alpha
      
      # Ajustar distribución y simular datos con manejo robusto de errores
      tryCatch({
        fit_result <- cdfs_SC_ggplot(x, input$selected_dist)
        params <- fit_result$parametros[[input$selected_dist]]
        
        
        
        
        
        # Validación adicional para Lognormal
        if(input$selected_dist == "Lognormal" && any(x <= 0)) {
          showNotification("Datos no positivos no permitidos en Lognormal", type = "error")
          return()
        }
        
       
        nsim <- inputs_Entrada$nSim()
       
        
        rv$simulated_data <- switch(
          input$selected_dist,
          "Normal" = rnorm(nsim, mean = params$mu, sd = params$sigma),
          "Lognormal" = {
            if(is.null(params$mu)) stop("Parámetros mu no definidos")
               rlnorm(nsim, meanlog = params$mu, sdlog = params$sigma)
          },
          "Gamma" = rgamma(nsim, shape = params$alpha, rate = params$beta),
          "Beta" = {
            # Asegurar parámetros válidos para Beta
            if(params$alpha <= 0 || params$beta <= 0) {
              showNotification("Parámetros alpha/beta inválidos para Beta", type = "error")
              return()
            }
            rbeta(nsim, shape1 = params$alpha, shape2 = params$beta)
          },
          "PERT" = {
            a <-  params$min
            b <-  params$max
            mode <-  params$mode
            if(a >= b) {
              showNotification("Valores min/max inválidos para PERT", type = "error")
              return()
            }
            rpert(nsim, a, b, mode)
          },
          "Triangular" = rtriang(nsim, min = params$min, max = params$max, mode = params$mode),
          "Uniforme" = runif(nsim, min = params$Min, max = params$Max),
          "Exponencial" = rexp(nsim, rate = params$rate),
          "Poisson"=rpois(nsim,   lambda=params$lambda)
            )
            
            if(is.null(rv$simulated_data) || any(is.na(rv$simulated_data))) {
              stop("Datos simulados contienen NA/NaN")
            }
            
            # Calcular estadísticas con na.rm = TRUE
            stats <- list(
              Componente= input$IDComp,
              Variable = inputs_Entrada$nivel_Variable(),
              Fecha = as.character(Sys.time()),
              Distribucion = input$selected_dist,
              Nivel_Confianza = input$confidence_level,
              Media = mean(rv$simulated_data, na.rm = TRUE),
              Mediana = median(rv$simulated_data, na.rm = TRUE),
              Limite_Inferior = quantile(rv$simulated_data, probs = lower_percentile, na.rm = TRUE) %>% as.numeric(),
              Limite_Superior = quantile(rv$simulated_data, probs = upper_percentile, na.rm = TRUE) %>% as.numeric(),
              Estado = "Éxito"
            )
            
            # Guardar en histórico
            rv$results_history <- dplyr::bind_rows(rv$results_history, stats)
            rv$last_calculation <- stats
            
            # Actualizar gráfico
            output$distribution_plot <- renderPlot({
              ggplot(data.frame(x = rv$simulated_data), aes(x)) +
                geom_density(fill = "skyblue", alpha = 0.7) +
                geom_vline(xintercept = c(stats$Limite_Inferior, stats$Limite_Superior), 
                           color = "red", linetype = "dashed") +
                geom_vline(xintercept = stats$Media, color = "blue") +
                labs(title = paste("Distribución", input$selected_dist),
                     x = "Valor", y = "Densidad") +
                theme_minimal()
            })
            
            output$plotAjuste <- renderPlotly({
              ggplotly(fit_result$grafico)
            })
            
      }, error = function(e) {
        error_msg <- paste("Error en", input$selected_dist, ":", e$message)
        showNotification(error_msg, type = "error", duration = 10)
        
        # Registrar fallo en el histórico
        rv$results_history <- dplyr::bind_rows(
          rv$results_history,
          tibble::tibble(
            Variable = inputs_Entrada$nivel_Variable(),
            Fecha = as.character(Sys.time()),
            Distribucion = input$selected_dist,
            Nivel_Confianza = input$confidence_level,
            Media = NA,
            Mediana = NA,
            Limite_Inferior = NA,
            Limite_Superior = NA,
            Estado = paste("Error:", e$message)
          )
        )
      })
    })
      
      
    
    # Mostrar resumen estadístico
    output$stats_summary <- renderPrint({
      req(rv$last_calculation)
      cat(
        "Distribución:", rv$last_calculation$Distribucion, "\n",
        "Media:", round(rv$last_calculation$Media, 4), "\n",
        "Mediana:", round(rv$last_calculation$Mediana, 4), "\n",
        "Desv. Estándar:", round(sd(rv$simulated_data), 4), "\n",
        "Mínimo:", round(min(rv$simulated_data), 4), "\n",
        "Máximo:", round(max(rv$simulated_data), 4)
      )
    })
    
    # Mostrar intervalo de confianza
    output$interval_output <- renderPrint({
      req(rv$last_calculation)
      cat(
        "Nivel de confianza:", rv$last_calculation$Nivel_Confianza, "%\n\n",
        "Límite inferior (", (100 - rv$last_calculation$Nivel_Confianza)/2, "%):\n",
        round(rv$last_calculation$Limite_Inferior, 4), "\n\n",
        "Límite superior (", 100 - (100 - rv$last_calculation$Nivel_Confianza)/2, "%):\n",
        round(rv$last_calculation$Limite_Superior, 4)
      )
    })
    
    # Mostrar tabla con histórico
    output$results_table <- renderDT({
      tabla <- rv$results_history %>% 
        mutate(across(where(is.numeric), ~round(.x, 4)))
      
      datatable(
        tabla,
        options = list(
          pageLength = 25,
          order = list(list(0, 'desc'))
        )
      ) %>%
        formatDate("Fecha", method = "toLocaleString") %>%
        formatStyle(
          c("Limite_Inferior", "Limite_Superior"),
          backgroundColor = styleInterval(
            mean(rv$results_history$Limite_Inferior),
            c("#FFEEEE", "#FFFFFF")
          )
        )
    })
    
    
    # Exportar resultados
    output$export <- downloadHandler(
      filename = function() {
        paste0("resultados_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        # Escribe el archivo xlsx usando writexl
        writexl::write_xlsx(rv$results_history, path = file)
      }
    )
    
    # Limpiar histórico
    observeEvent(input$clear, {
      rv$results_history <- tibble::tibble(
        Fecha = character(),
        Distribucion = character(),
        Nivel_Confianza = numeric(),
        Media = numeric(),
        Mediana = numeric(),
        Limite_Inferior = numeric(),
        Limite_Superior = numeric()
      )
      showNotification("Histórico borrado", type = "message")
    })
    
    # Función para distribución PERT
    rpert <- function(n, a, b, mode) {
      if(a >= b || mode <= a || mode >= b) {
        stop("Parámetros inválidos para PERT: a < mode < b requerido")
      }
      lambda <- (b - a) / (mode - a)
      alpha <- 4 / lambda + 1
      beta <- 4 * (1 - 1/lambda) + 1
      a + (b - a) * rbeta(n, alpha, beta)
    }
  })
}
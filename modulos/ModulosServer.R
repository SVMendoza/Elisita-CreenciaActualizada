# ---- Modulo server tabla input Empirico (experto) -----

ModuloDatos <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dt <- reactiveValues(
      dtRend = data.frame(
        Variable = character(),
        ID = character(),
        Min = numeric(),
        Media = numeric(),
        Max = numeric(),
        Peso = numeric(),
        Distribucion = character()
      )
    )
    
    # Detectar cuando se carga un archivo
    observeEvent(input$fileExp, {
      req(input$fileExp)  # Asegurarnos de que el archivo fue cargado
      
      
      ext <- tools::file_ext(input$fileExp$name)
      datos <- switch(ext,
             csv = fread(input$fileExp$datapath), 
             txt = fread(input$fileExp$datapath),   
             xlsx = as.data.table(readxl::read_excel(input$fileExp$datapath)),  
             stop("Formato no soportado 😓")  
      )
    
      
    #  datos <- readxl::read_excel(input$fileExp$datapath)
      
      # Validación: Verificar que el archivo contiene las columnas esperadas
      required_columns <- c("Variable", "ID", "Min", "Media", "Max", "Peso", "Distribucion")
      
      if (!all(required_columns %in% colnames(datos))) {
        showModal(modalDialog(
          title = "Error en el archivo",
          "El archivo no contiene las columnas esperadas. Por favor, asegúrese de que el archivo tenga las siguientes columnas: 'Variable', 'ID', 'Min', 'Media', 'Max', 'Peso', 'Distribucion'.",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        return(NULL)  # Salir de la función si el archivo no es válido
      }
      
      # Si el archivo es válido, se agrega a la tabla
      dt$dtRend <- datos  # Actualizamos la tabla con los datos del archivo
      Contener$guardarTabla("dtExpertos", dt$dtRend)
      
      # Si es necesario, actualizamos los valores de los inputs con el primer valor de la tabla
      updateTextInput(session, "IDVariable", value = datos$Variable[1])
      updateTextInput(session, "n_expertos", value = datos$ID[1])
      updateNumericInput(session, "min_expertos", value = datos$Min[1])
      updateNumericInput(session, "mean_expertos", value = datos$Media[1])
      updateNumericInput(session, "max_expertos", value = datos$Max[1])
      updateNumericInput(session, "peso_expertos", value = datos$Peso[1])
      updateSelectInput(session, "dist_options", selected = datos$Distribucion[1])
    })
    
    # Agregar nueva entrada manual
    observeEvent(input$Add, {
      validate(need(input$IDVariable != "", "Por favor, incluya nombre de variable."))
      validate(need(input$n_expertos != "", "Por favor ingrese un ID para el experto."))
      validate(need(input$min_expertos != "", "Por favor ingrese valor mínimo."))
      validate(need(input$mean_expertos != "", "Por favor ingrese un valor promedio."))
      validate(need(input$max_expertos != "", "Por favor ingrese un valor máximo."))
      validate(need(input$peso_expertos != "", "Por favor ingrese un peso de experto."))
      validate(need(input$dist_options != "", "Por favor ingrese la distribución."))
      
      # Crear un nuevo ingreso con los valores actuales de los inputs
      nuevoIngreso <- data.frame(
        Variable = input$IDVariable,
        ID = input$n_expertos,
        Min = input$min_expertos,
        Media = input$mean_expertos,
        Max = input$max_expertos,
        Peso = input$peso_expertos,
        Distribucion = input$dist_options
      )
      
      # Si ya existen datos en la tabla (ya sea manualmente o cargados), agregamos el nuevo ingreso
      dt$dtRend <- rbind(dt$dtRend, nuevoIngreso)
      
      # Guardar la tabla actualizada
      Contener$guardarTabla("dtExpertos", dt$dtRend)
    })
    
    # Renderizar la tabla de expertos
    output$expertTabla <- DT::renderDataTable({
      datatable(
        dt$dtRend,
        editable = TRUE,
        rownames = FALSE
      )
    })
    
    observe({
      updateSelectInput(session, "nivel_Variable", 
           choices = unique(dt$dtRend$Variable))
    })
    
    proxy <- DT::dataTableProxy("expertTabla")
    
    observeEvent(input$expertTabla_cell_edit, {
      info <- input$expertTabla_cell_edit
      dt$dtRend <<- editData(dt$dtRend, info)
      replaceData(proxy, dt$dtRend, resetPaging = FALSE)
      Contener$guardarTabla("dtExpertos", dt$dtRend)
    })
  })
}

# ---- Modulo server expertos -----

          #######################################
# -----   Distribución experto y consenso #
# ----    Distribuciones para datos observados o empiricos
          #######################################

#
distributionSimulationExpertoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
   
    observeEvent(input$run, {
     x<-data.frame(try(Contener$obtenerDatos("dtExpertos"), silent=TRUE))
    
     if (is.null(x) || class(x) == "try-error" || nrow(x) == 0) {
       shinyalert(" ", "No hay archivos o no hay una estructura de datos lógica.", type = "error")
       return(NULL)  
     }
      
      
     req(input$nivel_Variable)
     x <- x %>% filter(Variable == input$nivel_Variable)
     
     # Remover NA
     x <- na.omit(x)
     
     # Control de valores constantes o mal definidos
     selExpert <- (x$Max != x$Min | x$Max != x$Media) & (x$Min <= x$Max)
     x <- x[selExpert, ]
     
     # Validación final: si no queda ningún dato
     if (nrow(x) == 0) {
       shinyalert(" ", "No hay datos válidos después del control de calidad.", type = "error")
       return(NULL)
     }
     
      
      
  xList<-split(x,x$ID)
 
    salida<-lapply(xList, function(x) {
      if(x$Distribucion[1]=='Normal') {
      sal<-normalF(x)
      datosSimul<-rnorm(input$nSim, sal$mean, sal$sd)
    } else if(x$Distribucion[1]=='PERT'){
      sal<-pertF(x)
      datosSimul<- rbeta(input$nSim, sal$shape1, sal$shape2)*(sal$max - sal$min)+sal$min
    } else if(x$Distribucion[1]=="Triangular"){
      sal<-triangularF(x)
      datosSimul<-rtriang(input$nSim, min =sal$a, max=sal$c, mode =sal$b)
      } else if(x$Distribucion[1]=="Lognormal"){
        sal<-lognormalF(x)
        datosSimul<-rlnorm(input$nSim, sal$meanlog, sal$sdlog)
      } else if(x$Distribucion[1]=="Beta"){
        sal<-betaF(x)
        datosSimul<-rbeta(input$nSim, sal$shape1, sal$shape2)
      }   else if(x$Distribucion[1]=="Gamma"){
        sal<-gammaF(x)
        datosSimul<-rgamma(input$nSim, shape = sal$shape, rate = sal$rate)
      } else if(x$Distribucion[1]=="Uniforme"){
        sal<-uniformef(x)
        datosSimul<-runif(input$nSim, sal$min, sal$max)
      }   else if(x$Distribucion[1]=="Poisson"){
        sal<-poissonf(x)
        datosSimul<-rpois(input$nSim, sal$lambda)
      } else if(x$Distribucion[1]=='Exponencial'){
        sal<-expF(x)
        datosSimul<-rexp(input$nSim, rate = sal$lambda)
      } else if(x$Distribucion[1]=='Binomial'){
        sal<-binomialF(x)
        datosSimul<-rbinom(input$nSim, size = sal$size, prob = sal$exitos)
      } else {sal<-NULL}
      datosSimul
    })
    nam <- sapply(xList, function(x) paste0(x$ID[1], ' ', '(Dist:', x$Distribucion[1], ') 😊 '))
    Distribu <- as.vector(sapply(xList, function(x) x$Distribucion[1]))
    
     Combinarsim <- do.call(cbind, salida)
    # Combinarsim <- Combinarsim * (x$Peso / sum(x$Peso))
if(all(x$Peso == x$Peso[1]) ) {consenso <- as.vector(rowMeans(Combinarsim))} else {
Combinarsim <- sweep(Combinarsim, 2, x$Peso/sum(x$Peso), "*")
consenso <- as.vector(rowSums(Combinarsim)) 
}
    
   # consenso <- as.vector(rowMeans(Combinarsim))
    
    Contener$guardarTabla("SimConsenso",  consenso)
    
    ListaPlot <- lapply(1:length(salida), function(i) {
      datosSimul<-salida[[i]]
      ggplot(data.frame(valor = datosSimul), aes(x = valor)) +
        geom_density(fill = "blue", alpha = 0.5) +  # Densidad con color azul
        labs(title =nam[i] , x = "Valor", y = "Densidad") +
        theme_minimal()
    })
    
    ListaPlot[[length(ListaPlot) + 1]] <-ggplot(data.frame(valor = consenso), aes(x = valor)) +
      geom_density(fill = "darkgreen", alpha = 0.6) +
      labs(title = 'Consenso Ponderado 🎯', x = "Valor", y = "Densidad") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#2E7D32"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")
      )
    
    #ListaPlot[[length(ListaPlot) + 1]] <-cdfs_SC_ggplot (consenso, Distribu)
    
    
    ###############################
    output$gridPlot <- renderPlot({
     gridExtra::grid.arrange(grobs = ListaPlot, ncol = 2)
    })
    
    salidaPrior<-cdfs_SC_ggplot(consenso, Distribu)
    for(i in Distribu)  parametros_guardados$set_parametros(i, salidaPrior$parametros[[i]])
    output$plotConsensoAjust <- renderPlotly({
      ggplotly(salidaPrior$grafico)
    })
    
    
  #  }
   
  })
    ## Parametros al modulo de elicitación ###
    return(list(
      nivel_Variable = reactive({ input$nivel_Variable }),
      nSim = reactive({ input$nSim })
    ))
})
}
 
# ---- Modulo server Distribución de los datos empíricos  -----

distributionDatosCampoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    datos <- reactive({
      req(input$MiDtEmpirico)  
      
      ext <- tools::file_ext(input$MiDtEmpirico$name)
      switch(ext,
             csv = fread(input$MiDtEmpirico$datapath), 
             txt = fread(input$MiDtEmpirico$datapath),   
             xlsx = as.data.table(readxl::read_excel(input$MiDtEmpirico$datapath)),  
             stop("Formato no soportado 😓")  
      )
    })
    
    
    output$colselec <- renderUI({
      req(datos())  
      selectInput(session$ns("columna"), "Seleccionar variable:",
                  choices = colnames(datos()),  
                  selected = colnames(datos())[1])  
    })
    
    
    output$Salidatabla <- DT::renderDataTable({
      req(datos())  
      DT::datatable(datos(), options = list(pageLength = 10))
    })
    
    
    resultadosTestBondad <- eventReactive(input$run, {
      req(datos(), input$columna, input$distribuciones)
      xdat <- datos()[[input$columna]]
      xdat <- xdat[is.finite(xdat)]
      if (input$BtnRemuestreo) {
        xdat <- sample(xdat, size = 500, replace = TRUE)
      }
      Contener$guardarTabla("DatosCampo",  xdat)
      
      testDistribuciones(xdat, input$distribuciones)
      
    })
    
    output$TestEmpiricoresultados <- DT::renderDataTable({
      req(resultadosTestBondad(), input$distribuciones)
      DT::datatable(resultadosTestBondad(), 
                    options = list(pageLength = 10))
    })
    
    
    # Graficar CDFs
    output$graficoCFD <- renderPlotly({
      req(resultadosTestBondad())
      xx<-Contener$obtenerDatos('DatosCampo')[,1]
     
      ggplotly(cdfs_SC_ggplot(xx, input$distribuciones)$grafico)
    })
    
    
  })
}


##########################################################
# ---------- MODULO SERVER BAYES ------------------------
#########################################################

# --  Módulo Server para mostrar los inputs según la distribución seleccionada --
mod_priorDist_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Mapeo de nombres lógicos a inputId
    input_ids <- list(
      "Normal" = list(mu = "mean", sigma = "sd"),
      "Lognormal" = list(mu = "meanlog", sigma = "sdlog"),
      "Gamma" = list(alpha = "Gammaalpha", beta = "Gammabeta"),
      "Exponencial" = list(rate = "rate"),
      "Beta" = list(alpha = "Betaalpha", beta = "Betabeta", min = "Betamin", max = "Betamax"),
      "Triangular" = list(min = "Trianmin", max = "Trianmax", mode = "Trianmode"),
      "PERT" = list(min = "PERTmin", max = "PERTmax", mode = "PERTmode", shape = "PERTshape")
    )
    
    # Distribución seleccionada
    distribucion <- reactive({
      input$PriorDist
    })
    
    # Valores iniciales al cargar/cambiar distribución
    valores_iniciales <- reactive({
      req(distribucion())
      dist <- distribucion()
      parametros_guardados$get_parametros(dist)
    })
    
    # UI dinámica - construida una vez al cambiar distribución
    output$paramsUI <- renderUI({
      req(distribucion(), valores_iniciales())
      dist <- distribucion()
      params <- valores_iniciales()
      
      # Construcción condicional de la UI
      switch(dist,
             "Normal" = tagList(
               numericInput(ns("mean"), "Media:", value = params$mu),
               numericInput(ns("sd"), "Desviación estándar:", value = params$sigma)
             ),
             "Lognormal" = tagList(
               numericInput(ns("meanlog"), "Media log:", value = params$mu),
               numericInput(ns("sdlog"), "Desv. estándar log:", value = params$sigma)
             ),
             "Gamma" = tagList(
               numericInput(ns("Gammaalpha"), "Alpha:", value = params$alpha),
               numericInput(ns("Gammabeta"), "Beta:", value = params$beta)
             ),
             "Exponencial" = numericInput(ns("rate"), "Tasa:", value = params$rate),
             "Beta" = tagList(
               numericInput(ns("Betaalpha"), "Alpha:", value = params$alpha),
               numericInput(ns("Betabeta"), "Beta:", value = params$beta),
               numericInput(ns("Betamin"), "Min:", value = params$min),
               numericInput(ns("Betamax"), "Max:", value = params$max)
             ),
             "Triangular" = tagList(
               numericInput(ns("Trianmin"), "Min:", value = params$min),
               numericInput(ns("Trianmax"), "Max:", value = params$max),
               numericInput(ns("Trianmode"), "Moda:", value = params$mode)
             ),
             "PERT" = tagList(
               numericInput(ns("PERTmin"), "Min:", value = params$min),
               numericInput(ns("PERTmax"), "Max:", value = params$max),
               numericInput(ns("PERTmode"), "Moda:", value = params$mode),
               numericInput(ns("PERTshape"), "Forma:", value = params$shape)
             )
      )
    })
    
    # Parámetros actuales basados solo en inputs
    parametros_actuales <- reactive({
      req(distribucion())
      dist <- distribucion()
      ids <- input_ids[[dist]]
      
      # Obtener valores solo de los inputs
      valores <- lapply(names(ids), function(nombre_logico) {
        input[[ids[[nombre_logico]]]]
      })
      names(valores) <- names(ids)
      
      # Filtrar valores no numéricos
      if (all(sapply(valores, is.numeric))) {
        valores
      } else {
        NULL
      }
    })
    
    # Actualizar R6 solo cuando cambian los inputs
    observeEvent(parametros_actuales(), {
      req(distribucion(), parametros_actuales())
      dist <- distribucion()
      parametros_guardados$set_parametros(dist, parametros_actuales())
    })
    
    return(list(
      distribucion = distribucion,
      parametros = reactive({
        req(distribucion())
        parametros_guardados$get_parametros(distribucion())
      })
    ))
  })
}

# --  Módulo Server Bayes --

BayesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    is_processing <- reactiveVal(FALSE)
    prior_module <- mod_priorDist_server("prior")
    
   parametros_actuales <- reactive({
      req(prior_module$distribucion())
      
      dist <- prior_module$distribucion()
      if (dist %in% names(parametros_guardados$ParaDatos)) {
        return(parametros_guardados$ParaDatos[[dist]])
      }
      
      warning("No se encontraron parámetros para: ", dist)
      #return(list())
    })
    
    observeEvent(input$runBayes, {
      req(prior_module$distribucion(), parametros_actuales())
     
      
      tryCatch({
        
        dist_actual <- prior_module$distribucion()
        params <- parametros_guardados$get_parametros(dist_actual)
        
        LikelihoodDist<-input$VeroDist
        datosCamp<-try(Contener$obtenerDatos("DatosCampo")[,1], silent=TRUE)
       
        if (is.null(datosCamp) || class(datosCamp)=="try-error" || length(datosCamp)==0)
        {
         showNotification("Error: No hay datos cargados.", type = "error")
        } else { 
          
        cat("\n=== INICIANDO ANÁLISIS BAYESIANO ===\n")
        cat("Distribución Prior:", dist_actual, "\n")
        cat("Parámetros Prior:\n")
        print(params)
        cat("Verosimilitud:", input$VeroDist, "\n")
        cat("Método:", ifelse(input$BtnBayes, "MCMC", "Analítico"), "\n")
        
        chains<-as.numeric(input$ChainCha)
        iter<-as.numeric(input$Itera)
        cred_mass<-as.numeric(input$ICRE)
        distActual<-tolower(dist_actual)
        if(distActual=='exponencial') distActual<-'exponential'
        #######Modelo
        is_processing(TRUE)
        #resultadoModelo <- bayesian_inference(
        resultadoModelo <-bayesian_inference_prec(
          prior_name =distActual ,
          prior_params = params,
          likelihood_name = tolower(LikelihoodDist),
          data =datosCamp,
          total_trials = NULL,
          iter = iter, chains = chains
        )
      
        output$DiagnosticoModel<-renderPrint({
          Sys.sleep(41)
          check_stan_diagnostics(resultadoModelo, cred_mass=cred_mass)
                    })
        # Organizar los gráficos usando grid.arrange con la lista
        
         output$graficoPosterior<- renderPlot({
          ffPostPlot(resultadoModelo, cred_mass=cred_mass)
        })
         is_processing(FALSE)
        }
      }, error = function(e) {
        showNotification(paste("Error en análisis:", e$message), type = "error")
        cat("ERROR:", e$message, "\n")
      })
    })
  })
}


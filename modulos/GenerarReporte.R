
reportGeneratorUI <- function(id, label = "Descargar Reporte") {
  ns <- NS(id)
  
  tagList(
    downloadButton(ns("download_rmd"), "Descargar .Rmd"),
    downloadButton(ns("download_report"), label)
  )
}

reportGeneratorServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Objeto reactivo para almacenar el contenido del reporte
    report_content <- reactiveVal(list())
    
    # Función para agregar contenido al reporte
    add_to_report <- function(title, content, type = "code", eval = TRUE) {
      current_content <- report_content()
      new_item <- list(
        title = title,
        content = content,
        type = type,
        eval = eval
      )
      report_content(c(current_content, list(new_item)))
    }
    
    # Generar el contenido del RMarkdown
    generate_rmd_content <- function() {
      content <- report_content()
      if (length(content) == 0) return(NULL)
      
      rmd_lines <- c(
        "---",
        "title: 'Reporte Generado Dinámicamente'",
        "output:",
        "  html_document: default",
        "  pdf_document: default",
        "---",
        "\n```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "```"
      )
      
      for (item in content) {
        rmd_lines <- c(
          rmd_lines,
          "",
          paste("#", item$title),
          ""
        )
        
        if (item$type == "code") {
          rmd_lines <- c(
            rmd_lines,
            paste0("```{r ", make.names(item$title), ", eval=", item$eval, "}"),
            item$content,
            "```"
          )
        } else if (item$type == "text") {
          rmd_lines <- c(rmd_lines, item$content)
        } else if (item$type == "table") {
          rmd_lines <- c(
            rmd_lines,
            paste0("```{r ", make.names(item$title), "}"),
            "knitr::kable(", item$content, ")",
            "```"
          )
        } else if (item$type == "plot") {
          rmd_lines <- c(
            rmd_lines,
            paste0("```{r ", make.names(item$title), ", fig.width=8, fig.height=5}"),
            item$content,
            "```"
          )
        }
      }
      
      paste(rmd_lines, collapse = "\n")
    }
    
    # Descargar el archivo Rmd
    output$download_rmd <- downloadHandler(
      filename = function() {
        paste0("reporte_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rmd")
      },
      content = function(file) {
        content <- generate_rmd_content()
        if (is.null(content)) {
          showNotification("No hay contenido para generar el reporte", type = "warning")
          return()
        }
        writeLines(content, file)
      }
    )
    
    # Descargar el reporte renderizado
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("reporte_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        content <- generate_rmd_content()
        if (is.null(content)) {
          showNotification("No hay contenido para generar el reporte", type = "warning")
          return()
        }
        
        # Crear un directorio temporal
        temp_dir <- tempdir()
        temp_rmd <- file.path(temp_dir, "temp_report.Rmd")
        writeLines(content, temp_rmd)
        
        # Renderizar el documento
        rmarkdown::render(
          input = temp_rmd,
          output_file = file,
          envir = new.env()
        )
      }
    )
    
    # Devolver las funciones para interactuar con el módulo
    list(
      add_to_report = add_to_report,
      get_content = report_content
    )
  })
}



########################################################################

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Generador de Reportes Dinámicos"),
  sidebarLayout(
    sidebarPanel(
      actionButton("add_plot", "Agregar Gráfico"),
      actionButton("add_table", "Agregar Tabla"),
      actionButton("add_text", "Agregar Texto"),
      reportGeneratorUI("report")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table"),
      textOutput("text")
    )
  )
)

server <- function(input, output, session) {
  # Inicializar el módulo
  report <- reportGeneratorServer("report")
  
  # Generar algunos outputs
  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()
  })
  
  output$table <- renderTable({
    head(mtcars)
  })
  
  output$text <- renderText({
    "Este es un texto de ejemplo para el reporte."
  })
  
  # Agregar contenido al reporte cuando se hacen clic en los botones
  observeEvent(input$add_plot, {
    report$add_to_report(
      "Gráfico de MPG vs WT",
      "ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point()",
      type = "plot"
    )
    showNotification("Gráfico agregado al reporte", type = "message")
  })
  
  observeEvent(input$add_table, {
    report$add_to_report(
      "Tabla de datos mtcars",
      "head(mtcars)",
      type = "table"
    )
    showNotification("Tabla agregada al reporte", type = "message")
  })
  
  observeEvent(input$add_text, {
    report$add_to_report(
      "Texto descriptivo",
      "Este es un texto de ejemplo que se incluirá en el reporte.",
      type = "text"
    )
    showNotification("Texto agregado al reporte", type = "message")
  })
}

shinyApp(ui, server)
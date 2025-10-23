# Definir las funciones para cada distribución

'%||%' <- function(a, b) if (!is.null(a)) a else b

# ------ Distribuciones Generar entrada ---- 

pertF <- function(x) {
  
  modo <- ((4 + 2) * x$Media - x$Min - x$Max )/4
  Media <- (x$Min + 4 * modo + x$Max) / (4 + 2)
  
  # Calcular alpha1
  denom1 <- (modo - Media) * (x$Max - x$Min)
  numer1 <- (Media - x$Min) * (2 * modo - x$Min - x$Max)
  
  #a1 <- 1 + 4 * (mode - x$Min)/(x$Max - x$Min)
  a1 <- numer1 / denom1
  denom2 <- (Media - x$Min)
  #a2 <- 1 + 4 * (x$Max - mode)/(x$Max - x$Min)
  a2 <- a1 * (x$Max - Media) / denom2

  list(shape1 = a1, shape2 = a2, min = x$Min, max = x$Max)
}
triangularF <- function(x) {
  list(
    a = x$Min,
    b = x$Media,
    c = x$Max
  )
}
lognormalF <- function(x) {
  if(x$Min==0) x$Min<-1
  list(meanlog = log(x$Media), sdlog = (log(x$Max) - log(x$Min))/6)
}
betaF <- function(x) {
  if(x$Min>=0 && x$Max<=1) {
    mu_std<-x$Media
  } else {
  mu_std <- (x$Media - x$Min) / (x$Max - x$Min)
  }
  #var_std <- 1 / 36
  list(
    shape1 = mu_std * 15,
    shape2 = (1 - mu_std) * 15
    )
}
gammaF <- function(x) {
  shape <- (x$Media^2) / ((x$Max - x$Min) / 6)^2
  rate <- x$Media / ((x$Max - x$Min) / 6)^2
  list(shape = shape, rate = rate)
}
poissonf<-function(x) {
  list(
    lambda = x$Media
  )}
uniformef<-function(x){
  list(
    min = x$Min,
    max = x$Max
  )
}
normalF<-function(x) { list(
  mean = x$Media,
  sd = (x$Max - x$Min)/6
)}

expF<-function(x) {
  list(
    lambda =(1 / x$Media)
    )
}

binomialF<-function(x) {
    list(
      exitos=(x$Media / x$Max),
      size=x$Max
    )}

functionGenerarDistrib<-function(x){
  rnorm(input$nSim, p$mean, p$sd)
}

# ------ Contenedor de datos -------
ContenedorDatos <- R6::R6Class("ContenedorDatos",
                               public = list(
                                 Datos = list(),
                                 
                                 guardarTabla = function(nombre, tabla) {
                                   self$Datos[[nombre]] <- as.data.frame(tabla)
                                 },
                                 
                                 obtenerDatos = function(nombre) {
                                   return(self$Datos[[nombre]])
                                 },
                                 DeleteDatos = function(nombre) {
                                   if(is.null(nombre)) {self$Datos <- list()}
                                   else {
                                     if (nombre %in% names(self$Datos)) {
                                       self$Datos[[nombre]] <- NULL
                                     } else {
                                       self$Datos <- list()
                                     }
                                   }
                                 }
                               )
)


#------- Contenedor de parametros ---------------
ParametrosPriorDist <- R6Class("ParametrosPriorDist",
                               public = list(
                                 ParaDatos = list(),
                                 
                                 set_parametros = function(distribucion, parametros) {
                                   self$ParaDatos[[distribucion]] <- parametros
                                 },
                                 
                                 get_parametros = function(distribucion) {
                                   return(self$ParaDatos[[distribucion]] %||% self$valores_por_defecto(distribucion))
                                 },
                                 
                                 valores_por_defecto = function(distribucion) {
                                   switch(distribucion,
                                          "Normal" = list(mu = 0, sigma = 1),
                                          "Lognormal" = list(mu = 0, sigma = 1),
                                          "Gamma" = list(alpha = 2, beta = 1),
                                          "Exponencial" = list(rate = 1),
                                          "Beta" = list(alpha = 2, beta = 2, min = 0, max = 1),
                                          "Triangular" = list(min = 0, max = 1, mode = 0.5),
                                          "PERT" = list(min = 0, max = 1, mode = 0.5, shape = 4)
                                   )
                                 }
                               )
)

                               
#---- Instancia de la clase para ParametrosPriorDist ---------#

Contener <- ContenedorDatos$new() # Crear una instancia de la clase ContenedorDatos
parametros_guardados <- ParametrosPriorDist$new()


# Guardar parámetros
#parametros_guardados$set_parametros("Normal", list(mean = 5, sd = 2))
#parametros_guardados$set_parametros("Gamma", list(shape = 3, rate = 1.2))
#nombres_distribuciones <- parametros_guardados$get_nombresDist()
# Consultar
#parametros_guardados$get_parametros("Normal")
# $mean
# [1] 5
# $sd
# [1] 2

# Ver todo
#parametros_guardados$print_todos()



# ------ Función cuantil para la distribución triangular ----

qtriang <- function(p, min, max, mode) {
  # Aplicamos la fórmula de la distribución triangular
  
  q <- ifelse(p <= (mode - min) / (max - min),
              min + sqrt(p * (max - min) * (mode - min)),
              max - sqrt((1 - p) * (max - min) * (max - mode)))
  return(q)
}

# Función rtriang para generar n-números aleatorios con la distribución triangular
rtriang <- function(n, min, max, mode) {
  p <- runif(n)  # Generamos un vector de números uniformemente distribuidos
  return(qtriang(p, min, max, mode))  # Aplicamos la función cuantil
}



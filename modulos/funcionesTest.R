# Test de bondad de ajuste
# === Función KS ===
ks_test_dist <- function(x, dist) {
  tryCatch({
    if (dist == "Normal") {
      return(ks.test(x, "pnorm", mean = mean(x), sd = sd(x)))
    } else if (dist == "Lognormal") {
      x_pos <- x[x > 0]
      return(ks.test(x_pos, "plnorm", meanlog = mean(log(x_pos)), sdlog = sd(log(x_pos))))
    } else if (dist == "Gamma") {
      m <- mean(x); v <- var(x)
      shape <- m^2 / v; rate <- m / v
      return(ks.test(x, "pgamma", shape = shape, rate = rate))
    } else if (dist == "Poisson") {
      return(ks.test(round(x), "ppois", lambda = as.integer(mean(round(x)))))
    } else if (dist == "Uniforme") {
      return(ks.test(x, "punif", min = min(x), max = max(x)))
    } else if (dist == "Beta") {
      x_norm <- (x - min(x)) / (max(x) - min(x))
      return(ks.test(x_norm, "pbeta", shape1 = 2, shape2 = 2))
    } else if (dist == "Exponencial") {
      rate <- 1 / mean(x)
      return(ks.test(x, "pexp", rate = rate))
    } else if (dist == "Binomial") {
      x_round <- round(x)
      n <- max(x_round)
      p <- mean(x_round) / n
      return(ks.test(x_round, "pbinom", size = n, prob = p))
    } else if (dist == "PERT") {
      a <- min(x); b <- max(x); mode <- mean(x)
      mean_pert <- (a + 4 * mode + b) / 6
      alpha <- (mean_pert - a) * (2 * mode - a - b) / ((mode - mean_pert) * (b - a))
      beta <- alpha * (b - mean_pert) / (mean_pert - a)
      x_norm <- (x - a) / (b - a)
      return(ks.test(x_norm, "pbeta", shape1 = alpha, shape2 = beta))
    } else if (dist == "Triangular") {
      a <- min(x); b <- max(x); c <- mean(x)
      p_tri <- function(q) {
        ifelse(q < a, 0,
               ifelse(q <= c, ((q - a)^2) / ((b - a) * (c - a)),
                      1 - ((b - q)^2) / ((b - a) * (b - c))))
      }
      return(ks.test(x, p_tri))
    } else {
      return(NA)
    }
  }, error = function(e) NA)  # Capturamos el error y devolvemos NA
}

# === Función Chi2 ===
chi_test_dist <- function(x, dist, bins) {
  tabla_obs <- table(cut(x, breaks = bins, include.lowest = TRUE))
  tryCatch({
    if (dist == "Normal") {
      pbin <- pnorm(bins, mean = mean(x), sd = sd(x))
    } else if (dist == "Lognormal") {
      pbin <- plnorm(bins, meanlog = mean(log(x)), sdlog = sd(log(x)))
    } else if (dist == "Gamma") {
      m <- mean(x); v <- var(x)
      shape <- m^2 / v; rate <- m / v
      pbin <- pgamma(bins, shape = shape, rate = rate)
    } else if (dist == "Poisson") {
      x_round <- round(x)
      tabla_obs <- table(factor(x_round, levels = min(x_round):max(x_round)))
      tabla_esp <- dpois(as.integer(names(tabla_obs)), lambda = mean(x_round)) * length(x)
      return(chisq.test(as.numeric(tabla_obs), p = tabla_esp / sum(tabla_esp), rescale.p = TRUE))
    } else if (dist == "Uniforme") {
      return(chisq.test(as.numeric(tabla_obs), p = rep(1 / length(tabla_obs), length(tabla_obs))))
    } else if (dist == "Exponencial") {
      rate <- 1 / mean(x)
      pbin <- pexp(bins, rate = rate)
    } else if (dist == "Binomial") {
      x_round <- round(x)
      tabla_obs <- table(factor(x_round, levels = min(x_round):max(x_round)))
      n <- max(x_round)
      p <- mean(x_round) / n
      tabla_esp <- dbinom(as.integer(names(tabla_obs)), size = n, prob = p) * length(x)
      return(chisq.test(as.numeric(tabla_obs), p = tabla_esp / sum(tabla_esp), rescale.p = TRUE))
    } else if (dist == "Beta") {
      pbin <- pbeta((bins - min(x)) / (max(x) - min(x)), shape1 = 2, shape2 = 2)
    } else if (dist == "PERT") {
      a <- min(x); b <- max(x); mode <- mean(x)
      mean_pert <- (a + 4 * mode + b) / 6
      alpha <- (mean_pert - a) * (2 * mode - a - b) / ((mode - mean_pert) * (b - a))
      beta <- alpha * (b - mean_pert) / (mean_pert - a)
      pbin <- pbeta((bins - a) / (b - a), shape1 = alpha, shape2 = beta)
    } else if (dist == "Triangular") {
      a <- min(x); b <- max(x); c <- mean(x)
      p_tri <- function(q) {
        ifelse(q < a, 0,
               ifelse(q <= c, ((q - a)^2) / ((b - a) * (c - a)),
                      1 - ((b - q)^2) / ((b - a) * (b - c))))
      }
      pbin <- sapply(bins, p_tri)
    } else {
      return(NA)
    }
    
    return(chisq.test(as.numeric(tabla_obs), p = diff(pbin), rescale.p = TRUE))
  }, error = function(e) NA)  # Capturamos el error y devolvemos NA
}


## funcion global #
testDistribuciones <- function(x, distribuciones) {
  bins <- pretty(x, n = 10)
  resultados_df <- data.frame(Distribucion = character(0),
                              KS = numeric(0),
                              Chi2 = numeric(0),
                              stringsAsFactors = FALSE)
  
 for (dist in distribuciones) {
    ks_result <- suppressWarnings(ks_test_dist(x, dist))
    chi_result <- suppressWarnings(chi_test_dist(x, dist, bins))
    
  resultados_df <- rbind(resultados_df, data.frame(
      Distribucion = dist,
      KS = if (inherits(ks_result, "htest")) round(ks_result$p.value, 5) else NA,
      Chi2 = if (inherits(chi_result, "htest")) round(chi_result$p.value,5) else NA
    ))
  
 }
 resultados_df
     }

# === Función gráfico de densidad acumulada ===
# incluye suma de cuadrados (SC)

cdfs_SC_ggplot <- function(x, distribuciones) {
  x<-x[!is.na(x)]
  cdf_empirica <- ecdf(x)
  xx <- seq(min(x), max(x), length.out = 500)
  
  # Datos empíricos
  data_total <- data.frame(
    x = sort(x),
    y = cdf_empirica(sort(x)),
    Distribucion = "Empírica"
  )
  
  suma_cuadrados_error <- numeric(length(distribuciones))
  
  # Lista para almacenar los parámetros de cada distribución
  parametros_distribuciones <- list()
  
  for (i in seq_along(distribuciones)) {
    dist <- distribuciones[i]
    try({
      if (dist == "Normal") {
        mean_val <- mean(x)
        sd_val <- sd(x)
        cdf_teorica <- pnorm(xx, mean = mean_val, sd = sd_val)
        parametros_distribuciones[[dist]] <- list(mu = mean_val, sigma = sd_val)
        
      } else if (dist == "Lognormal" && all(x > 0)) {
        meanlog_val <- mean(log(x))
        sdlog_val <- sd(log(x))
        cdf_teorica <- plnorm(xx, meanlog = meanlog_val, sdlog = sdlog_val)
        parametros_distribuciones[[dist]] <- list(mu = meanlog_val, sigma = sdlog_val)
        
      } else if (dist == "Gamma") {
        m <- mean(x)
        v <- var(x)
        cdf_teorica <- pgamma(xx, shape = m^2 / v, rate = m / v)
        parametros_distribuciones[[dist]] <- list(alpha = m^2 / v, beta = m / v)
        
      } else if (dist == "Poisson") {
        lambda <- mean(round(x))
        cdf_teorica <- ppois(round(xx), lambda = lambda)
        parametros_distribuciones[[dist]] <- list(lambda = lambda)
        
      } else if (dist == "Uniforme") {
        min_val <- min(x)
        max_val <- max(x)
        cdf_teorica <- punif(xx, min = min_val, max = max_val)
        parametros_distribuciones[[dist]] <- list(Min = min_val, Max = max_val)
        
      } else if (dist == "Exponencial") {
        rate <- 1 / mean(x)
        cdf_teorica <- pexp(xx, rate = rate)
        parametros_distribuciones[[dist]] <- list(rate = rate)
        
      } else if (dist == "Binomial") {
        x_round <- round(x)
        n <- max(x_round)
        p_binom <- mean(x_round) / n
        xx_int <- floor(seq(min(x_round), max(x_round), length.out = 500))
        cdf_teorica <- pbinom(xx_int, size = n, prob = p_binom)
        parametros_distribuciones[[dist]] <- list(size = n, prob = p_binom)
        
      } else if (dist == "Beta") {
        if(min(x)>=0 && max(x)<=1) {
          x_scaled<-mean(x)
        } else {
          
          x_scaled <- (mean(x) - min(x)) / (max(x) - min(x))
        }
        #var_std <- 1/36
          shape1 = x_scaled * 15
          shape2 = (1 - x_scaled) * 15
       
        cdf_teorica <- pbeta(xx, shape1 = shape1, shape2 = shape2)
        parametros_distribuciones[[dist]] <- list(alpha = shape1, beta = shape2, min=min(x), max=max(x))
        
      } else if (dist == "PERT") {
        a <- min(x)
        b <- max(x)
        mode <- mean(x)
        mean_pert <- (a + 4 * mode + b) / 6
        alpha <- (mean_pert - a) * (2 * mode - a - b) / ((mode - mean_pert) * (b - a))
        beta <- alpha * (b - mean_pert) / (mean_pert - a)
        x_scaled <- (xx - a) / (b - a)
        cdf_teorica <- pbeta(x_scaled, shape1 = alpha, shape2 = beta)
        parametros_distribuciones[[dist]] <- list(min = a, max = b, mode = mean(x), shape=4)
        
      } else if (dist == "Triangular") {
        a <- min(x)
        b <- max(x)
        c <- mean(x)
        p_tri <- function(q) {
          ifelse(q < a, 0,
                 ifelse(q <= c, ((q - a)^2) / ((b - a) * (c - a)),
                        1 - ((b - q)^2) / ((b - a) * (b - c))))
        }
        cdf_teorica <- sapply(xx, p_tri)
        parametros_distribuciones[[dist]] <- list(min = a, max = b, mode = c)
        
      } else {
        next
      }
      
      # Calcular SSE
      diff_cuadrada <- (cdf_empirica(xx) - cdf_teorica)^2
      suma_cuadrados_error[i] <- sum(diff_cuadrada)
      
      # Agregar datos a conjunto general
      nombre_con_sse <- paste0(dist, " (SSE: ", round(suma_cuadrados_error[i], 3), ")")
      
      data_total <- rbind(data_total,
                          data.frame(x = xx,
                                     y = cdf_teorica,
                                     Distribucion = nombre_con_sse))
      
    }, silent = TRUE)
  }
  
  # Crear gráfico
  grafico<-ggplot(data_total, aes(x = x, y = y, color = Distribucion)) +
    geom_step(data = subset(data_total, Distribucion == "Empírica"), linewidth = 1.3, direction = "hv") +
    geom_line(data = subset(data_total, Distribucion != "Empírica"), linewidth = 1) +
    labs(title = "Comparación de CDFs", x = "x", y = "F(x)") +
    theme_minimal() +
    theme(legend.title = element_blank())
  
  return(list(
    grafico = grafico,
    parametros = parametros_distribuciones
  ))
}


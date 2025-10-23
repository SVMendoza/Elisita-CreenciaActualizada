# ------------------Función para precompilar modelos ---------------------------------------
precompile_models <- function() {
  # Lista de combinaciones prior/likelihood que quieres precompilar
  common_combinations <- list(
    c("normal", "normal"),
    c("normal", "lognormal"),
    c("lognormal", "normal"),
    c("exponential", "exponential"),
    c("gamma", "gamma"),
    c("beta", "beta"),
    c("triangular", "normal"),
    c("pert", "normal"),
c("triangular", "lognormal"),
c("pert", "lognormal"),
c("triangular","poisson"),
c("triangular","beta"),
c("pert","beta"),
c("pert","binomial")



     )
  
  compiled_models <- list()
  
  # Mostrar mensaje de progreso
  withProgress(message = 'Precompilando modelos', value = 0, {
    n_models <- length(common_combinations)
    
    for (i in seq_along(common_combinations)) {
      prior <- common_combinations[[i]][1]
      likelihood <- common_combinations[[i]][2]
      model_key <- paste(prior, likelihood, sep = "_")
      
      setProgress(
        value = i/n_models,
        detail = paste("Compilando:", prior, "+", likelihood)
      )
      
      tryCatch({
        stan_code <- build_stan_code(prior, likelihood)
        temp_file <- tempfile(fileext = ".stan")
        writeLines(stan_code, temp_file)
        compiled_models[[model_key]] <- rstan::stan_model(file = temp_file)
        file.remove(temp_file)
      }, error = function(e) {
        warning("Error al compilar ", model_key, ": ", e$message)
      })
    }
  })
  
  return(compiled_models)
}


# ---------------------------------------------------------------------------

build_stan_code <- function(prior, likelihood, is_bounded = FALSE) {
  ## Validación de entradas
  valid_priors <- c("normal", "lognormal", "exponential", "gamma", "beta", "triangular", "pert")
  valid_likelihoods <- c("normal", "lognormal", "exponential", "gamma", "beta", "poisson", "binomial")
  
  if (!prior %in% valid_priors) {
    stop("Prior no válido. Opciones válidas: ", paste(valid_priors, collapse = ", "))
  }
  
  if (!likelihood %in% valid_likelihoods) {
    stop("Likelihood no válido. Opciones válidas: ", paste(valid_likelihoods, collapse = ", "))
  }
  
  # Determinar si los datos son discretos o continuos
  is_discrete <- likelihood %in% c("poisson", "binomial")
  
  ## 1. Bloque de funciones personalizadas
  functions_block <- switch(prior,
                            "triangular" = '
    real triangular_lpdf(real x, real a, real b, real c) {
      if (x < a || x > b) return negative_infinity();
      return (x < c) ? 
        log(2 * (x - a) / ((b - a) * (c - a))) :
        log(2 * (b - x) / ((b - a) * (b - c)));
    }',
                            "pert" = '
    real pert_lpdf(real x, real a, real b, real m, real s) {
      real alpha = 1 + s * (m - a)/(b - a);
      real beta = 1 + s * (b - m)/(b - a);
      return beta_lpdf((x - a)/(b - a) | alpha, beta) - log(b - a);
    }',
                            ""
  )
  
  # Forzar is_bounded para priors que lo requieren
  if (prior %in% c("triangular", "pert", "beta")) {
    is_bounded <- TRUE
  }
  
  ## 2. Bloque de datos (con sintaxis moderna)
  data_block <- sprintf('
  data {
    int<lower=0> n_obs;
    %s
    %s
    %s
  }',
                        ifelse(is_discrete, "array[n_obs] int y_int;", "array[n_obs] real y;"),
                        
                        # Parámetros del prior
                        switch(prior,
                               "normal" = "real prior_mu;\n    real<lower=0> prior_sigma;",
                               "lognormal" = "real prior_logmu;\n    real<lower=0> prior_logsigma;",
                               "exponential" = "real<lower=0> prior_rate;",
                               "gamma" = "real<lower=0> gamma_alpha_prior;\n    real<lower=0> gamma_beta_prior;",
                               "beta" = "real<lower=0> beta_alpha_prior;\n    real<lower=0> beta_beta_prior;\n    real prior_min;\n    real prior_max;",
                               "triangular" = "real prior_min;\n    real prior_max;\n    real prior_mode_tri;",
                               "pert" = "real prior_min;\n    real prior_max;\n    real prior_mode_pert;\n    real<lower=0> pert_shape;"
                        ),
                        
                        # Parámetros adicionales del likelihood
                        switch(likelihood,
                               "binomial" = "int<lower=0> n_ensayos;",
                               "beta" = "real<lower=0> beta_alpha;\n    real<lower=0> beta_beta;",
                               "gamma" = "real<lower=0> gamma_alpha_hyper;\n    real<lower=0> gamma_beta_hyper;",
                               ""
                        )
  )
  
  ## 3. Bloque de parámetros
  param_block <- sprintf('
  parameters {
    %s
    %s
    %s
  }',
                         ifelse(is_bounded, "real theta_raw;", "real theta;"),
                         
                         # Sigma para modelos normales
                         ifelse(likelihood %in% c("normal", "lognormal"), "real<lower=0> sigma;", ""),
                         
                         # Parámetro adicional para gamma
                         ifelse(likelihood == "gamma", "real<lower=0> gamma_alpha;", "")
  )
  
  ## 4. Bloque de parámetros transformados
  trans_block <- sprintf('
  transformed parameters {
    real theta_trans;
    %s
    %s
    %s
  }',
                         ifelse(is_bounded, 
                                "theta_trans = prior_min + (prior_max - prior_min) * inv_logit(theta_raw);",
                                "theta_trans = theta;"),
                         
                         ifelse(likelihood == "gamma", "real<lower=0> gamma_beta = theta_trans;", ""),
                         
                         ifelse(likelihood == "lognormal", "real mu = log(theta_trans);", "")
  )
  
  ## 5. Bloque del modelo
  prior_stmt <- switch(prior,
                       "normal" = "theta_trans ~ normal(prior_mu, prior_sigma);",
                       "lognormal" = "theta_trans ~ lognormal(prior_logmu, prior_logsigma);",
                       "exponential" = "theta_trans ~ exponential(prior_rate);",
                       "gamma" = "theta_trans ~ gamma(gamma_alpha_prior, gamma_beta_prior);",
                       "beta" = "theta_trans ~ beta(beta_alpha_prior, beta_beta_prior);",
                       "triangular" = "target += triangular_lpdf(theta_trans | prior_min, prior_max, prior_mode_tri);",
                       "pert" = "target += pert_lpdf(theta_trans | prior_min, prior_max, prior_mode_pert, pert_shape);"
  )
  
  likelihood_stmt <- switch(likelihood,
                            "normal" = "y ~ normal(theta_trans, sigma);",
                            "lognormal" = "y ~ lognormal(mu, sigma);",
                            "exponential" = "y ~ exponential(theta_trans);",
                            "gamma" = "y ~ gamma(gamma_alpha, gamma_beta);",
                            "beta" = "y ~ beta(beta_alpha, beta_beta);",
                            "poisson" = "y_int ~ poisson(exp(theta_trans));",
                            "binomial" = "y_int ~ binomial(n_ensayos, theta_trans);"
  )
  
  model_block <- sprintf('
  model {
    // Prior
    %s
    
    // Priors para parámetros adicionales
    %s
    %s
    
    // Likelihood
    %s
  }',
                         prior_stmt,
                         ifelse(likelihood %in% c("normal", "lognormal"), "sigma ~ cauchy(0, 1);", ""),
                         ifelse(likelihood == "gamma", "gamma_alpha ~ gamma(2, 1);", ""),
                         likelihood_stmt
  )
  
  ## 6. Bloque de cantidades generadas (opcional)
  gq_block <- sprintf('
  generated quantities {
    vector[n_obs] log_lik;
    for (i in 1:n_obs) {
      %s
    }
  }',
                      switch(likelihood,
                             "normal" = "log_lik[i] = normal_lpdf(y[i] | theta_trans, sigma);",
                             "lognormal" = "log_lik[i] = lognormal_lpdf(y[i] | mu, sigma);",
                             "exponential" = "log_lik[i] = exponential_lpdf(y[i] | theta_trans);",
                             "gamma" = "log_lik[i] = gamma_lpdf(y[i] | gamma_alpha, gamma_beta);",
                             "beta" = "log_lik[i] = beta_lpdf(y[i] | beta_alpha, beta_beta);",
                             "poisson" = "log_lik[i] = poisson_lpmf(y_int[i] | exp(theta_trans));",
                             "binomial" = "log_lik[i] = binomial_lpmf(y_int[i] | n_ensayos, theta_trans);"
                      )
  )
  
  ## Ensamblar el código completo
  stan_code <- paste(
    ifelse(nchar(functions_block) > 0, paste("functions {", functions_block, "}\n", sep = "\n"), ""),
    data_block,
    param_block,
    trans_block,
    model_block,
    gq_block,
    sep = "\n\n"
  )
  
  return(stan_code)
}



###################
bayesian_inference <- function(prior_name, prior_params,
                               likelihood_name, data,
                               total_trials = NULL,
                               iter = iter, chains = chains,
                               control = list(adapt_delta = 0.95),
                               seed = 123) {
  
  # 1. Validación inicial
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop("Por favor instala el paquete 'rstan': install.packages('rstan')")
  }
  
  # 2. Generar código Stan
  stan_code <- build_stan_code(prior_name, likelihood_name)
  
  # 3. Preparar datos de manera robusta
  is_discrete <- likelihood_name %in% c("poisson", "binomial")
  n <- length(data)
  
  stan_data <- list(
    n_obs = n,
    y = if (!is_discrete) as.array(as.numeric(data)) else array(numeric(0), dim = c(0)),
    y_int = if (is_discrete) as.array(as.integer(data)) else array(integer(0), dim = c(0))
  )
  
  # Añadir parámetros del prior
  stan_data <- switch(prior_name,
                      "normal" = c(stan_data, list(
                        prior_mu = prior_params$mu,
                        prior_sigma = prior_params$sigma
                      )),
                      "lognormal" = c(stan_data, list(
                        prior_logmu = prior_params$mu,
                        prior_logsigma = prior_params$sigma
                      )),
                      "exponential" = c(stan_data, list(
                        prior_rate = prior_params$rate
                      )),
                      "gamma" = c(stan_data, list(
                        gamma_alpha_prior = prior_params$alpha,
                        gamma_beta_prior = prior_params$beta
                      )),
                      "beta" = c(stan_data, list(
                        beta_alpha_prior = prior_params$alpha,
                        beta_beta_prior = prior_params$beta,
                        prior_min = prior_params$min,
                        prior_max = prior_params$max
                      )),
                      "triangular" = c(stan_data, list(
                        prior_min = prior_params$min,
                        prior_max = prior_params$max,
                        prior_mode_tri = prior_params$mode
                      )),
                      "pert" = c(stan_data, list(
                        prior_min = prior_params$min,
                        prior_max = prior_params$max,
                        prior_mode_pert = prior_params$mode,
                        pert_shape = prior_params$shape
                      ))
  )
  
  # Parámetros adicionales del likelihood
  if (likelihood_name == "binomial") {
    stan_data$n_ensayos <- total_trials
  } else if (likelihood_name == "gamma") {
    stan_data$gamma_alpha_hyper <- 2  # Hiperparámetros por defecto
    stan_data$gamma_beta_hyper <- 1
  }
  
  # 4. Manejo robusto de la ejecución
  tryCatch({
    # Crear archivo temporal para diagnóstico
    temp_file <- tempfile(fileext = ".stan")
    writeLines(stan_code, temp_file)
    
    # Compilación con verificación
    message("Compilando modelo Stan...")
    model <- rstan::stan_model(file = temp_file)
    
    # Ejecución con controles
    message("Muestreo en progreso...")
    fit <- rstan::sampling(
      object = model,
      data = stan_data,
      iter = iter,
      chains = chains,
      control = control,
      seed = seed,
      refresh = max(iter/10, 1)  # Mostrar progreso
    )
    
    # Verificar convergencia
    #check_hmc_diagnostics(fit)
    
    # Extraer resultados
    samples <- rstan::extract(fit)$theta_trans
    
    # Resultados estructurados
    list(
      summary = list(
        mean = mean(samples),
        median = median(samples),
        sd = sd(samples),
        hdi_95 = HDInterval::hdi(samples, credMass = 0.95)
      ),
      diagnostics = rstan::get_sampler_params(fit),
      stan_code = stan_code,
      model = model,
      fit = fit
    )
    
  }, error = function(e) {
    # Manejo detallado de errores
    error_msg <- paste(
      "Error en la inferencia bayesiana:\n",
      "Prior:", prior_name, "\n",
      "Likelihood:", likelihood_name, "\n",
      "Mensaje:", e$message, "\n\n",
      "Sugerencias:\n",
      "1. Verifica que todos los parámetros requeridos estén definidos\n",
      "2. Revisa que los datos sean del tipo correcto\n",
      "3. Intenta reducir 'iter' o 'chains' para diagnóstico\n",
      "4. Verifica que tengas suficiente memoria disponible"
    )
    
    stop(error_msg, call. = FALSE)
  }, finally = {
    # Limpieza de archivos temporales
    if (exists("temp_file") && file.exists(temp_file)) {
      file.remove(temp_file)
    }
  })
}


# ---------------------------- modelo precompilados

bayesian_inference_prec <- function(prior_name, prior_params,
                             likelihood_name, data,
                             total_trials = NULL,
                             iter = iter, chains = chains,
                             control = list(adapt_delta = 0.95),
                             seed = 123,
                             compiled_models = NULL) {
  
  # Generar clave del modelo
  model_key <- paste(prior_name, likelihood_name, sep = "_")
  
  # Verificar si tenemos el modelo precompilado
  if (!is.null(compiled_models)) {
    if (model_key %in% names(compiled_models)) {
      model <- compiled_models[[model_key]]
    } else {
      # Si no está precompilado, compilar sobre la marcha
      stan_code <- build_stan_code(prior_name, likelihood_name)
      temp_file <- tempfile(fileext = ".stan")
      writeLines(stan_code, temp_file)
      model <- rstan::stan_model(file = temp_file)
      file.remove(temp_file)
    }
  } else {
    # Si no se proporcionan modelos precompilados
    stan_code <- build_stan_code(prior_name, likelihood_name)
    temp_file <- tempfile(fileext = ".stan")
    writeLines(stan_code, temp_file)
    model <- rstan::stan_model(file = temp_file)
    file.remove(temp_file)
  }

 # 3. Preparar datos de manera robusta
  is_discrete <- likelihood_name %in% c("poisson", "binomial")
  n <- length(data)
  
  stan_data <- list(
    n_obs = n,
    y = if (!is_discrete) as.array(as.numeric(data)) else array(numeric(0), dim = c(0)),
    y_int = if (is_discrete) as.array(as.integer(data)) else array(integer(0), dim = c(0))
  )
  
  # Añadir parámetros del prior
  stan_data <- switch(prior_name,
                      "normal" = c(stan_data, list(
                        prior_mu = prior_params$mu,
                        prior_sigma = prior_params$sigma
                      )),
                      "lognormal" = c(stan_data, list(
                        prior_logmu = prior_params$mu,
                        prior_logsigma = prior_params$sigma
                      )),
                      "exponential" = c(stan_data, list(
                        prior_rate = prior_params$rate
                      )),
                      "gamma" = c(stan_data, list(
                        gamma_alpha_prior = prior_params$alpha,
                        gamma_beta_prior = prior_params$beta
                      )),
                      "beta" = c(stan_data, list(
                        beta_alpha_prior = prior_params$alpha,
                        beta_beta_prior = prior_params$beta,
                        prior_min = prior_params$min,
                        prior_max = prior_params$max
                      )),
                      "triangular" = c(stan_data, list(
                        prior_min = prior_params$min,
                        prior_max = prior_params$max,
                        prior_mode_tri = prior_params$mode
                      )),
                      "pert" = c(stan_data, list(
                        prior_min = prior_params$min,
                        prior_max = prior_params$max,
                        prior_mode_pert = prior_params$mode,
                        pert_shape = prior_params$shape
                      ))
  )
  
  # Parámetros adicionales del likelihood
  if (likelihood_name == "binomial") {
    stan_data$n_ensayos <- total_trials
  } else if (likelihood_name == "gamma") {
    stan_data$gamma_alpha_hyper <- 2  # Hiperparámetros por defecto
    stan_data$gamma_beta_hyper <- 1
  }
  
  # 4. Manejo robusto de la ejecución
  tryCatch({
    # Crear archivo temporal para diagnóstico
    temp_file <- tempfile(fileext = ".stan")
    writeLines(stan_code, temp_file)
    
    # Compilación con verificación
    message("Compilando modelo Stan...")
    model <- rstan::stan_model(file = temp_file)
    
    # Ejecución con controles
    message("Muestreo en progreso...")
    fit <- rstan::sampling(
      object = model,
      data = stan_data,
      iter = iter,
      chains = chains,
      control = control,
      seed = seed,
      refresh = max(iter/10, 1)  # Mostrar progreso
    )
    
    # Verificar convergencia
    #check_hmc_diagnostics(fit)
    
    # Extraer resultados
    samples <- rstan::extract(fit)$theta_trans
    
    # Resultados estructurados
    list(
      summary = list(
        mean = mean(samples),
        median = median(samples),
        sd = sd(samples),
        hdi_95 = HDInterval::hdi(samples, credMass = 0.95)
      ),
      diagnostics = rstan::get_sampler_params(fit),
      stan_code = stan_code,
      model = model,
      fit = fit
    )
    
  }, error = function(e) {
    # Manejo detallado de errores
    error_msg <- paste(
      "Error en la inferencia bayesiana:\n",
      "Prior:", prior_name, "\n",
      "Likelihood:", likelihood_name, "\n",
      "Mensaje:", e$message, "\n\n",
      "Sugerencias:\n",
      "1. Verifica que todos los parámetros requeridos estén definidos\n",
      "2. Revisa que los datos sean del tipo correcto\n",
      "3. Intenta reducir 'iter' o 'chains' para diagnóstico\n",
      "4. Verifica que tengas suficiente memoria disponible"
    )
    
    stop(error_msg, call. = FALSE)
  }, finally = {
    # Limpieza de archivos temporales
    if (exists("temp_file") && file.exists(temp_file)) {
      file.remove(temp_file)
    }
  })
}

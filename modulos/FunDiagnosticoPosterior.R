library(bayesplot)
check_stan_diagnostics <- function(Modelo, cred_mass) {
 
  PosMu<-rstan::extract(Modelo$fit)$theta_trans
  
  ICred<-HDInterval::hdi(PosMu, credMass=cred_mass)
  
  ICI<-c(paste0(((1-cred_mass)/2)*100,'%'), paste0((cred_mass+(1-cred_mass)/2)*100,'%'))
 
  cat("\n=== RESUMEN DE DIAGNÓSTICOS ===\n")
  rhats <- rhat(Modelo$fit)
  neff <- neff_ratio(Modelo$fit)
  # 1. Estadísticas básicas
  fit_summary <- c(unlist(Modelo$summary)[1:3],ICred,  n_eff=mean(neff), Rhat=mean(rhats)) 
   names(fit_summary)<-c("mean","median", "sd", ICI, "n_eff", "Rhat")
  cat("\n1. Estimaciones:\n")
    print(fit_summary)
  
  #print(summary(rhats))
  if (any(rhats > 1.05, na.rm = TRUE)) {
    warning("Algunos R-hat > 1.05 indican posible falta de convergencia")
  }
  if (any(neff < 0.1, na.rm = TRUE)) {
    warning("Algunos n_eff muy bajos (<10% del total)")
  }
  cat("\n4. Diagnóstico de divergencias:\n")
  sampler_params <- get_sampler_params(Modelo$fit, inc_warmup = FALSE)
  divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  cat("Divergencias:", divergences, "\n")
  if (divergences > 0) {
    warning(divergences, " divergencias detectadas. Considera aumentar adapt_delta.")
  }
cat("\n=== FIN DEL DIAGNÓSTICO ===\n")
}

###############Diagnostico
ffPostPlot<-function(Modelo, cred_mass){

mcmc.data<-mcmc_trace_data(Modelo$fit, 'theta_trans')
PosMu<-rstan::extract(Modelo$fit)$theta_trans
df<-data.frame(PosMu=PosMu)
mcmc.data$chain<-as.factor(mcmc.data$chain)

p0<-ggplot(mcmc.data, aes(x=iteration, y=value))+
geom_line()+facet_grid(chain~.)+ 
  ylab(expression(mu))+xlab('Iteration')+
  theme_minimal()

p1<-ggplot(df, aes(x = PosMu)) +
  geom_density(fill = "orange", alpha = 0.3) +
  geom_vline(xintercept = quantile(PosMu, probs = c((1 - cred_mass)/2, 1 - (1 - cred_mass)/2)),
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = mean(PosMu), color = "blue", linewidth = 1) +
  xlab(expression(mu))+ylab("Densidad") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
  theme_minimal()

return(grid.arrange(p1,p0, widths = c(1, 1)))
}

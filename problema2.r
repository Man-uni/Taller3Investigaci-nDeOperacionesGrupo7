# -----------------------------
# MODELO M/M/1 - Detective Columbo
# -----------------------------

# 1. Definir parámetros
lambda <- 1/4      # tasa de llegada (casos por semana)
mu <- 1/1.5        # tasa de servicio (casos por semana)

cat("Tasa de llegada (lambda):", lambda, "casos/semana\n")
cat("Tasa de servicio (mu):", mu, "casos/semana\n\n")

# 2. Calcular utilización
rho <- lambda / mu
cat("Utilización del sistema (rho):", rho, "\n")
cat("Porcentaje de tiempo ocupado:", rho * 100, "%\n\n")

# 3. Verificar estabilidad
if(rho < 1){
  cat("El sistema es ESTABLE (rho < 1)\n\n")
} else {
  cat("El sistema NO es estable (rho >= 1)\n\n")
}

# 4. Calcular número promedio en cola (Lq)
Lq <- rho^2 / (1 - rho)
cat("Promedio de casos en espera (Lq):", Lq, "\n\n")

# 5. Calcular tiempo promedio en el sistema (W)
W <- 1 / (mu - lambda)
cat("Tiempo promedio en el sistema (W):", W, "semanas\n\n")

# 6. Interpretación final
cat("----- INTERPRETACIÓN -----\n")
cat("En promedio hay", round(Lq, 3), "casos esperando.\n")
cat("El detective está ocupado el", round(rho * 100, 2), "% del tiempo.\n")
cat("Un caso tarda aproximadamente", round(W, 2), "semanas en resolverse.\n\n")

# 7. Conclusión
if(rho < 0.8){
  cat("CONCLUSIÓN: No se justifica contratar un asistente.\n")
} else {
  cat("CONCLUSIÓN: Podría considerarse contratar un asistente.\n")
}
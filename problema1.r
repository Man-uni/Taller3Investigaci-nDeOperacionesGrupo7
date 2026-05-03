# -----------------------------
# MODELO M/M/1 - John Macko
# -----------------------------

# 1. Parámetros
lambda <- 1/5   # llegadas (trabajos por día)
mu <- 1/4       # servicio (trabajos por día)

cat("Lambda (llegadas):", lambda, "\n")
cat("Mu (servicio):", mu, "\n\n")

# 2. Utilización
rho <- lambda / mu
cat("Utilización (rho):", rho, "\n")
cat("Porcentaje ocupado:", rho * 100, "%\n\n")

# 3. Estabilidad
if(rho < 1){
  cat("El sistema es ESTABLE\n\n")
} else {
  cat("El sistema NO es estable\n\n")
}

# (a) Probabilidad de sistema vacío (sin trabajos)
P0 <- 1 - rho
cat("(a) Probabilidad de no tener trabajos (P0):", P0, "\n\n")

# (b) Ingreso mensual
# tasa efectiva = lambda (no hay pérdidas)
trabajos_mes <- lambda * 30
ingreso_mensual <- trabajos_mes * 50

cat("(b) Trabajos promedio por mes:", trabajos_mes, "\n")
cat("Ingreso mensual promedio: $", ingreso_mensual, "\n\n")

# (c) Número promedio de trabajos en cola
Lq <- rho^2 / (1 - rho)
cat("(c) Trabajos promedio en espera (Lq):", Lq, "\n\n")

# (d) Tiempo promedio de espera en cola
Wq <- Lq / lambda
cat("(d) Tiempo promedio de espera (Wq):", Wq, "días\n\n")

# (e) Porcentaje de tiempo ocupado
cat("(e) Porcentaje de tiempo ocupado:", rho * 100, "%\n\n")

# Interpretación
cat("----- INTERPRETACIÓN -----\n")
cat("Probabilidad de estar sin trabajo:", round(P0, 3), "\n")
cat("Ingreso mensual estimado: $", ingreso_mensual, "\n")
cat("Promedio de trabajos en cola:", round(Lq, 3), "\n")
cat("Tiempo de espera promedio:", round(Wq, 2), "días\n")
cat("Ocupación:", round(rho * 100, 2), "%\n")
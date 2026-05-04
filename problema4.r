# Parámetros
lambda <- 20      # pacientes/hora
mu <- 7.5         # pacientes/hora
K <- 15           # capacidad total (14 espera + 1 en servicio)

rho <- lambda / mu

# Probabilidad estado 0
pi0 <- (1 - rho) / (1 - rho^(K + 1))

# Vector de probabilidades
pi <- numeric(K + 1)
for (n in 0:K) {
  pi[n + 1] <- pi0 * rho^n
}

# (a) Probabilidad de no esperar
P_no_espera <- pi[1]

# (b) Probabilidad de ocupar asiento (n=1 a 14)
P_asiento <- sum(pi[2:15])

# (c) Tiempo promedio en el sistema
P_bloqueo <- pi[K + 1]
lambda_eff <- lambda * (1 - P_bloqueo)

L <- sum((0:K) * pi)
W <- L / lambda_eff  # en horas

# (d) Ingreso diario
ingreso_diario <- lambda_eff * 25 * 8

# Resultados
cat("Resultados:\n")
cat("(a) Probabilidad de no esperar:", P_no_espera, "\n")
cat("(b) Probabilidad de ocupar un asiento:", P_asiento, "\n")
cat("(c) Tiempo total esperado (horas):", W, "\n")
cat("(c) Tiempo total esperado (min):", W * 60, "\n")
cat("(d) Ingreso medio diario ($):", ingreso_diario, "\n")
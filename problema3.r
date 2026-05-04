# Parámetros
lambda <- 10   # tasa de llegada (por hora)
mu <- 4        # tasa de servicio (por hora)
K <- 7         # capacidad máxima

rho <- lambda / mu

# Probabilidad de estado 0
pi0 <- (1 - rho) / (1 - rho^(K + 1))

# Vector de probabilidades
pi <- numeric(K + 1)
for (n in 0:K) {
  pi[n + 1] <- pi0 * rho^n
}

# (a) Probabilidad de bloqueo
P_bloqueo <- pi[K + 1]

# (b) Número promedio en sistema
L <- sum((0:K) * pi)

# (c) Tiempo promedio en sistema
lambda_eff <- lambda * (1 - P_bloqueo)
W <- L / lambda_eff  # en horas

# (d) Probabilidad de estar en posiciones 5,6,7
P_posiciones <- pi[5] + pi[6] + pi[7]  # n=4,5,6

# Resultados
cat("Resultados:\n")
cat("(a) Probabilidad de bloqueo:", P_bloqueo, "\n")
cat("(b) Promedio en la banda (L):", L, "\n")
cat("(c) Tiempo promedio en la banda (horas):", W, "\n")
cat("(c) Tiempo promedio en minutos:", W * 60, "\n")
cat("(d) Probabilidad posiciones 5,6 o 7:", P_posiciones, "\n")
# Parámetros
lambda <- 4     # por hora
mu <- 2         # por hora
c <- 3
K <- 18

a <- lambda / mu

# Probabilidades
pi <- numeric(K + 1)

# Calcular pi0 (normalización)
sumatoria <- 0

for (n in 0:K) {
  if (n < c) {
    sumatoria <- sumatoria + (a^n) / factorial(n)
  } else {
    sumatoria <- sumatoria + (a^n) / (factorial(c) * c^(n - c))
  }
}

pi0 <- 1 / sumatoria

# Calcular todas las probabilidades
for (n in 0:K) {
  if (n < c) {
    pi[n + 1] <- (a^n / factorial(n)) * pi0
  } else {
    pi[n + 1] <- (a^n / (factorial(c) * c^(n - c))) * pi0
  }
}

# (a) Mecánicos ociosos
Ls <- sum(sapply(0:K, function(n) min(n, c) * pi[n + 1]))
mecanicos_ociosos <- c - Ls

# (b) Clientes perdidos
P_bloqueo <- pi[K + 1]
lambda_eff <- lambda * (1 - P_bloqueo)
perdidos_por_hora <- lambda - lambda_eff
perdidos_dia <- perdidos_por_hora * 10

# (c) Probabilidad de al menos un mecánico libre
P_libre <- sum(pi[1:c])

# (d) Promedio en cola
Lq <- sum(sapply(0:K, function(n) max(0, n - c) * pi[n + 1]))

# (e) Productividad
productividad <- (lambda_eff / (c * mu)) * 100

# Resultados
cat("Resultados:\n")
cat("(a) Mecánicos sin trabajo:", mecanicos_ociosos, "\n")
cat("(b) Clientes perdidos por día:", perdidos_dia, "\n")
cat("(c) Probabilidad de al menos un mecánico libre:", P_libre, "\n")
cat("(d) Promedio de segadoras esperando:", Lq, "\n")
cat("(e) Productividad (%):", productividad, "\n")
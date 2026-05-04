# Parámetros
lambda <- 12
mu <- 60/13
c <- 3

a <- lambda / mu
rho <- lambda / (c * mu)

# Calcular P0
sum1 <- sum((a^(0:(c-1))) / factorial(0:(c-1)))
sum2 <- (a^c / factorial(c)) * (1 / (1 - rho))
P0 <- 1 / (sum1 + sum2)

# Probabilidad de espera (Erlang C)
P_espera <- (a^c / factorial(c)) * (1 / (1 - rho)) * P0

# Lq
Lq <- P_espera * (rho / (1 - rho))

# Wq
Wq <- Lq / lambda

# W total
W <- Wq + (1 / mu)

# Probabilidades pi_n
pi <- numeric(20)

for (n in 0:19) {
  if (n < c) {
    pi[n+1] <- (a^n / factorial(n)) * P0
  } else {
    pi[n+1] <- (a^n / (factorial(c) * c^(n-c))) * P0
  }
}

# (d)
P_tercera_pos <- 1 - sum(pi[1:3])

# (e)
Ls <- a
ociosos <- c - Ls

# Resultados
cat("Resultados:\n")
cat("(a) Utilización:", rho, "\n")
cat("(b) Lq (espera promedio):", Lq, "\n")
cat("(c) Tiempo total W (horas):", W, "\n")
cat("(c) Tiempo total W (min):", W * 60, "\n")
cat("(d) Probabilidad de tercera posición:", P_tercera_pos, "\n")
cat("(e) Médicos ociosos promedio:", ociosos, "\n")
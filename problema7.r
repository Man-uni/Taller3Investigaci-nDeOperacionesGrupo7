calcular_modelo <- function(c) {
  N <- 22
  lambda <- 0.5   # por máquina
  mu <- 5

  pi <- numeric(N + 1)
  pi[1] <- 1  # pi0 sin normalizar

  # Construcción recursiva
  for (n in 1:N) {
    lambda_n <- (N - (n - 1)) * lambda
    mu_n <- min(n, c) * mu
    pi[n + 1] <- pi[n] * (lambda_n / mu_n)
  }

  # Normalización
  pi <- pi / sum(pi)

  # Longitud promedio
  L <- sum((0:N) * pi)

  # Servidores ocupados promedio
  Ls <- sum(sapply(0:N, function(n) min(n, c) * pi[n + 1]))

  return(list(pi = pi, L = L, Ls = Ls))
}

# ===== c = 4 =====
res4 <- calcular_modelo(4)
mecanicos_ociosos_4 <- 4 - res4$Ls

# ===== c = 3 =====
res3 <- calcular_modelo(3)
pi3 <- res3$pi

# (b)
P_todos_ociosos <- pi3[1]

# (c)
P_al_menos_2_ociosos <- pi3[1] + pi3[2]

# (d)
L_c3 <- res3$L
L_c4 <- res4$L

# Resultados
cat("Resultados:\n")
cat("(a) Mecánicos ociosos (c=4):", mecanicos_ociosos_4, "\n")
cat("(b) P(todos ociosos) (c=3):", P_todos_ociosos, "\n")
cat("(c) P(al menos 2 ociosos) (c=3):", P_al_menos_2_ociosos, "\n")
cat("(d) Longitud promedio (c=3):", L_c3, "\n")
cat("(d) Longitud promedio (c=4):", L_c4, "\n")
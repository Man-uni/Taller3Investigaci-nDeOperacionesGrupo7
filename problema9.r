N <- 10
lambda <- 1/200
mu <- 1/10
c <- 1

pi <- numeric(N + 1)
pi[1] <- 1  # pi0 sin normalizar

# Construcción
for (n in 1:N) {
  lambda_n <- (N - (n - 1)) * lambda
  mu_n <- min(n, c) * mu
  pi[n + 1] <- pi[n] * (lambda_n / mu_n)
}

# Normalización
pi <- pi / sum(pi)

# Número promedio de robots averiados
L <- sum((0:N) * pi)

# (a) costo
costo <- L * 30 + 10

# (b)
P_todos_funcionando <- pi[1]

# (c)
P_segundo_lugar <- pi[4]  # n=3

# (d)
funcionando <- N - L

# Resultados
cat("Resultados:\n")
cat("(a) Costo total por hora (€):", costo, "\n")
cat("(b) Probabilidad todos funcionando:", P_todos_funcionando, "\n")
cat("(c) Probabilidad segundo en fila:", P_segundo_lugar, "\n")
cat("(d) Robots funcionando promedio:", funcionando, "\n")
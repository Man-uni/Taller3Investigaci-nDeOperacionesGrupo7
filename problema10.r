N <- 100
c <- 10
lambda <- 1/8
mu <- 2

pi <- numeric(c + 1)
pi[1] <- 1  # pi0 sin normalizar

# Construcción
for (n in 1:c) {
  lambda_n <- (N - (n - 1)) * lambda
  mu_n <- n * mu
  pi[n + 1] <- pi[n] * (lambda_n / mu_n)
}

# Normalización
pi <- pi / sum(pi)

# (a)
P_todos_libres <- pi[1]

# (e)
L <- sum((0:c) * pi)

# Resultados
cat("Resultados:\n")
cat("(a) Probabilidad todos los puertos libres:", P_todos_libres, "\n")
cat("(b) Profesores en espera:", 0, "\n")
cat("(c) Tiempo de espera (horas):", 0, "\n")
cat("(d) Tiempo total en sistema (horas):", 1/mu, "\n")
cat("(e) Profesores en el sistema:", L, "\n")
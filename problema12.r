lambda <- 10
mu <- 15
horas <- 8

# ===== M/M/1 =====
rho1 <- lambda / mu

Wq1 <- rho1 / (mu - lambda)
Lq1 <- lambda * Wq1
P01 <- 1 - rho1

costo1_h <- Lq1 * 25 + 12
costo1_dia <- costo1_h * horas

# ===== M/M/2 =====
c <- 2
rho2 <- lambda / (c * mu)
a <- lambda / mu

P0_2 <- 1 / (1 + a + (a^2 / (2 * (1 - rho2))))

Pw <- (a^2 / (2 * (1 - rho2))) * P0_2

Lq2 <- Pw * (rho2 / (1 - rho2))
Wq2 <- Lq2 / lambda

costo2_h <- Lq2 * 25 + (2 * 12)
costo2_dia <- costo2_h * horas

# Resultados
cat("=== 1 CAJERO ===\n")
cat("Clientes/día:", lambda * horas, "\n")
cat("Wq (min):", Wq1 * 60, "\n")
cat("P0:", P01, "\n")
cat("Uso:", rho1, "\n")
cat("Costo diario:", costo1_dia, "\n\n")

cat("=== 2 CAJEROS ===\n")
cat("Clientes/día:", lambda * horas, "\n")
cat("Wq (min):", Wq2 * 60, "\n")
cat("P0:", P0_2, "\n")
cat("Actividad:", 1 - P0_2, "\n")
cat("Costo diario:", costo2_dia, "\n\n")

if (costo2_dia < costo1_dia) {
  cat("Conviene usar 2 cajeros\n")
} else {
  cat("Conviene usar 1 cajero\n")
}
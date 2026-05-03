# Problema 11 - Modelo M/M/c : (GD/∞/∞)

# Datos del problema
lambda <- 30          # clientes por hora
mu <- 20              # clientes atendidos por hora por empleado
Cw <- 100             # costo de espera por hora
Cs <- 10              # salario por empleado por hora
horas_dia <- 10       # horas de trabajo por día

# Cantidad de empleados a evaluar
servidores <- c(2, 3, 4)

# Función para calcular medidas M/M/c
calcular_mmc <- function(lambda, mu, c, Cw, Cs, horas_dia) {
  
  # Intensidad de tráfico
  a <- lambda / mu
  
  # Factor de utilización del sistema
  rho <- lambda / (c * mu)
  
  # Cálculo de p0
  suma <- 0
  for (n in 0:(c - 1)) {
    suma <- suma + (a^n / factorial(n))
  }
  
  termino_final <- (a^c / factorial(c)) * (1 / (1 - rho))
  p0 <- 1 / (suma + termino_final)
  
  # Número promedio de clientes en cola
  Lq <- (p0 * (a^c) * rho) / (factorial(c) * (1 - rho)^2)
  
  # Tiempo promedio de espera en cola
  Wq <- Lq / lambda
  
  # Tiempo promedio de espera en cola en minutos
  Wq_minutos <- Wq * 60
  
  # Costo de espera por hora
  costo_espera_hora <- lambda * Wq * Cw
  
  # Costo de espera diario
  costo_espera_diario <- costo_espera_hora * horas_dia
  
  # Costo de servicio por hora
  costo_servicio_hora <- c * Cs
  
  # Costo de servicio diario
  costo_servicio_diario <- costo_servicio_hora * horas_dia
  
  # Costo total diario
  costo_total_diario <- costo_espera_diario + costo_servicio_diario
  
  return(data.frame(
    empleados = c,
    lambda = lambda,
    mu = mu,
    rho = rho,
    p0 = p0,
    Lq = Lq,
    Wq_horas = Wq,
    Wq_minutos = Wq_minutos,
    costo_espera_diario = costo_espera_diario,
    costo_servicio_diario = costo_servicio_diario,
    costo_total_diario = costo_total_diario
  ))
}

# Aplicar la función a 2, 3 y 4 empleados
resultados <- do.call(
  rbind,
  lapply(servidores, function(c) {
    calcular_mmc(lambda, mu, c, Cw, Cs, horas_dia)
  })
)

# Redondear resultados para presentación
resultados_redondeados <- round(resultados, 4)

# Mostrar tabla final
print(resultados_redondeados)

# Identificar el costo total mínimo diario
minimo <- resultados[which.min(resultados$costo_total_diario), ]

cat("\nEl costo total mínimo diario se obtiene con",
    minimo$empleados, "empleados.\n")

cat("Costo total mínimo diario = $",
    round(minimo$costo_total_diario, 2), "\n")
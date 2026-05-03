# ==========================================================
# MODELO DE COLAS M/M/2
# Oficina de correos con dos ventanillas
# ==========================================================

# 1. Datos del problema

c <- 2                      # Número de ventanillas abiertas
lambda_total <- 1/3          # Tasa de llegada: 1 cliente cada 3 minutos
p_atendidos <- 0.80          # Proporción de clientes que debe pasar por ventanilla

# Tasa efectiva de llegada a la cola
lambda <- lambda_total * p_atendidos

# Tiempo promedio de servicio: 5 minutos
# Por tanto, la tasa de servicio por ventanilla es:
mu <- 1/5

# 2. Factor de utilización del sistema
rho <- lambda / (c * mu)

# 3. Cálculo de P0: probabilidad de que el sistema esté vacío
suma <- 0

for(n in 0:(c-1)){
  suma <- suma + (lambda/mu)^n / factorial(n)
}

parte_final <- ((lambda/mu)^c / factorial(c)) * (1 / (1 - rho))

P0 <- 1 / (suma + parte_final)

# 4. Probabilidad de que un cliente deba esperar
Pw <- parte_final * P0

# 5. Número promedio de clientes en cola
Lq <- Pw * (rho / (1 - rho))

# 6. Tiempo promedio de espera en cola
Wq <- Lq / lambda

# 7. Tiempo promedio total en el sistema
W <- Wq + (1 / mu)

# 8. Cálculo de costos
costo_empleado_hora <- 20
costo_espera_hora <- 35
horas_abiertas <- 8

costo_empleados <- c * costo_empleado_hora * horas_abiertas
costo_espera <- Lq * costo_espera_hora * horas_abiertas
costo_total <- costo_empleados + costo_espera

# 9. Presentación de resultados
cat("RESULTADOS DEL MODELO M/M/2\n")
cat("--------------------------------------\n")
cat("Tasa efectiva de llegada:", round(lambda, 4), "clientes por minuto\n")
cat("Tasa de servicio por ventanilla:", round(mu, 4), "clientes por minuto\n")
cat("Factor de utilización:", round(rho, 4), "\n\n")

cat("a) Probabilidad de que las dos ventanillas estén vacías:", 
    round(P0, 4), "\n")

cat("b) Probabilidad de que un cliente deba esperar:", 
    round(Pw, 4), "\n")

cat("c) Número promedio de clientes en cola:", 
    round(Lq, 4), "clientes\n")

cat("d) Tiempo promedio para salir de la oficina:", 
    round(W, 4), "minutos\n")

cat("e) Costo diario de empleados: $", 
    round(costo_empleados, 2), "\n")

cat("   Costo diario por espera: $", 
    round(costo_espera, 2), "\n")

cat("   Costo total diario: $", 
    round(costo_total, 2), "\n")
 

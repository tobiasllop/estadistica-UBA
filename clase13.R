# Parámetros
mu0 <- 100
mu1 <- 110
sigma <- 20
n <- 16
alpha <- 0.05

# Media y desviación del promedio muestral
se <- sigma / sqrt(n)  # = 5

# Valor crítico (punto de corte)
corte <- mu0 + qnorm(1 - alpha) * se  # = 108.225

# Secuencia de valores de x para graficar
x <- seq(80, 130, length = 1000)

# Densidades bajo H0 y bajo H1
f0 <- dnorm(x, mean = mu0, sd = se)
f1 <- dnorm(x, mean = mu1, sd = se)

# Graficar densidades
plot(x, f0, type = "l", col = "blue", lwd = 2, ylab = "Densidad", main = "Distribución de X̄ bajo H₀ y H₁")
lines(x, f1, col = "red", lwd = 2)

# Sombrear región de rechazo bajo H0 (nivel del test)
x_rechazo <- x[x > corte]
y_rechazo <- f0[x > corte]
polygon(c(corte, x_rechazo, max(x_rechazo)), c(0, y_rechazo, 0), col = rgb(0, 0, 1, 0.3), border = NA)

# Sombrear potencia bajo H1
y_potencia <- f1[x > corte]
polygon(c(corte, x_rechazo, max(x_rechazo)), c(0, y_potencia, 0), col = rgb(1, 0, 0, 0.3), border = NA)

# Leyenda
legend("topright", legend = c("H₀: μ = 100", "H₁: μ = 110", "Nivel α", "Potencia"), 
       col = c("blue", "red", rgb(0,0,1,0.3), rgb(1,0,0,0.3)), 
       lwd = c(2, 2, NA, NA), pch = c(NA, NA, 15, 15), pt.cex = 2)

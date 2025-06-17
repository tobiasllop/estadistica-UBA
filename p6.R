###################################################
## Practica 7 ####################################
##################################################

###################################################
## Ejercicio 4 ####################################
##################################################

data <- read.csv("/media/tobi/ssd2/Documentos/Estadistica/Data/glakes.csv",header=TRUE)
LT = log(data[,3])
W = data[,2]^0.25

#a.
# Ajustar modelo lineal
modelo <- lm(LT ~ W, data = data)

# Ver resumen del modelo
summary(modelo)

#b.
summary(modelo)$coefficients
theta0 <- coef(modelo)[1]
theta1 <- coef(modelo)[2]
cat("θ0 =", theta0, "θ1 =", theta1, "\n")

#c
sigma2 <- summary(modelo)$sigma^2
cat("Estimación de sigma^2:", sigma2, "\n")

#d
t_stat <- summary(modelo)$coefficients["W", "t value"]
p_val <- summary(modelo)$coefficients["W", "Pr(>|t|)"]

cat("Estadístico t:", t_stat, "\n")
cat("p-valor:", p_val, "\n")

if (p_val < 0.01) {
  cat("Se rechaza H0: hay evidencia de que θ1 ≠ 0 al nivel 0.01\n")
} else {
  cat("No se rechaza H0: no hay evidencia suficiente al nivel 0.01\n")
}

#e
theta0_hat <- coef(modelo)[1]
se_theta0 <- summary(modelo)$coefficients["(Intercept)", "Std. Error"]
t_custom <- (theta0_hat - 10) / se_theta0

gl <- modelo$df.residual
p_val_custom <- 1 - pt(t_custom, df = gl)

cat("Estadístico t:", t_custom, "\n")
cat("p-valor unilateral:", p_val_custom, "\n")

if (p_val_custom < 0.05) {
  cat("Se rechaza H0: hay evidencia de que θ₀ > 10\n")
} else {
  cat("No se rechaza H0: no hay evidencia suficiente\n")
}

#f
# IC para θ₀
confint(modelo, level = 0.95)["(Intercept)",]

# R²
summary(modelo)$r.squared

# Correlación entre LT observado y LT predicho
LT_hat <- predict(modelo)
correlacion <- cor(LT, LT_hat)
cat("Correlación:", correlacion, "\n")
cat("R² manual (cor^2):", correlacion^2, "\n")

#g
peso_nuevo <- 625
W_nuevo <- peso_nuevo^0.25

# Estimación
predict(modelo, newdata = data.frame(W = W_nuevo), interval = "confidence", level = 0.95)

#h
predict(modelo, newdata = data.frame(W = W_nuevo), interval = "prediction", level = 0.95)

#i
intervalo_log <- predict(modelo, newdata = data.frame(W = W_nuevo), interval = "prediction", level = 0.95)
exp(intervalo_log)

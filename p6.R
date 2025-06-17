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


#####################################################################
##### Ejercicio 5 ###################################################
#####################################################################

#a
#Cargar datos
credit <- read.csv("~/Descargas/credit.txt", header = TRUE)
str(credit)

#Definimos variables indicadoras
credit$GenderFemale <- as.integer(trimws(credit$Gender) == "Female")
credit$StudentYes <- as.integer(credit$Student == "Yes")
credit$MarriedYes <- as.integer(credit$Married == "Yes")
str(credit)

#Ajustar el modelo propuesto
modelo_credit <- lm(Balance ~ Income + Rating + Limit + Cards + Age + Education +
                      GenderFemale + StudentYes + MarriedYes,
                    data = credit)
summary(modelo_credit)


#b
# Valor predicho
y_hat2 <- predict(modelo_credit)[2]

# Residuo
residuo2 <- residuals(modelo_credit)[2]

# Suma total de residuos
suma_residuos <- sum(residuals(modelo_credit))

# Correlaciones con residuos
cor_edu_res <- cor(credit$Education, residuals(modelo_credit))
cor_lim_res <- cor(credit$Limit, residuals(modelo_credit))

# Correlaciones con todas las variables explicativas
cor_con_covariables <- sapply(credit[, c("Income", "Rating", "Limit", "Cards", "Age", "Education",
                                         "GenderFemale", "StudentYes", "MarriedYes")],
                              function(x) cor(x, residuals(modelo_credit)))

# Mostrar resultados
cat("Predicción para obs 2:", y_hat2, "\n")
cat("Residuo para obs 2:", residuo2, "\n")
cat("Suma total de residuos:", suma_residuos, "\n")
cat("Correlación Education ~ residuos:", cor_edu_res, "\n")
cat("Correlación Limit ~ residuos:", cor_lim_res, "\n")
cat("Correlaciones con otras variables:\n")
print(cor_con_covariables)


#c
sigma2 <- summary(modelo_credit)$sigma^2
cat("Estimación de sigma²:", sigma2, "\n")

#d
coefs <- summary(modelo_credit)$coefficients
significativas <- coefs[coefs[, "Pr(>|t|)"] < 0.05, ]
cat("Variables estadísticamente significativas:\n")
print(significativas)

#d
t_age <- coefs["Age", "t value"]
p_bilateral_age <- coefs["Age", "Pr(>|t|)"]
gl <- modelo_credit$df.residual
p_unilateral_izq <- pt(t_age, df = gl)

cat("p-valor bilateral para Age:", p_bilateral_age, "\n")
cat("p-valor unilateral (cola izquierda):", p_unilateral_izq, "\n")

#e
confint(modelo_credit, level = 0.90)["Education", ]

#f
beta_age <- coefs["Age", "Estimate"]
se_age <- coefs["Age", "Std. Error"]

diferencia <- -3 * beta_age
se_dif <- sqrt((3 * se_age)^2)
t_crit <- qt(0.975, df = gl)

ic_inf <- diferencia - t_crit * se_dif
ic_sup <- diferencia + t_crit * se_dif

cat("Diferencia esperada:", diferencia, "\n")
cat("IC 95% de la diferencia: [", ic_inf, ",", ic_sup, "]\n")



#####################################################################
###### Practica 4 ###################################################
#####################################################################


#Ejercicio 2.b)
# Datos observados
muestra <- c(500, 488, 426, 510, 450, 368, 508, 514, 426, 476, 512, 526, 444, 524, 236)

# Estimador T1 = media muestral
T1_obs <- mean(muestra)

# Cantidad de simulaciones bootstrap
B <- 1000

# Vector donde guardamos los estimadores bootstrapeados
T1_boot <- numeric(B)

# Bootstrap paramétrico
set.seed(123)  # Para reproducibilidad
for (i in 1:B) {
  muestra_boot <- rpois(n = 15, lambda = T1_obs)
  T1_boot[i] <- mean(muestra_boot)
}

# Histograma
hist(T1_boot, breaks = 30, main = "Distribución bootstrap de T1", xlab = "T1")

# Estimación de la varianza del estimador
var_T1_boot <- var(T1_boot)
var_T1_boot

#T2

# En vez de T1_boot[i] <- mean(muestra_boot)
# hacemos:

m2_obs <- mean(muestra^2)
T2_obs <- (-1 + sqrt(1 + 4 * m2_obs)) / 2

# Vector donde guardamos los estimadores bootstrapeados
T2_boot <- numeric(B)

# Bootstrap paramétrico
set.seed(123)  # Para reproducibilidad
for (i in 1:B) {
  muestra_boot <- rpois(n = 15, lambda = T2_obs)
  m2 <- mean(muestra_boot^2)
  T2_boot[i] <- (-1 + sqrt(1 + 4 * m2)) / 2
}

# Histograma
hist(T2_boot, breaks = 30, main = "Distribución bootstrap de T2", xlab = "T2")

# Estimación de la varianza del estimador
var_T2_boot <- var(T2_boot)
var_T2_boot


### Ejercicio 3 ########################################################
datos <- read.delim('/media/tobi/ssd2/Documentos/Estadistica/Data/datos1.txt')
datos <- c(datos[,1])
n <- length(datos) 
n
media_obs <- mean(datos)
mediana_obs <- median(datos)

# Cantidad de repeticiones
B <- 1000

# Vectores para guardar los estimadores bootstrapeados
medias_boot <- numeric(B)
medianas_boot <- numeric(B)

# Bootstrap
set.seed(123)
for (i in 1:B) {
  muestra_boot <- sample(datos, size = n, replace = TRUE)
  medias_boot[i] <- mean(muestra_boot)
  medianas_boot[i] <- median(muestra_boot)
}

se_media <- sd(medias_boot)
se_mediana <- sd(medianas_boot)

cat("Error estándar estimado para la media:", se_media, "\n")
cat("Error estándar estimado para la mediana:", se_mediana, "\n")

hist(medias_boot, breaks = 30, main = "Bootstrap de la media", xlab = "Media")
hist(medianas_boot, breaks = 30, main = "Bootstrap de la mediana", xlab = "Mediana")

### Ejercicio 4 ########################################################
dados <- c(
  2, 2, 4, 6, 1, 3, 1, 3, 2, 4, 4, 4, 4, 4, 6, 3, 3, 4, 1, 2,
  1, 6, 3, 2, 3, 4, 1, 1, 5, 4, 1, 4, 6, 4, 1, 2, 1, 5, 4, 3,
  3, 1, 3, 1, 6, 5, 1, 3, 2, 3, 6, 2, 4, 2, 6, 6, 5, 2, 4, 4,
  1, 4, 3, 1, 2, 1, 6, 1, 1, 3, 1, 6, 6, 1, 2, 6, 1, 1, 4, 5,
  4, 1, 5, 2, 2, 1, 6, 6, 1, 2, 1, 3, 1, 3, 3, 4, 3, 3, 3, 5
)
n <- length(dados)  # n = 100
n
theta_obs <- mean(dados %% 2 == 0)
cat("Proporción de números pares observada:", theta_obs, "\n")

set.seed(123)
B <- 5000
theta_boot <- numeric(B)

for (b in 1:B) {
  muestra_boot <- sample(dados, size = n, replace = TRUE)
  theta_boot[b] <- mean(muestra_boot %% 2 == 0)
}

hist(theta_boot, breaks = 30, main = "Bootstrap de θ (proporción de pares)", xlab = "θ")
abline(v = 0.5, col = "red", lwd = 2, lty = 2)  # línea del dado justo


se_theta <- sd(theta_boot)
cat("Error estándar estimado de θ:", se_theta, "\n")

#Intervalos de confianza
z <- qnorm(0.975)
IC_normal <- c(theta_obs - z * se_theta, theta_obs + z * se_theta)
IC_percentil <- quantile(theta_boot, c(0.025, 0.975))

cat("IC Normal (95%):", IC_normal, "\n")
cat("IC Percentil (95%):", IC_percentil, "\n")


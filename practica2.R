###################################################################
######################### Practica 2 ##############################
###################################################################

#################################################
# 2.14
#################################################

#2.14.a
n <- 100 #Tamaño de la muestra
X_n <-rexp(n, rate=1) # n variables aleatorias indep e identicamente distribuidas como exp(1)
F_emp <- ecdf(X_n) #Distribucion empirica

#Graficamos la distribucion empirica vs la teorica
plot(F_emp, main = "Función de distribución empírica vs. real", xlab = "x", ylab = "F(x)")
curve(pexp(x, rate = 1), add = TRUE, col = "red")

# 2.14.b

#Estimador de momentos
#La esperanza de X_i es 1/\lambda. Luego igualamos Esp(x_1) = promedio(X_i).
#Nos queda que \lambda_mom = 1/promedio(X_i)

lambda_mom <- 1/mean(X_n)

#Estimador de max. verosimilitud
#Hacemos la productoria de las funciones de densidad nos queda:
#prod_i=1^i=n=\lambda * e^(-\lambda * x_i)
#El lambda sale afuera y nos queda \lambda^n * e^(-\lambda * sum_i=1^i=n{xi})
#Tomando logaritmo nos queda: n * ln(\lambda) + -\lambda * sum_i=1^i=n{xi}
#Que derivando e igualando a 0 nos queda igual a n * 1/\lambda - sum_i=1^i=n{xi} = 0
#Luego \lambda_mv = n/sum_i=1^i=n{xi}

lambda_mv <- n / sum(X_n)

#2.14.c

#Estimacion parametrica de la densidad (Preguntar Igual)
plot(F_emp, main = "Función de distribución empírica vs. estimada", xlab = "x", ylab = "F(x)")
curve(pexp(x, rate = lambda_mom), add = TRUE, col = "blue", type="l") #grafico una exponencial con el lambda estimado


#################################################
# 2.15
#################################################


#2.15.a
n <- 6 #tamaño de la muestra
theta_0 <- 3
X_n <- runif(n,0,theta_0) # n vars iid distribuidas como U(0,\theta_0)

#Por ejercicio 10 sabemos que los estimadores de theta son:
theta_mom <- 2 * mean(X_n) #Estimador de momentos de theta
theta_mv <- max(X_n) #Estimador de maxima verosimilitud de theta
theta_mod <- ((n+1)/n) * theta_mv #Estimador de maxima verosimilitud modificado de theta

#Definimos una funcion para almacenar estimadores segun el tamaño de la muestra
estimadores <- function(n){
  
    #Inicializamos una matriz para guardar los valores estimados de theta  
    res <- matrix(NA, nrow = 1000, ncol = 3)
    
    for (k in 1:1000){
      X_n <- runif(n, 0, theta_0) #muestra
      
      theta_mom <- 2 * mean(X_n) #estimador de momentos
      theta_mv <- max(X_n) #estimador de max ver
      theta_mod <- ((n + 1) / n) * theta_mv #estimador modificado
      
      res[k, ] <- c(theta_mom, theta_mv, theta_mod) #agregamos los valores estimados para la muestra k
    }
    colnames(res) <- c("momentos", "verosimilitud", "modificado")
    return(res) 
}

#Guardamos los valores para n=6,10,20,40,80,200
for (n in c(6, 10, 20, 40, 80, 200)) {
  assign(paste0("estimadores_", n), estimadores(n))
}

#Calculamos el estimador de primer momento del ECM de cada estimador para cada n
#La cuenta ejemplo seria ecm_mom_theta_mom_10 = mean((estimadores_10[1] - theta_0)^2)
for (n in c(6, 10, 20, 40, 80, 200)){ 
    for (i in c(1,2,3)){
      assign(paste0("ecm_mom_theta_",i,"_", n), mean((get(paste0("estimadores_", n))[,i] - theta_0)^2))
   }
}

#2.15.b

#Boxplot de los EMV simulados versus los tamanos de muestra n
boxplot(estimadores_6[,2], estimadores_10[,2], estimadores_20[,2], 
        estimadores_40[,2], estimadores_80[,2], estimadores_200[,2],  
        names = c("n=6", "n=10", "n=20", "n=40", "n=80", "n=200"),
        main = "Boxplots de estimadores EMV",
        ylab = "Valor estimado",
        col = "lightblue")
abline(h = theta_0, col = "red", lty = 2) #Agregamos linea punteada (lty=2) en el valor de theta_0

#Boxplot de los EMmom simulados versus los tamanos de muestra n
boxplot(estimadores_6[,1], estimadores_10[,1], estimadores_20[,1], 
        estimadores_40[,1], estimadores_80[,1], estimadores_200[,1],  
        names = c("n=6", "n=10", "n=20", "n=40", "n=80", "n=200"),
        main = "Boxplots de estimadores EMmom",
        ylab = "Valor estimado",
        col = "lightblue")
abline(h = theta_0, col = "red", lty = 2) #Agregamos linea punteada (lty=2) en el valor de theta_0

#Boxplot de los EMmod simulados versus los tamanos de muestra n
boxplot(estimadores_6[,3], estimadores_10[,3], estimadores_20[,3], 
        estimadores_40[,3], estimadores_80[,3], estimadores_200[,3],  
        names = c("n=6", "n=10", "n=20", "n=40", "n=80", "n=200"),
        main = "Boxplots de estimadores EMmod",
        ylab = "Valor estimado",
        col = "lightblue")
abline(h = theta_0, col = "red", lty = 2) #Agregamos linea punteada (lty=2) en el valor de theta_0


## Grafico de ECM estimado del EMV, Mom y Mod en el eje y, versus n en el eje x, en negro
# Eje x
n_vals <- c(6, 10, 20, 40, 80, 200)

# Primer estimador (EMV) en negro
y1 <- c(ecm_mom_theta_1_6, ecm_mom_theta_1_10, ecm_mom_theta_1_20,
        ecm_mom_theta_1_40, ecm_mom_theta_1_80, ecm_mom_theta_1_200)

# Segundo estimador (Momentos) en rojo
y2 <- c(ecm_mom_theta_2_6, ecm_mom_theta_2_10, ecm_mom_theta_2_20,
        ecm_mom_theta_2_40, ecm_mom_theta_2_80, ecm_mom_theta_2_200)

# Tercer estimador (Modificado) en azul
y3 <- c(ecm_mom_theta_3_6, ecm_mom_theta_3_10, ecm_mom_theta_3_20,
        ecm_mom_theta_3_40, ecm_mom_theta_3_80, ecm_mom_theta_3_200)

# Graficar la primera curva con plot()
plot(n_vals, y1, type = "b", pch = 16, col = "black", lwd = 2,
     xlab = "n", ylab = "ECM", main = "ECM de los tres estimadores")

# Agregar las otras dos curvas con lines()
lines(n_vals, y2, type = "b", pch = 17, col = "red", lwd = 2)
lines(n_vals, y3, type = "b", pch = 15, col = "blue", lwd = 2)

# Agregar leyenda
legend("topright", legend = c("EMV", "Momentos", "Modificado"),
       col = c("black", "red", "blue"), pch = c(16, 17, 15), lwd = 2)

#################################################
# 2.19
#################################################

#2.19.a
n <- 10
theta_0 <- 1
pmom <- c()
pemv <- c()
k <- 1000

for (i in 1:k){
  X_n <- runif(n, min = 0, max=theta_0)
  theta_mom <- 2 * mean(X_n)
  theta_mv <- max(X_n)
  epsilon <- 0.01
  pmom <- append(pmom, Mod(theta_mom - theta_0) < epsilon)
  pemv <- append(pemv, Mod(theta_mv - theta_0) < epsilon)
}

print(paste("Probabilidad de que |theta_mom - theta_0| < e: ", sum(pmom)/k))
print(paste("Probabilidad de que |theta_mv - theta_0| < e: ", sum(pemv)/k))

# 2.19.b y 2.19.c me dieron fiaca hacerlos :p

#################################################
###### Clase practica 21-04-2025 ################
#################################################

###################################################
### Ejercicio 2 ##################################
##################################################
intervalo <- function(datos, sigma, nivel){
    res <- c(mean(datos) - qnorm(-(nivel-1)/2, lower.tail = FALSE) * sigma/sqrt(length(datos)), mean(datos) + qnorm(-(nivel-1)/2, lower.tail = FALSE) * sigma/sqrt (length(datos)))
    return(res)
}

#2.a
datos_normales <- rnorm(5, mean = 4, sd= 3)

#2.b
int_confianza <- intervalo(datos_normales, 3, 0.95)
# Resultado obtenido: (1.8,7.05)

#2.c

if (4 >= int_confianza[1] && 4 <= int_confianza[2]){
   print(TRUE)
} else { 
  print(FALSE)
}
#Si, u=4 pertenece al intervalo anterior

#2.d
nrep <- 1000
esta_4_en_el_intervalo <- c()

for (i in 1:nrep){
  datos_normales <- rnorm(5, mean = 4, sd= 3)
  int_confianza <- intervalo(datos_normales, 3, 0.95)
  
  if (4 >= int_confianza[1] && 4 <= int_confianza[2]){
    esta_4_en_el_intervalo <- append(esta_4_en_el_intervalo, TRUE)
  } else{
    esta_4_en_el_intervalo <- append(esta_4_en_el_intervalo, FALSE)
  }
}

proporcion = sum(esta_4_en_el_intervalo)/nrep
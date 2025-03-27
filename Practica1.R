# Ejercicio 3 
iridio <- read.table("iridio.txt", header=TRUE)
rodio <- read.table("rodio.txt", header=TRUE)

#3.a
# Histograma de temperatura de sublimacion del iridio
hist(iridio[,1], main="Histograma de iridio")

#Histograma de temperatura de sublimacion del rodio
hist(rodio[,1], main="Histograma de rodio")

#Boxplot de temperatura de sublimacion del iridio
boxplot(iridio[,1], main="Boxplot de iridio")

#Boxplot de temperatura de sublimacion del rodio
boxplot(rodio[,1], main="Boxplot de rodio")

#3.b
#medias
media_iridio <- mean(iridio[,1])
media_rodio <- mean(rodio[,1])
#medianas
mediana_iridio <- median(iridio[,1])
mediana_rodio <- median(rodio[,1])
#medias podadas
media_iridio_podada_10 <- mean(iridio[,1], trim=0.1)
media_iridio_podada_20 <- mean(iridio[,1], trim=0.2)
media_rodio_podada_10 <- mean(rodio[,1], trim=0.1)
media_rodio_podada_20 <- mean(rodio[,1], trim=0.2)

#3.c
#Desvios estandares
desvio_est_iridio <- sd(iridio[,1])
desvio_est_rodio <- sd(rodio[,1])
#Distancia intercuantil
dist_intercuantil_iridio <- IQR(iridio[,1])
dist_intercuantil_rodio <- IQR(rodio[,1])

#cuantiles
cuantiles_iridio <- quantile(iridio[,1], probs=c(0.1, 0.25, 0.5, 0.75, 0.9))
cuantiles_rodio <- quantile(rodio[,1], probs=c(0.1, 0.25, 0.5, 0.75, 0.9))


#Ejercicio 6
data_credit_card <- read.csv("data_credit_card.csv", header=TRUE)
purchases <- data_credit_card[,1]
credit_limit <- data_credit_card[,2]
purchases_freq <- data_credit_card[,3]
tenure <- data_credit_card[,4]

#6.a
plot.ecdf(purchases, main="Func de distribucion de purchases")
plot.ecdf(credit_limit, main="Func de distribucion de credit_limit")
plot.ecdf(purchases_freq, main="Func de distribucion de purchases_freq")
plot.ecdf(tenure, main="Func de distribucion de tenure")

#6.b
hist(credit_limit, probability=TRUE, main="histograma de credit_limit") #Preguntar por lo de probability=TRUE
lines(density(credit_limit), col = "red", lwd = 2)

#6.c
frecuencia_relativa <- prop.table(table(tenure))
print(frecuencia_relativa)

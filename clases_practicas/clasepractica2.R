#
estim_mm = function(v){
  return (2*mean(v))
}

estima 

estim_mv = function(v){
  return (max(v))
  }

datos <- runif(1000, 0, 2)
estim_mm(datos)
estim_mv(datos)

result_mm = c()
result_mv = c()

for (x in 1:1000) {
  datos <- runif(1000, 0, 2)
  result_mm <- c(result_mm, estim_mm(datos))
  result_mv <- c(result_mv, estim_mv(datos))
}

boxplot(result_mm)
boxplot(result_mv)

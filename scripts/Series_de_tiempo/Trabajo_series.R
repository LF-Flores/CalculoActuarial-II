# Se cargan las base de datos como data frames
setwd("~/git_repositories")
temp <- read.csv('TEMPERATURA.csv', check.names = F)
precip <- read.csv('PRECIPITACION.csv', check.names = F)

library(readxl)
temp2 <- read_excel("TEMPERATURA.xlsx")

#Creamos función para convertir los data frames en series de tiempo
create_ts <- function(datos, na_number_start){
  # dimensiones del data frame de datos
  size <- dim(datos)
  # Se crea vector numérico que servirá para crear la serie de tiempo
  x <- numeric((size[1]-1)*(size[2]-1)+na_number_start+2)
  
  # Se rellenan la primeras entradas considerando los NA iniciales
  for (k in (na_number_start+2):size[2]){
    x[k-na_number_start-1] <- datos[1,k]
  }
  
  # Se rellenan el resto de entradas
  for (k in 2:size[1]){
    for (j in 2:size[2]){
      x[(12-na_number_start)+1+(size[2]-1)*(k-2)+(j-2)] <- datos[k,j]
    }
  }
  
  na.omit(ts(x, start=c(1979, na_number_start+1), end=c(1979+size[1]-1, 12), frequency=12))
}

# Se crean las series de tiempo
temp_serie <- create_ts(temp, 6)
prec_serie <- create_ts(precip, 4) 
# Se visualizan como tablas para verificar
temp_serie
prec_serie
# Podemos ver que ahora R las reconoce como series de tiempo
is.ts(temp_serie)
is.ts(prec_serie)

###############
# EXPLORATIVO #
###############
# Podemos ver resúmenes de ambas series de tiempo
summary(temp_serie)
summary(prec_serie)

# Esto graficará las series de tiempo
ts.plot(temp_serie, xlab="Año", ylab="Temperatura", main="Temperatura promedio por mes, 1979-2013")
abline(reg=lm(temp_serie~time(temp_serie)), col = 'red') # Ajusta una recta de tendencia

ts.plot(prec_serie, xlab="Año", ylab="Temperatura", main="Temperatura promedio por mes, 1979-2013")
abline(reg=lm(prec_serie~time(prec_serie)), col = 'red') # Ajusta una recta de tendencia

# Se graficará la función de autocorrelación para ver el mejor modelo a ajustar:
acf(temp_serie)
acf(prec_serie)


#COMENTARIOS:
#
# Ambos gráficos de función de autocorrelación muestran una tendencia  
# estacional en los datos, como es esperado, ya que el clima es un fenómeno
# que depende de estaciones marcadas por la órbita y movimientos rotatorios
# de la tierra.
#
# La periodicidad de la tendencia se lee del eje horizontal. Como estamos
# con una serie de tiempo con frecuencia en años (utilizar el comando 
# "frequency()" revela 12, que son as observaciones por UNIDAD DE TEMPO.
# Es decir, la unidad de tiempo es AÑOS), entonces el Lag está en dichas unidades.
#
# Como se ve claramente que el periodo es de 1, podemos sospechar que es un 
# fenómeno de ciclo anual (como esperamos)

# Vamos a verificar (y encontrar) la periodicidad de nuestro comportamiento estacionañ
library('TSA')
p1 <- periodogram(temp_serie)
p2 <- periodogram(prec_serie)
# Los periodogramas muestran las frecuencias más sobresalientes (mayor poder) de los datos como señal

# Se define una función que extrae los periodos con mayor poder espectral de la señal transformada
periodo_estacion <- function(periodigrama, periodos){
  dd = data.frame(freq=periodigrama$freq, spec=periodigrama$spec)
  order = dd[order(-dd$spec),]
  top = head(order, periodos)
  for (k in 1:periodos){
    cat('Existe una estación con periodo de ', 1/top$f[k], ' meses y poder especral de ', top$spec[k], ".\n")
  }
}

# Se ejecutan para nuestros datos
periodo_estacion(p1,2)
periodo_estacion(p2,2)

# Podemos descomponer nuestra serie de la siguiente forma:
plot(stl(temp_serie, "per"))
plot(stl(prec_serie, "per"))

# La descomposición asume que el proceso X_t es de la forma
#        X_t = X_t(S_t, T_t, E_t) 
# Donde:
#      S_t : Componente estacional  
#      T_t : Componente de tendencia
#      E_t : componente de errores externos y aleatorios.
#
# El procedimiento que la función 'stl' utiliza es se llama:
#   "Seasonal and Trending decomposition using LOESS"
# donde, 'LOESS' se abreviatura para el método de suavizado llamado
#   "LOcally Estimated Scatterplot Smoothing"    

# Otra alternativa podría ser utilizar la función decompose() de R:
plot(cbind(decompose(prec_serie)$x, decompose(prec_serie)$seasonal, 
           decompose(prec_serie)$trend, decompose(prec_serie)$random),
     main = 'Utilizando función decompose()')

# Aparte del trabajo a modifica con el título y etiquetas, El resultado es similar para la parte 
# estacional. No obstante, la parte de tendencia está mucho menos suave, y más ajustada, lo que
# provee un riesgo de "overfitting" (sobreajuste). 
#
# Esto se debe a que el comando decompose() debe asumir una decomposición rígida entre dos posibles
# modelos:
#   1) X_t = S_t + T_t + E_t 
#   2) X_t = (S_t)*(T_t)*(E_t)



##########
# AJUSTE #
##########

# Se hace una función para imprimir una comparación del AIC entre modelos posibles
# NOTA: el AIC es un índice que incrementa a mayor verosimilitud y al mismo tiempo
# penaliza el número de parámetros a utilizar, para evitar "overfitting".
imprimir_aic <- function(x){
  # Posibles correcciones al asumir estaciones
  #AR_1 <- arima(temp_serie, order = c(1,0,0), seasonal = c(1,0,0))
  #MA_1 <- arima(temp_serie, order = c(0,0,1), seasonal = c(0,0,1))
  #ARMA <- arima(temp_serie, order = c(1,0,1), seasonal = c(1,0,1))
  
  # Se guardan los modelos
  AR_1 <- arima(x, order = c(1,0,0))
  MA_1 <- arima(x, order = c(0,0,1))
  ARMA <- arima(x, order = c(1,0,1))
  
  # Se guardan los AIC
  Inform <- c(ARMA$aic, AR_1$aic, MA_1$aic)
  min <- min(Inform)
  
  # Función para comparar
  compara_modelos <- function(modelo_comp, informa){
    prob <- exp((min-informa)/2)
    nombre_modelo <- deparse(substitute(modelo_comp))
    cat("El modelo ", nombre_modelo, " tiene ", 
        prob, "de probabilidad (relativa) de minimizar la pérdida de información.\n")
  }
  
  # Salida
  cat(
    "Tenemos un AIC mínimo de ", min, ".\n", 
    compara_modelos(ARMA, Inform[1]),
    compara_modelos(AR_1, Inform[2]),
    compara_modelos(MA_1, Inform[3])
  )
}

imprimir_aic(temp_serie)
imprimir_aic(prec_serie)

# En vista de que el modelo ARMA(1,1) se ajusta mejor, elegimos ese entre los tres

# AJUSTE DE ARMA PARA TEMPERATURA
ARMA_temp <- arima(temp_serie, order = c(1,0,1))
ts.plot(temp_serie)
ARMA_fit <- temp_serie - residuals(ARMA_temp)
points(ARMA_fit, type = "l", col = 2, lty = 2)


# AJUSTE DE ARMA PARA PRECIPITACIÓN
acf(prec_serie)
ARMA_prec <- arima(prec_serie, order = c(5,1,6)) # El modelo c(5,1,6) se ajusta mejor.
ARMA_prec$aic
ts.plot(prec_serie)
ARMA_fit <- prec_serie - residuals(ARMA_prec)
points(ARMA_fit, type = "l", col = 2, lty = 2)

# Se ve que la precipitación no se ajusta tan bien como la temperatura
# Por ello, puede que queramos verificar estacionariedad.
library('aTSA')

# El test "aumentado de Dickey-Fuller" se utiliza para verificar UNA de las
# maneras en que una serie puede no ser estacionaria. Esta es, verificar si
# su polinomio característico tiene raíces unitarias (en el círculo completo
# unitario).
#
# Si este fuese el caso, ENTONCES no es estacionario. Pero hay más maneras en
# que la serie puede fallar a ser estacionaria.

# Corremos el test 
adf.test(temp_serie)
adf.test(prec_serie)



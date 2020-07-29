### ENUNCIADO DEL PROBLEMA
# 
# Utilizando la tabla de vida ilustrativa de la sociedad de actuarios:
#    a) Generar una tabla actuarial utilizando una taza de interés del 6%
#    b) Exporte la tabla resultante en un dataframe. Esto mostrará las 
#       funciones de conmutación


### SOLUCIÓN
#
# a)
library(lifecontingencies)

# Cargamos la data
data(soaLt)

# Recordamos cómo construir un objeto de tipo "actuarialtable"
soaAct <- new("actuarialtable", x=soaLt$x, lx=soaLt$Ix, interest=0.06)

# b) Para visualizarlo mejor, esta se puede exportar a un dataframe:
soaActDf <- as(soaAct, "data.frame")

### ENUNCIADO DEL PROBLEMA
# Calcular el valor presente actuarial de un seguro de vida a un término de
# tres años donde el asegurado/tomador tiene 36 años y está asegurándose por
# 500,000 lempiras. (Seguro temporal)

### SOLUCIÓN
#
# Comando diff(): Toma diferencias entre términos consecutivos
diff(c(1,2,3,4))
diff(c(1,5,7,3,10))

# Este se utiliza para encontrar la diferencia de términos consecutivos lx
tabla_diferencias <- -diff(soaActDf$lx)

# Guardamos los interesan
diferencias <- tabla_diferencias[soaActDf$x%in%36:38]

# probabilidades. 
# RECORDAR: Se divide entre lx con x = edad actual
probdeath <- diferencias/soaActDf$lx[soaActDf$x==36]

# Factores de descuento
desc <- (1+0.06)^(-(1:3))

# Se calcula el APV para apreciar el seguro.
sum(desc*probdeath) * 500000

# NOTA: El cálculo anterior es un cálculo clásico utilizando información
# solamente de la tabla actuarial (lx). No obstante, también es posible
# obtener la misma respuesta mediante:

probabilidades <- numeric(3)
for(k in 1:3) probabilidades[k] <- qxt(soaAct, x=36+k-1, t=1)*pxt(soaAct, x=36, t=k-1)
A <- sum(probabilidades*desc)
A*500000

# Además, el paquete lifecontingencies tiene una función que resume todo
# el cálculo:

500000 * Axn(actuarialtable=soaAct, x=36, n=3)

# Por completitud, notemos que otra manera extra para calcular la misma 
# cantidad utiliza las funciones de conmutación (en este caso, Mx y Dx):
# Internamente, la función Axn está numéricamente optimizada. Pero, en 
# segundo lugar de eficiencia, yace el cálculo con funciones de conmutación 

# Utilizando el administrador de contexto with() para acceder a los 
# atributos de soaActDf:

500000 * with(soaActDf, (Mx[37]-Mx[40])/Dx[37])



### ENUNCIADO DEL PROBLEMA
# Considere un seguro de vida entera con pago anticipado anual de 1000
# lempiras. Éste es pagado por un asegurado a sus 65 años. 
# calcular el valor presente actuarial.

axn(actuarialtable=soaAct, x=65)

?axn

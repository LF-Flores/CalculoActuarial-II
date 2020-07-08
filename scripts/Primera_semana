###############################################################
# Este script se ha diseñado con RStudio en mente. Eso significa que pueden haber ciertos comentarios
# o instrucciones que no tendrán sentido o no se podrán realizar de no hacerse en RStudio. La versión
# utilizada en la elaboración de este script es RStudio 1.2.1335
###############################################################


## Tipos de datos

mode(8)    # La forma en que es guardado, es decir, es el tipo de dato más complejo que puede ser almacenado
           # en la misma localidad en la cual el argumento está siendo guardado.

typeof(8)  # El tipo que R usa internamente para calculos

class(8)   # La clase del objeto bajo la jerarquía de clases de R (que es orientado a objetos)


# Operaciones aritméticas 
# Suma
6+3   

# Divisiones
4/6

# Residuo / matemática modular
123%%4

# Potencia
5**3


### El operador "Combinacion" 

# Combinando dos objetos "numeric"
c(3,4)

# No ha cambiado sus propiedades
mode(c(3,4))
typeof(c(3,4))
class(c(3,4))

# Podemos incluso combinar otros objetos combinados:
c(5, 6, c(3,4), 7, c(6,9))


# Nos gustaría guardar cosas en variables... 
# Hay dos maneras:

# El operador = (local)
x = 5 
matrix(1, nrow = 2)

# El operador <- (global)
y <- 6
matrix(1, nrow <- 2)

# Creando vectores
#1) Con el operador combinación
#2) Como secuenca:
1:10
seq(10)
seq(3, 10)
seq(10, 3)
seq(1, 10, 2)

# Creando matrices
# Ya vimos cómo crear una matriz declarándola explícitamente, pero podemos utilizar
# el comando cbind para unir varios objetos como columnas de una matriz

# cbind de un numero con un vector
cbind(1, 1:7)

# cbind con dos vectores (del mismo tamaño)
cbind(3:9, 1:7)

# cbind con dos vectores (de diferente tamaño)
cbind(3:14, 1:7)

# cbind con dos vectores (con tamaños que comparten un divisor común)
cbind(1:15, 1:5, 1:3)

# Graficando
curve(sin(sqrt(x)-sqrt(1/x))^2, from = 10, to = 20)

# ¿Cómo referirnos a columbas o filas especificas de una matríz? 
# Considere la siguiente matriz aleatoria:

matriz_norm <- matrix(rnorm(10), nrow=5, ncol=2)
matriz_norm

# Refererencia a filas
matriz_norm[1,]

# Referencia a columnas
matriz_norm[,2]

# Transposición de una matriz
t(matriz_norm)


## Trucos específicos de RStudio.

# 1) Es posible escribir " <- " con presionar "ALT -"
# 2) Es posible obtener la definición de CUALQUIER función al tener el cursor
#    sobre ella y presionar F2. Ejemplo:

lapply()

## Administrando la memoria
rm(matriz_norm)

# Limpiando la memoria completa

rm(list = ls())  

# Cargando paquetes 
library("TDA")

# Utilizando Vignettes en el menú de ayuda:
vignette()

# 
vignette("article", package = "TDA")

# Ejemplo sacado del vignette
X <- circleUnif(400)
plot(X)

Xlim <- c(-1.6, 1.6); Ylim <- c(-1.7, 1.7);
by <- 0.065

Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Xlim[1], Xlim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)

persp(Xseq, Yseq,
      matrix(KDE, ncol = length(Yseq), nrow = length(Xseq)), xlab = "",
      ylab = "", zlab = "", theta = -20, phi = 35, ltheta = 50,
      col = 2, border = NA, main = "KDE", d = 0.5, scale = FALSE,
      expand = 3, shade = 0.9)






#### ENUNCIADO DEL PROBLEMA:
# Un préstamo de 10,000 USD se estipula con 7% APR (Annual percentage 
# rate) y periodos mensuales para pagarlo de vuelta. Encuentre el 
# monto que se debe pagar mensual si se planea pagar completo el 
# préstamo en un lapso de 5 años.

library(lifecontingencies)

#### SOLUCIÓN
# Se desea que los 10,000 USD se balanceen con un flujo de dinero en 
# forma de una anualidad. Para encontrar el monto de pago se realiza
# la siguiente operación:

# Se despeja de la fórmula de anualidad. Note que el interés es un
# un interés anual, y utilizamos el argumento "k=12" para indicar
# que se harán 12 pagos anuales (o sea, pagos mensuales).
monto <- 10000/annuity(i=0.07, n=5, k=12)

# No obstante, esto nos da la respuesta en MONTO ANUAL. Para obtener
# el MONTO MENSUAL necesitamos dividir entre 12 meses.
monto <- monto/12

# Esto ya resuelve el problema. Pero haremos uso de los dataframes de
# R para visualizar mejor la evolución de nuestras finanzas en este 
# ejemplo.
#
# Nos creamos un vector del flujo de dinero de tamaño igual al número
# de periodos de pagos.
flujo <- rep(monto, 5*12)

# Guardaremos el balance en cada tiempo en un vector de tamaño 60
# (que es el número de periodos).
balance <- numeric(5*12)

# En el primer periodo, este balance va a ser igual a la deuda que 
# tenemos, capitalizada un periodo (mes), menos el monto que pagamos
balance[1] <- 10000*(1+0.07)^(1/12)-monto

# Esto mismo se hace para el resto de periodos:
for(i in 2:60) balance[i] <- balance[i-1]*(1+0.07)^(1/12)-monto

# Nos creamos un dataframe que resuma nuestra amortización (separación
# de pagos en periodos)
amortization <- data.frame(mes = 1:60, 
                          pagos = round(monto,1), 
                          balance = round(balance,1))

# Visualizamos el principio y el final de éste. 
head(amortization)
tail(amortization)


#############################################################

#### ENUNCIADO DEL PROBLEMA:
# Utilizando la tabla de muestra en lifecontingencies de la sociedad
# de actuarios, calcular:
#    a) La probabilidad de que alguien con 65 años fallecerá antes de
#       obtener los 85 años.
#
#   b) Probabilidad de que alguien con 25 años sobreviva por 40 años
#      más.



#### SOLUCIÓN
## INCISO a)

# Se carga la data
data(soa08Act)

# Recordemos la data dentro de esta tabla:
head(soa08Act)

# Podemos utilizar lx para obtener el número de sobrevivientes a 
# cualquier edad. Por ejemplo, 65.
soa08Act@lx[soa08Act@x==65]

# Para obtener la probabilidad condicional solicitada, podemos hacer
# la siguiente operación:
vivos65 <- soa08Act@lx[soa08Act@x==65]
vivos85 <- soa08Act@lx[soa08Act@x==85]

# probabilidad
(vivos65 - vivos85)/vivos65

# Convenientemente, el paquete lifecontingencies ya contiene una 
# función que resume esas operaciones:
qxt(soa08Act, 65, 20)

## INCISO b)
pxt(soa08Act, 25, 40)

#### ENUNCIADO DEL PROBLEMA
#  a) Calcule la esperanza de vida de la tabla
#  b) Calcule la esperanza de vida COMPLETA entre 80 a 90.

#### SOLUCIÓN
## INCISO a)

# Se calcula la proporción 
prop <- soa08Act@lx/soa08Act@lx[soa08Act@x==0]
sum(prop) - 1

# De nuevo, es posible calcularla con una función ya existente:
exn(object = soa08Act)

 
## INCISO b)
# La idea central es que, si alguien fallece a edad x, esta muerte
# ocurre en promedio a la edad x+1/2:
(sum(soa08Act@lx[soa08Act@x%in%(81:90)]/soa08Act@lx[soa08Act@x==80])+
    sum(soa08Act@lx[soa08Act@x%in%(80:89)]/soa08Act@lx[soa08Act@x==80]))/2

# Utilizndo exn
exn(soa08Act, 80, 10, "complete")

# Comparación con esperanzas de tipo "curtate":
exn(soa08Act, 80, 10)



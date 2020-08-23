# Leyendo y limpiando la data
library(readxl)
df <- read_excel("Tabla_de_Mortalidad.xls")
colnames(df) <- df[2,]
df <- df[-c(1, 2) ,]
df <- df[,-(11:length(df))]
df <- data.frame(lapply(df,as.numeric))

# Tasa de ínteres
i <- 0.04

# Creando la tabla actuarial y el df
library(lifecontingencies)
tbl <- new("actuarialtable", x=df$x, lx=df$lx, interest=i)
tbldf <- as(tbl, "data.frame")

# Comparación de precisión:
plot(df$Dx[-101] - tbldf$Dx); abline(0,0); abline(mean(df$Dx[-101] - tbldf$Dx), 0,
                                                  col = "red")
plot(df$Cx[-101] - tbldf$Cx); abline(0,0); abline(mean(df$Cx[-101] - tbldf$Cx), 0, 
                                                  col = "red")
plot(df$Mx[-101] - tbldf$Mx); abline(0,0); abline(mean(df$Mx[-101] - tbldf$Mx), 0, 
                                                  col = "red")
Dx_test = df$Dx[-101] - tbldf$Dx
Cx_test = df$Cx[-101] - tbldf$Cx
Mx_test = df$Mx[-101] - tbldf$Mx



# 1)
# Método con funciones de conmutación
sumas_aseguradas <- 80000 + (0:4)*10000
terminos <- (suma_asegurada*df$Cx[df$x%in%30:34])/df$Dx[df$x == 30]
sum(terminos)

# Metodo con lifecontingencies
Axn(tbl, 30, 5)*80000+IAxn(tbl, 30, 5-1)*10000


# 2) 
# Método con funciones de conmutación
200000*df$Mx[df$x == 30]/df$Dx[df$x == 30] # APV for seguro de vida ordinario

# Metodo con lifecontingencies
Axn(tbl, 30)*200000

# 3)
# Dos años:
# probabilidad en el primer año = 0.1
# probabilidad en el primer año = 0.2
# Tasa de interés = 0.02

# Respuesta:
p1 <- 0.1
p2 <- 0.2
ps <- c(p1, p2)
interes <- 0.2
10000*(p1*(1+interes)^-1 + p2*(1+interes)^-2)
sum(10000*(ps*(1+interes)^(-1:-2)))

# 4)
# Método con funciones de conmutación
sumas_aseguradas <- c(45, 60, 80, 60, 45)*1000
sum(df$Cx[df$x%in%28:(28+4)]/df$Dx[df$x == 28]*sumas_aseguradas)

# Método con LifeContingencies
edades <- 28:(28+4)
sum(Axn(tbl, edades, 1)*sumas_aseguradas*(1+i)^-(0:4)*pxt(tbl, 28, 0:4))

# ^^^ Se consideran como seguros independientes, por lo que se necesitan capitalizar
# de vuelta al tiempo en el cual la persona tiene 28 años y además compensar la 
# asumida probabilidad que se asume de que la persona sobrevive los 28, 29,... años

# 5) 
coef1 <- (df$Cx[df$x == 30]+df$Cx[df$x == 31])/df$Dx[df$x == 30]
coef2 <- (df$Cx[df$x == 32]+df$Cx[df$x == 31])/df$Dx[df$x == 31]
# Respuesta:
coef2/coef1*245.1




  

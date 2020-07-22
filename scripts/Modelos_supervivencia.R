# Funcion de fuerza de mortalidad 
mu_x <- function(x, B, C) B*C^x

# Visualizamos
curve(mu_x(x, 0.006, 1.07), from = 0, to = 120)


# Analicemos data real...
library("lifecontingencies")
data("demoUsa")
usaMale07 <- demoUsa[,c("age", "USSS2007M")]
usaMale00 <- demoUsa[,c("age", "USSS2000M")]

# Cambiamos los nombres por razones estéticas solamente
names(usaMale07) <- c("x","lx")
names(usaMale00) <- c("x","lx")

# Creamos las tablas de vida
tabla07 <- as(usaMale07,"lifetable")
tabla00 <- as(usaMale00,"lifetable")

print(tabla07)

p07 <- pxt(tabla07, 0, 1:100)
p00 <- pxt(tabla00, 0, 1:100)

# Expresiones para mu acorde a diferencias finitas (o simplemenete Taylor)
mu07 <- -0.5*(log(p07)[length(p07)] + log(p07)[-1])
mu00 <- -0.5*(log(p00)[length(p00)] + log(p00)[-1])

# Nos creamos un muestreo de la función
edades <- 1:100
mu <- mu_x(edades, 0.006, 1.07)

# Graficamos
plot(mu, col=4)
points(mu07, col=2)
points(mu00, col=3)

# Densidad
f <- p07[-1]*mu07
f <- f/sum(f)
plot(f)

# Modelo de grado 3
model <- lm(f ~ poly(edades[-length(edades)],3))
summary(model)

# Ajuste
plot(f, col = 2)
lines(predict(model), col = 4, lwd = 2)



# Cargamos las librerias necesarias
#install.packages("car")
#install.packages("lmtest")
# install.packages("MASS")
#install.packages("plotly")
library(plotly)
library(MASS)
library(car)
library(lmtest)
library(sm)
library(dplyr)

########################
# LECTURA DE LOS DATOS #
########################

# Leemos el archivo CSV
datos <- read.csv("CO2_Canada.csv")

# Mostramos los nombres de las columnas para quedarnos con las deseadas
head(datos)
names(datos)
summary(datos)


#############
# PROCESADO #
#############

# Tomamos:
# 4-El tamaño del motor (litros): será continua (explicativa)
# 5-El número de cilindros: será categórica (explicativa)
# 7-El tipo de combustible: será categórica (explicativa)
# 8, 9-Consumo ciudad y autopista (litros por 100 km): será continua (explicativa)
# 12-La emisión de C02 (gramos por km): será continua (respuesta)
columnas_deseadas <- c(4, 5, 7, 8, 9, 12)
datos <- datos[, columnas_deseadas]
# Omitimos el consumo combinado por depender directamente del consumo en ciudad
# y el consumo en autopista

# Cambiamos el nombre de las variables y las sacamos del dataframe
nombres_columnas <- c("tam_motor", "n_cilindros", "tipo_combustible",
                      "consumo_ciudad", "consumo_autopista", 
                      "emision_CO2")
colnames(datos) <- nombres_columnas

# Cambiamos el tipo de variable a los datos
datos$tam_motor <- as.numeric(datos$tam_motor)
datos$consumo_ciudad <- as.numeric(datos$consumo_ciudad)
datos$consumo_autopista <- as.numeric(datos$consumo_autopista)
datos$emision_CO2 <- as.numeric(datos$emision_CO2)
datos$n_cilindros <- as.factor(datos$n_cilindros)
datos$tipo_combustible <- as.factor(datos$tipo_combustible)
names(datos)

# Como hay demasiados grupos, nos quedamos con los 3 que más entradas tienen
table(datos$n_cilindros)
datos <- subset(datos, n_cilindros %in% c("4", "6", "8"))
datos$n_cilindros <- droplevels(datos$n_cilindros)
# X: gasolina normal
# Z: gasolina premium
# E: ethanol
table(datos$tipo_combustible)
datos <- subset(datos, tipo_combustible %in% c("E", "X", "Z"))
datos$tipo_combustible <- droplevels(datos$tipo_combustible)

# Eliminamos filas duplicadas
datos <- distinct(datos)

# Comprobamos que no hay NAs
summary(datos)

attach(datos)


##############################
# TRANSFORMACIÓN LOGARÍTMICA #
##############################

# Tamaño del motor
par(mfrow = c(1,2))
hist(tam_motor, main = "Tamaño del motor", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(tam_motor), col = "red", lty = 2)

log_tam_motor <- log(tam_motor)
hist(log_tam_motor, main = "Logaritmo del tamaño del motor", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_tam_motor), col = "red", lty = 2)


# Consumo en ciudad
hist(consumo_ciudad, main = "Consumo en ciudad", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(consumo_ciudad), col = "red", lty = 2)

log_consumo_ciudad <- log(consumo_ciudad)
hist(log_consumo_ciudad, main = "Logaritmo del consumo en ciudad", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_consumo_ciudad), col = "red", lty = 2)

# Consumo en autopista
hist(consumo_autopista, main = "Consumo en autopista", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(consumo_autopista), col = "red", lty = 2)

log_consumo_autopista <- log(consumo_autopista)
hist(log_consumo_autopista, main = "Logaritmo del consumo en autopista", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_consumo_autopista), col = "red", lty = 2)

# Emisión de CO2
hist(emision_CO2, main = "Emisión de CO2", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(emision_CO2), col = "red", lty = 2)

log_emision_CO2 <- log(emision_CO2)
hist(log_emision_CO2, main = "Logaritmo de la emisión de CO2", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_emision_CO2), col = "red", lty = 2)

##################
# AJUSTE INICIAL # 
##################

# Ajustamos el modelo
modelo1 <- lm(emision_CO2 ~ tam_motor+consumo_ciudad+consumo_autopista)

# Interpretación
summary(modelo1)

# Tamaño del motor: Por cada unidad que aumenta el tamaño del 
# motor, la emisión de CO2 aumenta en 12.44 unidades
# Consumo en ciudad: Por cada unidad que aumenta el consumo en 
# ciudad, la emisión de CO2 aumenta en 7.81 unidades
# Consumo en autopista: Por cada unidad que aumenta el consumo en 
# autopista, la emisión de CO2 aumenta en 5.93 unidades

# Observamos como las 3 variables explicativas son significativas


################
# COLINEALIDAD #
################

# Observamos bastante correlación entres las 3 covariables, sobre todo
# entre las de consumo
x <- cbind(tam_motor, consumo_ciudad, consumo_autopista)
cor(x)

# Factor de inflación de la varianza (FIV):
FIV <- c()

Rj2 <- cor(tam_motor, fitted(lm(tam_motor ~ consumo_ciudad + consumo_autopista)))^2
FIV["tam_motor"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_ciudad, fitted(lm(consumo_ciudad ~ tam_motor + consumo_autopista)))^2
FIV["consumo_ciudad"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_autopista, fitted(lm(consumo_autopista~  tam_motor + consumo_ciudad)))^2
FIV["consumo_autopista"] <- 1/(1-Rj2)

# Obtenemos valores muy elevados para las variables relativas al consumo en
# ciudad y autopista
FIV

# Sin embargo ni el AIC, ni el BIC nos indican que debamos simplificar el modelo
step(modelo1)
step(modelo1, k=log(nrow(datos)))

# Para deshacernos de la colinearidad, nos quedaremos con el consumo que
# que nos permita obtener un R ajustado más alto
summary(modelo1) # 0.8617 
m2 <- lm(emision_CO2 ~ tam_motor+consumo_ciudad)
summary(m2) # 0.8562
m3 <- lm(emision_CO2 ~ tam_motor+consumo_autopista)
summary(m3) # 0.844

# Nos quedamos con el consumo en ciudadd
modelo2 <- lm(emision_CO2 ~ tam_motor + consumo_ciudad)
summary(modelo2)

# Factor de inflación de la varianza (FIV):
FIV <- c()

Rj2 <- cor(tam_motor, fitted(lm(tam_motor ~ consumo_ciudad)))^2
FIV["tam_motor"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_ciudad, fitted(lm(consumo_ciudad ~ tam_motor)))^2
FIV["consumo_ciudad"] <- 1/(1-Rj2)

# Obtenemos valores razonables
FIV


#############
# DIAGNOSIS #
#############

### Capacidad de influencia ###
# Para estudiar la capacidad de influencia analizamos los leverages
leverages <- hat(model.matrix(modelo2)) # diagonal de la matriz hat
n <- dim(model.matrix(modelo2))[1] # número de muestras
p <- dim(model.matrix(modelo2))[2] # número de variables
ind_lev <- which(leverages >= 2*p/n) # buscamos los puntos con un leverage "alto"
length(ind_lev)

################

### Datos atípicos ###
# Para ver si una observación es atípica se analizan los residuos
res <- rstandard(modelo2) # Residuos estandarizados
# Los representamos gráficamente con un histograma
par(mfrow=c(1,1))
# Vemos que hay otra cola más pequeña
hist(res)
rug(res)
# Consideramos atípicas aquellas observaciones cuyos residuos 
# están por arriba o por abajo del 95%
ind_res <- which(abs(res) > 1.96)
length(ind_res)

############

### Influyentes ###
# Para ver si una observación es influyente utilizamos la distancia de Cook
# Se consideran influyentes aquellas variables para las que la distancia 
# de Cook supera la mediana de una distribución F de Snedecor p, n-p
med <- qf(0.5, p, n-p)
indc <- which(cooks.distance(modelo1) > med)
# No hay influyentes
indc

###############

### Gráficas de apoyo para la validación y diagnosis ###
par(mfrow=c(1,1))
#plot(modelo2)
# Observamos dos nubes de puntos distintas en los residuos

# Obtenemos las filas de los datos atípicos
datos_res <- datos[ind_res, ]
# Miramos cuantos forman parte del grupo E
nE_res <- nrow(datos_res[datos_res$tipo_combustible == 'E', ])
totalE <- sum(datos$tipo_combustible=="E")
# El 97% de la clase E se consideran atípicos
nE_res/totalE

# Dibujamos el gráfico de dispersión en 3D
grafico_scatter <- plot_ly(x = datos$tam_motor, 
                           y = datos$consumo_ciudad, 
                           z = datos$emision_CO2,
                           color = tipo_combustible,
                           colors = c("red", "blue", "green"),
                           type = "scatter3d", mode = "markers", marker = list(size=3))

# Función para calcular la altura del plano en cada punto (z = ax + by + c)
altura_del_plano <- function(x, y) {
  return(modelo2$coefficients[2] * x + modelo2$coefficients[3]*y + modelo2$coefficients[1])
}

# Crear un conjunto de datos para el plano
datos_plano <- expand.grid(x = c(min(tam_motor), max(tam_motor)),
                           y = c(min(consumo_ciudad), max(consumo_ciudad)))

datos_plano$z <- altura_del_plano(datos_plano$x, datos_plano$y)
matrix = matrix(datos_plano$z, nrow=2, ncol=2)

# Crear un gráfico de superficie para representar el plano
grafico_plano <- plot_ly(x = c(min(tam_motor), max(tam_motor)), 
                         y = c(min(consumo_ciudad), max(consumo_ciudad)),
                         z = t(matrix))
grafico_plano <- grafico_plano %>% add_surface()

grafico_plano

# Combinar el gráfico de dispersión con el gráfico del plano
grafico_combinado <- subplot(grafico_scatter, grafico_plano)

# Mostrar el gráfico
grafico_combinado %>%
  layout(scene = list(
    xaxis = list(title = "Tamaño del motor"),
    yaxis = list(title = "Consumo en ciudad"),
    zaxis = list(title = "Emisión de CO2")
  ))

grafico_combinado

################
# NUEVO AJUSTE #
################

# Eliminamos los datos atípicos y los de la clase E
indE <- which(datos$tipo_combustible == "E")
ind <- union(ind_res, indE)
n <- n-length(ind)

datos <- datos[-ind, ]
attach(datos)

modelo3 <- lm(emision_CO2 ~ tam_motor + consumo_ciudad)  
summary(modelo3)

# Con AIC no quitamos ninguna variable
step(modelo3)

# Pero con BIC si que quitamos el tamaño del motor
modelo4 <- step(modelo3, k = log(nrow(datos)))
summary(modelo4)

#############
# DIAGNOSIS #
#############

### Capacidad de influencia ###
# Para estudiar la capacidad de influencia analizamos los leverages
leverages <- hat(model.matrix(modelo4)) # diagonal de la matriz hat
n <- dim(model.matrix(modelo4))[1] # número de muestras
p <- dim(model.matrix(modelo4))[2] # número de variables
ind_lev <- which(leverages >= 2*p/n) # buscamos los puntos con un leverage "alto"
length(ind_lev)

################

### Datos atípicos ###
# Para ver si una observación es atípica se analizan los residuos
res <- rstandard(modelo4) # Residuos estandarizados
# Los representamos gráficamente con un histograma
par(mfrow=c(1,1))
# Vemos que hay otra cola más pequeña
hist(res)
rug(res)
# Consideramos atípicas aquellas observaciones cuyos residuos 
# están por arriba o por abajo del 95%
ind_res <- which(abs(res) > 1.96)
length(ind_res)

############

### Influyentes ###
# Para ver si una observación es influyente utilizamos la distancia de Cook
# Se consideran influyentes aquellas variables para las que la distancia 
# de Cook supera la mediana de una distribución F de Snedecor p, n-p
med <- qf(0.5, p, n-p)
indc <- which(cooks.distance(modelo4) > med)
# No hay influyentes
indc

###############

### Gráficas de apoyo para la validación y diagnosis ###
par(mfrow=c(1,1))
#plot(modelo4)

################
# NUEVO AJUSTE #
################

# Eliminamos los datos atípicos
n <- n-length(ind_res)

datos <- datos[-ind_res, ]
attach(datos)

modelo5 <- lm(emision_CO2 ~ consumo_ciudad)
summary(modelo5)
     

##############
# VALIDACIÓN #
##############

### Normalidad ###

res <- rstandard(modelo5)

# Test de Shapiro-Wilk
shapiro.test(res)

# Histograma
hist(res,freq=F)

# QQplot
qqPlot(res)


### Homocedasticidad ###
# Test de Breusch-Pagan:
# H0: homocedast vs Ha: varianza dependente da explicativa
bptest(modelo5)

# Test de Harrison-McCabe:
# H0: homocedast vs Ha: varianza ten un punto de cambio.
hmctest(modelo5)

# Gráficos de apoio: residuos vs explicativa
plot(res ~ emision_CO2, pch=19)
abline(h=0, lty=2) # Sospeitamos que non hai homocedasticidade

# Residuos frente frente a la predicción
# Aquí hay un patrón muy raro
plot(res ~ modelo5$fitted.values, pch=19)
abline(h=0, lty=2)
abline(h=2, lty=2, col="red")
abline(h=-2, lty=2, col="red")

### Linealidad ###
# Test de Ramsey:
# H0: linealidade; Ha: polinomio de grao 2 ou 3
resettest(modelo5)

# Test de Harvey-Collier:
# H0: linealidade; Ha: non linealidade (concavidad/convexidade
# globais*)
harvtest(modelo5)

# De novo as conclusións son diferentes porque o plantexamento
# dos distintos tests é diferente.

# Graficamente:
plot(emision_CO2 ~ modelo5$fitted.values, pch=19) # Xeral
abline(c(0,1))

# Calculamos intervalo de confianza al 95% para la pendiente
confint(modelo5)

# Representación del ajuste final
par(mfrow=c(1,1), cex.lab = 1.25)
plot(consumo_ciudad, emision_CO2)
abline(a=modelo5$coefficients[1], b=modelo5$coefficients[2], col="red")

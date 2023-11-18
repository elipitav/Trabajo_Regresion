# Cargamos las librerias necesarias
#install.packages("car")
#install.packages("lmtest")
library(car)
library(lmtest)
library(sm)

########################
# LECTURA DE LOS DATOS #
########################

# Leemos el archivo CSV
datos <- read.csv("CO2_Canada.csv")

# Mostramos los nombres de las columnas para quedarnos con las deseadas
head(datos)
names(datos)

# Tomamos:
# 4-El tamaño del motor (litros): será continua (explicativa)
# 5-El número de cilindros: será categórica (explicativa)
# 7-El tipo de combustible: será categórica (explicativa)
# 10-Consumo combinado en ciudad y autopista (litros por 100 km): será continua (explicativa)
# 12-La emisión de C02 (gramos por km): será continua (respuesta)
columnas_deseadas <- c(4, 5, 7, 8, 9, 10, 12)
datos <- datos[, columnas_deseadas]

# Cambiamos el nombre de las variables y las sacamos del dataframe
nombres_columnas <- c("tam_motor", "n_cilindros", "tipo_combustible",
                      "consumo_ciudad", "consumo_autopista", 
                      "consumo_combinado", "emision_CO2")
colnames(datos) <- nombres_columnas

# Cambiamos el tipo de variable a los datos
datos$tam_motor <- as.numeric(datos$tam_motor)
datos$consumo_ciudad <- as.numeric(datos$consumo_ciudad)
datos$consumo_autopista <- as.numeric(datos$consumo_autopista)
datos$consumo_autopista <- as.numeric(datos$consumo_combinado)
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
attach(datos)

##################

##########################
# REPRESENTACIÓN INICIAL #
##########################

# Dibujamos las gráficas para ver si puede llegar a haber relación entre las
# continuas
pairs(datos[,c(1,4,5,6,7)])


# Comprobamos si la distribución de las variables continuas es simétrica

# Tamaño del motor
hist(tam_motor, main = "Tamaño del motor", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(tam_motor), col = "red", lty = 2)
abline(v = median(tam_motor), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

log_tam_motor <- log(tam_motor)
hist(log_tam_motor, main = "Logaritmo del tamaño del motor", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_tam_motor), col = "red", lty = 2)
abline(v = median(log_tam_motor), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)


# Consumo en ciudad
hist(consumo_ciudad, main = "Consumo en ciudad", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(consumo_ciudad), col = "red", lty = 2)
abline(v = median(consumo_ciudad), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

log_consumo_ciudad <- log(consumo_ciudad)
hist(log_consumo_ciudad, main = "Logaritmo del consumo en ciudad", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_consumo_ciudad), col = "red", lty = 2)
abline(v = median(log_consumo_ciudad), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

# Consumo en autopista
hist(consumo_autopista, main = "Consumo en autopista", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(consumo_autopista), col = "red", lty = 2)
abline(v = median(consumo_autopista), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

log_consumo_autopista<- log(consumo_autopista)
hist(log_consumo_autopista, main = "Logaritmo del consumo en autopista", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_consumo_autopista), col = "red", lty = 2)
abline(v = median(log_consumo_autopista), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

# Emisión de CO2
hist(emision_CO2, main = "Emisión de CO2", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(emision_CO2), col = "red", lty = 2)
abline(v = median(emision_CO2), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)

log_emision_CO2<- log(emision_CO2)
hist(log_emision_CO2, main = "Logaritmo de la emisión de CO2", xlab = "Valores", ylab = "Frecuencia", col = "lightblue", border = "black")
abline(v = mean(log_emision_CO2), col = "red", lty = 2)
abline(v = median(log_emision_CO2), col = "blue", lty = 2)
legend("topright", legend = c("Media", "Mediana"), 
       col = c("red", "blue"), lty = 2)


# Representamos las variables continuas distinguiendo en número de cilindros
par(mfrow=c(1,3), mar=c(4,4,4,4))

n4 <- n_cilindros == "4"
n6 <- n_cilindros == "6"
n8 <- n_cilindros == "8"

# Tam_motor
plot(tam_motor~emision_CO2)
points(tam_motor[n4]~emision_CO2[n4], col="Red", pch=16)
points(tam_motor[n6]~emision_CO2[n6], col="Blue", pch=16)
points(tam_motor[n8]~emision_CO2[n8], col="Green", pch=16)
legend("bottomright", 
       legend=c("4", "6", "8"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo_ciudad
plot(consumo_ciudad~emision_CO2)
points(consumo_ciudad[n4]~emision_CO2[n4], col="Red", pch=16)
points(consumo_ciudad[n6]~emision_CO2[n6], col="Blue", pch=16)
points(consumo_ciudad[n8]~emision_CO2[n8], col="Green", pch=16)
legend("bottomright", 
       legend=c("4", "6", "8"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo autopista
plot(consumo_autopista~emision_CO2)
points(consumo_autopista[n4]~emision_CO2[n4], col="Red", pch=16)
points(consumo_autopista[n6]~emision_CO2[n6], col="Blue", pch=16)
points(consumo_autopista[n8]~emision_CO2[n8], col="Green", pch=16)
legend("bottomright", 
       legend=c("4", "6", "8"), 
       col=c("Red", "Blue", "Green"), pch=16)


# Añadir título a la disposición completa
title("Número de cilindros", line = -2, outer = TRUE, cex = 1.5)



# Representamos las variables continuas distinguiendo en número de cilindros
par(mfrow=c(1,3), mar=c(4,4,4,4))

cE <- tipo_combustible == "E"
cX <- tipo_combustible == "X"
cZ <- tipo_combustible == "Z"

# Tam_motor
plot(tam_motor~emision_CO2)
points(tam_motor[cE]~emision_CO2[cE], col="Red", pch=16)
points(tam_motor[cX]~emision_CO2[cX], col="Blue", pch=16)
points(tam_motor[cZ]~emision_CO2[cZ], col="Green", pch=16)
legend("bottomright", 
       legend=c("E", "X", "Z"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo_ciudad
plot(consumo_ciudad~emision_CO2)
points(consumo_ciudad[cE]~emision_CO2[cE], col="Red", pch=16)
points(consumo_ciudad[cX]~emision_CO2[cX], col="Blue", pch=16)
points(consumo_ciudad[cZ]~emision_CO2[cZ], col="Green", pch=16)
legend("bottomright", 
       legend=c("E", "X", "Z"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo autopista
plot(consumo_autopista~emision_CO2)
points(consumo_autopista[cE]~emision_CO2[cE], col="Red", pch=16)
points(consumo_autopista[cX]~emision_CO2[cX], col="Blue", pch=16)
points(consumo_autopista[cZ]~emision_CO2[cZ], col="Green", pch=16)
legend("bottomright", 
       legend=c("E", "X", "Z"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Añadir título a la disposición completa
title("Tipo de combustible", line = -2, outer = TRUE, cex = 1.5)

##################


###################
# MODELO MÚLTIPLE # 
###################

# Ajustamos el modelo
modelo1 <- lm(emision_CO2 ~ tam_motor+consumo_autopista+consumo_ciudad)
#modelo1 <- lm(log_emision_CO2 ~ log_tam_motor+log_consumo_autopista+log_consumo_ciudad)

# Interpretación
summary(modelo1)

# Tamaño del motor: Por cada unidad que aumenta el tamaño del motor,
# el consumo de CO2 aumenta en 12.5 unidades
# Consumo en autopista: Por cada unidad que aumenta el consumo en autopista,
# el consumo de CO2 aumenta en 12.8 unidades
# Consumo en ciudad: Por cada unidad que aumenta el consumo en ciudad,
# el consumo de CO2 aumenta en 0.49 unidades
# Las 2 primeras covariables salen significativas pero las dos últimas no

##################

################
# COLINEALIDAD #
################

# Observamos bastante correlación entres las 3 covariables
x <- cbind(tam_motor, consumo_ciudad, consumo_autopista)
cor(x)

# Factor de inflación de la varianza (FIV):
FIV <- c()

Rj2 <- cor(tam_motor, fitted(lm(tam_motor ~ consumo_ciudad + consumo_autopista)))^2
FIV["tam_motor"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_ciudad, fitted(lm(consumo_ciudad~ tam_motor + consumo_autopista)))^2
FIV["consumo_ciudad"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_autopista, fitted(lm(consumo_autopista~  tam_motor + consumo_ciudad)))^2
FIV["consumo_autopista"] <- 1/(1-Rj2)

# Obtenemos valores muy elevados para las variables relativas al consumo en
# ciudad y autopista
FIV


# Procedemos a simplificar el modelo
# Nos quedamos con el modelo simplificado
modelo2 <- step(modelo1)
summary(modelo2)

# Tamaño del motor: Por cada unidad que aumenta el tamaño del motor,
# el consumo de CO2 aumenta en 12.5 unidades
# Consumo en autopista: Por cada unidad que aumenta el consumo en autopista,
# el consumo de CO2 aumenta en 13.3 unidades

# Factor de inflación de la varianza (FIV):
FIV <- c()

Rj2 <- cor(tam_motor, fitted(lm(tam_motor ~  consumo_autopista)))^2
FIV["tam_motor"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_autopista, fitted(lm(consumo_autopista~  tam_motor)))^2
FIV["consumo_autopista"] <- 1/(1-Rj2)

# Ahora obtenemos valores razonables
FIV

##################

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

# Obtenemos las filas de los que tienen capacidad de influencia
datos_lev <- datos[ind_lev, ]
# Miramos cuantos de ellos son de la clase "E"
nE_lev <- nrow(datos_lev[datos_lev$tipo_combustible == 'E', ])
totalE <- nrow(datos[datos$tipo_combustible == 'E', ])
# El 75% de la clase E se consideran con capacidad de influencia
nE_lev/totalE

################

### Datos atípicos ###
# Para ver si una observación es atípica se analizan los residuos
res <- rstandard(modelo2) # Residuos estandarizados
# Los representamos gráficamente con un histograma
par(mfrow=c(1,1))
hist(res)
rug(res)

# Consideramos atípicas aquellas observaciones cuyos residuos 
# están por arriba o por abajo del 95%
ind_res<- which(abs(res) > 1.96)
length(ind_res)

# Obtenemos las filas de los que tienen capacidad de influencia
datos_res <- datos[ind_res, ]
nE_res <- nrow(datos_res[datos_res$tipo_combustible == 'E', ])
# El 100% de la clase E se consideran atípicos
nE_res/totalE

############

### Influyentes ###

# Para ver si una observación es influyente utilizamos la distancia de Cook
# Se consideran influyentes aquellas variables para las que la distancia 
# de Cook supera la mediana de una distribución F de Snedecor p, n-p
med <- qf(0.5, p, n-p)
indc <- which(cooks.distance(modelo2) > med)
# No hay influyentes
indc

# Gráficamente
hist(cooks.distance(modelo2))
rug(cooks.distance(modelo2))

###############

### Gráficas de apoyo para la validación y diagnosis ###
par(mfrow=c(1,1))
plot(modelo2)


################
# NUEVO AJUSTE #
################

# Eliminamos las observaciones relativas a la clase E
ind_E <- which(tipo_combustible == "E")
n <- n-length(ind_E)

datos <- datos[-ind_E, ]
attach(datos)

modelo3 <- lm(emision_CO2 ~ tam_motor + consumo_autopista)  
summary(modelo3)
# Tamaño del motor: Por cada unidad que aumenta el tamaño del motor,
# el consumo de CO2 aumenta en 0.09 unidades
# Consumo en autopista: Por cada unidad que aumenta el consumo en autopista,
# el consumo de CO2 aumenta en 23.22 unidades

# Al haber quitado las variables pertenecientes a la categoría "E", la covariable
# relativa al tamaño del motor ha perdido significación. De hecho, su valor se
# ha reducido en gran medida, casi hasta cero. Sin embargo sigue siendo
# significativa para un nivel del 5%.
# Por otro lado, el del consumo de autopista ha aumentado.

################
# COLINEALIDAD #
################

# Observamos bastante correlación entres las 3 covariables
x <- cbind(tam_motor, consumo_autopista)
cor(x)

# Factor de inflación de la varianza (FIV):
FIV <- c()

Rj2 <- cor(tam_motor, fitted(lm(tam_motor ~ consumo_autopista)))^2
FIV["tam_motor"] <- 1/(1-Rj2)

Rj2 <- cor(consumo_autopista, fitted(lm(consumo_autopista~  tam_motor)))^2
FIV["consumo_autopista"] <- 1/(1-Rj2)

# Obtenemos valores razonables, por lo que no haría falta simplificar el modelo
FIV

##################
# SIMPLIFICACIÓN #
##################

summary(modelo3) # R2 ajustado = 0.9977 

m1 <- lm(emision_CO2 ~ tam_motor)  
summary(m1) # 0.7013

m2 <- lm(emision_CO2 ~ consumo_autopista)  
summary(m2) # 0.9977: Se mantiene igual, por lo que podemos quitar la covariable
modelo4 <- m2

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
hist(res)
rug(res)

# Consideramos atípicas aquellas observaciones cuyos residuos 
# están por arriba o por abajo del 95%
ind_res<- which(abs(res) > 1.96)
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

# Gráficamente
hist(cooks.distance(modelo4))
rug(cooks.distance(modelo4))

###############

### Gráficas de apoyo para la validación y diagnosis ###
par(mfrow=c(1,1))
plot(modelo4)

################
# NUEVO AJUSTE #
################

# Eliminamos las observaciones atípicas:
n <- n-length(ind_res)

datos <- datos[-ind_res,]
attach(datos)

modelo5 <- lm(emision_CO2 ~ consumo_autopista)  
summary(modelo5)

##############
# VALIDACIÓN #
##############

### Normalidad ###

  res <- rstandard(modelo5)
  
  # Test de Shapiro-Wilk
  shapiro.test(sample(res, 5000, replace = TRUE))
  
  # Histograma
  hist(res,freq=F)
  
  # QQplot
  qqPlot(res)
  
  # Gráfico de dispersión
  plot(consumo_autopista, emision_CO2, pch=19)
  points(consumo_autopista[c(632,853)], emision_CO2[c(632,853)], col=2, pch=19)


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
  
  # Test contra non-paramétrica (métodos de suavizado):
  # H0: linealidade; Ha: axuste non paramétrico (segue os puntos)
  sm.regression(consumo_autopista, emision_CO2, model="linear")
  
  # De novo as conclusións son diferentes porque o plantexamento
  # dos distintos tests é diferente.
  
  # Graficamente:
  plot(emision_CO2 ~ consumo_autopista, pch=19) # Para RLS
  plot(emision_CO2 ~ modelo5$fitted.values, pch=19) # Xeral

    
  
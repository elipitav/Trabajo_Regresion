# Cargamos las librerias necesarias
#install.packages("car")
#install.packages("lmtest")
#install.packages(c("ggplot2", "plotly"))
# install.packages("MASS")
library(MASS)
library(car)
library(lmtest)
library(sm)
library(ggplot2)
library(plotly)

########################
# LECTURA DE LOS DATOS #
########################

# Leemos el archivo CSV
datos <- read.csv("CO2_Canada.csv")

# Mostramos los nombres de las columnas para quedarnos con las deseadas
head(datos)
names(datos)
summary(datos)


#########################################
# ANÁLISIS DE LAS VARIABLES CATEGÓRICAS #
#########################################

# Tomamos las columnas de variables categóricas
# 1. Marca
# 2. Modelo
# 3. Tipo de vehículo
# 5. Número de cilindros
# 6. Tipo de transmisión
# 7. Tipo de combustible
columnas_deseadas <- c(1, 2, 3, 5, 6, 7)
var_cat <- datos[, columnas_deseadas]

# Cambiamos el nombre de las variables y las sacamos del dataframe
nombres_columnas <- c("marca", "modelo", "tipo_vehiculo",
                      "n_cilindros", "transmision", "tipo_combustible")
colnames(var_cat) <- nombres_columnas

# Convertimos todas las columnas a factor
var_cat <- data.frame(lapply(var_cat, factor))

# Vemos cuantas clases hay en cada una
print(sapply(var_cat, nlevels))

sapply(var_cat[,c(4,6)], table)


####################
# ANÁLISIS GENERAL #
####################

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

##########################
# REPRESENTACIÓN INICIAL #
##########################

# Representamos las variables continuas distinguiendo en número de cilindros
par(mfrow=c(1,2), mar=c(4,4,4,1), cex.lab = 1.25)

n4 <- n_cilindros == "4"
n6 <- n_cilindros == "6"
n8 <- n_cilindros == "8"

# Tam_motor
plot(tam_motor, emision_CO2)
points(tam_motor[n4], emision_CO2[n4], col="Red", pch=16)
points(tam_motor[n6], emision_CO2[n6], col="Blue", pch=16)
points(tam_motor[n8], emision_CO2[n8], col="Green", pch=16)
legend("bottomright", 
       legend=c("4", "6", "8"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo autopista
plot(consumo_autopista, emision_CO2)
points(consumo_autopista[n4], emision_CO2[n4], col="Red", pch=16)
points(consumo_autopista[n6], emision_CO2[n6], col="Blue", pch=16)
points(consumo_autopista[n8], emision_CO2[n8], col="Green", pch=16)
legend("bottomright", 
       legend=c("4", "6", "8"), 
       col=c("Red", "Blue", "Green"), pch=16)


# Representamos las variables continuas distinguiendo en número de cilindros
par(mfrow=c(1,2), mar=c(4,4,4,1), cex.lab = 1.25)

cE <- tipo_combustible == "E"
cX <- tipo_combustible == "X"
cZ <- tipo_combustible == "Z"

# Tam_motor
plot(tam_motor, emision_CO2)
points(tam_motor[cE], emision_CO2[cE], col="Red", pch=16)
points(tam_motor[cX], emision_CO2[cX], col="Blue", pch=16)
points(tam_motor[cZ], emision_CO2[cZ], col="Green", pch=16)
legend("bottomright", 
       legend=c("E", "X", "Z"), 
       col=c("Red", "Blue", "Green"), pch=16)

# Consumo autopista
plot(consumo_autopista, emision_CO2)
points(consumo_autopista[cE], emision_CO2[cE], col="Red", pch=16)
points(consumo_autopista[cX], emision_CO2[cX], col="Blue", pch=16)
points(consumo_autopista[cZ], emision_CO2[cZ], col="Green", pch=16)
legend("bottomright", 
       legend=c("E", "X", "Z"), 
       col=c("Red", "Blue", "Green"), pch=16)






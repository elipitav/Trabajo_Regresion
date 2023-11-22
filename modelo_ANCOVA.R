library(dplyr)

########################
# LECTURA DE LOS DATOS #
########################

# Leemos el archivo CSV
datos <- read.csv("CO2_Canada.csv")

# Mostramos los nombres de las columnas para quedarnos con las deseadas
head(datos)
names(datos)

# Quedamonos soamente coa columna 7 para o modelo ANOVA: Fuel.Type
columnas_deseadas <- c(4, 5, 7, 8, 9, 10, 12)
datos <- datos[, columnas_deseadas]
nombres_columnas <- c(
  "tam_motor", "n_cilindros", "tipo_combustible",
  "consumo_ciudad", "consumo_autopista",
  "consumo_combinado", "emision_CO2"
)
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

# Eliminamos filas duplicadas
datos <- distinct(datos)

attach(datos)

##################



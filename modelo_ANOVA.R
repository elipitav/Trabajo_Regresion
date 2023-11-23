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

# Representamos nun diagrama de caixas os datos
par(mfrow = c(1, 1))
plot(emision_CO2 ~ tipo_combustible, xlab = "Tipo de combustible", ylab = "Emisión de CO2")

# Axustamos un modelo ANOVA. Utilizamos a parametrizacion
# que emprega un grupo de referencia
# No noso, caso, o grupo de referencia sera o correspondente co tipo
# de combustible E
levels(tipo_combustible)
table(tipo_combustible)
modelo <- lm(emision_CO2 ~ tipo_combustible)
summary(modelo)
# Interpretación:
# - O intercepto correspóndese coa media do
# primeiro grupo (tipo de combustible E). Vemos que é significativamente
# distinta de 0 (p-valor: < 2.2e-16).
# - A desviación do grupo X respecto do grupo E é significativamente
# distinta de 0 (p-valor: < 2.2e-16).
# - A desviación do grupo Z respecto do grupo E non é significativamente
# distinta de 0 para os valores de significación habituais (p-valor: 0.137).
# Polo tanto, non existen evidencias significativas para rexeitar que a media
# do grupo Z é igual á media do grupo E, polo que podemos aceptar que son iguais
# - O R2 é bastante baixo (0.07627), o que indica que podemos estar omitindo
# outras variables que axudan a explicar a variabilidade das emisións de CO2.
# - O p-valor do F-test é significativo (< 2.2e-16), polo que podemos rexeitar a
# hipótese nula de que as medias dos grupos son iguais.

# Podemos obter a táboa de ANOVA co seguinte comando, onde obteremos o mesmo
# p-valor
anova(modelo)

#############
#    IOC    #
#############

# Calculamos os intervalos de confianza para as desviacións
# entre todos os grupos co metodo Tukey
TukeyHSD(aov(emision_CO2 ~ tipo_combustible))
# Observando a saída, en primeiro lugar, na columna "p adj" observamos que
# todas as desviacións son significativamente distintas de 0 (para os niveis
# de significación habituais) excepto a desviación do grupo Z fronte á media
# do grupo E, como xa vimos na saída do `summary`.
# Por outro lado, podemos observar os extremos dos intervalos de confianza
# para as desviacións das medias entre os grupos nas columans "lwr" e "upr".
# Na columna "diff" observamos a diferenza entre as medias dos grupos.

# Representamos os intervalos de confianza para as desviacións entre todos os
# grupos co metodo Tukey
par(mfrow = c(1, 1))
plot(TukeyHSD(aov(emision_CO2 ~ tipo_combustible)))
# Vemos que os intervalos de confianza para as desviacións X-E e Z-X non
# contenhen ao 0, co que podemos dicir, de novo, que as medias dos grupos son
# significativamente distintas cun nivel de significación do 95%.
# Non obstante, o intervalo de confianza para a desviación Z-E contén ao 0,
# polo que non podemos dicir que as medias dos grupos Z e E sexan
# significativamente distintas cun nivel de significación do 95%, como xa
# apuntamos anteriormente

# Calculamos os intervalos de confianza co metodo de Bonferroni
I <- length(unique(tipo_combustible))
k <- I * (I - 1) / 2
# Construimos intervalos de confianza segundo o metodo de Bonferroni con nivel
# de significacion alpha/2k
alpha_global <- 0.05
alpha <- alpha_global / (2 * k)
# Diferencias entre as medias
medias <- tapply(emision_CO2, tipo_combustible, mean)
# Numero de observacions en cada grupo
numero_observacions <- tapply(emision_CO2, tipo_combustible, length)
n <- sum(numero_observacions) # total de observacions
desv <- sqrt(sum((emision_CO2 - medias[tipo_combustible])^2) / (n - I))

intervalos_bonferroni <- data.frame(matrix(0, nrow = 0, ncol = 3))
colnames(intervalos_bonferroni) <- c("diff", "lwr", "upr")
tipos_combustible <- levels(tipo_combustible)

for (i in seq_len(I)) {
  if (i == I) break
  for (j in seq(i + 1, I)) {
    # Obtemos a diferencia entre as medias
    diferencia <- medias[j] - medias[i]
    # Obtemos o intervalo de confianza
    intervalo <- diferencia + c(-1, 1) * qt(1 - alpha, n - I) * desv * sqrt(1 / numero_observacions[i] + 1 / numero_observacions[j])
    df <- data.frame(matrix(0, nrow = 1, ncol = 3))
    colnames(df) <- c("diff", "lwr", "upr")
    df[1, ] <- c(diferencia, intervalo)
    rownames(df) <- c(paste(tipos_combustible[j], tipos_combustible[i], sep = "-"))
    intervalos_bonferroni <- rbind(intervalos_bonferroni, df)
  }
}
# Podemos observar que os intervalos de confianza para as desviacions
# obtidos co metodo de Bonferroni son moi semellantes aos obtidos
# co metodo de Tukey
intervalos_bonferroni
# Podemos obter os p-valores para as desviacións entre todos os grupos co
# metodo de Bonferroni do seguinte xeito
pairwise.t.test(emision_CO2, tipo_combustible, p.adjust.method = "bonferroni")

#############
# DIAGNOSIS #
#############

# Estudamos os datos atipicos por grupos. Para iso, calculamos
# os residuos estandarizados por grupos e calculamos aqueles que
# son maiores que 1.96 (que corresponden a un nivel de significación
# do 95% para unha distribución normal)

indices_E <- which(datos$tipo_combustible == "E")
indices_X <- which(datos$tipo_combustible == "X")
indices_Z <- which(datos$tipo_combustible == "Z")

# Atipicos para o grupo E
res_E <- rstandard(lm(emision_CO2[indices_E] ~ 1))
ind_res_E <- which(abs(res_E) > 1.96)
indices_E_atipicos = indices_E[ind_res_E]
length(indices_E_atipicos) # Temos 13 atipicos

# Atipicos para o grupo X
res_X <- rstandard(lm(emision_CO2[indices_X] ~ 1))
ind_res_X <- which(abs(res_X) > 1.96)
indices_X_atipicos <- indices_X[ind_res_X]
length(indices_X_atipicos) # Temos 104 atipicos

# Atipicos para o grupo Z
res_Z <- rstandard(lm(emision_CO2[indices_Z] ~ 1))
ind_res_Z <- which(abs(res_Z) > 1.96)
indices_Z_atipicos <- indices_Z[ind_res_Z]
length(indices_Z_atipicos) # Temos 64 atipicos

par(mfrow = c(1, 3), cex.main = 3)
plot(emision_CO2[indices_E], col = ifelse(indices_E %in% indices_E_atipicos, "red", "black"), pch = 19)
plot(emision_CO2[indices_X], col = ifelse(indices_X %in% indices_X_atipicos, "red", "black"), pch = 19)
plot(emision_CO2[indices_Z], col = ifelse(indices_Z %in% indices_Z_atipicos, "red", "black"), pch = 19)
title("Datos atípicos en cada grupo", line = -2, outer = TRUE)

# Debuxamos os atipicos en vermello, en total hai 315 atipicos
indices_atipicos <- c(indices_E_atipicos, indices_X_atipicos, indices_Z_atipicos)
par(mfrow = c(1, 1), cex.main = 2.5)
plot(emision_CO2 ~ as.numeric(tipo_combustible),
  col = ifelse(seq_along(emision_CO2) %in% indices_atipicos, "red", "black"),
  pch = 19, xlab="Tipo de combustible", ylab="Emisión de CO2"
)
title("Datos atípicos en cada grupo", line = -2, outer = TRUE)

##############
# VALIDACION #
##############

# Comprobamos a hipotese de normalidade dentro de cada grupo
# mediante o test de Shapiro-Wilk
plot(density(emision_CO2[tipo_combustible == "E"]), 
main = "Densidad de las emisiones de CO2 en el grupo E")
shapiro.test(emision_CO2[tipo_combustible == "E"])
# Envista do p-valor, 0.07586, non existen evidencias
# significativas para rexeitar a hipótese de normalidade
# con niveis de significación iguais ou inferiores a 0.05
# para os datos do grupo E

shapiro.test(emision_CO2[tipo_combustible == "X"])
plot(density(emision_CO2[tipo_combustible == "X"]))
# p-valor: 1.339e-14

shapiro.test(emision_CO2[tipo_combustible == "Z"])
plot(density(emision_CO2[tipo_combustible == "Z"]))
# p-valor: 3.303e-16

# En vista dos p-valores, soamente podemos aceptar que os datos
# do grupo E veñen dunha distribución normal.

# Realizamos duas transformacions dos datos para ver se conseguimos
# solucionar os problemas asociados a non normalidade (log e sqrt).
# Non conseguimos solucionar o problema en vista dos p-valores obtidos

# LOG
emision_CO2_log <- log(emision_CO2)

plot(density(emision_CO2_log[tipo_combustible == "E"]))
shapiro.test(emision_CO2_log[tipo_combustible == "E"])
# p-valor: 0.000314 -> empeoramos o p-valor sen a transformación

shapiro.test(emision_CO2_log[tipo_combustible == "X"])
plot(density(emision_CO2_log[tipo_combustible == "X"]))
# p-valor: 7.238e-09

shapiro.test(emision_CO2_log[tipo_combustible == "Z"])
plot(density(emision_CO2_log[tipo_combustible == "Z"]))
# p-valor: 8.935e-06

# SQRT
emision_CO2_sqrt <- sqrt(emision_CO2)
plot(density(emision_CO2_sqrt[tipo_combustible == "E"]))
shapiro.test(emision_CO2_sqrt[tipo_combustible == "E"])
# p-valor: 0.03394 -> empeoramos o p-valor sen a transformación

shapiro.test(emision_CO2_sqrt[tipo_combustible == "X"])
plot(density(emision_CO2_sqrt[tipo_combustible == "X"]))
# p-valor: 1.369e-08

shapiro.test(emision_CO2_sqrt[tipo_combustible == "Z"])
plot(density(emision_CO2_sqrt[tipo_combustible == "Z"]))
# p-valor: 4.71e-10

# Comprobamos a hipotese de homocedasticidade mediante o test de
# Levene. En vista do p-valor, existen evidencias para todos os niveis
# de significación habituais de que as varianzas son distintas
abs_res <- abs(modelo$residuals)
levene_modelo <- lm(abs_res ~ tipo_combustible)
anova(levene_modelo)
# p-valor: 2.587e-08

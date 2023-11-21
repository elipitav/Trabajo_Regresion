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

datos$tam_motor <- log(datos$tam_motor)
datos$consumo_ciudad <- log(datos$consumo_ciudad)
datos$consumo_autopista <- log(datos$consumo_autopista)
datos$emision_CO2 <- log(datos$emision_CO2)

attach(datos)

##################

# Representamos nun diagrama de caixas os datos
par(mfrow = c(1, 1))
plot(emision_CO2 ~ tipo_combustible)

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
# distinta de 0 (p-valor < 2.2e-16).
# - As desviacións dos grupos X e Z respecto do grupo E son significativamente
# distintas de 0 (p-valor < 2.2e-16 e p-valor = 1.92e-06, respectivamente).
# - O R2 é bastante baixo (0.07053), o que indica que podemos estar omitindo
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
# de significación habituais). Isto é, podemos rexeitar a hipótese nula de que
# as medias dos grupos son iguais.
# Por outro lado, podemos observar os extremos dos intervalos de confianza
# para as desviacións das medias entre os grupos nas columans "lwr" e "upr".
# Na columna "diff" observamos a diferenza entre as medias dos grupos.

# Representamos os intervalos de confianza para as desviacións entre todos os
# grupos co metodo Tukey
par(mfrow = c(1, 1))
plot(TukeyHSD(aov(emision_CO2 ~ tipo_combustible)))
# Vemos que ningún intervalo de confianza non contén ao 0, co que podemos dicir,
# de novo, que as medias dos grupos son significativamente distintas cun nivel
# de significación do 95%.

# Calculamos os intervalos de confianza co metodo de Bonferroni
I <- length(unique(Lugar))
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
    rownames(df) <- c(paste(j, i, sep = "-"))
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

##############
# VALIDACION #
##############



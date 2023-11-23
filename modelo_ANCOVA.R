library(dplyr)
library(plotly)
library(car)
library(lmtest)
library(lawstat)

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

##########################
# MODELO SEN INTERACCIÓN #
##########################

modelo_si <- lm(emision_CO2 ~ consumo_ciudad + tipo_combustible)
summary(modelo_si)
# Interpretación:
# - O intercepto correspóndese co intercepto da recta axustada
# correspondente ao primeiro grupo (tipo de combustible E).
# Vemos que é significativamente distinto de 0 (p-valor: < 2.2e-16).
# - As desviacións do intercepto das rectas axustadas para os outros dous
# grupos (tipo de combustible X e Z) tamén son significativamente
# distintas de 0 (p-valor: < 2.2e-16 en ambos casos).
# - A pendente das rectas axustadas para os tres grupos é significativamente
# distinta de 0 (p-valor: < 2e-16). Isto indica que ten sentido incorporar
# a variable consumo_ciudad ao modelo.
# - O R2 é moi alto (0.9766), o que indica que estamos sendo capaces de
# explicar a maior parte da variabilidade dos datos.

plot(emision_CO2 ~ consumo_ciudad)
pe <- tipo_combustible == "E"
px <- tipo_combustible == "X"
pz <- tipo_combustible == "Z"
# Coloreamos distinto os puntos de grupos distintos
points(emision_CO2[pe] ~ consumo_ciudad[pe], col = "red", pch = 16)
points(emision_CO2[px] ~ consumo_ciudad[px], col = "blue", pch = 16)
points(emision_CO2[pz] ~ consumo_ciudad[pz], col = "green", pch = 16)

# Representamos as rectas paralelas axustadas
betas <- modelo_si$coefficients
abline(a = betas[1], b = betas[2], col = "red", lwd = 2)
abline(a = betas[1] + betas[3], b = betas[2], col = "blue", lwd = 2)
abline(a = betas[1] + betas[4], b = betas[2], col = "green", lwd = 2)

legend(
  "bottomright",
  legend = c("E", "X", "Z"),
  col = c("red", "blue", "green"),
  pch = 20
)

# Facemos un F-test para ver se preferimos un modelo ANOVA
# Posto que o p-valor é moi pequeno, preferimos o modelo ANCOVA
# sen interaccion (rexeitamos a hipotese nula: o modelo ANOVA)
anova(lm(emision_CO2 ~ tipo_combustible), modelo_si)
# p-valor: < 2.2e-16

##########################
# MODELO CON INTERACCIÓN #
##########################

modelo_ci <- lm(emision_CO2 ~ consumo_ciudad * tipo_combustible)
summary(modelo_ci)
# Interpretación:
# - O intercepto correspóndese co intercepto da recta axustada
# correspondente ao primeiro grupo (tipo de combustible E).
# Vemos que agora, non é significativamente distinto de 0 (p-valor: 0.269).
# Isto pode ter sentido, xa que se o consumo e 0 o loxico seria que as
# emisións tamén o fosen.
# - As desviacións do intercepto das rectas axustadas para os outros dous
# grupos (tipo de combustible X e Z) tamén son significativamente
# distintas de 0 (p-valor: 4.23e-09 e 1.27e-12, respectivamente).
# - A pendente da recta axustada para o grupo E é significativamente distinta
# de 0 (p-valor: < 2e-16).
# - As desviacións da pendente das rectas axustadas para os outros dous
# grupos (tipo de combustible X e Z) tamén son significativamente
# distintas de 0 (p-valor: < 2e-16 en ambos casos).
# - O R2 é moi alto (0.9811), o que indica que estamos sendo capaces de
# explicar a maior parte da variabilidade dos datos. E maior que o do
# modelo sen interacción.

plot(emision_CO2 ~ consumo_ciudad)
pe <- tipo_combustible == "E"
px <- tipo_combustible == "X"
pz <- tipo_combustible == "Z"
# Coloreamos distinto os puntos de grupos distintos
points(emision_CO2[pe] ~ consumo_ciudad[pe], col = "red", pch = 16)
points(emision_CO2[px] ~ consumo_ciudad[px], col = "blue", pch = 16)
points(emision_CO2[pz] ~ consumo_ciudad[pz], col = "green", pch = 16)

# Representamos as rectas paralelas axustadas
betas <- modelo_ci$coefficients
abline(a = betas[1], b = betas[2], col = "red", lwd = 2)
abline(a = betas[1] + betas[3], b = betas[2] + betas[5], col = "blue", lwd = 2)
abline(a = betas[1] + betas[4], b = betas[2] + betas[6], col = "green", lwd = 2)

legend(
  "bottomright",
  legend = c("E", "X", "Z"),
  col = c("red", "blue", "green"),
  pch = 20
)
title("Emisión de CO2 en función do cosumo en cidade e do tipo de combustible",
  line = -2, outer = TRUE, cex = 1.5
)

# Facemos un F-test para ver se preferimos un modelo ANCOVA
# sen interaccion ou un modelo ANCOVA con interacción.
# En vista de que o p-valor e menor que os niveis de significación
# habituais, rexeitamos a hipotese nula: o modelo ANCOVA sen interaccion
anova(modelo_si, modelo_ci)
# p-valor: < 2.2e-16

#################################
# MULTIPLES VARIABLES CONTINUAS #
#################################

# Representamos os datos en 3D
cores <- c("red", "blue", "green")
plot_ly(
  x = consumo_ciudad, y = tam_motor, z = emision_CO2,
  color = tipo_combustible, colors = cores,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3)
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

modelo_ci_1 <- lm(emision_CO2 ~ consumo_ciudad * tipo_combustible + tam_motor)
modelo_ci_2 <- lm(emision_CO2 ~ consumo_ciudad + tam_motor * tipo_combustible)
modelo_ci_3 <- lm(emision_CO2 ~ consumo_ciudad * tipo_combustible + tam_motor * tipo_combustible)
modelo_si_2 <- lm(emision_CO2 ~ consumo_ciudad + tam_motor + tipo_combustible)

summary(modelo_ci_1)
# Interpretación:
# - A pendente asociada a variable tam_motor non e significativamente
# distinta de 0 (p-valor: 0.743).
# - O R2 é moi alto (0.9811)
summary(modelo_ci_2)
# Interpretación:
# - Todos os coeficientes e as desviacions son significativamente
# distintas de 0 (p-valor: < 2e-16 en todos os casos).
# - O R2 é moi alto (0.9799), mais algo mais baixo que o do modelo
# anterior.
summary(modelo_ci_3)
# Interpretación:
# - A pendente asociada a variable tam_motor non e significativamente
# distinta de 0 (p-valor: 0.389) para os valores habituais de significación.
# - As desviacións da pendente das rectas axustadas para os outros dous
# grupos (tipo de combustible X e Z) asociadas á variable tam_motor non
# son significativamente distintas de 0 (p-valor: 0.864 e 0.121)
# - O R2 é moi alto (0.9811)
summary(modelo_si_2)
# Interpretación:
# - A pendente asociada á variable tam_motor non e significativamente
# distinta de 0 (p-valor: 0.309).

# A continuación realizamos un estudo comparativo dos modelos
# mediante un F-test.
anova(modelo_ci_1, modelo_ci_3)
# En vista do p-valor, 0.001539, rexeitamos a hipotese nula con valores de
# significación superiores a 0.01 e quedamonos co modelo máis complexo,
# que inclue interacción en ambas variables continuas.
# Rexeitamos o modelo con dúas variables continuas e interacción en
# consumo_ciudad.
anova(modelo_ci_2, modelo_ci_3)
# En vista do p-valor, < 2.2e-16, rexeitamos a hipotese nula para todos
# os valores de significación habituais e quedamonos co modelo máis
# complexo, que inclue interacción en ambas variables continuas.
# Rexeitamos o modelo con dúas variables continuas e interacción en tam_motor.
anova(modelo_ci, modelo_ci_3)
# En vista do p-valor, 0.004505, rexeitamos a hipotese nula con valores de
# significación superiores a 0.01 e quedamonos co modelo máis complexo,
# que inclue interacción en ambas variables continuas.
# Rexeitamos o modelo con unha variable continua e con interacción.
anova(modelo_si_2, modelo_ci_3)
# En vista do p-valor, < 2.2e-16, máis pequeno que os valores de significacion
# habituais, rexeitamos a hipotese nula e quedamonos co modelo máis
# complexo, que inclue interacción en ambas variables continuas.
# Rexeitamos o modelo con dúas variables continuas sen interacción.

# En vista dos resultados quedámonos co modelo máis complexo que inclue as
# variables continuas: consumo_ciudad e tam_motor, e a variable categórica
# tipo_combustible, con interacción en ambas variables continuas.
# Poderíamos comparar os modelos modelo_ci_1 e modelo_ci_2, mais non co comando
# anova, xa que non son anidados. Non obstante, ao preferir modelo_ci_3 ante
# estes dous, xa non realizamos a comparación.

#################
# COLINEALIDADE #
#################

indices_E <- which(datos$tipo_combustible == "E")
indices_X <- which(datos$tipo_combustible == "X")
indices_Z <- which(datos$tipo_combustible == "Z")

modelo_e <- lm(emision_CO2[indices_E] ~ consumo_ciudad[indices_E] + tam_motor[indices_E])
modelo_x <- lm(emision_CO2[indices_X] ~ consumo_ciudad[indices_X] + tam_motor[indices_X])
modelo_z <- lm(emision_CO2[indices_Z] ~ consumo_ciudad[indices_Z] + tam_motor[indices_Z])

# En xeral, obtemos covarianzas altas en ambos os tres modelos
x <- cbind(tam_motor[indices_E], consumo_ciudad[indices_E])
cor(x)
x <- cbind(tam_motor[indices_X], consumo_ciudad[indices_X])
cor(x)
x <- cbind(tam_motor[indices_Z], consumo_ciudad[indices_Z])
cor(x)

FIV <- c()
Rj2 <- cor(tam_motor[indices_E], fitted(lm(tam_motor[indices_E] ~ consumo_ciudad[indices_E])))^2
FIV["modelo_e"] <- 1 / (1 - Rj2)
Rj2 <- cor(tam_motor[indices_X], fitted(lm(tam_motor[indices_X] ~ consumo_ciudad[indices_X])))^2
FIV["modelo_x"] <- 1 / (1 - Rj2)
Rj2 <- cor(tam_motor[indices_Z], fitted(lm(tam_motor[indices_Z] ~ consumo_ciudad[indices_Z])))^2
FIV["modelo_z"] <- 1 / (1 - Rj2)

# En ningun dos 3 casos o FIV supera o valor de 5, polo que consideramos
# que non hai problemas de colinealidade en ningun dos 3 modelos
FIV

############
# DIAGNOSE #
############

# Analizamos os datos atipicos en cada grupo
indices_E <- which(datos$tipo_combustible == "E")
indices_X <- which(datos$tipo_combustible == "X")
indices_Z <- which(datos$tipo_combustible == "Z")

modelo_e <- lm(emision_CO2[indices_E] ~ consumo_ciudad[indices_E] + tam_motor[indices_E])
modelo_x <- lm(emision_CO2[indices_X] ~ consumo_ciudad[indices_X] + tam_motor[indices_X])
modelo_z <- lm(emision_CO2[indices_Z] ~ consumo_ciudad[indices_Z] + tam_motor[indices_Z])

# Atipicos para o grupo E
res_E <- rstandard(modelo_e)
ind_res_E <- which(abs(res_E) > 1.96)
indices_E_atipicos = indices_E[ind_res_E]
length(indices_E_atipicos) # Temos 7 atipicos

# Debuxamos os atipicos
atipicos_e <- ifelse(indices_E %in% indices_E_atipicos, 1, 0)
cores_e <- c("red", "black")
puntos_e <- plot_ly(
  x = consumo_ciudad[indices_E], y = tam_motor[indices_E], z = emision_CO2[indices_E],
  color = atipicos_e, colors = cores_e,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3)
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Debuxamos o plano que axusta o modelo correspondente ao grupo E
coef_e <- coef(modelo_e)

consumo <- c(min(consumo_ciudad[indices_E]), max(consumo_ciudad[indices_E]))
motor <- c(min(tam_motor[indices_E]), max(tam_motor[indices_E]))

# Creamos un grid para calcular os valores preditos
grid <- expand.grid(
  consumo_ciudad = consumo,
  tam_motor = motor
)

# Calculamos as predicions de emisions para o grid
predicted_values <- coef_e[1] + coef_e[2] * grid$consumo_ciudad + coef_e[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_e <- plot_ly(
  x = consumo, y = motor, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Xuntamos os graficos
grafico <- subplot(puntos_e, plano_e)
grafico

# Atipicos para o grupo X
res_X <- rstandard(modelo_x)
ind_res_X <- which(abs(res_X) > 1.96)
indices_X_atipicos <- indices_X[ind_res_X]
length(indices_X_atipicos) # Temos 129 atipicos
# Debuxamos os atipicos
atipicos_x <- ifelse(indices_X %in% indices_X_atipicos, 1, 0)
cores_x <- c("blue", "black")
puntos_x <- plot_ly(
  x = consumo_ciudad[indices_X], y = tam_motor[indices_X], z = emision_CO2[indices_X],
  color = atipicos_x, colors = cores_x,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3)
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Debuxamos o plano que axusta o modelo correspondente ao grupo X
coef_x <- coef(modelo_x)

consumo_x <- c(min(consumo_ciudad[indices_X]), max(consumo_ciudad[indices_X]))
motor_x <- c(min(tam_motor[indices_X]), max(tam_motor[indices_X]))

# Creamos un grid para calcular os valores preditos
grid <- expand.grid(
  consumo_ciudad = consumo_x,
  tam_motor = motor_x
)

# Calculamos as predicions de emisions para o grid
predicted_values <- coef_x[1] + coef_x[2] * grid$consumo_ciudad + coef_x[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_x <- plot_ly(
  x = consumo_x, y = motor_x, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Xuntamos os graficos: plano + puntos (resaltados atipicos en negro)
grafico <- subplot(puntos_x, plano_x)
grafico


# Atipicos para o grupo Z
res_Z <- rstandard(modelo_z)
ind_res_Z <- which(abs(res_Z) > 1.96)
indices_Z_atipicos <- indices_Z[ind_res_Z]
length(indices_Z_atipicos) # Temos 81 atipicos

# Debuxamos os atipicos
atipicos_z <- ifelse(indices_Z %in% indices_Z_atipicos, 1, 0)
cores_z <- c("green", "black")
puntos_z <- plot_ly(
  x = consumo_ciudad[indices_Z], y = tam_motor[indices_Z], z = emision_CO2[indices_Z],
  color = atipicos_z, colors = cores_z,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3)
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Debuxamos o plano que axusta o modelo correspondente ao grupo X
coef_z <- coef(modelo_z)
consumo_z <- c(min(consumo_ciudad[indices_Z]), max(consumo_ciudad[indices_Z]))
motor_z <- c(min(tam_motor[indices_Z]), max(tam_motor[indices_Z]))

# Creamos un grid para calcular os valores preditos
grid <- expand.grid(
  consumo_ciudad = consumo_z,
  tam_motor = motor_z
)

# Calculamos as predicions de emisions para o grid
predicted_values <- coef_z[1] + coef_z[2] * grid$consumo_ciudad + coef_z[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_z <- plot_ly(
  x = consumo_z, y = motor_z, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

# Xuntamos os graficos: plano + puntos (resaltados atipicos en negro)
grafico <- subplot(puntos_z, plano_z)
grafico

# Analizamos os influintes
# Influente para o grupo E
n <- length(indices_E)
p <- length(modelo_e$coefficients)
med <- qf(0.5, p, n - p)
infl_e <- which(cooks.distance(modelo_e) > med)
infl_e # non hai influintes

# Influente para o grupo X
n <- length(indices_X)
p <- length(modelo_x$coefficients)
med <- qf(0.5, p, n - p)
infl_x <- which(cooks.distance(modelo_x) > med)
infl_x # non hai influintes

# Influente para o grupo Z
n <- length(indices_Z)
p <- length(modelo_z$coefficients)
med <- qf(0.5, p, n - p)
infl_z <- which(cooks.distance(modelo_z) > med)
infl_z # non hai influintes

###############
# NOVO AXUSTE #
###############

# Quitamos os atipicos de cada grupo
indices_atipicos <- c(indices_E_atipicos, indices_X_atipicos, indices_Z_atipicos)
datos_sen_atipicos <- datos[-indices_atipicos, ]

emision_CO2_sa <- datos_sen_atipicos$emision_CO2
tipo_combustible_sa <- datos_sen_atipicos$tipo_combustible
consumo_ciudad_sa <- datos_sen_atipicos$consumo_ciudad
tam_motor_sa <- datos_sen_atipicos$tam_motor

# Axustamos o modelo de novo
modelo_sa <- lm(emision_CO2_sa ~ consumo_ciudad_sa * tipo_combustible_sa + tam_motor_sa * tipo_combustible_sa)
summary(modelo_sa)
# Interpretación:
# - A pendente asociada á variable tam_motor non e significativamente
# distinta de 0 (p-valor: 0.5749) para os valores habituais de significación.
# Ademais, empeora o p-valor do modelo anterior (0.389).
# - A desviación para a pendente asociada ao grupo X e significativamente
# distinta de 0 para niveis de significación superiores a 0.1 (p-valor: 0.0767).
# - A desviación para a pendente asociada ao grupo Z non e significativamente
# distinta de 0 (p-valor: 0.5265) e empeora o p-valor do modelo anterior
# (0.121).
# - O R2 mellora con respecto ao anterior modelo (0.9811).


############
# DIAGNOSE #
############

# Volvemos calcular os atipicos
indices_E <- which(datos_sen_atipicos$tipo_combustible == "E")
indices_X <- which(datos_sen_atipicos$tipo_combustible == "X")
indices_Z <- which(datos_sen_atipicos$tipo_combustible == "Z")

modelo_e <- lm(emision_CO2_sa[indices_E] ~ consumo_ciudad_sa[indices_E] + tam_motor_sa[indices_E])
modelo_x <- lm(emision_CO2_sa[indices_X] ~ consumo_ciudad_sa[indices_X] + tam_motor_sa[indices_X])
modelo_z <- lm(emision_CO2_sa[indices_Z] ~ consumo_ciudad_sa[indices_Z] + tam_motor_sa[indices_Z])

# Atipicos para o grupo E
res_E <- rstandard(modelo_e)
ind_res_E <- which(abs(res_E) > 1.96)
indices_E_atipicos = indices_E[ind_res_E]
length(indices_E_atipicos) # Temos 12 atipicos

# Atipicos para o grupo X
res_X <- rstandard(modelo_x)
ind_res_X <- which(abs(res_X) > 1.96)
indices_X_atipicos <- indices_X[ind_res_X]
length(indices_X_atipicos) # Temos 109 atipicos

# Atipicos para o grupo Z
res_Z <- rstandard(modelo_z)
ind_res_Z <- which(abs(res_Z) > 1.96)
indices_Z_atipicos <- indices_Z[ind_res_Z]
length(indices_Z_atipicos) # Temos 61 atipicos

##############
# VALIDACION #
##############

### Normalidade ###

# Para un nivel de significacion de 0.05, podemos rexeitar
# a hipotese nula de normalidade para os residuos dos modelos
# dos 3 grupos
res_e <- rstandard(modelo_e)
shapiro.test(res_e) # p-valor: 0.04134
par(mfrow = c(1, 2), cex = 2)
hist(res_e, freq = FALSE, main = "", xlab = "Residuos estandarizados", ylab = "Densidad")
qqPlot(res_e, cex = 0.5, ylab = "Residuos estandarizados")
title("Residuos estandarizados del grupo E", line = -2, outer = TRUE, cex = 2.5)

res_x <- rstandard(modelo_x)
shapiro.test(res_x) # p-valor: 4.645e-08
par(mfrow = c(1, 2), cex = 2)
hist(res_x, freq = FALSE, main = "", xlab = "Residuos estandarizados", ylab = "Densidad")
qqPlot(res_x, cex = 0.5, ylab = "Residuos estandarizados")
title("Residuos estandarizados del grupo X", line = -2, outer = TRUE, cex = 2.5)


res_z <- rstandard(modelo_z)
shapiro.test(res_z) # p-valor: 3.425e-07
par(mfrow = c(1, 2), cex = 2)
hist(res_z, freq = FALSE, main = "", xlab = "Residuos estandarizados", ylab = "Densidad")
qqPlot(res_z, cex = 0.5, ylab = "Residuos estandarizados")
title("Residuos estandarizados del grupo Z", line = -2, outer = TRUE, cex = 2.5)


### Homocedasticidade ###

# Test de Breusch-Pagan:
# H0: homocedasticidade vs Ha: varianza dependente das explicativas
bptest(modelo_e) # p-valor: 0.0002312

# Test de Harrison-McCabe:
# H0: homocedasticidade vs Ha: varianza ten un punto de cambio.
set.seed(1234)
hmctest(modelo_e) # p-valor: 0.819

# En vista dos p-valores dos test anteriores, podemos rexeitar a
# hipotese de homocedasticidade para o grupo E, mais non existen
# evidencias para rexeitar a hipotese de homocedasticidade fronte
# a hipotese alternativa de que a varianza ten un punto de cambio.

# Residuos fronte a predicion
par(mfrow = c(1, 1))
plot(res_e ~ modelo_e$fitted.values, pch = 19)
abline(h = 0, lty = 2)
abline(h = 1.96, lty = 2, col = "red")
abline(h = -1.96, lty = 2, col = "red")

# En vista dos p-valores dos test, sucede o mesmo para o grupo X
bptest(modelo_x) # p-valor: 4.828e-16
set.seed(1234)
hmctest(modelo_x) # p-valor: 0.892
plot(res_x ~ modelo_x$fitted.values, pch = 19)
abline(h = 0, lty = 2)
abline(h = 1.96, lty = 2, col = "red")
abline(h = -1.96, lty = 2, col = "red")

# En vista dos p-valores dos test, sucede o mesmo para o grupo Z
bptest(modelo_z) # p-valor: 6.219e-16
set.seed(1234)
hmctest(modelo_z) # p-valor: 0.558
plot(res_z ~ modelo_z$fitted.values, pch = 19)
abline(h = 0, lty = 2)
abline(h = 1.96, lty = 2, col = "red")
abline(h = -1.96, lty = 2, col = "red")

### Linealidade ###
# Test de Ramsey:
# H0: linealidade; Ha: polinomio de grao 2 ou 3
resettest(modelo_e) # p-valor: 1.571e-08
# Test de Harvey-Collier:
# H0: linealidade; Ha: non linealidade (concavidade/convexidade
# globais)
harvtest(modelo_e) # p-valor: 1.88e-14

# En vista dos p-valores dos test, podemos rexeitar a hipotese de
# linealidade para o grupo E.

# Graficamente (so incluimos a variable consumo_ciudad)
plot(emision_CO2_sa[indices_E] ~ modelo_e$fitted.values, col = "red", pch = 19) # Xeral
abline(c(0, 1))

# En vista dos p-valores dos test, podemos rexeitar a hipotese de
# linealidade para o grupo X.
resettest(modelo_x) # p-valor: < 2.2e-16
harvtest(modelo_x) # p-valor: < 2.2e-16

# Graficamente (so incluimos a variable consumo_ciudad)
plot(emision_CO2_sa[indices_X] ~ modelo_x$fitted.values, col = "blue", pch = 19) # Xeral
abline(c(0, 1))

# En vista dos p-valores dos test, podemos rexeitar a hipotese de
# linealidade para o grupo Z.
resettest(modelo_z) # p-valor: 6.263e-06
harvtest(modelo_z) # p-valor: < 2.2e-16

# Graficamente (so incluimos a variable consumo_ciudad)
plot(emision_CO2_sa[indices_Z] ~ modelo_z$fitted.values, col = "green", pch = 19) # Xeral
abline(c(0, 1))

##############
# IOC FINAIS #
##############

# Calculamos intervalo de confianza ao 95% das pendentes
# Tanto para ao coeficiente asociado a tam_motor como para o
# intercepto non existen evidencias significativas de que sexan
# distintos de 0 para o grupo E
confint(modelo_e)
# No caso dos modelos para os grupos X e Z, si que existen evidencias
# significativas de que os coeficientes asociados a tam_motor e o
# intercepto son distintos de 0
confint(modelo_x)
confint(modelo_z)

################################
# REPRESENTACION FINAL GRAFICA #
################################

### GRUPO E ###

puntos_e <- plot_ly(
  x = consumo_ciudad_sa[indices_E], y = tam_motor_sa[indices_E], z = emision_CO2_sa[indices_E],
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, color = "red")
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

coef_e <- coef(modelo_e)
consumo_e <- c(min(consumo_ciudad_sa[indices_E]), max(consumo_ciudad_sa[indices_E]))
motor_e <- c(min(tam_motor_sa[indices_E]), max(tam_motor_sa[indices_E]))

grid <- expand.grid(
  consumo_ciudad = consumo_e,
  tam_motor = motor_e
)

predicted_values <- coef_e[1] + coef_e[2] * grid$consumo_ciudad + coef_e[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_e <- plot_ly(
  x = consumo_e, y = motor_e, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

grafico <- subplot(puntos_e, plano_e)
grafico

### GRUPO X ###

puntos_x <- plot_ly(
  x = consumo_ciudad_sa[indices_X], y = tam_motor_sa[indices_X], z = emision_CO2_sa[indices_X],
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, color = "blue")
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

coef_x <- coef(modelo_x)
consumo_x <- c(min(consumo_ciudad_sa[indices_X]), max(consumo_ciudad_sa[indices_X]))
motor_x <- c(min(tam_motor_sa[indices_X]), max(tam_motor_sa[indices_X]))

grid <- expand.grid(
  consumo_ciudad = consumo_x,
  tam_motor = motor_x
)

predicted_values <- coef_x[1] + coef_x[2] * grid$consumo_ciudad + coef_x[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_x <- plot_ly(
  x = consumo_x, y = motor_x, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

grafico <- subplot(puntos_x, plano_x)
grafico

### GRUPO Z ###

puntos_z <- plot_ly(
  x = consumo_ciudad_sa[indices_Z], y = tam_motor_sa[indices_Z], z = emision_CO2_sa[indices_Z],
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, color = "green")
) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

coef_z <- coef(modelo_z)
consumo_z <- c(min(consumo_ciudad_sa[indices_Z]), max(consumo_ciudad_sa[indices_Z]))
motor_z <- c(min(tam_motor_sa[indices_Z]), max(tam_motor_sa[indices_Z]))

grid <- expand.grid(
  consumo_ciudad = consumo_z,
  tam_motor = motor_z
)

predicted_values <- coef_z[1] + coef_z[2] * grid$consumo_ciudad + coef_z[3] * grid$tam_motor
predicted_values <- t(matrix(predicted_values, ncol = 2, nrow = 2))
predicted_values

plano_z <- plot_ly(
  x = consumo_z, y = motor_z, z = predicted_values
) %>%
  add_surface(surfacecolor = c(0, 0, 0), opacity = 0.75) %>%
  layout(scene = list(
    xaxis = list(title = "consumo en ciudad"),
    yaxis = list(title = "tamanho de motor"),
    zaxis = list(title = "emision de CO2")
  ))

grafico <- subplot(puntos_z, plano_z)
grafico

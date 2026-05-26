# Simulacion base de datos

library(nlme)
library(lme4)
library(ggplot2)

set.seed(123)

n <- 200   # número de pacientes

n
max_med <- 4
max_med2 <- 10
duracion_estudio <- 10

# Escenario 1--> cada 2.5 años

# Número de mediciones por paciente:
# Se simula mediante una distribución discreta entre 1 y 4,

num_med_esc1 <- sample(
  1:4,
  size = n,
  replace = TRUE,
  prob = c(0.15, 0.2, 0.25, 0.4)
)

# Escenario 2--> cada año
# Número de mediciones por paciente:
# Se simula entre 2 y 10 observaciones


num_med_esc2 <- sample(
  2:10,
  size = n,
  replace = TRUE,
  prob = c(0.05, 0.05, 0.05, 0.05, 0.15, 0.15, 0.15, 0.15, 0.2)
)

# PRIMER ESCENARIO 

# Genero data frame vacío
sim_esc1 <- data.frame()

for(i in 1:n){
  
  # Mediciones del paciente i
  k <- num_med_esc1[i]
  
  # observacion aleatoria distrb uniforme
  # (numero de observaciones, límite inferior, límite superior)
  
  inicio <- runif(1,0,2.5)  # cada paciente empieza en momento distinto
  
  # Cumsum es la suma acumulada (La primera medición es el 0, las siguientes k-1 rep de 2.5)
  # Se le suma el inicio de cada paciente
  tiempos <- inicio + cumsum(c(0, rep(2.5,k-1)))
  
  # rnorm es para añadir ruido a las mediciones. (Num de observaciones, media, desv. típica)

  tiempos <- tiempos + c(0, rnorm(k-1,0,0.3))
  
  # Rellenar el data frame
  temp <- data.frame(
    id = rep(i,k),
    tiempo_real = tiempos
  )
  
  sim_esc1 <- rbind(sim_esc1,temp)
}
nrow(sim_esc1)
View(sim_esc1)

# Compruebo que los intervalos de tiempo estén entre 0 y 10 años
min(sim_esc1$tiempo_real)
max(sim_esc1$tiempo_real)


# ESCENARIO 2

# Aquí asumimos que que las visitas se realizan cada año.

sim_esc2 <- data.frame()

for(i in 1:n){
  
  k <- num_med_esc2[i]
  
  inicio <- runif(1, 0, 1)
  
  tiempos <- inicio + cumsum(c(0, rep(1,k-1)))
  tiempos <- tiempos + c(0, rnorm(k-1,0,0.1))
  
  temp <- data.frame(
    id = rep(i,k),
    tiempo_real = tiempos
  )
  
  sim_esc2 <- rbind(sim_esc2,temp)
}

nrow(sim_esc2)
View(sim_esc2)

# Compruebo que los intervalos de tiempo estén entre 0 y 10 años
min(sim_esc2$tiempo_real)
max(sim_esc2$tiempo_real)



## CREAMOS LAS VARIABLES DEL TIEMPO COMO EN MI ANÁLISIS

# ESCENARIO 1

# Ordenamos por tiempo
sim_esc1 <- sim_esc1[order(sim_esc1$tiempo_real), ]

# CASO 1: tiempo global (referencia = primer tiempo del estudio)
t0_global_esc1 <- min(sim_esc1$tiempo_real)

sim_esc1$tiempo_caso1 <- sim_esc1$tiempo_real - t0_global_esc1

# CASO 2: tiempo individual (referencia = primer tiempo de cada paciente)
sim_esc1$t0_paciente <- ave(
  sim_esc1$tiempo_real,
  sim_esc1$id,
  FUN = min
)

sim_esc1$tiempo_caso2 <- sim_esc1$tiempo_real - sim_esc1$t0_paciente

# Comprobación
head(sim_esc1)

# Pacientes 18 y 4

sim_esc1[sim_esc1$id == 18,
         c("id","tiempo_real","tiempo_caso1","tiempo_caso2")]

sim_esc1[sim_esc1$id == 4,
         c("id","tiempo_real","tiempo_caso1","tiempo_caso2")]

# ESCENARIO 2

# Ordenamos
sim_esc2 <- sim_esc2[order(sim_esc2$tiempo_real), ]

# CASO 1: tiempo global
t0_global_esc2 <- min(sim_esc2$tiempo_real)

sim_esc2$tiempo_caso1 <- sim_esc2$tiempo_real - t0_global_esc2

# CASO 2: tiempo individual
sim_esc2$t0_paciente <- ave(
  sim_esc2$tiempo_real,
  sim_esc2$id,
  FUN = min
)

sim_esc2$tiempo_caso2 <- sim_esc2$tiempo_real - sim_esc2$t0_paciente

# Comprobación
head(sim_esc2)

# Pacientes 18 y 4

sim_esc2[sim_esc2$id == 18,
         c("id","tiempo_real","tiempo_caso1","tiempo_caso2")]

sim_esc2[sim_esc2$id == 4,
         c("id","tiempo_real","tiempo_caso1","tiempo_caso2")]




# ELECCIÓN DE BETA (HE MODIFICADO LAS PENDIENTES)

beta0_1 <- 50
beta1_1 <- -1.5   # negativa grande

beta0_2  <- 45
beta1_2  <- -0.3   # negativa pequeña

beta0_3  <- 25
beta1_3  <- 0.4    # positiva pequeña

beta0_4  <- 35
beta1_4  <- 1.2    # positiva grande




# EFECTOS ALEATORIOS POR PACIENTE



## ESCENARIO 1

set.seed(123)

random_intercept <- rnorm(n, 0, 5)
random_slope     <- rnorm(n, 0, 0.5)

# Los asignamos a cada fila
sim_esc1$u0 <- random_intercept[sim_esc1$id]
sim_esc1$u1 <- random_slope[sim_esc1$id]


# Error residual
error_Y1_esc1 <- rnorm(nrow(sim_esc1), 0, 3)
error_Y2_esc1 <- rnorm(nrow(sim_esc1), 0, 3)
error_Y3_esc1 <- rnorm(nrow(sim_esc1), 0, 3)
error_Y4_esc1 <- rnorm(nrow(sim_esc1), 0, 3)


## ESCENARIO 2

# Nuevos efectos aleatorios

sim_esc2$u0 <- random_intercept[sim_esc2$id]
sim_esc2$u1 <- random_slope[sim_esc2$id]

# Error residual
error_Y1_esc2 <- rnorm(nrow(sim_esc2), 0, 3)
error_Y2_esc2 <- rnorm(nrow(sim_esc2), 0, 3)
error_Y3_esc2 <- rnorm(nrow(sim_esc2), 0, 3)
error_Y4_esc2 <- rnorm(nrow(sim_esc2), 0, 3)

# ----------------------------

# SUPUESTO 1 DE SIMULACIÓN

# UTILIZAMOS EL TIEMPO GLOBAL (TIEMPO_CASO1) Y EL ESCENARIO 1 (POCAS MEDICIONES Y ESPACIADAS, CADA 2.5 AÑOS)

# FÓRMULA

# Y = beta0 + beta1 * tiempo_caso1 + efectos_aleatorios + error

# Y1_S1
  sim_esc1$Y1_s1 <- beta0_1 +
  sim_esc1$u0 +
  (beta1_1 + sim_esc1$u1) * sim_esc1$tiempo_caso1 +
  error_Y1_esc1

# Y2_S1
sim_esc1$Y2_s1 <- beta0_2 +
  sim_esc1$u0 +
  (beta1_2 + sim_esc1$u1) * sim_esc1$tiempo_caso1 +
  error_Y2_esc1

# Y3_S1
sim_esc1$Y3_s1 <- beta0_3 +
  sim_esc1$u0 +
  (beta1_3 + sim_esc1$u1) * sim_esc1$tiempo_caso1 +
  error_Y3_esc1

# Y4_S1
sim_esc1$Y4_s1 <- beta0_4 +
  sim_esc1$u0 +
  (beta1_4 + sim_esc1$u1) * sim_esc1$tiempo_caso1 +
  error_Y4_esc1

# ----------------------------------

# SUPUESTO 2 DE SIMULACIÓN

# UTILIZAMOS EL TIEMPO INDIVIDUAL (TIEMPO_CASO2) Y EL ESCENARIO 1 (POCAS MEDICIONES Y ESPACIADAS, CADA 2.5 AÑOS)
# SE ASUME QUE TODOS COMIENZAN EN EL MISMO INSTANTE (tiempo_caso2)
  
# Y1_S2
  sim_esc1$Y1_s2 <- beta0_1 +
  sim_esc1$u0 +
  (beta1_1 + sim_esc1$u1) * sim_esc1$tiempo_caso2 +
  error_Y1_esc1

# Y2_S2
sim_esc1$Y2_s2 <- beta0_2 +
  sim_esc1$u0 +
  (beta1_2 + sim_esc1$u1) * sim_esc1$tiempo_caso2 +
  error_Y2_esc1

# Y3_S2
sim_esc1$Y3_s2 <- beta0_3 +
  sim_esc1$u0 +
  (beta1_3 + sim_esc1$u1) * sim_esc1$tiempo_caso2 +
  error_Y3_esc1

# Y4_S2
sim_esc1$Y4_s2 <- beta0_4 +
  sim_esc1$u0 +
  (beta1_4 + sim_esc1$u1) * sim_esc1$tiempo_caso2 +
  error_Y4_esc1

# -----------------------------------------------------
  
# SUPUESTO 3 DE SIMULACIÓN
  

# UTILIZAMOS EL TIEMPO GLOBAL (TIEMPO_CASO1)
# ESCENARIO 2: MUCHAS MEDICIONES (cada 1 año) y entradas desiguales
  
# Y1_S3
  sim_esc2$Y1_s3 <- beta0_1 +
  sim_esc2$u0 +
  (beta1_1 + sim_esc2$u1) * sim_esc2$tiempo_caso1 +
  error_Y1_esc2

# Y2_S3
sim_esc2$Y2_s3 <- beta0_2 +
  sim_esc2$u0 +
  (beta1_2 + sim_esc2$u1) * sim_esc2$tiempo_caso1 +
  error_Y2_esc2

# Y3_S3
sim_esc2$Y3_s3 <- beta0_3 +
  sim_esc2$u0 +
  (beta1_3 + sim_esc2$u1) * sim_esc2$tiempo_caso1 +
  error_Y3_esc2

# Y4_S3
sim_esc2$Y4_s3 <- beta0_4 +
  sim_esc2$u0 +
  (beta1_4 + sim_esc2$u1) * sim_esc2$tiempo_caso1 +
  error_Y4_esc2
  
  
# -------------------------------------------------------

# SUPUESTO 4 DE SIMULACIÓN
  
# UTILIZAMOS EL TIEMPO INDIVIDUAL (TIEMPO_CASO2)
# ESCENARIO 2: MUCHAS MEDICIONES (cada 1 año) y mismo instante de inicio
  
 # Y1_S4
  sim_esc2$Y1_s4 <- beta0_1 +
  sim_esc2$u0 +
  (beta1_1 + sim_esc2$u1) * sim_esc2$tiempo_caso2 +
  error_Y1_esc2

# Y2_S4
sim_esc2$Y2_s4 <- beta0_2 +
  sim_esc2$u0 +
  (beta1_2 + sim_esc2$u1) * sim_esc2$tiempo_caso2 +
  error_Y2_esc2

# Y3_S4
sim_esc2$Y3_s4 <- beta0_3 +
  sim_esc2$u0 +
  (beta1_3 + sim_esc2$u1) * sim_esc2$tiempo_caso2 +
  error_Y3_esc2

# Y4_S4
sim_esc2$Y4_s4 <- beta0_4 +
  sim_esc2$u0 +
  (beta1_4 + sim_esc2$u1) * sim_esc2$tiempo_caso2 +
  error_Y4_esc2

# --------------------------------------------------------
  
# MODELOS 

# SUPUESTO 1

# Y1
m_y1_c1_s1 <- lme(Y1_s1 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y1_c2_s1 <- lme(Y1_s1 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y2
m_y2_c1_s1 <- lme(Y2_s1 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y2_c2_s1 <- lme(Y2_s1 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y3
m_y3_c1_s1 <- lme(Y3_s1 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y3_c2_s1 <- lme(Y3_s1 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y4
m_y4_c1_s1 <- lme(Y4_s1 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y4_c2_s1 <- lme(Y4_s1 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)
# ------------------------------------------------

# SUPUESTO 2

# Y1
m_y1_c1_s2 <- lme(Y1_s2 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y1_c2_s2 <- lme(Y1_s2 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y2
m_y2_c1_s2 <- lme(Y2_s2 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y2_c2_s2 <- lme(Y2_s2 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y3
m_y3_c1_s2 <- lme(Y3_s2 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y3_c2_s2 <- lme(Y3_s2 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# Y4
m_y4_c1_s2 <- lme(Y4_s2 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc1)

m_y4_c2_s2 <- lme(Y4_s2 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc1)

# ------------------------------------------------

# SUPUESTO 3

# Y1
m_y1_c1_s3 <- lme(Y1_s3 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y1_c2_s3 <- lme(Y1_s3 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y2
m_y2_c1_s3 <- lme(Y2_s3 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y2_c2_s3 <- lme(Y2_s3 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y3
m_y3_c1_s3 <- lme(Y3_s3 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y3_c2_s3 <- lme(Y3_s3 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y4
m_y4_c1_s3 <- lme(Y4_s3 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y4_c2_s3 <- lme(Y4_s3 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# ------------------------------------------------

# SUPUESTO 4

# Y1
m_y1_c1_s4 <- lme(Y1_s4 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y1_c2_s4 <- lme(Y1_s4 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y2
m_y2_c1_s4 <- lme(Y2_s4 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y2_c2_s4 <- lme(Y2_s4 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y3
m_y3_c1_s4 <- lme(Y3_s4 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y3_c2_s4 <- lme(Y3_s4 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)

# Y4
m_y4_c1_s4 <- lme(Y4_s4 ~ tiempo_caso1,
                  random = ~tiempo_caso1 | id,
                  data = sim_esc2)

m_y4_c2_s4 <- lme(Y4_s4 ~ tiempo_caso2,
                  random = ~tiempo_caso2 | id,
                  data = sim_esc2)



# ------------------------------------------------

## RESUMEN SUPUESTO 1

# Y1 RESUMEN

# Caso 1
m_y1_c1_s1
VarCorr(m_y1_c1_s1)
summary(m_y1_c1_s1)$tTable
intervals(m_y1_c1_s1, which = "fixed")

# Caso 2
m_y1_c2_s1
VarCorr(m_y1_c2_s1)
summary(m_y1_c2_s1)$tTable
intervals(m_y1_c2_s1, which = "fixed")


# GRÁFICAS Y1
sim_esc1$pred_y1_c1_s1 <- predict(m_y1_c1_s1, level = 0)
sim_esc1$pred_y1_c2_s1 <- predict(m_y1_c2_s1, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y1_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c1_s1), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y1_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c2_s1), color = "red", linewidth = 1) +
  theme_bw()


# Y2 RESUMEN

# Caso 1
m_y2_c1_s1
VarCorr(m_y2_c1_s1)
summary(m_y2_c1_s1)$tTable
intervals(m_y2_c1_s1, which = "fixed")

# Caso 2
m_y2_c2_s1
VarCorr(m_y2_c2_s1)
summary(m_y2_c2_s1)$tTable
intervals(m_y2_c2_s1, which = "fixed")

# GRÁFICAS Y2
sim_esc1$pred_y2_c1_s1 <- predict(m_y2_c1_s1, level = 0)
sim_esc1$pred_y2_c2_s1 <- predict(m_y2_c2_s1, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y2_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c1_s1), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y2_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c2_s1), color = "red", linewidth = 1) +
  theme_bw()

# Y3 RESUMEN
# Caso 1
m_y3_c1_s1
VarCorr(m_y3_c1_s1)
summary(m_y3_c1_s1)$tTable
intervals(m_y3_c1_s1, which = "fixed")

# Caso 2
m_y3_c2_s1
VarCorr(m_y3_c2_s1)
summary(m_y3_c2_s1)$tTable
intervals(m_y3_c2_s1, which = "fixed")

# GRÁFICAS Y3

sim_esc1$pred_y3_c1_s1 <- predict(m_y3_c1_s1, level = 0)
sim_esc1$pred_y3_c2_s1 <- predict(m_y3_c2_s1, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y3_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c1_s1), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y3_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c2_s1), color = "red", linewidth = 1) +
  theme_bw()

# Y4 RESUMEN

# Caso 1
m_y4_c1_s1
VarCorr(m_y4_c1_s1)
summary(m_y4_c1_s1)$tTable
intervals(m_y4_c1_s1, which = "fixed")

# Caso 2
m_y4_c2_s1
VarCorr(m_y4_c2_s1)
summary(m_y4_c2_s1)$tTable
intervals(m_y4_c2_s1, which = "fixed")

# GRÁFICAS Y4

sim_esc1$pred_y4_c1_s1 <- predict(m_y4_c1_s1, level = 0)
sim_esc1$pred_y4_c2_s1 <- predict(m_y4_c2_s1, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y4_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c1_s1), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y4_s1)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c2_s1), color = "red", linewidth = 1) +
  theme_bw()

# --------------------------------------------------------------------

## RESUMEN SUPUESTO 2

# Y1 RESUMEN

# Caso 1
m_y1_c1_s2
VarCorr(m_y1_c1_s2)
summary(m_y1_c1_s2)$tTable
intervals(m_y1_c1_s2, which = "fixed")

# Caso 2
m_y1_c2_s2
VarCorr(m_y1_c2_s2)
summary(m_y1_c2_s2)$tTable
intervals(m_y1_c2_s2, which = "fixed")


# GRÁFICAS Y1
sim_esc1$pred_y1_c1_s2 <- predict(m_y1_c1_s2, level = 0)
sim_esc1$pred_y1_c2_s2 <- predict(m_y1_c2_s2, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y1_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c1_s2), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y1_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c2_s2), color = "red", linewidth = 1) +
  theme_bw()


# Y2 RESUMEN

# Caso 1
m_y2_c1_s2
VarCorr(m_y2_c1_s2)
summary(m_y2_c1_s2)$tTable
intervals(m_y2_c1_s2, which = "fixed")

# Caso 2
m_y2_c2_s2
VarCorr(m_y2_c2_s2)
summary(m_y2_c2_s2)$tTable
intervals(m_y2_c2_s2, which = "fixed")

# GRÁFICAS Y2
sim_esc1$pred_y2_c1_s2 <- predict(m_y2_c1_s2, level = 0)
sim_esc1$pred_y2_c2_s2 <- predict(m_y2_c2_s2, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y2_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c1_s2), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y2_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c2_s2), color = "red", linewidth = 1) +
  theme_bw()


# Y3 RESUMEN

# Caso 1
m_y3_c1_s2
VarCorr(m_y3_c1_s2)
summary(m_y3_c1_s2)$tTable
intervals(m_y3_c1_s2, which = "fixed")

# Caso 2
m_y3_c2_s2
VarCorr(m_y3_c2_s2)
summary(m_y3_c2_s2)$tTable
intervals(m_y3_c2_s2, which = "fixed")

# GRÁFICAS Y3
sim_esc1$pred_y3_c1_s2 <- predict(m_y3_c1_s2, level = 0)
sim_esc1$pred_y3_c2_s2 <- predict(m_y3_c2_s2, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y3_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c1_s2), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y3_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c2_s2), color = "red", linewidth = 1) +
  theme_bw()


# Y4 RESUMEN

# Caso 1
m_y4_c1_s2
VarCorr(m_y4_c1_s2)
summary(m_y4_c1_s2)$tTable
intervals(m_y4_c1_s2, which = "fixed")

# Caso 2
m_y4_c2_s2
VarCorr(m_y4_c2_s2)
summary(m_y4_c2_s2)$tTable
intervals(m_y4_c2_s2, which = "fixed")

# GRÁFICAS Y4
sim_esc1$pred_y4_c1_s2 <- predict(m_y4_c1_s2, level = 0)
sim_esc1$pred_y4_c2_s2 <- predict(m_y4_c2_s2, level = 0)

ggplot(sim_esc1, aes(x = tiempo_caso1, y = Y4_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c1_s2), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc1, aes(x = tiempo_caso2, y = Y4_s2)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c2_s2), color = "red", linewidth = 1) +
  theme_bw()

# ------------------------------------------------------------------------------
## RESUMEN SUPUESTO 3

# Y1 RESUMEN

# Caso 1
m_y1_c1_s3
VarCorr(m_y1_c1_s3)
summary(m_y1_c1_s3)$tTable
intervals(m_y1_c1_s3, which = "fixed")

# Caso 2
m_y1_c2_s3
VarCorr(m_y1_c2_s3)
summary(m_y1_c2_s3)$tTable
intervals(m_y1_c2_s3, which = "fixed")


# GRÁFICAS Y1
sim_esc2$pred_y1_c1_s3 <- predict(m_y1_c1_s3, level = 0)
sim_esc2$pred_y1_c2_s3 <- predict(m_y1_c2_s3, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y1_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c1_s3), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y1_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c2_s3), color = "red", linewidth = 1) +
  theme_bw()


# Y2 RESUMEN

# Caso 1
m_y2_c1_s3
VarCorr(m_y2_c1_s3)
summary(m_y2_c1_s3)$tTable
intervals(m_y2_c1_s3, which = "fixed")

# Caso 2
m_y2_c2_s3
VarCorr(m_y2_c2_s3)
summary(m_y2_c2_s3)$tTable
intervals(m_y2_c2_s3, which = "fixed")

# GRÁFICAS Y2
sim_esc2$pred_y2_c1_s3 <- predict(m_y2_c1_s3, level = 0)
sim_esc2$pred_y2_c2_s3 <- predict(m_y2_c2_s3, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y2_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c1_s3), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y2_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c2_s3), color = "red", linewidth = 1) +
  theme_bw()


# Y3 RESUMEN

# Caso 1
m_y3_c1_s3
VarCorr(m_y3_c1_s3)
summary(m_y3_c1_s3)$tTable
intervals(m_y3_c1_s3, which = "fixed")

# Caso 2
m_y3_c2_s3
VarCorr(m_y3_c2_s3)
summary(m_y3_c2_s3)$tTable
intervals(m_y3_c2_s3, which = "fixed")

# GRÁFICAS Y3
sim_esc2$pred_y3_c1_s3 <- predict(m_y3_c1_s3, level = 0)
sim_esc2$pred_y3_c2_s3 <- predict(m_y3_c2_s3, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y3_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c1_s3), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y3_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c2_s3), color = "red", linewidth = 1) +
  theme_bw()


# Y4 RESUMEN

# Caso 1
m_y4_c1_s3
VarCorr(m_y4_c1_s3)
summary(m_y4_c1_s3)$tTable
intervals(m_y4_c1_s3, which = "fixed")

# Caso 2
m_y4_c2_s3
VarCorr(m_y4_c2_s3)
summary(m_y4_c2_s3)$tTable
intervals(m_y4_c2_s3, which = "fixed")

# GRÁFICAS Y4
sim_esc2$pred_y4_c1_s3 <- predict(m_y4_c1_s3, level = 0)
sim_esc2$pred_y4_c2_s3 <- predict(m_y4_c2_s3, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y4_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c1_s3), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y4_s3)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c2_s3), color = "red", linewidth = 1) +
  theme_bw()

# ------------------------------------------------------------------------------

## RESUMEN SUPUESTO 4

# Y1 RESUMEN

# Caso 1
m_y1_c1_s4
VarCorr(m_y1_c1_s4)
summary(m_y1_c1_s4)$tTable
intervals(m_y1_c1_s4, which = "fixed")

# Caso 2
m_y1_c2_s4
VarCorr(m_y1_c2_s4)
summary(m_y1_c2_s4)$tTable
intervals(m_y1_c2_s4, which = "fixed")


# GRÁFICAS Y1
sim_esc2$pred_y1_c1_s4 <- predict(m_y1_c1_s4, level = 0)
sim_esc2$pred_y1_c2_s4 <- predict(m_y1_c2_s4, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y1_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c1_s4), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y1_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y1_c2_s4), color = "red", linewidth = 1) +
  theme_bw()

# Y2 RESUMEN

# Caso 1
m_y2_c1_s4
VarCorr(m_y2_c1_s4)
summary(m_y2_c1_s4)$tTable
intervals(m_y2_c1_s4, which = "fixed")

# Caso 2
m_y2_c2_s4
VarCorr(m_y2_c2_s4)
summary(m_y2_c2_s4)$tTable
intervals(m_y2_c2_s4, which = "fixed")

# GRÁFICAS Y2
sim_esc2$pred_y2_c1_s4 <- predict(m_y2_c1_s4, level = 0)
sim_esc2$pred_y2_c2_s4 <- predict(m_y2_c2_s4, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y2_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c1_s4), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y2_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y2_c2_s4), color = "red", linewidth = 1) +
  theme_bw()

# Y3 RESUMEN

# Caso 1
m_y3_c1_s4
VarCorr(m_y3_c1_s4)
summary(m_y3_c1_s4)$tTable
intervals(m_y3_c1_s4, which = "fixed")

# Caso 2
m_y3_c2_s4
VarCorr(m_y3_c2_s4)
summary(m_y3_c2_s4)$tTable
intervals(m_y3_c2_s4, which = "fixed")

# GRÁFICAS Y3
sim_esc2$pred_y3_c1_s4 <- predict(m_y3_c1_s4, level = 0)
sim_esc2$pred_y3_c2_s4 <- predict(m_y3_c2_s4, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y3_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c1_s4), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y3_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y3_c2_s4), color = "red", linewidth = 1) +
  theme_bw()

# Y4 RESUMEN

# Caso 1
m_y4_c1_s4
VarCorr(m_y4_c1_s4)
summary(m_y4_c1_s4)$tTable
intervals(m_y4_c1_s4, which = "fixed")

# Caso 2
m_y4_c2_s4
VarCorr(m_y4_c2_s4)
summary(m_y4_c2_s4)$tTable
intervals(m_y4_c2_s4, which = "fixed")

# GRÁFICAS Y4
sim_esc2$pred_y4_c1_s4 <- predict(m_y4_c1_s4, level = 0)
sim_esc2$pred_y4_c2_s4 <- predict(m_y4_c2_s4, level = 0)

ggplot(sim_esc2, aes(x = tiempo_caso1, y = Y4_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c1_s4), color = "blue", linewidth = 1) +
  theme_bw()

ggplot(sim_esc2, aes(x = tiempo_caso2, y = Y4_s4)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_y4_c2_s4), color = "red", linewidth = 1) +
  theme_bw()

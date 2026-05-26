library(nlme)
library(lme4)

head(bbdd)
summary(bbdd)
bbdd1<-bbdd

# Primer caso. Vamos a tomar el primer paciente que fue a consulta como 0 de referencia

bbdd1$FEC_ <- as.Date(bbdd1$FEC_)

# Ordeno la bbdd1 de menor a mayor en el tiempo

bbdd1 <- bbdd1[order(bbdd1$FEC_), ]

# Una vez ordenadas las  medidas de los pacientes, saco una columna que me mide 
# la diferencia de de días del primer paciente y medición (0) respecto al 
# resto de mediciones. Este será el caso 1:

fecha_t0_global <- min(bbdd1$FEC_, na.rm = TRUE)

# Con años de diferencia.

bbdd1$tiempo_caso1 <- 
  as.numeric(bbdd1$FEC_ - fecha_t0_global) / 365.25



# Pasamos al caso 2. Aquí, tomamos el tiempo 0 de cada paciente como fecha de
# su primera medición.

# Fecha inicial de cada paciente.
# Dividimos FEC_ en grupos según AF_P,y los ordenamos por el mínimo.
bbdd1$fecha_t0_paciente <- ave(
  bbdd1$FEC_,
  bbdd1$AF_P,
  FUN = min
)

# Tiempo en años desde la primera medición del paciente
bbdd1$tiempo_caso2 <- 
  as.numeric(bbdd1$FEC_ - bbdd1$fecha_t0_paciente) / 365.25

# Ver un paciente concreto
bbdd1[bbdd1$AF_P == bbdd1$AF_P[1],
      c("AF_P", "vs", "FEC_", "tiempo_caso1", "tiempo_caso2")]

# Ver otro paciente concreto
bbdd1[bbdd1$AF_P == bbdd1$AF_P[18],
      c("AF_P", "vs", "FEC_", "tiempo_caso1", "tiempo_caso2")]


# MODELOS CASO 1 - TIEMPO GLOBAL

# Symptoms
m_symp_c1 <- lme(
  SYMPTOMS ~ tiempo_caso1,
  random = ~tiempo_caso1| AF_P,
  data = bbdd1
)

# Activity
m_act_c1 <- lme(
  ACTIVITY ~ tiempo_caso1,
  random = ~tiempo_caso1 | AF_P,
  data = bbdd1
)

# Impacts
m_imp_c1 <- lme(
  IMPACTS ~ tiempo_caso1,
  random = ~tiempo_caso1 | AF_P,
  data = bbdd1
)

# Total
m_tot_c1 <- lme(
  TOTAL_STG ~ tiempo_caso1,
  random = ~tiempo_caso1 | AF_P,
  data = bbdd1
)

# MODELOS CASO 2 - TIEMPO PARA CADA PACIENTE

# Symptoms
m_symp_c2 <- lme(
  SYMPTOMS ~ tiempo_caso2,
  random = ~tiempo_caso2 | AF_P,
  data = bbdd1
  )

# Activity
m_act_c2 <- lme(
  ACTIVITY ~ tiempo_caso2,
  random = ~tiempo_caso2 | AF_P,
  data = bbdd1
  )

# Impacts
m_imp_c2 <- lme(
  IMPACTS ~ tiempo_caso2,
  random = ~tiempo_caso2 | AF_P,
  data = bbdd1
  )

# Total
m_tot_c2 <- lme(
  TOTAL_STG ~ tiempo_caso2,
  random = ~tiempo_caso2 | AF_P,
  data = bbdd1
  )

# RESUMEN DE LOS MODELOS

# Symptoms
# Caso 1 - Tiempo global
m_symp_c1
VarCorr(m_symp_c1)
summary(m_symp_c1)$tTable
intervals(m_symp_c1, which = "fixed")

# Caso 2 - Tiempo individual
m_symp_c2
VarCorr(m_symp_c2)
summary(m_symp_c2)$tTable
intervals(m_symp_c2, which = "fixed")

# Activity
# Caso 1 - Tiempo global
m_act_c1
VarCorr(m_act_c1)
summary(m_act_c1)$tTable
intervals(m_act_c1, which = "fixed")

# Caso 2 - Tiempo individual
m_act_c2
VarCorr(m_act_c2)
summary(m_act_c2)$tTable
intervals(m_act_c2, which = "fixed")

# Impacts
# Caso 1 - Tiempo global
m_imp_c1
VarCorr(m_imp_c1)
summary(m_imp_c1)$tTable
intervals(m_imp_c1, which = "fixed")

# Caso 2 - Tiempo individual
m_imp_c2
VarCorr(m_imp_c2)
summary(m_imp_c2)$tTable
intervals(m_imp_c2, which = "fixed")

# Total
# Caso 1 - Tiempo global
m_tot_c1
VarCorr(m_tot_c1)
summary(m_tot_c1)$tTable
intervals(m_tot_c1, which = "fixed")

# Caso 2 - Tiempo individual
m_tot_c2
VarCorr(m_tot_c2)
summary(m_tot_c2)$tTable
intervals(m_tot_c2, which = "fixed")

# GRÁFICAS

library(ggplot2)

# Symptoms

bbdd1$pred_c1 <- predict(m_symp_c1, level = 0)
bbdd1$pred_c2 <- predict(m_symp_c2, level = 0)

# Caso1
ggplot(bbdd1, aes(x = tiempo_caso1, y = SYMPTOMS)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_c1), color = "blue", linewidth = 1) +
  labs(title = "Modelo con tiempo global",
       x = "Tiempo (caso 1)",
       y = "Síntomas") +
  theme_bw()

# Caso2
ggplot(bbdd1, aes(x = tiempo_caso2, y = SYMPTOMS)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_c2), color = "red", linewidth = 1) +
  labs(title = "Modelo con tiempo individual",
       x = "Tiempo (caso 2)",
       y = "Síntomas") +
  theme_bw()

# Activity
bbdd1$pred_act_c1 <- predict(m_act_c1, level = 0)
bbdd1$pred_act_c2 <- predict(m_act_c2, level = 0)

# Caso 1 - Tiempo global
ggplot(bbdd1, aes(x = tiempo_caso1, y = ACTIVITY)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_act_c1), color = "blue", linewidth = 1) +
  labs(title = "Actividad – Modelo con tiempo global",
       x = "Tiempo (caso 1)",
       y = "Actividad") +
  theme_bw()

# Caso 2 - Tiempo individual
ggplot(bbdd1, aes(x = tiempo_caso2, y = ACTIVITY)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_act_c2), color = "red", linewidth = 1) +
  labs(title = "Actividad – Modelo con tiempo individual",
       x = "Tiempo (caso 2)",
       y = "Actividad") +
  theme_bw()

# Impacts
bbdd1$pred_imp_c1 <- predict(m_imp_c1, level = 0)
bbdd1$pred_imp_c2 <- predict(m_imp_c2, level = 0)

# Caso 1 - Tiempo global
ggplot(bbdd1, aes(x = tiempo_caso1, y = IMPACTS)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_imp_c1), color = "blue", linewidth = 1) +
  labs(title = "Impactos – Modelo con tiempo global",
       x = "Tiempo (caso 1)",
       y = "Impactos") +
  theme_bw()

# Caso 2 - Tiempo individual
ggplot(bbdd1, aes(x = tiempo_caso2, y = IMPACTS)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_imp_c2), color = "red", linewidth = 1) +
  labs(title = "Impactos – Modelo con tiempo individual",
       x = "Tiempo (caso 2)",
       y = "Impactos") +
  theme_bw()

# Total
bbdd1$pred_tot_c1 <- predict(m_tot_c1, level = 0)
bbdd1$pred_tot_c2 <- predict(m_tot_c2, level = 0)

# Caso 1 - Tiempo global
ggplot(bbdd1, aes(x = tiempo_caso1, y = TOTAL_STG)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_tot_c1), color = "blue", linewidth = 1) +
  labs(title = "Puntuación total – Modelo con tiempo global",
       x = "Tiempo (caso 1)",
       y = "Puntuación total") +
  theme_bw()

# Caso 2 - Tiempo individual
ggplot(bbdd1, aes(x = tiempo_caso2, y = TOTAL_STG)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = pred_tot_c2), color = "red", linewidth = 1) +
  labs(title = "Puntuación total – Modelo con tiempo individual",
       x = "Tiempo (caso 2)",
       y = "Puntuación total") +
  theme_bw()


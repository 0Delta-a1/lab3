# Ejecute en R una sola vez.
paquetes <- c("ggplot2", "dplyr", "lmtest", "car", "MASS", "glmnet", "leaps", "rsample", "recipes")
faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes)) install.packages(faltantes)



base<- read.csv("EME8_base_full_original.csv")
ids<- read.csv("ids_particiones_pliegues.csv")

datos <- merge(base, ids, by="id", all=FALSE)



#----a----


length(datos)
dim(datos)
stopifnot(nrow(base)==7170, nrow(datos)==6747, !anyDuplicated(datos$id))
train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")
stopifnot(nrow(train)==5360, nrow(test)==1387)
print(table(train$fold))

# Estructura de los datos
dim(datos)
names(datos)
str(datos)

# Tipos de variables
sapply(datos[, c("m2_1", "m2_2", "m2_3", "m2_4", "m2_5")], class)

# Revisar identificadores
# Reemplazar id por el nombre real de la clave
sum(duplicated(datos$id))


#-----------------------
#----b-----
# 1. Aseguramos la recodificación previa de m2_1 a m2_4 y preparamos m2_5
datos <- datos %>%
  mutate(
    # Recodificación binaria de m2_1 a m2_4
    across(c(m2_1, m2_2, m2_3, m2_4), ~ case_when(
      .x == 1 ~ 1,
      .x == 2 ~ 0,
      .x %in% c(88, 99) ~ NA_real_,
      TRUE ~ NA_real_
    )),
    # m2_5 a formato binario 1/0
    m2_5_bin = case_when(
      m2_5 == 1 ~ 1,
      m2_5 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )



#----c----
# 2. Construcción de 'alguna_amenaza' y auditoría de inconsistencias
datos <- datos %>%
  mutate(
    # Corrección: Usamos lógica OR (|) en lugar de suma matemática.
    # Si hay al menos un 1, devuelve 1. Si son puros 0, devuelve 0. Si hay mezcla de 0 y NA, devuelve NA.
    alguna_amenaza = if_else(m2_1 == 1 | m2_2 == 1 | m2_3 == 1 | m2_4 == 1, 1, 0),
    
    # Evaluación de consistencia lógica
    chequeo_consistencia = case_when(
      is.na(alguna_amenaza) | is.na(m2_5_bin) ~ "Dato Faltante (NA)",
      alguna_amenaza == 1 & m2_5_bin == 1 ~ "Inconsistencia: Declara amenaza y marca ninguna",
      alguna_amenaza == 0 & m2_5_bin == 0 ~ "Inconsistencia: No declara amenaza ni marca ninguna",
      TRUE ~ "Consistente"
    )
  )

# 3. Ver las frecuencias de inconsistencias en la muestra
table(datos$chequeo_consistencia, useNA = "always")



#---2a-------
train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")

by(train$ganancia_final, train$alguna_amenaza, summary)



# ============================================================
# PAQUETES E IMPORTACIÓN DE DATOS
# ============================================================

paquetes <- c(
  "ggplot2", "dplyr", "lmtest", "car",
  "glmnet", "tseries", "reshape2"
)

faltantes <- setdiff(paquetes, rownames(installed.packages()))
if (length(faltantes) > 0) install.packages(faltantes)

invisible(lapply(paquetes, library, character.only = TRUE))

base <- read.csv("EME8_base_full_original.csv")
ids  <- read.csv("ids_particiones_pliegues.csv")

datos <- merge(base, ids, by = "id", all = FALSE)


# ============================================================
# PREGUNTA 1: REVISIÓN Y PREPARACIÓN DE LOS DATOS
# ============================================================

# 1.a. Dimensiones, estructura e identificadores

length(datos)
dim(datos)
head(datos)
str(datos)

stopifnot(
  nrow(base) == 7170,
  nrow(datos) == 6747,
  !anyDuplicated(datos$id)
)

sum(duplicated(datos$id))

train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")

stopifnot(
  nrow(train) == 5360,
  nrow(test) == 1387
)

table(train$fold)

sapply(datos[, c("m2_1", "m2_2", "m2_3", "m2_4", "m2_5")], class)


# 1.b. Recodificación de variables ambientales

# 1 = Sí
# 2 = No
# 88 y 99 = NA

datos <- datos %>%
  mutate(
    across(
      c(m2_1, m2_2, m2_3, m2_4),
      ~ case_when(
        .x == 1 ~ 1,
        .x == 2 ~ 0,
        .x %in% c(88, 99) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ),
    m2_5_bin = case_when(
      m2_5 == 1 ~ 1,
      m2_5 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )


# 1.c. Construcción de alguna_amenaza y auditoría

datos <- datos %>%
  mutate(
    alguna_amenaza = case_when(
      if_any(c(m2_1, m2_2, m2_3, m2_4), ~ .x == 1) ~ 1,
      if_all(c(m2_1, m2_2, m2_3, m2_4), ~ !is.na(.x) & .x == 0) ~ 0,
      TRUE ~ NA_real_
    ),
    
    chequeo_consistencia = case_when(
      is.na(alguna_amenaza) | is.na(m2_5_bin) ~
        "Dato Faltante (NA)",
      
      alguna_amenaza == 1 & m2_5_bin == 1 ~
        "Inconsistencia: Declara amenaza y marca ninguna",
      
      alguna_amenaza == 0 & m2_5_bin == 0 ~
        "Inconsistencia: No declara amenaza ni marca ninguna",
      
      TRUE ~ "Consistente"
    ),
    
    y_asinh = asinh(ganancia_final / 100000),
    
    y_signlog = sign(ganancia_final) *
      log1p(abs(ganancia_final) / 100000)
  )

table(datos$chequeo_consistencia, useNA = "always")


# Actualizar las particiones con los datos preparados

train <- subset(datos, particion == "entrenamiento")
test  <- subset(datos, particion == "prueba")

# Mantener los mismos niveles categóricos entre entrenamiento y prueba

factores <- c("informalidad", "c1_b", "sexo", "region")

for (v in factores) {
  train[[v]] <- factor(train[[v]])
  test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
}


# ============================================================
# PREGUNTA 2: ANÁLISIS DESCRIPTIVO
# ============================================================

train_df <- train

# 2.a. Comparación de ganancias según exposición ambiental

cat("\n=== PREGUNTA 2a: GANANCIAS POR AMENAZA ===\n")

resumen_ganancias <- train_df %>%
  filter(!is.na(alguna_amenaza)) %>%
  group_by(alguna_amenaza) %>%
  summarise(
    n = n(),
    Minimo = min(ganancia_final, na.rm = TRUE),
    Q1 = quantile(ganancia_final, 0.25, na.rm = TRUE),
    Mediana = median(ganancia_final, na.rm = TRUE),
    Media = mean(ganancia_final, na.rm = TRUE),
    Q3 = quantile(ganancia_final, 0.75, na.rm = TRUE),
    Maximo = max(ganancia_final, na.rm = TRUE),
    IQR = IQR(ganancia_final, na.rm = TRUE),
    Desv_Est = sd(ganancia_final, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen_ganancias)

ggplot(
  train_df %>% filter(!is.na(alguna_amenaza)),
  aes(
    x = factor(
      alguna_amenaza,
      levels = c(0, 1),
      labels = c("Sin Amenaza", "Con Amenaza")
    ),
    y = ganancia_final / 1e6
  )
) +
  geom_boxplot(
    fill = c("#377eb8", "#e41a1c"),
    alpha = 0.7,
    outlier.colour = "black"
  ) +
  labs(
    title = "Distribución de Ganancias Mensuales por Exposición Ambiental",
    x = "Condición de Amenaza Ambiental",
    y = "Ganancia Mensual (Millones de CLP)"
  ) +
  theme_minimal()


# 2.b. Frecuencias de amenazas y cruce por sector económico

cat("\n=== PREGUNTA 2b: FRECUENCIA DE AMENAZAS ===\n")

frec_amenazas <- train_df %>%
  summarise(
    Sequias_Heladas = sum(m2_1 == 1, na.rm = TRUE),
    Inundaciones = sum(m2_2 == 1, na.rm = TRUE),
    Incendios = sum(m2_3 == 1, na.rm = TRUE),
    Tormentas = sum(m2_4 == 1, na.rm = TRUE)
  )

print(frec_amenazas)

cruce_sector <- train_df %>%
  filter(!is.na(c1_b), !is.na(alguna_amenaza)) %>%
  group_by(c1_b, alguna_amenaza) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(Proporcion = n / sum(n)) %>%
  ungroup()

ggplot(
  cruce_sector,
  aes(
    x = factor(c1_b),
    y = Proporcion,
    fill = factor(alguna_amenaza, labels = c("No", "Sí"))
  )
) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c("#2b8cbe", "#e41a1c")) +
  labs(
    title = "Proporción de Amenazas Declaradas por Sector Económico",
    x = "Sector Económico (c1_b)",
    y = "Proporción",
    fill = "Amenaza Declarada"
  ) +
  coord_flip() +
  theme_minimal()


# 2.c. Prevalencia de amenazas por región

cat("\n=== PREGUNTA 2c: PREVALENCIA POR REGIÓN ===\n")

prevalencia_regional <- train_df %>%
  filter(!is.na(region), !is.na(alguna_amenaza)) %>%
  group_by(region) %>%
  summarise(
    Total = n(),
    Con_Amenaza = sum(alguna_amenaza == 1),
    Prevalencia_Pct = round(mean(alguna_amenaza == 1) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(Prevalencia_Pct))

print(prevalencia_regional)


# ============================================================
# PREGUNTA 3: MODELO OLS Y DIAGNÓSTICOS
# ============================================================


# 3.a. Ajuste del modelo OLS

train_df <- train

mod_ols <- lm(
  ganancia_final ~ m2_1 + m2_2 + m2_3 + m2_4 +
    c6_3 + c7 + total_trabajadores +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region),
  data = train_df
)

cat("\n=== RESUMEN DEL MODELO OLS ===\n")
print(summary(mod_ols))


# 3.b. Verificación de supuestos

bp_test <- bptest(mod_ols)
jb_test <- jarque.bera.test(residuals(mod_ols))

cat("\n=== DIAGNÓSTICO DE RESIDUOS ===\n")
cat("P-valor Breusch-Pagan:", bp_test$p.value, "\n")
cat("P-valor Jarque-Bera:", jb_test$p.value, "\n")

par(mfrow = c(1, 2))

plot(
  fitted(mod_ols),
  residuals(mod_ols),
  xlab = "Valores Ajustados",
  ylab = "Residuos",
  main = "Residuos vs. Ajustados (OLS)"
)

abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(mod_ols), main = "Q-Q Plot Residuos OLS")
qqline(residuals(mod_ols), col = "red")

par(mfrow = c(1, 1))


# 3.c. VIF, RESET y comparación con modelo de controles

cat("\n=== DIAGNÓSTICO VIF Y RESET ===\n")

print(vif(mod_ols))

reset_res <- resettest(
  mod_ols,
  power = 2:3,
  type = "fitted"
)

print(reset_res)

mod_controles <- lm(
  ganancia_final ~ c6_3 + c7 + total_trabajadores +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region),
  data = train_df
)

cat("\n=== COMPARACIÓN DE MODELOS ===\n")
cat("R2 ajustado (Modelo Completo):", summary(mod_ols)$adj.r.squared, "\n")
cat("R2 ajustado (Solo Controles):", summary(mod_controles)$adj.r.squared, "\n")


# ============================================================
# PREGUNTA 4: TRANSFORMACIONES
# ============================================================

theta <- 100000

variables_modelo <- c(
  "ganancia_final", "m2_1", "m2_2", "m2_3", "m2_4",
  "c6_3", "c7", "total_trabajadores",
  "informalidad", "c1_b", "sexo", "region"
)

train_df <- train
test_df  <- test

# 4.a. Verificación de ceros y negativos

cat("\n=== AUDITORÍA DE GANANCIA FINAL ===\n")

cat("Valores negativos:", sum(train_df$ganancia_final < 0, na.rm = TRUE), "\n")
cat("Valores en cero:", sum(train_df$ganancia_final == 0, na.rm = TRUE), "\n")
cat("Valores positivos:", sum(train_df$ganancia_final > 0, na.rm = TRUE), "\n")

# Casos completos

train_q4 <- train_df[complete.cases(train_df[, variables_modelo]), ]
test_q4  <- test_df[complete.cases(test_df[, variables_modelo]), ]

# Evitar niveles categóricos que no existen en entrenamiento

for (v in factores) {
  test_q4 <- test_q4[
    test_q4[[v]] %in% levels(train_q4[[v]]),
  ]
}

cat("Observaciones completas en entrenamiento:", nrow(train_q4), "\n")
cat("Observaciones completas en prueba:", nrow(test_q4), "\n")


# 4.b. Ajuste de los tres modelos

formula_pred <- ganancia_final ~
  m2_1 + m2_2 + m2_3 + m2_4 +
  c6_3 + c7 + total_trabajadores +
  factor(informalidad) + factor(c1_b) +
  factor(sexo) + factor(region)

m_orig <- lm(formula_pred, data = train_q4)

m_asinh <- lm(
  update(formula_pred, y_asinh ~ .),
  data = train_q4
)

m_signlog <- lm(
  update(formula_pred, y_signlog ~ .),
  data = train_q4
)


# Funciones de métricas

mae_fun <- function(y_real, y_pred) {
  mean(abs(y_real - y_pred), na.rm = TRUE)
}

rmse_fun <- function(y_real, y_pred) {
  sqrt(mean((y_real - y_pred)^2, na.rm = TRUE))
}


# 4.c. Evaluación en entrenamiento

pred_train_orig <- predict(m_orig, newdata = train_q4)

pred_train_asinh <- theta * sinh(
  predict(m_asinh, newdata = train_q4)
)

z_train_signlog <- predict(m_signlog, newdata = train_q4)

pred_train_signlog <- sign(z_train_signlog) *
  theta * expm1(abs(z_train_signlog))

tabla_4c_train <- data.frame(
  Modelo = c(
    "Original (Niveles CLP)",
    "asinh(Y / 100.000)",
    "Sign-log(Y / 100.000)"
  ),
  MAE_CLP = c(
    mae_fun(train_q4$ganancia_final, pred_train_orig),
    mae_fun(train_q4$ganancia_final, pred_train_asinh),
    mae_fun(train_q4$ganancia_final, pred_train_signlog)
  ),
  RMSE_CLP = c(
    rmse_fun(train_q4$ganancia_final, pred_train_orig),
    rmse_fun(train_q4$ganancia_final, pred_train_asinh),
    rmse_fun(train_q4$ganancia_final, pred_train_signlog)
  )
)

cat("\n=== MÉTRICAS EN ENTRENAMIENTO ===\n")
print(tabla_4c_train)


# Evaluación predictiva en prueba

pred_test_orig <- predict(m_orig, newdata = test_q4)

pred_test_asinh <- theta * sinh(
  predict(m_asinh, newdata = test_q4)
)

z_test_signlog <- predict(m_signlog, newdata = test_q4)

pred_test_signlog <- sign(z_test_signlog) *
  theta * expm1(abs(z_test_signlog))

tabla_4c_test <- data.frame(
  Modelo = c(
    "Original (Niveles CLP)",
    "asinh(Y / 100.000)",
    "Sign-log(Y / 100.000)"
  ),
  MAE_CLP = c(
    mae_fun(test_q4$ganancia_final, pred_test_orig),
    mae_fun(test_q4$ganancia_final, pred_test_asinh),
    mae_fun(test_q4$ganancia_final, pred_test_signlog)
  ),
  RMSE_CLP = c(
    rmse_fun(test_q4$ganancia_final, pred_test_orig),
    rmse_fun(test_q4$ganancia_final, pred_test_asinh),
    rmse_fun(test_q4$ganancia_final, pred_test_signlog)
  )
)

cat("\n=== MÉTRICAS PREDICTIVAS EN TEST ===\n")
print(tabla_4c_test)


# ============================================================
# PREGUNTA 5: NO LINEALIDADES E INTERACCIONES
# ============================================================

train_df <- train %>%
  mutate(
    log_c6_3 = log1p(c6_3),
    log_total_trab = log1p(total_trabajadores)
  )


# 5.a. Predictores logarítmicos

mod_5a <- lm(
  y_asinh ~ m2_1 + m2_2 + m2_3 + m2_4 +
    log_c6_3 + c7 + log_total_trab +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region),
  data = train_df
)

summary(mod_5a)
train_df$log_c6_3 <- log1p(train_df$c6_3)
train_df$log_total_trab <- log1p(train_df$total_trabajadores)

# Histogramas de variables originales y transformadas

par(mfrow = c(2, 2))

hist(train_df$c6_3,
     main = "c6_3 original",
     xlab = "c6_3",
     col = "lightblue",
     border = "white")

hist(train_df$log_c6_3,
     main = "log(1 + c6_3)",
     xlab = "log(1 + c6_3)",
     col = "lightgreen",
     border = "white")

hist(train_df$total_trabajadores,
     main = "Total de trabajadores original",
     xlab = "Total de trabajadores",
     col = "lightblue",
     border = "white")

hist(train_df$log_total_trab,
     main = "log(1 + total trabajadores)",
     xlab = "log(1 + total trabajadores)",
     col = "lightgreen",
     border = "white")

par(mfrow = c(1, 1))

resultados_5a <- summary(mod_5a)$coefficients[
  c("log_c6_3", "log_total_trab"),
  c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
]

round(resultados_5a, 4)


# 5.b. Término cuadrático en antigüedad

mod_5b <- lm(
  y_asinh ~ m2_1 + m2_2 + m2_3 + m2_4 +
    log_c6_3 + c7 + I(c7^2) + log_total_trab +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region),
  data = train_df
)


# 5.c. Interacción entre amenaza e informalidad

mod_5c <- lm(
  y_asinh ~ m2_1 + m2_2 + m2_3 + m2_4 +
    alguna_amenaza * factor(informalidad) +
    log_c6_3 + c7 + I(c7^2) + log_total_trab +
    factor(c1_b) + factor(sexo) + factor(region),
  data = train_df
)


cat("\n=== RESULTADOS PREGUNTA 5b ===\n")
print(summary(mod_5b)$coefficients[c("c7", "I(c7^2)"), ])

cat("\n=== RESULTADOS PREGUNTA 5c ===\n")

coef_5c <- summary(mod_5c)$coefficients

print(
  coef_5c[
    grepl("alguna_amenaza", rownames(coef_5c)),
    ,
    drop = FALSE
  ]
)

cat("\n=== COMPARACIÓN R2 AJUSTADO ===\n")
cat("Modelo 5a:", summary(mod_5a)$adj.r.squared, "\n")
cat("Modelo 5b:", summary(mod_5b)$adj.r.squared, "\n")
cat("Modelo 5c:", summary(mod_5c)$adj.r.squared, "\n")


# ============================================================
# PREGUNTA 6: OBSERVACIONES INFLUYENTES
# ============================================================

# Utilizar casos completos para identificar correctamente los ID

train_q6 <- train[
  complete.cases(train[, variables_modelo]),
]

mod_ols <- lm(
  formula_pred,
  data = train_q6
)


# 6.a. Medidas de influencia

residuos <- residuals(mod_ols)
residuos_est <- rstandard(mod_ols)
r_stud <- rstudent(mod_ols)
leverage <- hatvalues(mod_ols)
cooks_d <- cooks.distance(mod_ols)

# Relacionar los residuos con sus identificadores originales

ids_modelo <- train_q6$id[
  match(names(residuos), rownames(train_q6))
]

tabla_diagnostico <- data.frame(
  ID = ids_modelo,
  Residuo = residuos,
  Residuo_Estandarizado = residuos_est,
  Residuo_Studentizado = r_stud,
  Leverage = leverage,
  Distancia_Cook = cooks_d
)

tabla_diagnostico <- tabla_diagnostico[
  order(tabla_diagnostico$Distancia_Cook, decreasing = TRUE),
]

cat("\n=== DIAGNÓSTICO DE OBSERVACIONES ===\n")
print(head(tabla_diagnostico, 5))



# ============================================================
# PREGUNTA 7.c: ESTABILIDAD DE COEFICIENTES AMBIENTALES
# ============================================================

# Cinco observaciones con mayor distancia de Cook
ids_influyentes <- c(
  59593634,
  22055925,
  18361134,
  24680050,
  57955872
)

# Excluir temporalmente las cinco observaciones
train_sin5 <- train_df[
  !(train_df$id %in% ids_influyentes),
]

# Reestimar el mismo modelo original
mod_ols_sin5 <- update(
  mod_ols,
  data = train_sin5
)

cat("Observaciones modelo original:", nobs(mod_ols), "\n")
cat("Observaciones modelo sin cinco casos:", nobs(mod_ols_sin5), "\n")


# Coeficientes ambientales
variables_ambientales <- c("m2_1", "m2_2", "m2_3", "m2_4")

# Extraer coeficientes de ambos modelos
coef_original <- coef(mod_ols)
coef_sin5 <- coef(mod_ols_sin5)

# Construir tabla
tabla_estabilidad <- data.frame(
  Variable = variables_ambientales,
  Coef_Original = coef_original[variables_ambientales],
  Coef_Sin_Influyentes = coef_sin5[variables_ambientales]
)

# Diferencia entre coeficientes
tabla_estabilidad$Cambio <- 
  tabla_estabilidad$Coef_Sin_Influyentes -
  tabla_estabilidad$Coef_Original

# Cambio porcentual respecto al coeficiente original
tabla_estabilidad$Cambio_Porcentual <- ifelse(
  abs(tabla_estabilidad$Coef_Original) > 1e-10,
  100 * tabla_estabilidad$Cambio /
    abs(tabla_estabilidad$Coef_Original),
  NA
)





# 6.b. Cinco observaciones más influyentes

top5_idx <- order(cooks_d, decreasing = TRUE)[1:5]

filas_top5 <- match(
  names(cooks_d)[top5_idx],
  rownames(train_q6)
)

tabla_infl <- data.frame(
  Fila_ID = ids_modelo[top5_idx],
  Ganancia_CLP = train_q6$ganancia_final[filas_top5],
  Cooks_D = cooks_d[top5_idx],
  Leverage = leverage[top5_idx],
  R_Student = r_stud[top5_idx]
)

cat("\n=== CINCO OBSERVACIONES MÁS INFLUYENTES ===\n")
print(tabla_infl)


# 6.c. Reajuste sin las cinco observaciones influyentes

mod_sin_infl <- lm(
  formula(mod_ols),
  data = train_q6[-filas_top5, ]
)

coef_comp <- data.frame(
  Coef_Original = coef(mod_ols)[c("m2_1", "m2_2", "m2_3", "m2_4")],
  Pval_Original = summary(mod_ols)$coefficients[
    c("m2_1", "m2_2", "m2_3", "m2_4"), 4
  ],
  Coef_SinInfl = coef(mod_sin_infl)[
    c("m2_1", "m2_2", "m2_3", "m2_4")
  ],
  Pval_SinInfl = summary(mod_sin_infl)$coefficients[
    c("m2_1", "m2_2", "m2_3", "m2_4"), 4
  ]
)

cat("\n=== SENSIBILIDAD DE COEFICIENTES AMBIENTALES ===\n")
print(coef_comp)


# 6.d. Gráficos de diagnóstico

n <- nobs(mod_ols)
p <- length(coef(mod_ols))

lim_residuos <- 2
lim_leverage <- 2 * p / n
lim_cook <- 4 / n

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot(
  r_stud,
  type = "h",
  main = "Residuos studentizados",
  xlab = "Caso",
  ylab = "Residuo studentizado"
)

abline(h = c(-lim_residuos, lim_residuos), col = "red", lty = 2)

plot(
  leverage,
  type = "h",
  main = "Leverage",
  xlab = "Caso",
  ylab = "Apalancamiento"
)

abline(h = lim_leverage, col = "red", lty = 2)

plot(
  cooks_d,
  type = "h",
  main = "Distancia de Cook",
  xlab = "Caso",
  ylab = "Cook's D"
)

abline(h = lim_cook, col = "red", lty = 2)

mtext(
  "Diagnóstico de influencia del modelo OLS",
  outer = TRUE,
  font = 2
)

par(mfrow = c(1, 1))


# ============================================================
# PREGUNTA 7: SELECCIÓN DE VARIABLES
# ============================================================

# Mantener las mismas observaciones para todos los modelos

train_df <- train[
  complete.cases(train[, variables_modelo]),
]

n_train <- nrow(train_df)

cat("Observaciones utilizadas:", n_train, "\n")


# Modelo base: amenazas ambientales obligatorias

mod_base <- lm(
  ganancia_final ~ m2_1 + m2_2 + m2_3 + m2_4,
  data = train_df
)


# Modelo completo

mod_completo <- lm(
  formula_pred,
  data = train_df
)

formula_base <- formula(mod_base)
formula_completa <- formula(mod_completo)

terminos_base <- attr(terms(formula_base), "term.labels")


# 7.a. Selección Forward

seleccion_forward <- function(modelo, formula_superior, k = 2) {
  
  actual <- modelo
  
  repeat {
    
    candidatos <- add1(actual, scope = formula_superior, k = k)
    
    candidatos <- candidatos[
      rownames(candidatos) != "<none>",
      ,
      drop = FALSE
    ]
    
    if (nrow(candidatos) == 0) break
    
    mejor <- rownames(candidatos)[which.min(candidatos$AIC)]
    
    if (min(candidatos$AIC) >= extractAIC(actual, k = k)[2]) break
    
    actual <- update(
      actual,
      as.formula(paste(". ~ . +", mejor))
    )
  }
  
  actual
}

mod_forward <- seleccion_forward(mod_base, formula_completa)


# 7.b. Selección Backward

seleccion_backward <- function(modelo, terminos_base, k = 2) {
  
  actual <- modelo
  
  repeat {
    
    terminos_actuales <- attr(terms(actual), "term.labels")
    eliminables <- setdiff(terminos_actuales, terminos_base)
    
    if (length(eliminables) == 0) break
    
    candidatos <- drop1(actual, scope = eliminables, k = k)
    
    candidatos <- candidatos[
      rownames(candidatos) != "<none>",
      ,
      drop = FALSE
    ]
    
    if (nrow(candidatos) == 0) break
    
    mejor <- rownames(candidatos)[which.min(candidatos$AIC)]
    
    if (min(candidatos$AIC) >= extractAIC(actual, k = k)[2]) break
    
    actual <- update(
      actual,
      as.formula(paste(". ~ . -", mejor))
    )
  }
  
  actual
}

mod_backward <- seleccion_backward(mod_completo, terminos_base)


# 7.c. Selección Stepwise AIC / BIC

seleccion_stepwise <- function(
    modelo,
    formula_superior,
    terminos_base,
    k = 2
) {
  
  actual <- modelo
  
  repeat {
    
    agregar <- add1(actual, scope = formula_superior, k = k)
    
    agregar <- agregar[
      rownames(agregar) != "<none>",
      ,
      drop = FALSE
    ]
    
    terminos_actuales <- attr(terms(actual), "term.labels")
    eliminables <- setdiff(terminos_actuales, terminos_base)
    
    if (length(eliminables) > 0) {
      
      eliminar <- drop1(actual, scope = eliminables, k = k)
      
      eliminar <- eliminar[
        rownames(eliminar) != "<none>",
        ,
        drop = FALSE
      ]
      
    } else {
      eliminar <- data.frame(AIC = numeric(0))
    }
    
    opciones <- c(agregar$AIC, eliminar$AIC)
    
    if (length(opciones) == 0) break
    
    mejor_pos <- which.min(opciones)
    mejor_aic <- opciones[mejor_pos]
    
    if (mejor_aic >= extractAIC(actual, k = k)[2]) break
    
    if (mejor_pos <= nrow(agregar)) {
      
      variable <- rownames(agregar)[mejor_pos]
      
      actual <- update(
        actual,
        as.formula(paste(". ~ . +", variable))
      )
      
    } else {
      
      posicion <- mejor_pos - nrow(agregar)
      variable <- rownames(eliminar)[posicion]
      
      actual <- update(
        actual,
        as.formula(paste(". ~ . -", variable))
      )
    }
  }
  
  actual
}

mod_step_aic <- seleccion_stepwise(
  mod_base, formula_completa, terminos_base, k = 2
)

mod_step_bic <- seleccion_stepwise(
  mod_base, formula_completa, terminos_base, k = log(n_train)
)


# 7.d. Comparación de modelos

cat("\n=== FORWARD ===\n")
print(formula(mod_forward))

cat("\n=== BACKWARD ===\n")
print(formula(mod_backward))

cat("\n=== STEPWISE AIC ===\n")
print(formula(mod_step_aic))

cat("\n=== STEPWISE BIC ===\n")
print(formula(mod_step_bic))


obtener_metricas <- function(modelo, nombre, sigma2_full) {
  
  p <- length(coef(modelo))
  n <- nobs(modelo)
  RSS <- sum(resid(modelo)^2)
  
  Cp <- RSS / sigma2_full - (n - 2 * p)
  
  data.frame(
    Modelo = nombre,
    Parametros = p,
    R2_Adj = summary(modelo)$adj.r.squared,
    AIC = AIC(modelo),
    BIC = BIC(modelo),
    Cp = Cp
  )
}

sigma2_full <- deviance(mod_completo) / df.residual(mod_completo)

tabla_modelos <- rbind(
  obtener_metricas(mod_base, "Modelo Base", sigma2_full),
  obtener_metricas(mod_completo, "Modelo Completo", sigma2_full),
  obtener_metricas(mod_forward, "Forward", sigma2_full),
  obtener_metricas(mod_backward, "Backward", sigma2_full),
  obtener_metricas(mod_step_aic, "Stepwise AIC", sigma2_full),
  obtener_metricas(mod_step_bic, "Stepwise BIC", sigma2_full)
)

cat("\n=== COMPARACIÓN DE MODELOS ===\n")
print(tabla_modelos)


# 7.e. Identificación del modelo final

modelos_sel <- list(
  Forward = mod_forward,
  Backward = mod_backward,
  Stepwise_AIC = mod_step_aic,
  Stepwise_BIC = mod_step_bic
)

firma_modelo <- function(modelo) {
  paste(
    sort(attr(terms(modelo), "term.labels")),
    collapse = " + "
  )
}

firmas <- sapply(modelos_sel, firma_modelo)

cat("\n=== COMPARACIÓN DE MODELOS SELECCIONADOS ===\n")

for (i in seq_along(modelos_sel)) {
  for (j in seq_along(modelos_sel)) {
    if (j > i) {
      cat(
        names(modelos_sel)[i], "y",
        names(modelos_sel)[j], ":",
        ifelse(firmas[i] == firmas[j],
               "Mismo modelo", "Modelos diferentes"),
        "\n"
      )
    }
  }
}

modelo_final <- mod_step_bic

cat("\n=== MODELO FINAL SELECCIONADO POR BIC ===\n")
print(formula(modelo_final))
print(summary(modelo_final))

cat("\nMétodos que seleccionaron la misma fórmula que BIC:\n")
print(names(modelos_sel)[firmas == firma_modelo(modelo_final)])


# ============================================================
# PREGUNTA 8: VALIDACIÓN CRUZADA EN 5 PLIEGUES
# ============================================================

train_df <- train

formulas <- list(
  Base_m2 = ganancia_final ~ m2_1 + m2_2 + m2_3 + m2_4,
  
  Completo_OLS = ganancia_final ~
    m2_1 + m2_2 + m2_3 + m2_4 +
    c6_3 + c7 + total_trabajadores +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region),
  
  BIC_Parsimonioso = ganancia_final ~
    m2_1 + m2_2 + m2_3 + m2_4 +
    c6_3 + total_trabajadores +
    factor(informalidad),
  
  Transformado_asinh = y_asinh ~
    m2_1 + m2_2 + m2_3 + m2_4 +
    c6_3 + c7 + total_trabajadores +
    factor(informalidad) + factor(c1_b) +
    factor(sexo) + factor(region)
)

res_cv <- list()

for (nom in names(formulas)) {
  
  mae_k <- numeric(5)
  rmse_k <- numeric(5)
  
  for (k in 1:5) {
    
    df_tr_k <- subset(train_df, fold != k)
    df_val_k <- subset(train_df, fold == k)
    
    mod_k <- lm(formulas[[nom]], data = df_tr_k)
    
    pred_val <- predict(mod_k, newdata = df_val_k)
    
    if (nom == "Transformado_asinh") {
      pred_val <- 100000 * sinh(pred_val)
    }
    
    y_val <- df_val_k$ganancia_final
    
    mae_k[k] <- mae_fun(y_val, pred_val)
    rmse_k[k] <- rmse_fun(y_val, pred_val)
  }
  
  res_cv[[nom]] <- data.frame(
    Modelo = nom,
    CV_MAE_CLP = mean(mae_k),
    CV_RMSE_CLP = mean(rmse_k)
  )
}

tabla_8b <- do.call(rbind, res_cv)

cat("\n=== VALIDACIÓN CRUZADA EN 5 PLIEGUES ===\n")
print(tabla_8b)


# ============================================================
# PREGUNTA 9: REGRESIÓN PENALIZADA
# ============================================================

# Utilizar casos completos del entrenamiento

train_q9 <- train[
  complete.cases(train[, variables_modelo]),
]

formula_x <- ~ m2_1 + m2_2 + m2_3 + m2_4 +
  c6_3 + c7 + total_trabajadores +
  informalidad + c1_b + sexo + region

X_train <- model.matrix(
  formula_x,
  data = train_q9
)[, -1, drop = FALSE]

y_train <- train_q9$ganancia_final

grid_lambda <- 10^seq(6, -3, length = 200)

set.seed(42)

# 9.a. Ridge, LASSO y Elastic Net

cv_ridge <- cv.glmnet(
  X_train, y_train,
  alpha = 0,
  lambda = grid_lambda,
  nfolds = 5
)

cv_lasso <- cv.glmnet(
  X_train, y_train,
  alpha = 1,
  lambda = grid_lambda,
  nfolds = 5
)

cv_enet <- cv.glmnet(
  X_train, y_train,
  alpha = 0.5,
  lambda = grid_lambda,
  nfolds = 5
)


# Gráficos de validación cruzada

par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot(cv_ridge, main = "Ridge (alpha = 0)")
plot(cv_lasso, main = "LASSO (alpha = 1)")
plot(cv_enet, main = "Elastic Net (alpha = 0.5)")

par(mfrow = c(1, 1))


# 9.b. Comparación de coeficientes

coef_ridge <- coef(cv_ridge, s = "lambda.min")
coef_lasso <- coef(cv_lasso, s = "lambda.min")
coef_enet <- coef(cv_enet, s = "lambda.min")

tabla_penalizada <- data.frame(
  Variable = c(
    "m2_1 (Sequías)",
    "m2_2 (Inundaciones)",
    "m2_3 (Incendios)",
    "m2_4 (Tormentas)"
  ),
  Ridge = as.vector(
    coef_ridge[c("m2_1", "m2_2", "m2_3", "m2_4"), ]
  ),
  LASSO = as.vector(
    coef_lasso[c("m2_1", "m2_2", "m2_3", "m2_4"), ]
  ),
  Elastic_Net = as.vector(
    coef_enet[c("m2_1", "m2_2", "m2_3", "m2_4"), ]
  )
)

cat("\n=== COEFICIENTES AMBIENTALES PENALIZADOS ===\n")
print(tabla_penalizada)


# Conteo de predictores activos, sin intercepto

cat("\n=== PREDICTORES ACTIVOS ===\n")
cat("Ridge:", sum(coef_ridge[-1, 1] != 0), "/", nrow(coef_ridge) - 1, "\n")
cat("LASSO:", sum(coef_lasso[-1, 1] != 0), "/", nrow(coef_lasso) - 1, "\n")
cat("Elastic Net:", sum(coef_enet[-1, 1] != 0), "/", nrow(coef_enet) - 1, "\n")


# Función auxiliar para graficar trayectorias vs log(lambda) (invertido hacia la derecha)
plot_trayectoria_log <- function(cv_obj, titulo) {
  fit <- cv_obj$glmnet.fit
  log_lam <- log(fit$lambda)
  coefs <- t(as.matrix(fit$beta)) # Filas = lambdas, Columnas = predictores
  
  matplot(
    log_lam, coefs,
    type = "l", lty = 1, lwd = 1.5,
    xlab = expression(Log(lambda)),
    ylab = "Coefficients",
    main = titulo
  )
  abline(h = 0, lty = 3, col = "gray50")
  # Opcional: línea vertical indicando el lambda.min de validación cruzada
  abline(v = log(cv_obj$lambda.min), lty = 2, col = "red")
}

# Generar los 3 paneles lado a lado
par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1.5))

plot_trayectoria_log(cv_ridge, "Ridge")
plot_trayectoria_log(cv_lasso, "Lasso")
plot_trayectoria_log(cv_enet,  "Elastic Net")

par(mfrow = c(1, 1))
par(mfrow = c(1, 1))


# ============================================================
# PREGUNTA 10: EVALUACIÓN FINAL EN TEST
# ============================================================

# Casos completos de prueba y mismos niveles que entrenamiento

test_q10 <- test[
  complete.cases(test[, variables_modelo]),
]

X_test <- model.matrix(
  formula_x,
  data = test_q10
)[, -1, drop = FALSE]

# Asegurar las mismas columnas y orden que entrenamiento

X_test <- X_test[, colnames(X_train), drop = FALSE]

y_test <- test_q10$ganancia_final


# Modelos a comparar

modelos <- list(
  OLS = mod_ols,
  Ridge = cv_ridge,
  LASSO = cv_lasso,
  Elastic_Net = cv_enet,
  Forward = mod_forward,
  Backward = mod_backward,
  Stepwise_AIC = mod_step_aic,
  Stepwise_BIC = mod_step_bic
)

resultados <- lapply(names(modelos), function(nombre) {
  
  modelo <- modelos[[nombre]]
  
  if (nombre %in% c("Ridge", "LASSO", "Elastic_Net")) {
    
    predicciones <- predict(
      modelo,
      newx = X_test,
      s = "lambda.min"
    )
    
  } else {
    
    predicciones <- predict(
      modelo,
      newdata = test_q10
    )
  }
  
  predicciones <- as.numeric(predicciones)
  
  rmse <- sqrt(mean((y_test - predicciones)^2))
  mae <- mean(abs(y_test - predicciones))
  
  r2 <- 1 -
    sum((y_test - predicciones)^2) /
    sum((y_test - mean(y_test))^2)
  
  data.frame(
    Modelo = nombre,
    RMSE = rmse,
    MAE = mae,
    R2 = r2
  )
})

tabla_comparativa <- do.call(rbind, resultados)

tabla_comparativa <- tabla_comparativa[
  order(tabla_comparativa$RMSE),
]

cat("\n=== COMPARACIÓN FINAL DE MODELOS EN TEST ===\n")
print(tabla_comparativa)






# ============================================================
# COMPARACIÓN DE MODELOS: ENTRENAMIENTO VS. PRUEBA
# ============================================================

# Utilizar las mismas observaciones para todos los modelos
y_train <- train_q9$ganancia_final



# Evaluación sobre entrenamiento
resultados_train <- lapply(names(modelos), function(nombre) {
  modelo <- modelos[[nombre]]
  
  if (nombre %in% c("Ridge", "LASSO", "Elastic_Net")) {
    
    pred <- as.numeric(
      predict(modelo, newx = X_train, s = "lambda.min")
    )
    
  } else {
    
    pred <- as.numeric(
      predict(modelo, newdata = train_q9)
    )
  }
  
  data.frame(
    Modelo = nombre,
    RMSE = sqrt(mean((y_train - pred)^2)),
    MAE = mean(abs(y_train - pred)),
    R2 = 1 - sum((y_train - pred)^2) /
      sum((y_train - mean(y_train))^2)
  )
})

tabla_entrenamiento <- do.call(rbind, resultados_train)

# Unir resultados de entrenamiento y prueba
tabla_comparacion_final <- merge(
  tabla_entrenamiento,
  tabla_comparativa,
  by = "Modelo",
  suffixes = c("_Train", "_Test")
)

# Calcular diferencias entre ambos conjuntos
tabla_comparacion_final$Diferencia_RMSE <-
  tabla_comparacion_final$RMSE_Test -
  tabla_comparacion_final$RMSE_Train

tabla_comparacion_final$Diferencia_MAE <-
  tabla_comparacion_final$MAE_Test -
  tabla_comparacion_final$MAE_Train

tabla_comparacion_final$Diferencia_R2 <-
  tabla_comparacion_final$R2_Test -
  tabla_comparacion_final$R2_Train

# Ordenar por RMSE de prueba
tabla_comparacion_final <- tabla_comparacion_final[
  order(tabla_comparacion_final$RMSE_Test),
]

# Mostrar resultados
cat("\n=== RESULTADOS EN ENTRENAMIENTO ===\n")
print(tabla_entrenamiento)

cat("\n=== COMPARACIÓN ENTRENAMIENTO VS. PRUEBA ===\n")
print(tabla_comparacion_final)




# ============================================================
# EDA: PROBLEMA AMBIENTAL
# ============================================================


# Se trabaja con la muestra de entrenamiento
datos_eda <- train

# Etiquetas descriptivas de las amenazas
etiquetas_amenazas <- c(
  m2_1 = "Sequías y heladas",
  m2_2 = "Inundaciones y aluviones",
  m2_3 = "Incendios forestales",
  m2_4 = "Tormentas, lluvias y vientos"
)

# Verificar variables necesarias
variables_eda <- c(
  "ganancia_final",
  "m2_1", "m2_2", "m2_3", "m2_4",
  "region"
)

stopifnot(all(variables_eda %in% names(datos_eda)))


# ============================================================
# a) DISTRIBUCIÓN DE GANANCIAS:
#    NEGOCIOS CON Y SIN AMENAZAS
# ============================================================

# Crear variable que identifica si el negocio presenta
# al menos una de las cuatro amenazas ambientales

datos_eda <- datos_eda %>%
  mutate(
    grupo_amenazas = case_when(
      if_any(c(m2_1, m2_2, m2_3, m2_4), ~ .x == 1) ~ "Con amenazas",
      if_all(c(m2_1, m2_2, m2_3, m2_4), ~ .x == 0) ~ "Sin amenazas",
      TRUE ~ NA_character_
    )
  )

datos_eda$grupo_amenazas <- factor(
  datos_eda$grupo_amenazas,
  levels = c("Sin amenazas", "Con amenazas")
)


# Resumen de ganancias por grupo
resumen_ganancias <- datos_eda %>%
  filter(!is.na(grupo_amenazas), !is.na(ganancia_final)) %>%
  group_by(grupo_amenazas) %>%
  summarise(
    Observaciones = n(),
    Media = mean(ganancia_final),
    Mediana = median(ganancia_final),
    Q1 = quantile(ganancia_final, 0.25),
    Q3 = quantile(ganancia_final, 0.75),
    Minimo = min(ganancia_final),
    Maximo = max(ganancia_final),
    .groups = "drop"
  )

print(resumen_ganancias)


# Boxplot de ganancias según presencia de amenazas
ggplot(
  datos_eda %>%
    filter(!is.na(grupo_amenazas), !is.na(ganancia_final)),
  aes(x = grupo_amenazas, y = ganancia_final,
      fill = grupo_amenazas)
) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.3) +
  labs(
    title = "Distribución de ganancias según presencia de amenazas",
    x = "Presencia de amenazas ambientales",
    y = "Ganancia final"
  ) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  scale_fill_manual(
    values = c("Sin amenazas" = "#56B4E9",
               "Con amenazas" = "#E69F00")
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ============================================================
# b) FRECUENCIAS DE LAS CUATRO AMENAZAS Y CRUCES
#    POR SECTOR ECONÓMICO
# ============================================================

# Comprobar que existe la variable de sector económico.
# En este código se utiliza c1_b como sector.
stopifnot("c1_b" %in% names(datos_eda))

# Transformar las cuatro amenazas a formato largo
frecuencias_amenazas <- datos_eda %>%
  select(m2_1, m2_2, m2_3, m2_4) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Amenaza",
    values_to = "Respuesta"
  ) %>%
  filter(Respuesta == 1) %>%
  count(Amenaza, name = "Casos_Si") %>%
  mutate(
    Amenaza = case_when(
      Amenaza == "m2_1" ~ "Sequías y heladas",
      Amenaza == "m2_2" ~ "Inundaciones y aluviones",
      Amenaza == "m2_3" ~ "Incendios forestales",
      Amenaza == "m2_4" ~ "Tormentas, lluvias y vientos"
    )
  )

print(frecuencias_amenazas)
print(frecuencias_amenazas)



# Gráfico de frecuencias de las amenazas
ggplot(
  frecuencias_amenazas,
  aes(x = reorder(Amenaza, Casos_Si), y = Casos_Si)
) +
  geom_col(fill = "#3B82A0") +
  geom_text(
    aes(label = scales::comma(Casos_Si, big.mark = ".")),
    vjust = -0.4,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Frecuencia de amenazas ambientales",
    x = "Tipo de amenaza",
    y = "Número de negocios que reportan la amenaza"
  ) +
  theme_minimal()

# Cruce de amenazas por sector económico
amenazas_sector <- datos_eda %>%
  select(c1_b, m2_1, m2_2, m2_3, m2_4) %>%
  pivot_longer(
    cols = c(m2_1, m2_2, m2_3, m2_4),
    names_to = "Amenaza",
    values_to = "Respuesta"
  ) %>%
  filter(Respuesta == 1, !is.na(c1_b)) %>%
  mutate(
    Amenaza = case_when(
      Amenaza == "m2_1" ~ "Sequías y heladas",
      Amenaza == "m2_2" ~ "Inundaciones y aluviones",
      Amenaza == "m2_3" ~ "Incendios forestales",
      Amenaza == "m2_4" ~ "Tormentas, lluvias y vientos"
    )
  ) %>%
  count(c1_b, Amenaza, name = "Casos_Si")

print(amenazas_sector)


# Gráfico de amenazas por sector económico
ggplot(
  amenazas_sector,
  aes(x = c1_b, y = Casos_Si, fill = Amenaza)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Amenazas ambientales según sector económico",
    x = "Sector económico",
    y = "Número de negocios",
    fill = "Amenaza"
  ) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )


# ============================================================
# c) VARIACIÓN REGIONAL Y PRECAUCIÓN CAUSAL
# ============================================================

# Resumen regional de ganancias
resumen_regional <- datos_eda %>%
  filter(!is.na(region), !is.na(ganancia_final)) %>%
  group_by(region) %>%
  summarise(
    Observaciones = n(),
    Media = mean(ganancia_final),
    Mediana = median(ganancia_final),
    Q1 = quantile(ganancia_final, 0.25),
    Q3 = quantile(ganancia_final, 0.75),
    Desviacion = sd(ganancia_final),
    .groups = "drop"
  ) %>%
  arrange(desc(Mediana))

print(resumen_regional)


# Boxplot de ganancias por región
ggplot(
  datos_eda %>%
    filter(!is.na(region), !is.na(ganancia_final)),
  aes(x = factor(region), y = ganancia_final)
) +
  geom_boxplot(fill = "#70AD91", alpha = 0.8,
               outlier.alpha = 0.25) +
  labs(
    title = "Distribución de ganancias según región",
    x = "Región",
    y = "Ganancia final"
  ) +
  scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Frecuencia de negocios por región
frecuencia_regional <- datos_eda %>%
  filter(!is.na(region)) %>%
  count(region, name = "Negocios")

print(frecuencia_regional)


# Gráfico de cantidad de negocios por región
ggplot(
  frecuencia_regional,
  aes(x = factor(region), y = Negocios)
) +
  geom_col(fill = "#8C78A8") +
  geom_text(
    aes(label = Negocios),
    vjust = -0.3,
    size = 3
  ) +
  labs(
    title = "Cantidad de negocios por región",
    x = "Región",
    y = "Número de negocios"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




resumen_regional <- datos_eda %>%
  filter(!is.na(region), !is.na(ganancia_final)) %>%
  group_by(region) %>%
  summarise(
    Negocios = n(),
    Media = mean(ganancia_final),
    Mediana = median(ganancia_final),
    Q1 = quantile(ganancia_final, 0.25),
    Q3 = quantile(ganancia_final, 0.75),
    .groups = "drop"
  ) %>%
  arrange(region)

print(resumen_regional)

ggplot(
  datos_eda %>% filter(!is.na(region), !is.na(ganancia_final)),
  aes(x = factor(region), y = ganancia_final)
) +
  geom_boxplot(fill = "#70AD91", outlier.alpha = 0.25) +
  labs(
    title = "Distribución de ganancias según región",
    x = "Región",
    y = "Ganancia mensual (CLP)"
  ) +
  scale_y_continuous(
    labels = scales::label_comma(big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal()
# ============================================================
# FIN DEL EDA: PROBLEMA AMBIENTAL
# ============================================================










sessionInfo()
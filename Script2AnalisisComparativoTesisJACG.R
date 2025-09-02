# ---------------------------------------
# ANÁLISIS DE PANEL COMPARATIVO (ECUADOR vs. LATAM vs. MUNDO)
# ---------------------------------------

# 1. Limpieza y Configuración Inicial
rm(list = ls())
graphics.off()
cat("\014")
setwd("C:/Users/JoseAndresCastro/Desktop/AnalisisEconometrico")

# 2. Cargar Librerías
library(readxl)
library(dplyr)
library(plm)
library(ggplot2)

# 3. Cargar Base de Datos
datos_crudos <- read_excel("2BASEUNEPWDI.xlsx", sheet = "Flujos")

# 4. Preparación de Datos (Transformación a formato ancho)
datos_analisis <- datos_crudos %>%
  dplyr::select(Tipo, Pais_Region_Economia_Mundo, Definicion, Variable, Anio, Valor) %>%
  tidyr::pivot_wider(
    id_cols = c(Tipo, Pais_Region_Economia_Mundo, Anio),
    names_from = Variable,
    values_from = Valor
  )

# 5. Filtrar unidades según Tabla 4.2 (NOMBRES EXACTOS de LAS COLUMNAS)
unidades_seleccionadas <- c(
  # PAÍSES LATINOAMERICANOS (8 países)
  "Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Ecuador", "Paraguay", "Uruguay",
  
  # ECONOMÍAS MUNDIALES (4 países)
  "Alemania", "Estados_Unidos", "Japon", "Reino_Unido",
  
  # REGIÓN INCLUIDA (1 región)
  "America_Latina_y_el_Caribe"
)

# Filtrar SOLO las unidades seleccionadas
datos_panel <- datos_analisis %>%
  filter(Pais_Region_Economia_Mundo %in% unidades_seleccionadas)

# 6. Crear variables transformadas (logaritmos)
datos_panel <- datos_panel %>%
  mutate(
    ln_IMEP = log(IMEP),
    ln_r = log(r),
    ln_EXP = log(EXP)
  )

# 7. Verificación Final
cat("Dimensión del panel:", dim(datos_panel), "\n")
cat("\nUnidades incluidas en el análisis (deben ser 13):\n")
print(unique(datos_panel$Pais_Region_Economia_Mundo))
cat("\nNúmero de unidades:", length(unique(datos_panel$Pais_Region_Economia_Mundo)))
cat("\nAños disponibles:", range(datos_panel$Anio, na.rm = TRUE))

# 8. Crear variable dummy para modelo de eficiencia material
datos_panel <- datos_panel %>%
  mutate(
    dummy_ecuador = if_else(Pais_Region_Economia_Mundo == "Ecuador", 1, 0),
    dummy_latam = if_else(Pais_Region_Economia_Mundo %in% c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Paraguay", "Uruguay"), 1, 0)
  )

# 9. Verificar dummies
cat("\n\nVerificación de variables dummy:\n")
table(datos_panel$dummy_ecuador)
table(datos_panel$dummy_latam)

#ECONOMETRÍA:
# 10. PRUEBAS DE ESTACIONARIEDAD EN PANEL - LEVIN-LIN-CHU (LLC)
# ---------------------------------------

# 10.1 Prueba Levin-Lin-Chu (LLC) para ln_IMEP (variable dependiente principal)
# H0: Todas las series tienen raíz unitaria (no estacionarias)
# H1: Todas las series son estacionarias
llc_test_ln_IMEP <- purtest(ln_IMEP ~ 1, 
                            data = datos_panel, 
                            index = c("Pais_Region_Economia_Mundo", "Anio"), 
                            test = "levinlin")
print("Prueba Levin-Lin-Chu (LLC) para ln_IMEP:")
print(llc_test_ln_IMEP)

# 10.2 Prueba Levin-Lin-Chu (LLC) para IM (variable independiente principal)
llc_test_IM <- purtest(IM ~ 1, 
                       data = datos_panel, 
                       index = c("Pais_Region_Economia_Mundo", "Anio"), 
                       test = "levinlin")
print("Prueba Levin-Lin-Chu (LLC) para IM:")
print(llc_test_IM)

# 10.3 Prueba Levin-Lin-Chu (LLC) para ln_r
llc_test_ln_r <- purtest(ln_r ~ 1, 
                         data = datos_panel, 
                         index = c("Pais_Region_Economia_Mundo", "Anio"), 
                         test = "levinlin")
print("Prueba Levin-Lin-Chu (LLC) para ln_r:")
print(llc_test_ln_r)

# 10.4 Prueba Levin-Lin-Chu (LLC) para DEcap
llc_test_DEcap <- purtest(DEcap ~ 1, 
                          data = datos_panel, 
                          index = c("Pais_Region_Economia_Mundo", "Anio"), 
                          test = "levinlin")
print("Prueba Levin-Lin-Chu (LLC) para DEcap:")
print(llc_test_DEcap)

# 10.5 Prueba Levin-Lin-Chu (LLC) para ln_EXP
llc_test_ln_EXP <- purtest(ln_EXP ~ 1, 
                           data = datos_panel, 
                           index = c("Pais_Region_Economia_Mundo", "Anio"), 
                           test = "levinlin")
print("Prueba Levin-Lin-Chu (LLC) para ln_EXP:")
print(llc_test_ln_EXP)

# 11. PRUEBAS IM-PESARAN-SHIN (IPS) PARA VERIFICACIÓN ROBUSTA
# -----------------------------------------------------------
# 11.1 Prueba IPS para ln_IMEP
ips_test_ln_IMEP <- purtest(ln_IMEP ~ 1, 
                            data = datos_panel, 
                            index = c("Pais_Region_Economia_Mundo", "Anio"), 
                            test = "ips")
print("Prueba Im-Pesaran-Shin (IPS) para ln_IMEP:")
print(ips_test_ln_IMEP)

# 11.2 Prueba IPS para IM
ips_test_IM <- purtest(IM ~ 1, 
                       data = datos_panel, 
                       index = c("Pais_Region_Economia_Mundo", "Anio"), 
                       test = "ips")
print("Prueba Im-Pesaran-Shin (IPS) para IM:")
print(ips_test_IM)

# 11.3 Prueba IPS para DEcap (la variable conflictiva)
ips_test_DEcap <- purtest(DEcap ~ 1, 
                          data = datos_panel, 
                          index = c("Pais_Region_Economia_Mundo", "Anio"), 
                          test = "ips")
print("Prueba Im-Pesaran-Shin (IPS) para DEcap:")
print(ips_test_DEcap)

# 12. PRUEBA DE COINTEGRACIÓN - ENFOQUE DIRECTO (KAO-LIKE)
# ---------------------------------------------------------

# Estimar modelo pooled OLS para obtener residuos
modelo_pooled <- plm(ln_IMEP ~ IM + ln_r + DEcap + ln_EXP,
                     data = datos_panel,
                     index = c("Pais_Region_Economia_Mundo", "Anio"),
                     model = "pooling")

# Obtener residuos del modelo
residuos <- residuals(modelo_pooled)

# Prueba ADF sobre los residuos (enfoque similar a Kao)
adf_residuos <- tseries::adf.test(residuos, k = 0)
print("PRUEBA ADF SOBRE RESIDUOS - APPROACH KAO-LIKE")
print("==============================================")
print(adf_residuos)

# Interpretación
cat("\nINTERPRETACIÓN:")
cat("\n- H0: Los residuos tienen raíz unitaria → NO HAY COINTEGRACIÓN")
cat("\n- H1: Los residuos son estacionarios → HAY COINTEGRACIÓN")
cat("\n- Si p-value < 0.05: Se rechaza H0 → Existe relación de largo plazo\n")

# 14. PRUEBA DE HAUSMAN (EFECTOS FIJOS vs. ALEATORIOS)
# ----------------------------------------------------

# Estimar modelo de efectos fijos (within)
modelo_fe <- plm(ln_IMEP ~ IM + ln_r + DEcap + ln_EXP,
                 data = datos_panel,
                 index = c("Pais_Region_Economia_Mundo", "Anio"), 
                 model = "within")

# Estimar modelo de efectos aleatorios (random)
modelo_re <- plm(ln_IMEP ~ IM + ln_r + DEcap + ln_EXP,
                 data = datos_panel,
                 index = c("Pais_Region_Economia_Mundo", "Anio"),
                 model = "random")

# Realizar prueba de Hausman
hausman_test <- phtest(modelo_fe, modelo_re)
print("PRUEBA DE HAUSMAN - EFECTOS FIJOS vs. ALEATORIOS")
print("================================================")
print(hausman_test)

# Interpretación
cat("\nINTERPRETACIÓN:\n")
cat("- H0: Efectos aleatorios son consistentes (no hay correlación)\n") 
cat("- H1: Efectos fijos son necesarios (hay correlación → endogeneidad)\n")
cat("- Valor-p < 0.05: Se rechaza H0 → USAR EFECTOS FIJOS\n")
cat("- Valor-p > 0.05: No se rechaza H0 → USAR EFECTOS ALEATORIOS\n")

# 15. ESTIMACIÓN DE MODELO DE EFECTOS FIJOS (FE) - SOLUCIÓN DEFINITIVA
# --------------------------------------------------------------------

# OPCIÓN 1: Usar el modelo SIN rezagos (más simple y común en efectos fijos)
cat("MODELO DE EFECTOS FIJOS SIN REZAGOS (RECOMENDADO):\n")
cat("=================================================\n")
summary(modelo_fe_sin_rezagos)

# OPCIÓN 2: Comparar R² ajustado (criterio alternativo)
cat("\nCOMPARACIÓN POR R² AJUSTADO:\n")
cat("============================\n")
cat("Modelo SIN rezagos - R² ajustado:", summary(modelo_fe_sin_rezagos)$r.squared["adjrsq"], "\n")
cat("Modelo CON rezagos - R² ajustado:", summary(modelo_fe_rezago1)$r.squared["adjrsq"], "\n")

# Decisión basada en simplicidad y teoría económica
# (Los modelos de efectos fijos típicamente no incluyen rezagos de las variables dependientes)
modelo_final <- modelo_fe_sin_rezagos

cat("\nDECISIÓN: Se selecciona el modelo SIN rezagos por simplicidad y adecuación teórica.\n")
cat("Los modelos de efectos fijos estándar no suelen incluir rezagos de la variable dependiente.\n")

# Mostrar resultados finales
cat("\nRESULTADOS FINALES DEL MODELO DE EFECTOS FIJOS:\n")
cat("===============================================\n")
summary(modelo_final)

# 16. PRUEBAS DE DIAGNÓSTICO COMPLETAS DEL MODELO DE EFECTOS FIJOS
# ----------------------------------------------------------------

# a) Prueba de Heterocedasticidad (Modified Wald test para efectos fijos)
wald_test <- plm::pwaldtest(modelo_final, test = "Chisq")
print("16.1 PRUEBA DE HETEROCEDASTICIDAD (Modified Wald test)")
print("======================================================")
print(wald_test)

# b) Prueba de Heterocedasticidad (Breusch-Pagan - alternativa)
bp_test <- bptest(modelo_final)
print("16.2 PRUEBA DE HETEROCEDASTICIDAD (Breusch-Pagan)")
print("=================================================")
print(bp_test)

# c) Prueba de Autocorrelación Serial (Breusch-Godfrey)
bg_test <- pbgtest(modelo_final)
print("16.3 PRUEBA DE AUTOCORRELACIÓN (Breusch-Godfrey)")
print("================================================")
print(bg_test)

# d) Prueba de Normalidad (Jarque-Bera) - CORREGIDO
library(tseries)
residuos <- residuals(modelo_final)
jb_test <- jarque.bera.test(residuos)
print("16.4 PRUEBA DE NORMALIDAD (Jarque-Bera)")
print("=======================================")
print(jb_test)

# e) Mostrar residuales para inspección visual
cat("\n16.5 ESTADÍSTICOS DESCRIPTIVOS DE RESIDUALES:\n")
cat("============================================\n")
print(summary(residuos))
cat("Varianza de residuales:", var(residuos), "\n")

# 17. ESTIMACIÓN CON ERRORES ESTÁNDAR ROBUSTOS
# --------------------------------------------

# Instalar y cargar paquete para errores robustos
if (!require(clubSandwich)) install.packages("clubSandwich")
library(clubSandwich)

# Re-estimar modelo de efectos fijos con errores robustos (CRVE)
modelo_robusto <- plm(ln_IMEP ~ IM + ln_r + DEcap + ln_EXP,
                      data = datos_panel,
                      index = c("Pais_Region_Economia_Mundo", "Anio"),
                      model = "within")

# Calcular errores estándar robustos (método CRVE)
coef_test <- coeftest(modelo_robusto, vcov = vcovCR(modelo_robusto, type = "CR1S"))
print("17. MODELO CON ERRORES ESTÁNDAR ROBUSTOS (CRVE)")
print("==============================================")
print(coef_test)

# Mostrar resumen completo con errores robustos
cat("\nRESUMEN COMPLETO CON ERRORES ROBUSTOS:\n")
cat("======================================\n")
summary(modelo_robusto)
cat("\nERRORES ESTÁNDAR ROBUSTOS APLICADOS\n")

# Comparar errores estándar originales vs robustos
cat("\nCOMPARACIÓN ERRORES ESTÁNDAR:\n")
cat("=============================\n")
cat("Variable    | Coeficiente | Error Estándar Original | Error Estándar Robusto\n")
cat("----------------------------------------------------------------------------\n")
cat(sprintf("IM         | %9.4f  | %19.4f  | %21.4f\n", 
            coef(modelo_robusto)["IM"], 
            summary(modelo_robusto)$coefficients["IM", 2],
            coef_test["IM", 2]))
cat(sprintf("ln_r       | %9.4f  | %19.4f  | %21.4f\n",
            coef(modelo_robusto)["ln_r"],
            summary(modelo_robusto)$coefficients["ln_r", 2], 
            coef_test["ln_r", 2]))
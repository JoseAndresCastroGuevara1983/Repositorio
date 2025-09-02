#LIMPIEZA CONSOLA
rm(list = ls())
#Cerrar graficas
graphics.off()
#Limpieza total consola
cat("\014")

#CONFIGURACIÓN DIRECTORIO
setwd("C:/Users/JoseAndresCastro/Desktop/AnalisisEconometrico")

#Verificación
getwd()

#INSTALAR PAQUETES
install.packages(c("readxl", "dplyr", "ggplot2", "tseries", "urca", "vars", "lmtest", "plm", "car"))

#Cargar librerías (obligatorio cada vez que abro R)
library(readxl)    # Para leer Excel
library(dplyr)     # Para manipular datos
library(ggplot2)   # Para gráficos
library(tseries)   # Para pruebas ADF, PP, KPSS
library(urca)      # Para prueba de Johansen
library(vars)      # Para modelos VAR/VECM
library(lmtest)    # Para pruebas de diagnóstico
library(plm)       # Para datos de panel
library(car)       # Para pruebas de normalidad

# Lista todos los archivos en tu carpeta de trabajo
list.files()

#CARGAR BASE Y PESTAÑAS
datos_crudos <- read_excel("2BASEUNEPWDI.xlsx", sheet = "Flujos")

# Ver las primeras filas de los datos cargados
head(datos_crudos)

#Ver la estructura de los datos (nombres de columnas, tipos de datos)
str(datos_crudos)

#Ver los años únicos para confirmar el rango (1970-2021)
unique(datos_crudos$Anio)

#TRANSFORMAR DATOS DE LARGO A ANCHO
datos_analisis <- datos_crudos %>%
  select(Tipo, Pais_Region_Economia_Mundo, Definicion, Variable, Anio, Valor) %>%
  tidyr::pivot_wider(
    id_cols = c(Tipo, Pais_Region_Economia_Mundo, Anio), # Las columnas que identifican cada observación
    names_from = Variable,    # Los nombres de las variables vendrán de esta columna
    values_from = Valor       # Los valores numéricos vendrán de esta columna
  )

#Forzar el uso de la función select de dplyr usando ::
datos_analisis <- datos_crudos %>%
  dplyr::select(Tipo, Pais_Region_Economia_Mundo, Definicion, Variable, Anio, Valor) %>%
  tidyr::pivot_wider(
    id_cols = c(Tipo, Pais_Region_Economia_Mundo, Anio),
    names_from = Variable,
    values_from = Valor
  )

#Verificación
head(datos_analisis)

#FILTRAR ECUADOR
datos_ecuador <- datos_analisis %>%
  filter(Pais_Region_Economia_Mundo == "Ecuador", Anio >= 1970 & Anio <= 2021)

#Verificar Ecuador
head(datos_ecuador)
View(datos_ecuador)

# CREAR VARIABLES EN (ln) PARA ECUADOR
datos_ecuador <- datos_ecuador %>%
  mutate(
    ln_IMEP = log(IMEP),
    ln_r = log(r),
    ln_EXP = log(EXP)
  )

# Verificar que las nuevas variables se crearon
select(datos_ecuador, Anio, IMEP, ln_IMEP, r, ln_r, EXP, ln_EXP) %>%
  head()

# Verificar que las nuevas variables se crearon (usando dplyr::select)
dplyr::select(datos_ecuador, Anio, IMEP, ln_IMEP, r, ln_r, EXP, ln_EXP) %>%
  head()

#ECONOMETRÍA

#PRUEBA AUMENTADA DICKEY FULLER (ADF)
#Ejecutar Prueba Aumentada de Dickey-Fuller (ADF) para la variable (ln_IMEP) de Ecuador, 
#que es la variable dependiente en la hipótesis principal.

#Prueba ADF para ln_IMEP (con intercepto y tendencia)
adf_ln_IMEP <- tseries::adf.test(datos_ecuador$ln_IMEP, k = 0)

# Mostrar resultados
print(adf_ln_IMEP)

#PRUEBA AUMENTADA DE DICKEY-FULLER (ADF) para las variables independientes de Ecuador, 
#de la hipótesis principal.

# Prueba ADF para IM (Intensidad Material)
adf_IM <- tseries::adf.test(datos_ecuador$IM, k = 0)
print(adf_IM)

# Prueba ADF para ln_r (Porcentaje de Exportaciones Primarias en log)
adf_ln_r <- tseries::adf.test(datos_ecuador$ln_r, k = 0)
print(adf_ln_r)

# Prueba ADF para DEcap (Extracción Doméstica per cápita)
adf_DEcap <- tseries::adf.test(datos_ecuador$DEcap, k = 0)
print(adf_DEcap)

# Prueba ADF para ln_EXP (Exportaciones en log)
adf_ln_EXP <- tseries::adf.test(datos_ecuador$ln_EXP, k = 0)
print(adf_ln_EXP)

#Confirmación mediante pruebas PP y KPSS
# Prueba Phillips-Perron (PP) para ln_r
pp_ln_r <- tseries::pp.test(datos_ecuador$ln_r)
print(pp_ln_r)

#PRUEBA KPSS para ln_r (Ho: Estacionaria)
kpss_ln_r <- tseries::kpss.test(datos_ecuador$ln_r)
print(kpss_ln_r)

# Preparar las variables para la prueba de Johansen
# Seleccionamos las variables de la hipótesis principal: ln_IMEP, IM, ln_r, DEcap, ln_EXP
var_coint <- datos_ecuador %>% 
  dplyr::select(ln_IMEP, IM, ln_r, DEcap, ln_EXP) 

#PRUEBA DE JOHANSEN (con intercepto en la relación de cointegración y tendencia en los datos)
johansen_test <- urca::ca.jo(var_coint, type = "trace", ecdet = "const", K = 2)
summary(johansen_test)

# Seleccionar variables para el modelo ORDENADAS SEGÚN LA HIPÓTESIS
# Orden: Variable Dependiente (ln_IMEP) y luego las Independientes (IM, ln_r, DEcap, ln_EXP)
var_model <- datos_ecuador %>% 
  dplyr::select(ln_IMEP, IM, ln_r, DEcap, ln_EXP)

# SELECCIÓN DE REZAGOS (p) usando AIC y BIC
lag_selection <- vars::VARselect(var_model, type = "const", lag.max = 4)
cat("Número óptimo de rezagos según cada criterio:\n")
print(lag_selection$selection)

cat("\nValores de los criterios de información (a minimizar):\n")
print(lag_selection$criteria)

#ESTIMAR MODELO VECM
# r = 1 (un vector de cointegración, según Johansen)
# lag = p - 1 = 0 (porque en un VECM, el número de rezagos de corto plazo es 'p - 1')
modelo_vecm <- urca::cajorls(johansen_test, r = 1)

# Ver los resultados completos del modelo
summary(modelo_vecm)

# Estimar el Modelo VECM y obtener resultados detallados
modelo_vecm <- urca::cajorls(johansen_test, r = 1)

# Extraer y mostrar los resultados en formato de regresión
resultados_vecm <- modelo_vecm$rlm
summary(resultados_vecm)

# Mostrar el vector de cointegración (relación de largo plazo)
cat("\n\n--- VECTOR DE COINTEGRACIÓN (Relación de Largo Plazo) ---\n")
print(modelo_vecm$beta)

# Opcional: Mostrar ecuaciones individuales para mayor claridad
cat("\n\n--- ECUACIÓN DE CORTO PLAZO para ln_IMEP ---\n")
summary(resultados_vecm$ln_IMEP.d)

# Re-estimar VECM CORRECTAMENTE con 2 rezagos (p = 2)
# K = p - 1 = 1
johansen_test_p2_correcto <- urca::ca.jo(var_model, type = "trace", ecdet = "const", K = 1)
modelo_vecm_p2_correcto <- urca::cajorls(johansen_test_p2_correcto, r = 1)
summary(modelo_vecm_p2_correcto$rlm)

#EL PAQUETE URCA ES MUY RESTRICTIVO, HAY QUE REESTIMAR CORRECTAMENTE
# Estimar VECM directamente con el paquete VAR
# r = 1 (un vector de cointegración)
# lag = 2 (número de rezagos)
modelo_vecm_final <- vars::vec2var(johansen_test, r = 1)

# Resumen del modelo
summary(modelo_vecm_final)

# Extraer coeficientes específicos para ln_IMEP
coefs_ln_IMEP <- modelo_vecm_final$deterministic$ln_IMEP
print(coefs_ln_IMEP)

#VEC, no funcionó se ejecuta VAR
# Crear dataset con variables en diferencias donde sea necesario
var_model_final <- datos_ecuador %>%
  dplyr::mutate(
    d_ln_r = c(NA, diff(ln_r)),    # Primera diferencia de ln_r (I(1))
    d_ln_EXP = c(NA, diff(ln_EXP)) # Primera diferencia de ln_EXP
  ) %>%
  dplyr::select(ln_IMEP, IM, d_ln_r, DEcap, ln_EXP) %>%  
  na.omit()  # Eliminar NA's de las diferencias

# Verificar las primeras filas
head(var_model_final)

#ESTIMACIÓN MODELO VAR
# Estimar VAR con 2 rezagos (p = 2 según AIC) y constante
var_result <- vars::VAR(var_model_final, p = 2, type = "const")

# Ver resumen completo del modelo
summary(var_result)

#PRUEBA DE CAUSALIDAD DE GRANGER
# ¿La Intensidad Material (IM) causa Granger al Ingreso por Exportaciones Primarias (ln_IMEP)?
causalidad_IM <- causality(var_result, cause = "IM")
print(causalidad_IM$Granger)

# ¿El porcentaje de exportaciones primarias (d_ln_r) causa Granger a ln_IMEP?
causalidad_ln_r <- causality(var_result, cause = "d_ln_r")
print(causalidad_ln_r$Granger)

# ¿La extracción doméstica per cápita (DEcap) causa Granger a ln_IMEP?
causalidad_DEcap <- causality(var_result, cause = "DEcap")
print(causalidad_DEcap$Granger)

#PRUEBAS DE DIAGNOSTICO
# 1. Autocorrelación (Lagrange Multiplier) - H0: No autocorrelación
serial_test <- serial.test(var_result, lags.pt = 2, type = "PT.asymptotic")
print(serial_test)

# 2. Homocedasticidad (Prueba White-like) - H0: Homocedasticidad
arch_test <- arch.test(var_result, lags.multi = 2)
print(arch_test)

# 3. Normalidad (Jarque-Bera) - H0: Normalidad
normality_test <- normality.test(var_result)
print(normality_test)

# Verificar que todas las raíces inversas estén dentro del círculo unitario
roots <- roots(var_result)
print(roots)
# Si el máximo módulo es < 1, el modelo es estable

#PRUEBA DE ROBUSTEZ
# Estimar VAR con 1 rezago (según BIC) para comparar
var_result_robust <- vars::VAR(var_model_final, p = 1, type = "const")
summary(var_result_robust)

# Comparar causalidad de Granger con esta nueva especificación
causality(var_result_robust, cause = "IM")$Granger
# https://bookdown.org/gboccardo/manual-ED-UCH/estadistica-descriptiva-con-rstudio.html

library(readxl)
Base_Eviews_ <- read_excel("H:/Software Shop/Presentaci髇/Series/Base Eviews .xls")
View(Base_Eviews_)

Base_Eviews_

## Media
mean(Base_Eviews_$income_growth, na.rm = TRUE)

## Mediana

median(Base_Eviews_$income_growth, na.rm = TRUE)

## MOda

library(modeest)


##mfv(Base_Eviews_$year, na_rm = TRUE)

#Frecuencias Relativas

tabla <- table(Base_Eviews_$income_growth)
tabla
prop.table(tabla)
prop.table(tabla)*1000
round((prop.table(tabla)*1000),2)


## Frecuencias Absolutas Acumuladas

cumsum(tabla)

## Frecuencias Relativas Acumuladas

cumsum(prop.table(tabla))

## Porcentaje Acumulado Redondeado a Dos Decimales

round(cumsum(prop.table(tabla)*100),2)


## Quantiles

quantile(Base_Eviews_$income_growth, prob = c(0.25, 0.5, 0.75), na.rm = TRUE)


## Medidas de dispersi贸n: rango, varianza, desviaci贸n est谩ndar y coeficiente de variaci贸n

## RAngo
range(Base_Eviews_$income_growth, na.rm = TRUE)

## Valor Minimo Maximo

min(Base_Eviews_$income_growth, na.rm = TRUE)
max(Base_Eviews_$income_growth, na.rm = TRUE)

max(Base_Eviews_$income_growth, na.rm = TRUE) + min(Base_Eviews_$income_growth, na.rm = TRUE)

## Varianza

var(Base_Eviews_$income_growth, na.rm = TRUE)

## Desviaci贸n

sd(Base_Eviews_$income_growth, na.rm = TRUE)


## Algo As铆 Como Coeficiente de Variaci贸n

sd(Base_Eviews_$income_growth, na.rm = TRUE)/mean(Base_Eviews_$income_growth, na.rm = TRUE)


## Forma de una distribuci贸n: simetr铆a, curtosis y normalidad

library(mnomrt)
library(psych)

## Simetr铆a
skew(Base_Eviews_$income_growth)

## KUrtosis
kurtosi(Base_Eviews_$income_growth)

## Coeficiente de Simetr铆a

skew(Base_Eviews_$income_growth) - kurtosi(Base_Eviews_$income_growth)

## Pueba de Shapiro Wilk ( muestras perque帽as)

shapiro.test(Base_Eviews_$income_growth)

## Prueba de Kolmogorov Smirnov (muestras grandes)

ks.test(Base_Eviews_$income_growth, "pnorm", 
        mean(Base_Eviews_$exports_growth, na.rm = T ),sd(Base_Eviews_$income_growth, na.rm = T))


## Tablas para informes e interpretaci贸n de resultados

## Distribuciones de frecuencias

f <- table(Base_Eviews_$income_growth)
f
f_porc <- round((prop.table(tabla)*100),2)
f_porc
f_porc_acum <- round(cumsum(prop.table(tabla)*100),2)
f_porc_acum


## Exportar a CSV

write.csv2(f, file = "Tabla1.csv")
write.csv2(f_porc, file = "Tabla 2.csv")
write.csv2(f_porc_acum, file = "Tabla 3.csv")


## Estadisticas Descriptivas


summary(Base_Eviews_$income_growth)

des <- summary(Base_Eviews_$income_growth)
names(des)
as.numeric(des)

des_income_growth <- as.data.frame((rbind(names(des), as.numeric(des))))
View(des_income_growth)

write.csv2(des_income_growth, file = "Tabla 4.csv")

library(readxl)
Base_Eviews_ <- read_excel("H:/Software Shop/Presentaci髇/Series/Base Eviews .xls")
View(Base_Eviews_)

condicion1 <- sample(c(""), size = 253, replace = TRUE) ## Cear una nueva columna

Base_Eviews_ <- cbind(Base_Eviews_, condicion1)

Base_Eviews_$condicion1 <- ifelse(Base_Eviews_$income_growth >= 1, "DUPLICADO","UNICO")

  
Base_Eviews_$condicion  <- if(Base_Eviews_$income_growth >= 0 & <= 1) {
  print("Duplicado")
} else {
  print("Unico")
}


Base_Eviews_$condicion1

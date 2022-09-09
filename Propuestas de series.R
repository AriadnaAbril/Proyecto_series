library(TSstudio)
library(readr)

#Serie de periodo mensual

PM10PM <- read_csv("OAB-PM10PM.csv")
contaminacion<-PM10PM$Valor
aire=ts(contaminacion,start=c(2006,12),frequency=12)
aire
TSstudio::ts_plot(aire,title="",slider=TRUE)

summary(fit <- lm(aire~time(aire), na.action=NULL))
plot(aire, main ="Concentración de Material Particulado Inferior a 10 Micras")
abline(fit,col = "red") 
ElimiTendaire=aire-predict(fit)
plot(ElimiTendaire,main="Serie concentración sin tendencia")

plot(decompose(aire))

library(feasts)
library(fable)

tsibble_aire<-as_tsibble(aire)
str(tsibble_aire)
tsibble_aire %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()

#Serie de periodo diario venta platino
library(readxl)
metales_2018 <- read_excel("metales 2018.xlsx", range = "A11:G376") 
metales_2019 <- read_excel("metales 2019.xlsx", range = "A11:G376")
metales_2020 <- read_excel("metales 2020.xlsx", range = "A11:G376")
metales_2021 <- read_excel("metales 2021.xlsx", range = "A11:G376")
metales_2022 <- read_excel("metales 2022.xlsx", range = "A11:G262")


#Desde el primero de mayo de 2018

metales <- rbind(metales_2018[121:365,],metales_2019,metales_2020,metales_2021,metales_2022)
names(metales) <- c("Fecha","compra_oro","compra_plata","compra_platino","venta_oro","venta_plata","venta_platino")

venta_platino <- as.ts(metales$venta_platino)
ts_info(venta_platino)

plot(venta_platino)

summary(fit <- lm(venta_platino~time(venta_platino), na.action=NULL))
plot(venta_platino, main ="Precio de venta del platino en Colombia")
abline(fit,col = "red") 
ElimiTendplati=venta_platino-predict(fit)
plot(ElimiTendplati,main="Serie venta del platino sin tendencia")


tsibble_plati<-as_tsibble(venta_platino)
str(tsibble_plati)
tsibble_plati %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


library(xts)
Tssale_platino=xts(metales$venta_platino, order.by = as.Date(metales$Fecha, "%Y-%m-%d"))
plot(Tssale_platino)

# acciones Ecopetrol

Datos2 <- read_csv("Datos históricos ECO.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))
Apertura<-Datos2$Apertura
Aper<-as.ts(Apertura)
plot(Aper)
ts_info(Aper)

summary(fit <- lm(Aper~time(Aper), na.action=NULL))
abline(fit,col = "red") 
ElimiTendAper=Aper-predict(fit)
plot(ElimiTendAper,main="Serie Apertura Sin tendencia")


tsibble_aper<-as_tsibble(Aper)
str(tsibble_aper)
tsibble_aper %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


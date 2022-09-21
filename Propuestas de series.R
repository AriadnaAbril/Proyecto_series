library(TSstudio)
library(readr)

#Serie de periodo mensual ####################################################

PM10PM <- read_csv("OAB-PM10PM.csv")
contaminacion<-PM10PM$Valor
aire=ts(contaminacion,start=c(2006,12),frequency=12)
aire
TSstudio::ts_plot(aire,title="",slider=TRUE)

# Ajuste de tendencia lineal
summary(fit <- lm(aire~time(aire), na.action=NULL))
plot(aire, main ="Concentración de Material Particulado Inferior a 10 Micras")
abline(fit,col = "red") 
ElimiTendaire=aire-predict(fit)
plot(ElimiTendaire,main="Serie concentración sin tendencia")

# loess tendencia
plot(decompose(aire))

library(tidyverse)
library(lubridate)
library(timetk)
library(tsibble)

# Se configura para gráficas plotly(# FALSE retorna ggplots y no plotly)
interactive <- FALSE
indice_aire=as.Date(as.yearmon(tk_index(aire)))
## Otra forma de extraer el indice estimetk::tk_index(chicken)
df_aire=data.frame(Fecha=indice_aire,Concentracion=as.matrix(aire))
str(df_aire)
tibble_aire=tibble(df_aire)
duplicates(tibble_aire, key = NULL, index=Fecha)   ##Mirar si hay registros duplicados

tibble_aire%>%timetk::plot_time_series(Fecha, Concentracion, 
                                          .interactive = interactive,
                                          .plotly_slider = TRUE)
###Usa Loess para hacer el ajuste de la tendencia, es decir usar smooth_vec() como versión simplificada de stats::loess()
tibble_aire%>%mutate(Concentracion_ajus1=smooth_vec(Concentracion,span = 0.75, degree = 2))#%>%
  ggplot(aes(Fecha, Concentracion)) +
  geom_line() +
  geom_line(aes(y = Concentracion_ajus1), color = "green")


tibble_aire%>%mutate(Concentracion_ajus2=smooth_vec(Concentracion,span = 0.5, degree = 1))%>%
  ggplot(aes(Fecha, Concentracion)) +
  geom_line() +
  geom_line(aes(y = Concentracion_ajus2), color = "red")

tabla <- tibble_aire%>%mutate(Concentracion_ajus1=smooth_vec(Concentracion,span = 0.5, degree = 2))%>%
  mutate(resta_loess=Concentracion-Concentracion_ajus1)  

# eliminar tendencia
aire_diff <- diff(aire)
plot(aire_diff)


#Gráficos ACF
par(mar = c(3,2,3,2))
par(mfrow=c(4,1)) # plot ACFs
acf(aire, main="ACF Concentración")
acf(resid(fit), main="ACF Sin tendencia") 
acf(diff(aire), main="ACF Primera Diferencia")
acf(tabla$resta_loess, main="ACF loess")

#Box cox
library(forecast)
forecast::BoxCox.lambda(aire, method = "guerrero", lower = -1, upper = 3) ###Me entrega el valor de lambda 
plot(forecast::BoxCox(aire, lambda=1.135019))
plot(timetk::box_cox_vec(aire,lambda = 'auto',silent = F))

# Periodograma
par(mfrow=c(2,2))
spectrum(aire, log="no")
spectrum(aire_diff, log="no")
spectrum(resid(fit), log="no")
spectrum(tabla$resta_loess, log="no")

# Gráfico bonito
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


#### acciones Ecopetrol #######################################################
library(readxl)
Datos_históricos_ECO <- read_excel("Datos históricos ECO.xlsx")
Apertura<-Datos_históricos_ECO$Apertura
Aper<-as.ts(Apertura)
plot(Aper)
ts_info(Aper)

#Box cox
library(forecast)
forecast::BoxCox.lambda(Aper, method = "guerrero", lower = -1, upper = 3) ###Me entrega el valor de lambda 
plot(forecast::BoxCox(Aper, lambda=1.110104))
plot(timetk::box_cox_vec(Aper,lambda = 'auto',silent = F))

# tendencia lineal MALA
summary(fit <- lm(Aper~time(Aper), na.action=NULL))
plot(Aper)
abline(fit,col = "red") 
ElimiTendAper=Aper-predict(fit)
plot(ElimiTendAper,main="Serie Apertura Sin tendencia")

# eliminar tendencia
Aper_diff <- diff(Aper, differences = 2)
plot(Aper_diff)

# Tendencia loess
library(tidyverse)
library(lubridate)
library(timetk)
library(tsibble)

# Se configura para gráficas plotly(# FALSE retorna ggplots y no plotly)
interactive <- FALSE
indice_Aper=as.Date(as.yearmon(tk_index(Aper)))
## Otra forma de extraer el indice estimetk::tk_index(chicken)
df_Aper=data.frame(Fecha=indice_Aper,Precio=as.matrix(Aper))
str(df_Aper)
tibble_Aper=tibble(df_Aper)
duplicates(tibble_Aper, key = NULL, index=Fecha)   ##Mirar si hay registros duplicados

tibble_Aper%>%timetk::plot_time_series(Fecha, Precio, 
                                       .interactive = interactive,
                                       .plotly_slider = TRUE)
###Usa Loess para hacer el ajuste de la tendencia, es decir usar smooth_vec() como versión simplificada de stats::loess()
tibble_Aper%>%mutate(Precio_ajus1=smooth_vec(Precio,span = 0.2, degree = 2))%>%
ggplot(aes(Fecha, Precio)) +
  geom_line() +
  geom_line(aes(y = Precio_ajus1), color = "green")

tibble_Aper%>%mutate(Precio_ajus2=smooth_vec(Precio,span = 0.15, degree = 1))%>%
  ggplot(aes(Fecha, Precio)) +
  geom_line() +
  geom_line(aes(y = Precio_ajus2), color = "red")

tabla1 <- tibble_Aper%>%mutate(Precio_ajus1=smooth_vec(Precio,span = 0.2, degree = 2))%>%
  mutate(resta_loess=Precio-Precio_ajus1)  

#Gráficos ACF
par(mar = c(3,2,3,2))
par(mfrow=c(3,1)) # plot ACFs
acf(Aper, main="ACF Precio Ecopetrol")
acf(diff(Aper, differences = 2), main="ACF Segunda Diferencia")
acf(tabla1$resta_loess, main="ACF loess")

# Periodograma
par(mfrow=c(2,1))
spectrum(Aper_diff, log="no")
spectrum(tabla1$resta_loess, log="no")

# Descomnposición STL
tsibble_aper<-as_tsibble(Aper)
str(tsibble_aper)
tsibble_aper %>%
  model(
    STL(value ~ trend() +
          season(window = "periodic"),
        robust = TRUE)) %>%
  components() %>%
  autoplot()


# Gráfico Bonito
library(xts)
Ts_aperEco=xts(Datos_históricos_ECO$Apertura, order.by = as.Date(Datos_históricos_ECO$Fecha, "%Y-%m-%d"))
plot(Ts_aperEco)


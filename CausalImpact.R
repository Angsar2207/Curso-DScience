#1.1 CAUSAL IMPACT PARA 3.4.4 
#CON MODELO STRUCTURAL TIME SERIES

install.packages("bsts")
install.packages("devtools")
install.packages("xts")
install.packages("DBI")
install.packages("Rcpp")
install.packages("magrittr")
install.packages("tibble")
install.packages("gtable")
install.packages("scales")
devtools::install_github("google/CausalImpact", force = TRUE)
install.packages("CausalImpact")


library(devtools)
library("bsts")
library("xts")
library("DBI")
library("Rcpp")
library("magrittr")
library("tibble")
library("gtable")
library("scales")
library(CausalImpact)
library(ggplot2)


#EJEMPLO DE APLICACION
#Creando matriz de datos
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)
y[71:100] <- y[71:100] + 10
data <- cbind(y, x1)

dim(data)
head(data)
matplot(data, type = "l")


pre.period <- c(1, 70)
post.period <- c(71, 100)

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact)

#1.2 CAUSAL IMPACT PARA 3.4.4 
#CON MODELO PERSONALIZADO UTILIZANDO BSTS

#Antes de construir un modelo personalizado, establecemos los datos observados en el período posterior al tratamiento en NA, 
#lo que refleja el hecho de que la respuesta contrafactual no se observa después de la intervención. 
#Mantenemos una copia de la respuesta real observada en la variable post.period.response.

post.period <- c(71, 100)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA

#A continuación, configuramos y estimamos un modelo de serie temporal utilizando el paquete bsts.

ss <- AddLocalLevel(list(), y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)

#Finalmente, llamamos CausalImpact(). En lugar de proporcionar datos de entrada, simplemente pasamos el objeto de modelo ajustado(bsts.model).
#También debemos proporcionar la respuesta observada real. 
#Esto es necesario para que el paquete pueda calcular la diferencia entre la respuesta predicha (almacenada en  bsts.model) 
#y la respuesta observada real (almacenada en post.period.response).

impact <- CausalImpact(bsts.model = bsts.model,
                       post.period.response = post.period.response)
plot(impact)
summary(impact)
summary(impact, "report")

#1.3 PERSONALIZANDO LOS GRAFICOS
#1.3.1 La plot()función para CausalImpactobjetos devuelve un objeto ggplot2. 
#Esto significa que podemos personalizar la trama utilizando las funciones estándar de ggplot2. 
#Por ejemplo, para aumentar el tamaño de fuente, podemos hacer:

impact.plot <- plot(impact) + theme_bw(base_size = 20)
plot(impact.plot)

#1.3.2 ¿Cómo puedo obtener intervalos de 90% en lugar de intervalos de 95%?
#El tamaño de los intervalos se especifica mediante el argumento alpha, que por defecto es 0.05. 
#Para obtener intervalos de 90% en su lugar, usaríamos:

impact <- CausalImpact(data, pre.period, post.period, alpha = 0.1)

#1.3.3¿Qué variables predictoras se utilizaron en el modelo?
#los análisis pueden contener fácilmente decenas o cientos de predictores potenciales (es decir, columnas en el argumento 
#de la función de datos). ¿Cuáles de estos fueron informativos? 
#Podemos trazar la probabilidad posterior de que cada predictor se incluya en el modelo usando:
  
plot(impact$model$bsts.model, "coefficients")

#1.3.4 MODEL ARGS. AJUSTE FINO DEL MODELO

#NITER: Number of MCMC samples to draw. More samples lead to more accurate inferences. Defaults to 1000.
#NSEASONS:Period of the seasonal components. In order to include a seasonal component, set this to a whole number greater than 1. 
#For example, if the data represent daily observations, use 7 for a day-of-week component. 
#This interface currently only supports up to one seasonal component. To specify multiple seasonal components, 
#use bsts to specify the model directly, then pass the fitted model in as bsts.model. 
#Defaults to 1, which means no seasonal component is used.
#SEASON.DURATION: Duration of each season, i.e., number of data points each season spans. 
#Defaults to 1. For example, to add a day-of-week component to data with daily granularity, 
#use  model.args = list(nseasons = 7, season.duration = 1). To add a day-of-week component to data with hourly granularity, 
#set model.args = list(nseasons = 7, season.duration = 24).
#DYNAMIC REGRESSION:Whether to include time-varying regression coefficients. 
#In combination with a time-varying local trend or even a time-varying local level, this often leads to overspecification, 
#in which case a static regression is safer. Defaults to FALSE.
#STANDARDIZE.DATA: standardize.data: Whether to standardize all columns of the data before fitting the model. 
#This is equivalent to an empirical Bayes approach to setting the priors. It ensures that results are invariant to linear 
#transformations of the data. Defaults to TRUE.
#PRIOR.LEVEL.SD: prior.level.sd Prior standard deviation of the Gaussian random walk of the local level. 
#Expressed in terms of data standard deviations. Defaults to 0.01, a typical choice for well-behaved and stable datasets 
#with low residual volatility after regressing out known predictors (e.g., web searches or sales in high quantities). 
#When in doubt, a safer option is to use 0.1, as validated on synthetic data, although this may sometimes give rise 
#to unrealistically wide prediction intervals.

impact <- CausalImpact(data, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7 , season.duration = 2))

plot(impact)
summary(impact, "report")
impact$summary

#1.4 CAUSAL IMPACT PARA 3.4.4 
#CON MODELO STRUCTURAL TIME SERIES
#Trabajando con series temporales

time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), time.points)
head(data)

pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)
summary(impact, "report")
impact$summary

#EJEMPLO REAL ATRAPALO

#Importamos los datos
data <- read.csv("D:/DATAR/PVVMH_MODEL1.csv", header = TRUE, sep = ";")
#data[3] selecciono datos columna 3
head(data)

glimpse(data)

par(cex = 1.3)
matplot(data, type = "l", lwd = 3)

#dibujar una línea en el inicio del periodo de impacto
abline(v=250)

pre.period <- c(1, 250)
post.period <- c(251, 1400)

impact <- CausalImpact(data, pre.period, post.period)
plot(impact)

print(impact)
summary(impact)

#Trabajando con series temporales

#Importamos los datos
data <- read.csv("D:/DATAR/PVVMH_MODEL1.csv", header = TRUE, sep = ";")
#data[3] selecciono datos columna 3
head(data)

time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data1 <- zoo(cbind(data), time.points)
head(data1)

pre.period <- as.Date(c("2014-01-01", "2014-12-31"))
post.period <- as.Date(c("2015-01-01", "2015-08-31"))

impact <- CausalImpact(data1, pre.period, post.period, model.args = list(niter = 5000, nseasons = 7))

plot(impact)
summary(impact, "report")
impact$summary





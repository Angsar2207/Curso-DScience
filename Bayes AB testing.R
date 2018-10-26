#POWER TEST PARA DETERMINAR TAMAÑO MUESTRAL

#install.packages("pwr")

library(pwr)

#1.- POWER TEST PARA BINOMIAL 

#Determinar Power y tamaño de muestra para un test de 2 proporciones
#sample size n = 174 en cada grupo
#proportion1 prop1 = 0.73
#proportion2 prop2 = 0.64

power.prop.test( n = 174,
                 p1 = 0.73,
                 p2 = 0.64
                 )
power.prop.test( power = 0.9,
                 p1 = 0.73,
                 p2 = 0.64
                 )

#Determinar tamaño de muestra para un test binomial
#h = effect size
p.out <- pwr.p.test(h = ES.h(p1 = 0.55, p2 = 0.45),
                    sig.level = 0.05, 
                    power = 0.98, 
                    alternative = "greater")
plot(p.out)

#p1 Probabilidad hipótesis nula
#p2 Probabilidad hipótesis alternativa

#¿Cuál es el poder de nuestra prueba si tiramos la moneda 40 veces y bajamos nuestra tolerancia de error Tipo I a 0.01? 
#Observe que omitimos el powerargumento, agregamos n = 40 y cambiamos sig.level = 0.01:
  
pwr.p.test(h = ES.h(p1 = 0.55, p2 = 0.45),
             sig.level = 0.01, 
             n = 500,
             alternative = "greater")

#2.- POWER TEST PARA BINOMIAL METODO B 
#You have a one-sided, exact alternative hypothesis p1>p0 where p1=0.001 and p0=0.

#1.1 To Identify a threshold (umbral) c for the number of successes such that the probability to get at least c successes in a sample of size 
#n is very low under the null hypothesis (conventionally ??=0.05). 
#In your case, c=1, regardless of your particular choice for n???1 and ??>0.

#1.2 To find out the probability to get at least c successes in a sample of size n under the alternative hypothesis - 
#this is your power. Here, you need a fixed n such that the Binomial distribution B(n,p1) is fully specified. 
#This 2nd step in R with n=500:

n  <- 500                 # sample size
p1 <- 0.001               # success probability under alternative hypothesis
cc <- 1                   # threshold
sum(dbinom(cc:n, n, p1))  # power: probability for cc or more successes given p1

#To get an idea how the power changes with sample size, you can draw a power function

nn   <- 10:2000                 # sample sizes
pow  <- 1-pbinom(cc-1, nn, p1)  # corresponding power
tStr <- expression(paste("Power for ", X>0, " given ", p[1]==0.001))
plot(nn, pow, type="l", xaxs="i", xlab="sample size", ylab="power",
     lwd=2, col="blue", main=tStr, cex.lab=1.4, cex.main=1.4)

#If you want to know what sample size you need to achieve at least a pre-specified power, you can use the power values 
#calculated above. 

powMin <- 0.8
idx    <- which.min(abs(pow-powMin))  # index for value closest to 0.5
nn[idx]     # sample size for that index
pow[idx]    # power for that sample size

#BAYES AB TESTING
#install.packages("bayesAB")

library(bayesAB)
library(ggplot2)

#TEST BINOMIAL
testA <- rbinom(100, 1, .4)
testB <- rbinom(154, 1, .6)
plotBeta(8,8)#para buscar los valores de alpha y beta
Test1 <- bayesTest(testA, testB, distribution = "bernoulli", priors = c("alpha" = 8, "beta" = 8))
print(Test1)
summary(Test1)
plot(Test1)

testA2 <- rbinom(540, 1, .52)
testB2 <- rbinom(350, 1, .48)
plotBeta(170, 180)#para buscar los valores de alpha y beta
Test2 <- bayesTest(testA2, testB2, distribution = "bernoulli", priors = c("alpha" = 170, "beta" = 180))
print(Test2)
summary(Test2)
plot(Test2)

#Para combinar dos test consecutivos

TestAB3 <- combine(Test1, Test2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')

# also equivalent with %>% if you like piping
library(magrittr)

TestAB3 <- Test1 %>%
  combine(Test2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')





#TEST POISSON
A_pois <- rpois(250, 6.5)
B_pois <- rpois(250, 5.5)
plotGamma(30, 5) # 5-6 seem likely enough
AB2 <- bayesTest(A_pois, B_pois, priors = c('shape' = 30, 'rate' = 5), n_samples = 1e5, distribution = 'poisson')
print(AB2)
summary(AB2)
plot(AB2)

#COMBINACION DE FUNCIONES CON BAYES

AB3 <- combine(AB1, AB2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')

# also equivalent with %>% if you like piping
library(magrittr)

AB3 <- AB1 %>%
  combine(AB2, f = `*`, params = c('Probability', 'Lambda'), newName = 'Expectation')
print(AB3)
summary(AB3)
plot(AB3)

# DISTRIBUCION NORMAL

A_norm <- rnorm(100, 6, 1.5)
B_norm <- rnorm(100, 5, 2.5)
AB4 <- bayesTest(A_norm, B_norm,
                 priors = c('mu' = 5, 'sd' = 1, 'shape' = 3, 'scale' = 1), distribution = 'normal')
print(AB4)
summary(AB4)
plot(AB4)

#BAYES BINOMIAL FIRST AID

install.packages("devtools")
install.packages("rjags")
install.packages("coda")

#To install Bayesian First Aid then run:
library(devtools)
library(rjags)
devtools::install_github("rasmusab/bayesian_first_aid")

library(BayesianFirstAid)
bayes.binom.test(c(848, 750))

bayes.binom.test(848, 1598, conf.level = 0.8, p = 0.75, alternative = "greater")

bayes.binom.test(c(848, 750), n.iter = 999999)
fit <- bayes.binom.test(c(848, 750))
plot(fit)

summary(fit)
diagnostics(fit)
model.code(fit)

### Model code for the Bayesian First Aid alternative to the binomial test ###
require(rjags)

# Setting up the data
x <- 848 
n <- 1598 

# The model string written in the JAGS language
model_string <- "model {
x ~ dbinom(theta, n)
theta ~ dbeta(1, 1)
x_pred ~ dbinom(theta, n)
}"

# Running the model
model <- jags.model(textConnection(model_string), data = list(x = x, n = n), 
                    n.chains = 3, n.adapt=1000)
samples <- coda.samples(model, c("theta", "x_pred"), n.iter=5000)

# Inspecting the posterior
plot(samples)
summary(samples)  


#Test Binomial

#binom.test() function performs binomial test of null hypothesis about binomial distribution.

#binom.test(x,n,p=0.5,alternative=c("two.sided","less","greater"), conf.level=0.95)

#x: number of successes
#n: number of trials
#p: hypothesized probability of success
#alternative: alternative hypothesis, including "two.sided","greater","less", conf.level: confidence level

#Suppose in a coin tossing, the chance to get a head or tail is 50%. 
#In a real case, we have 100 coin tossings, and get 48 heads, is our original hypothesis true?

binom.test(48,100)

#Since the p-value is 0.7644, far greater than 0.05, the hypothesis is accepted.

binom.test(400, 767, p=(0.55))

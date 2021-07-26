#Cargar librerias ####
pkgload = function(p) {
  #Checks if package is installed.Once installed it loads the library  
  #p: package, string
  ifelse(require(package = p, character.only = T, quietly = T), 
         library(package = p, character.only = T, quietly = T), 
         c(install.packages(pkgs = p), 
           library(package = p, character.only = T, quietly = T))) 
}
pkgload('devtools')
pkgload("tidyverse") #IMPORTAN ONE
pkgload('ggplot2') #plotting routines
pkgload('foreign') #loads stata files
pkgload('readstata13') #loads old stata files
pkgload('R.matlab') #loads matlab files
pkgload('plyr') #data reshaping
pkgload('pryr') #data reshaping
pkgload('reshape2') #data reshaping
pkgload('ez')
pkgload('parallel')
pkgload('nloptr')
pkgload('dfoptim')
pkgload('rdd')
pkgload('lme4') #GLMs
pkgload('systemfit')
pkgload('MASS')
pkgload('margins')
pkgload('lfe')
pkgload('estimatr')
pkgload('clubSandwich')
pkgload('AER')
pkgload('nlme')
pkgload('lmerTest')
pkgload('pROC')
pkgload('quantreg')
pkgload('multcomp')
pkgload('e1071')
pkgload('ordinal')
pkgload('r2glmm')
pkgload('bbmle')
pkgload('optimx')
pkgload('emmeans')
pkgload('ggrepel')
pkgload('grid')
pkgload('gridExtra')
pkgload('png')
pkgload('ggalt')
pkgload('gapminder')
# devtools::install_github("dgrtwo/gganimate")
library(gganimate)
pkgload("ggExtra")
pkgload('ggcorrplot')
pkgload('scales')
pkgload('zoo')
pkgload('ggdendro')
pkgload('ggfortify')
pkgload('haven')
pkgload('Synth')
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)


#Data #####
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/HISP/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/HISP/'
HISP <- read.dta13(paste(wwdd,"evaluation.dta", sep=''))
coldes_HISP = cbind(attributes(HISP)$names, attributes(HISP)$var.labels) #variable description
#Columns HISP (Health Insurance Subsidy Program)
#Outcome variable
  #health_expenditures: Out of pocket health expenditure (per capita per year)
#Control variables
  #age_hh Age of the head of the household (in years)
  #Age_sp Age of the spouse (years)
  #educ_hh Education of the head of household (completed years of schooling)
  #educ_sp Education of the spouse (completed years of schooling)
  #indigenous Head of household speaks an indigenous language (0=no, 1=yes)
  #female_hh Head of the household is a woman (0=no, 1=yes)
  #hhsize #Number of household members (at baseline)
  #dirtfloor Home has a dirt floor at baseline (0=no, 1=yes)
  #bathroom Home with private bathroom at baseline (0=no, 1=yes)
  #land Number of hectares of land owned by household at baseline
  #hospital_dist Distance to closest hospital
#Other variables
  #locality_identifier Locality identifier
  #household_identifier Unique household identifier
  #round Survey round (0 = baseline; 1 = follow-up)
  #enrolled Household enrolled in HISP (0=no, 1=yes)
  #enrolled_rp Household enrolled in HISP under the randomized promotion scenario (0=no, 1=yes)
  #eligible Household eligible to enroll in HISP (0=no, 1=yes)
  #treatment_locality Household is located in treatment community (0=no, 1=yes)
  #promotion_locality Household is located in locality randomly assigned promotion of HISP (0=no, 1=yes)
  #poverty_index Poverty index 1-100 (eligible ≤ 58)
  #hospital HH member visited hospital in the past year (0=no, 1=yes), used in power calculations

#Bangladesh data (Khandker et al 2010 worldbank book)
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/Khandker2010/'
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/Khandker2010/' #working directory
bang = read.csv(paste(wwdd,"hh_98.csv", sep=''))
#setwd('..../DataSets/Khandker2010')
#bang = read.csv("hh_98.csv")
bang$lexptot = log(bang[,'exptot']) #log of expenditures
bang$lnland = log(1 + bang[,'hhland']/100) #log of land owned

# Lineal: Ejemplo problemas con heteroscedasticidad ####
# Fuente: https://www.datasciencecentral.com/profiles/blogs/weighted-linear-regression-in-r
n = 1000
X_data <- seq(1, 100, length.out = n)
Y_raw <- 3.5 + 2.1 * X_data
Y_noise <- rnorm(n = n, mean = 0, sd = 5)
Y <- data.frame(X = X_data, Y = Y_raw + Y_noise)
reg = lm(Y~X, data=Y)
summary(reg)
par(mfrow = c(2,2), mai = c(0.8,0.8,0.4,0.2))
plot(Y[,'X'], Y[,'Y'], main = 'Homoscedasticidad', pch = 20,
     xlab = 'x', ylab = 'y')
lines(Y[,'X'], predict(reg), col = 'red', lwd = 2)
qqnorm(reg$residuals)
qqline(reg$residuals, col ='red')
summary(reg) #compare intercept & X con Y_raw; deben ser cercanos 
plot(reg)

#ahora la sd depende de X
n=1000
X_data <- seq(1, 1000, length.out = n)
# Y is linear in x with uniform, periodic, and skewed noise
Y_raw <- 1.37 + 2.097 * X_data
Y_noise <- (X_data / 100) * 25 * (sin(2 * pi * X_data/100)) *
  runif(n = length(X_data), min = 3, max  = 4.5) +
  (X_data / 100)^3 * runif(n = n, min = 1, max = 5)
Y <- data.frame(X = X_data, Y = Y_raw + Y_noise)
reg = lm(Y~X, data=Y)
summary(reg)
plot(Y[,'X'], Y[,'Y'], main = 'Heteroscedasticidad',
     ylim = c(0, 7000), pch = 20,
     xlab = 'x', ylab = 'y')
lines(Y[,'X'], predict(reg), col = 'red', lwd = 2)
lines(X_data, Y_raw, col = 'green', lwd = 2)
#Some function so that variance increases with X 
#Variance of y also from data but in this example each x is a single point
Y$sd_variance = X_data^2 
Weighted_reg <- rlm(Y ~ X, data = Y, weights = 1/sd_variance)
summary(Weighted_reg)
lines(Y[,'X'], predict(Weighted_reg), col = 'purple', lwd = 2)
legend('topleft', c('OLS', 'WLS', 'Real\ndata gen. proc. (dgp)'),
       col = c('red', 'purple','green'), lty = 1, bty = 'n')
qqnorm(reg$residuals)
qqline(reg$residuals, col ='red')
summary(reg) #compare intercept & X con Y_raw; ya no son ni cercanos a y_raw 
qqnorm(Weighted_reg$residuals)
qqline(Weighted_reg$residuals, col ='red')
summary(Weighted_reg)

#ejemplo con robust standard error (robusto a heterostacidad)
reg_rA = lm_robust(Y~X, data=Y) #Computes robust standard errors 
summary(reg_rA)
summary(reg) #compare reg_rA & reg std. error; los Estimate son iguales
#otra opcion
reg_rB = lm(Y~X, data=Y)
coeftest(reg_rB, vcov=sandwich) #Computes robust standard errors
waldtest.default(reg_rB, vcov=sandwich, test ='F') #obtains F for the robust regression

#ejemplo con robust standard error (por clusters)
#Data on individuals with clustering on village or region or other 
# category such as industry, and state-year differences-in-differences 
# studies with clustering on state. In such settings default 
# standard errors can greatly overstate estimator precision (e.g. 
# puede haber un cluster muy ruidoso y si hay muchos clusters ese
# ruido se diluye en el promedio)
# url <- "http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt"
#p.df = read.csv('/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/clus')
p.df <- read.csv('E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/clus')
names(p.df) <- c('row',"firmid", "year", "x", "y")
head(p.df)
m1 <- lm(y ~ x, data = p.df)
summary(m1)
a = coef_test(m1, vcov='CR1', cluster = p.df$firmid)
a
unique(p.df[,'firmid'])
dim(p.df)

#ejemplo robust standard error (a heterostacidad) con data de verdad
reg = lm(health_expenditures ~ poverty_index*educ_hh, data=HISP)
summary(reg)
coeftest(reg, vcov=sandwich) #Computes robust standard errors
waldtest.default(reg, vcov=sandwich, test ='F') #obtains F for the robust regression
summary(margins(reg, vcov = sandwich(reg))) #this is partial dy/dx; 
#otra opcion
reg_rB = lm_robust(health_expenditures ~ poverty_index*educ_hh, data=HISP)
summary(reg_rB)
#algunos diagnosticos
qqPlot(reg) #BAD: The residuals don't look normal, they deviate from the line
leveragePlots(reg) # BAD: outlier influence, around the center of each plot there is an abundance of dots
resettest(reg) #ommited variables: añade como predictores formas cuadraticas y cubicas. Si p<0.05, el modelo tiene ommited variables i.e. esta mal especificado
bptest(reg) #test heteroskedasticity, si p<0.05 hay heteroskedasticity
#Con los diagnosticos hay que tener cuidado, el poder puede ser insuficiente
n = 50
x = runif(n)
u = rnorm(n)
y = exp(1+0.25*x + 4*x^2) + u #dgp (data generating process)
reg = lm(y~x+x^2)
bptest(reg) #test heteroskedasticity
#el test dice que hay heteroskedasticity pero no es cierto, u=rnorm(n)
#el problema es que la funcion en reg esta mal especificada relativo al dgp
#hay que testear la especificacion, una opcion es asi:
resettest(reg)


#Lineal cuantiles: ejemplo ######
par(mfrow = c(1,1))
x = seq(1,100,length.out = 100)
y = 4*x + 1.5
y_noise = rnorm(length(x), 0, x*1.5)
Y = data.frame(x = x, y = y + y_noise)
q1 = 0.9
reg_q1 = rq(y~x, tau = q1, data = Y) #En cada x modela la respuesta (lineal) del cuantil q de y
summary(reg_q1, se = 'boot') #boot is bootstrap ... kind of robust standard error
reg = lm(y~x, data=Y) #En cada x modela la respuesta promedio y
plot(Y[,'x'], Y[,'y'], 
     xlab ='x', ylab = 'y')
lines(Y[,'x'], predict(reg), col = 'red', lwd = 2)
lines(Y[,'x'], predict(reg_q1), col = 'green', lwd = 2)
q2 = 0.2
reg_q2 = rq(y~x, tau = q2, data = Y) #En cada x modela la respuesta (lineal) del cuantil q de y
lines(Y[,'x'], predict(reg_q2), col = 'purple', lwd = 2)
legend('topleft', c('OLS', 
                    sprintf('Reg. cuantil %s',q1),
                    sprintf('Reg. cuantil %s',q2)),
       col = c('red','green', 'purple'), bty ='n', lty =1)
q3 = 1:9/10 #varios cuantiles a la vez
req_q3 = rq(y~x, tau = q3, data = Y)
a = summary(req_q3)
plot(a, parm = 'x', mfrow = c(1,1),
     xlab = 'Quantiles', ylab = 'Slope') #the slope of extreme quantiles (e.g. 0.1 and 0.9) are clearly different from the mean-based OLS
legend('topleft',c('OLS+95% CI','QuantReg+95% CI'), bty='n', 
       col = c('red','black'), lty = 1)


# Simultaneity: ejemplo ####
layout(matrix(c(1,1,2,3,4,1,1,5,6,7), 2, 5, byrow = TRUE))
# par(mfrow = c(1,1), mai = c(0.8,0.8,0.2,0.2))
x = mvrnorm(200, mu = c(10,10), 
                 Sigma = matrix(c(1,2,0.5,1), nrow = 2, byrow = T))
plot(x, 
     xlab = 'Observable: precios de equilibrio', ylab = 'Observable: Oferta & Demanda')
idx = round(runif(6, 1, dim(x)[1])) #elegir puntos aleatorios
collors = rainbow(length(idx))
ep = x[idx,]
points(ep[,1], ep[,2], pch = 16, col = collors, cex=2)
titles = paste('Pais', 1:length(idx))
for (i in 1:length(idx)) {
  ep = x[idx[i],] #equilibrium price
  p = seq(-50,50,length.out = 100) #prices
  s_offer = 1 #slope (no conocida)
  i_offer = ep[2] - s_offer*ep[1] #intercept
  s_demand = -2
  i_demand = ep[2] - s_demand*ep[1]
  plot(p, s_offer*p +i_offer, type = 'l',
       main = titles[i],
       xlim = c(0,30), ylim = c(0,30),
       xlab = 'precios', ylab = 'Oferta & Demanda')
  points(ep[1],ep[2], col = collors[i], cex = 4, pch=16)
  lines(p, s_demand*p+i_demand)
}

#Diff. in Diff ejemplo ####
#tabla 7.3 Gertler et al 2016, WB book
HISP_temp = HISP[HISP[,'treatment_locality']==1,]
colnames(HISP)
head(HISP)
View(HISP)
#Manualmente: reste (enrolled 0 round 0 - enrolled 0 round 1) -(enrolled 1 round 0 - enrolled 1 round 1) 
ddply(HISP_temp, .(enrolled, round), 
      summarise, mT = mean(health_expenditures,na.rm=T))
#fixed effect linear regression. 
#El comando cl (cluster standard errors) de stata es el cuarto en la formula, donde esta locality_specifier. 
reg = felm(health_expenditures ~ round:enrolled + round + enrolled|0|0|locality_identifier,
           data = HISP_temp) #IMPORTANTE: diff en diff es una interaccion!!
summary(reg) #comparar la interaccion con el DandD de Gertler; 
# DandD

reg = felm(health_expenditures ~ round:enrolled + round + enrolled +
             age_hh + age_sp + educ_hh + educ_sp + indigenous +
             female_hh + hhsize + dirtfloor + bathroom + land + 
             hospital_distance|0|0|locality_identifier,
           data = HISP_temp)
summary(reg) #comparar la interaccion con el DandD de Gertler; 
# DandD


# Regression discontinuity ####
#Regression discontinuity simulation 
#https://mixtape.scunning.com/regression-discontinuity.html?panelset=r-code

# simulate the data
dat <- tibble(
  x = rnorm(1000, 50, 25)
) %>%
  mutate(
    x = if_else(x < 0, 0, x)
  ) %>%
  filter(x < 100)

# cutoff at x = 50
dat <- dat %>% 
  mutate(
    D  = if_else(x > 50, 1, 0),
    y1 = 25 + 0 * D + 1.5 * x + rnorm(n(), 0, 20)
  )

ggplot(aes(x, y1, colour = factor(D)), data = dat) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 50, colour = "grey", linetype = 2)+
  stat_smooth(method = "lm", se = F) +
  labs(x = "Test score (X)", y = "Potential Outcome (Y1)")

# fuente: khandker et al 2010 worldbank book pg 212
bang_d = bang[-which((bang[,'hhland']<50 &
                       (bang[,'dmmfd']==0 | bang[,'dfmfd']==0)) | #below threshold, should be in the program but aren't
                       (bang[,'hhland']>=50 & #above threshold and are (incorrectly) in the program
                          (bang[,'dmmfd']==1 | bang[,'dfmfd']==1))),] 
#The program was probabilistic i.e. people above the threshold could 
# or not be in the program. To produce a sharp cutoff only those below 
# the threshold who should be in the program and those above who 
# shouldn't are kept for the analysis.

cutoff = log(1 + 50/100)
bang_d$tratamiento = ifelse((bang_d[,'dmmfd']==0 | bang_d[,'dfmfd']==0), 0,
                            1) 
fit = lm(lexptot ~ I(lnland-cutoff)*tratamiento,
         data = bang_d) #lnland-cutoff: es el efecto de land normalizado a cero en el cutoff (facilita interpretación)
summary(fit) #tratamiento no significativo no hay main effect en el cutoff (el "salto"); no interaccion significativa en el cutoff no hay cambio de pendiente
plot(bang_d[,'lnland'], bang_d[,'lexptot'],
     pch = 16, cex =0.5)
fitL = lm(lexptot ~ lnland,
          data = bang_d[bang_d[,'lnland']<=cutoff,])
lines(bang_d[bang_d[,'lnland']<=cutoff,'lnland'], 
      predict(fitL), col = 'red')
fitR = lm(lexptot ~ lnland,
          data = bang_d[bang_d[,'lnland']>cutoff,])
lines(bang_d[bang_d[,'lnland']>cutoff,'lnland'], 
      predict(fitR), col = 'blue')
#LATE (local average treatment effect)
bw = cutoff*0.5 #bandwidth NOTA: esto es una heuristica, hay otras que incluyen kernels triangulares, gaussianos, otros!
R = mean(bang_d[bang_d[,'lnland']>cutoff &
                  bang_d[,'lnland']<(cutoff + bw) &
                           bang_d[,'tratamiento']==0
                           ,'lexptot']) #linea azul
L = mean(bang_d[bang_d[,'lnland']<cutoff &
                  bang_d[,'lnland']>(cutoff - bw) &
                  bang_d[,'tratamiento']==1,'lexptot']) #linea roja
exp(R-L) #diferencia post-cutoff menos pre-cutoff

# Synthetic control ####
#https://mixtape.scunning.com/synthetic-control.html

texas = read.csv("E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/texas.csv")

dataprep_out <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993,
  time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

path.plot(synth_out, dataprep_out)

gaps.plot(synth_out, dataprep_out)

placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 3,
                              strategy = "multiprocess" ) #Toma tiempo

plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

# OLS is inconsistent ####
#Fuente: Health econometrics using stata; 1st edition 2017, pp = 202-
set.seed(123456)
nobs = 10000
x = rnorm(nobs) #exogenous variable
w = rnorm(nobs) #instrumental variable
u = rnorm(nobs) #omitted variable
e1 = rnorm(nobs) #outcome error
e2 = rnorm(nobs) #endogenous error
y2 = x + .2*w + u + e2 #endogenous equation
y1 = y2 + x + u + e1 #outcome equation
#Next regression WITH u gives a CONSISTENT estimate of y2 
# i.e. it is close to the actual, 1, and the 95%CI includes 1
a = summary(lm(y1~y2 + x + u)) 
CI = c(a$coefficients['y2','Estimate'] - 1.96*a$coefficients['y2','Std. Error'],
       a$coefficients['y2','Estimate'] + 1.96*a$coefficients['y2','Std. Error']) #95% confidence intervals
a
CI
#Next regression WITHOUT u gives a INCONSISTENT estimate of y2 
# i.e. it is far from the actual, 1, and the 95%CI does not include 1. The
# reason is the the error due to unobserved variables (u) correlates
# with y2 i.e. y2 formula above includes u.
a = summary(lm(y1~y2 + x)) 
CI = c(a$coefficients['y2','Estimate'] - 1.96*a$coefficients['y2','Std. Error'],
       a$coefficients['y2','Estimate'] + 1.96*a$coefficients['y2','Std. Error']) #95% confidence intervals
a
CI



# 2SLS ####
# Fuente: Health econometrics using stata; 1st edition 2017, pp = 202-
# Usa definiciones de la seccion OLS is inconsistent
etapa1 = lm(y2 ~ x + w) #stage 1: 
summary(etapa1)
y2_hat = predict(etapa1)
etapa2 = lm(y1 ~ x + y2_hat) #stage 2:
a = summary(etapa2)
CI = c(a$coefficients['y2_hat','Estimate'] - 1.96*a$coefficients['y2_hat','Std. Error'],
       a$coefficients['y2_hat','Estimate'] + 1.96*a$coefficients['y2_hat','Std. Error']) #95% confidence intervals
a
CI

#Otra funcion del paquete systemfit para hacer 2SLS
twosls = systemfit(y1 ~ x + y2,
          inst = ~x+w, method = '2SLS') #instrumento en inst = 
summary(twosls)

#otra funcion del paquete AER
twosls = ivreg(y1 ~ x + y2|x+w)
summary(twosls, diagnostics = TRUE)
coeftest(twosls, vcov = sandwich)
#NOTE: in ivreg endogenous variables can only appear before 
#the vertical line; instruments can only appear after the 
#vertical line; exogenous regressors that are not instruments
#must appear both before and after the vertical line.

#Comparar con OLS 
a = summary(lm(y1~ x + y2)) 
CI = c(a$coefficients['y2','Estimate'] - 1.96*a$coefficients['y2','Std. Error'],
       a$coefficients['y2','Estimate'] + 1.96*a$coefficients['y2','Std. Error']) #95% confidence intervals
a
CI #El estimativo OLS es inconsistente con el dgp (data generating process) pues no incluye la variable instrumental

#Comparar cuando se incluye la variable u, que no observamos pero el 2SLS ayuda a solucionar
a = summary(lm(y1~y2 + x + u)) 
CI = c(a$coefficients['y2','Estimate'] - 1.96*a$coefficients['y2','Std. Error'],
       a$coefficients['y2','Estimate'] + 1.96*a$coefficients['y2','Std. Error']) #95% confidence intervals
a
CI


# 2SLS example 2 ####
#fuente:https://rstudio-pubs-static.s3.amazonaws.com/332491_1a839b62d1ed404dbef681d83f5d01c4.html
#model: y = a + bx + cd + e

R<-matrix(cbind(1,0.001,0.002,0.001,
                0.001,1,0.7,0.3,
                0.002,0.7,1,0.001,
                0.001,0.3,0.001,1),nrow=4) #Correlation matrix
rownames(R)<-colnames(R)<-c("x","d","z","e")
R
#cor(d,e)=0.3, this means that d is endogeneous;
#cor(d,z)=0.7, this means that z is a strong instrumental variable for d;
#cor(z,e)=0.001, this means that instrumental variable z satisfy the exclusion restriction in that it only affects y through d.

#generate data for x, d, z, and e with the specified correlation.
U = t(chol(R))
nvars = dim(U)[1]
numobs = 1000
set.seed(1)
random.normal = matrix(rnorm(nvars*numobs,0,1), nrow=nvars, ncol=numobs);
X = U %*% random.normal
newX = t(X)
data = as.data.frame(newX)
head(data)
cor(data)
x = data[,'x']
d = data[,'d']
e = data[,'e']
z = data[,'z']
y<-10+1*x+1*d+e #dgp (data generating process)
#OLS
ols<-lm(y~x+d)
summary(ols) #the estimated beta for d is 1.31 instead of 1
#2SLS
tsls1<-lm(d~x+z) #step 1
summary(tsls1) 
d.hat = predict(tsls1)        
tsls2<-lm(y~x+d.hat) #step2
summary(tsls2)  #comparar con dgp.      

# Specification & overidentification test ####
#simular data
nobs = 10000
x = rnorm(nobs) #exogenous variable
w = rnorm(nobs) #instrumental variable
u = rnorm(nobs) #omitted variable
e1 = rnorm(nobs) #outcome error
e2 = rnorm(nobs) #endogenous error
y2 = x + .2*w + u + e2 #endogenous equation
y1 = y2 + x + u + e1 #outcome equation

#test de endogenidad de x en y=beta*x + error
twosls = ivreg(y1 ~ x + y2|x+w)
summary(twosls, diagnostics = TRUE)
#weak instruments hace una regresion de la variable endogena x = instruments + error
    #con p<0.05 significa que el instrumento es bueno i.e. se correlaciona con la variable endogena
#wu-hausman test compara coeficientes de regresiones OLS vs IV
    #con p<0.05 significa que hay endogeneidad en el OLS i.e. incluir variable instrumental ayuda.
#Sargan test es para overidentification (ver abajo)

#Overspecification i.e. detecta si hay muchos instrumentos
wsq = w^2 #another instrumental variable
twosls = ivreg(y1 ~ x + y2|x+w+wsq)
summary(twosls, diagnostics = TRUE)
#Sargan test detecta si los instrumentos son exogenos y aportan informacion
  #con p<0.05 significa que uno o varios de los instrumentos no se necesitan

#Más sobre mala especificación (efectos marginales sesgados)
n = 10000
x = 2 + 6*runif(n)
z = 2 + 6*rbeta(n, 2, 4)
u = 3*rnorm(n)
y = 1000 - 2*x + 0.3*x^2 - 2*z + 0.3*z^2 + u #dgp (data generating process)

regA = lm (y ~ x + z) #modelo mal especificado i.e. no representa el dgp
summary(regA)

regB = lm (y ~ I(x^2) +  I(z^2)) #modelo con especificacion cuadrática
summary(regB)

regC = lm (y ~ x + I(x^2) + z + I(z^2)) #modelo con especificacion perfecta
summary(regC)

margins(regA) #dy/dx and dy/dz mal especificacion lineal
margins(regB) #mal especificacion cuadratica
margins(regC) #real

n = seq(500, 10000, by = 50)
dydx_x_mal = rep(0,length(n))
dydx_x_true = rep(0,length(n))
dydx_z_mal = rep(0,length(n))
dydx_z_true = rep(0,length(n))
for (i in 1:length(n)){
  ntemp = n[i]
  x = 2 + 6*runif(ntemp)
  z = 2 + 6*rbeta(ntemp, 2, 4)
  u = 3*rnorm(ntemp)
  y = 1000 - 2*x + 0.3*x^2 - 2*z + 0.3*z^2 + u #dgp (data generating process)
  
  temp = lm (y ~ x + z) #modelo mal especificado
  temp2 = margins(temp)
  dydx_x_mal[i] = mean(temp2$dydx_x) #x variable
  dydx_z_mal[i] = mean(temp2$dydx_z) #z variable
  
  temp = lm (y ~ x + I(x^2) + z + I(z^2)) #modelo bien especificado relativo a dgp
  temp2 = margins(temp)
  dydx_x_true[i] = mean(temp2$dydx_x) #x variable
  dydx_z_true[i] = mean(temp2$dydx_z) #z variable
}  
par(mfrow = c(2,2))
plot(density(dydx_x_mal), type ='l', lty=2,
     main = 'Average Marginal Effect (AME) of x (dydx)',
     xlab = 'AME')
lines(density(dydx_x_true))
legend('topleft', c('Perfect specification','Misspecifed'),
       bty = 'n', lty = c(1,2))
plot(density(dydx_z_true), type ='l', lty=2,
     main = 'Average Marginal Effect (AME) of z (dydz)',
     xlab = 'AME')
lines(density(dydx_z_mal)) #el efecto marginal de z (i.e. dydz) esta notablement sesgado (i.e. relativo a perfect)

#Model selection R2, AIC, BIC 
n = 10000
x = 2 + 6*runif(n)
z = 2 + 6*rbeta(n, 2, 4)
u = 3*rnorm(n)
y = 1000 - 2*x + 0.3*x^2 - 2*z + 0.3*z^2 + u #dgp (data generating process)

regA = lm (y ~ x + z) #modelo mal especificado i.e. no representa el dgp
summary(regA)
BIC(regA)
AIC(regA)

regB = lm (y ~ I(x^2) +  I(z^2)) #modelo con especificacion cuadrática
summary(regB)
BIC(regB)
AIC(regB)

regC = lm (y ~ x + I(x^2) + z + I(z^2)) #modelo con especificacion perfecta
summary(regC)
BIC(regC)
AIC(regC)
#Cual es mejor reg A, B o C (AIC y BIC bajos son mejores ajustes)




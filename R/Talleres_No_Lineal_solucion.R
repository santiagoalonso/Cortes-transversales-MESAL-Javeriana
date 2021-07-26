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
pkgload('lme4') #GLMs
pkgload('systemfit')
pkgload('MASS')
pkgload('margins')
pkgload('lfe')
pkgload('estimatr')
pkgload('clubSandwich')
pkgload('AER')
pkgload('ivprobit')
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


#Data #####
#HISP ((Health Insurance Subsidy Program))
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/HISP/' #working directory
HISP <- read.dta13(paste(wwdd,"evaluation.dta", sep=''))
#Description of columns HISP 
coldes_HISP = cbind(attributes(HISP)$names, attributes(HISP)$var.labels) #Variable description


#MEPS (Medical Expenditure Panel Survey) (del libro health econometrics using stata)
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/' #working directory
MEPS <- read.dta(paste(wwdd,"heus_mepssample.dta", sep=''))
#Description of columns MEPS 
coldes_MEPS = cbind(attributes(MEPS)$names, attributes(MEPS)$var.labels) #variable description
MEPS_p = MEPS[MEPS[,'exp_tot']>0,] #only positive total expenditures
colnam = colnames(MEPS_p)
MEPS_p[,'female'] = ifelse(MEPS_p[,'female'] == 'Male',
                           0,1)

#MEPS (Medical Expenditure Panel Survey)(del libro de Cameron y Trivedi Microeconometrics using stata)
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/CameronTrivediStata/' #working directory
MEPSct <- read.dta(paste(wwdd,"mus03data.dta", sep=''))
#Description of columns MEPS 
coldes_MEPSct = cbind(attributes(MEPSct)$names, attributes(MEPSct)$var.labels) #variable description
MEPSct_p = MEPSct[MEPSct[,'posexp']==1,] #only positive total expenditures

#MEPS con instrumento (Medical Expenditure Panel Survey)(del libro de Cameron y Trivedi Microeconometrics using stata)
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/CameronTrivediStata/' #working directory
MEPSins <- read.dta(paste(wwdd,"mus06data.dta", sep=''))
#Description of columns MEPS 
coldes_MEPSins = cbind(attributes(MEPSins)$names, attributes(MEPSins)$var.labels) #variable description


#MEPS doctor visits (Medical Expenditure Panel Survey)(del libro de Cameron y Trivedi Microeconometrics using stata)
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/CameronTrivediStata/' #working directory
MEPSdoc <- read.dta(paste(wwdd,"mus10data.dta", sep=''))
#Description of columns MEPS 
coldes_MEPSdoc = cbind(attributes(MEPSdoc)$names, attributes(MEPSdoc)$var.labels) #variable description



#Arsenic wells
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/GelmanHill_reg_hierarchical/arsenic/' #working directory
Ars <- read.dta(paste(wwdd,"all.dta", sep=''))
#Description of columns Ars 
coldes_Ars = cbind(attributes(Ars)$names, attributes(Ars)$var.labels) #variable description
Ars_r = read.table(paste(wwdd,"wells.txt", sep=''), sep = ' ')# reduced version
Ars_r$dist100 = Ars_r[,'dist']/100 #So that distance is in a scale of "every 100 mts"
#dist: dist to safe well
#arsenic: arsenic levels
#assoc: 1 if a household member forms part of any community organization
#educ: years of education of the well user


#HRS (Health and Retirement Study)(del libro de Cameron y Trivedi Microeconometrics using stata)
wwdd = '/Users/sadiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/CameronTrivediStata/' #working directory
HRS <- read.dta(paste(wwdd,"mus14data.dta", sep=''))
HRS$linc = ifelse(HRS[,'hhincome']>0, log(HRS[,'hhincome']), 0)
#Description of columns MEPS 
#hstatusg: health status: 0: good, 1:excelent
#adl: number of limitations in activities of daily living (max 5)
#chronic: number of chronic conditions
#age, female (0: male), white (0: non-white); hisp (0: non-hispanic)
#   married (1: yes); educyear (years of education); retire (1: retired)
#hhincome: household income
#linc: log hhincome if hhincome>0
#sretire: spouse is retired (if spouse is present)
#ins: 0:not insured, 1:insured



#NON-LINEAR REGRESSION ####
#Punto 0: defina una log. likelihood function para data poisson ####
N <- 100
DATA <- rpois(N, lambda = 20)
#grafique el dgp i.e. DATA
y = dpois(DATA, lambda = 20)
plot(DATA,y)
#Haga un log. likelihood gaussiano. Por que sacamos el log? underflow
LL = function (mu, sigma) {
  likelihood = dnorm(DATA, mu, sigma)
  -sum(log(likelihood))
}
#optimice la funcion con mle. En method use L-BFGS-B
fit = mle(LL, start = list(mu = 10, sigma = 10),
    lower = list(mu = 10, sigma = 1),
    method = 'L-BFGS-B')
#use summary y comente si se obtuvo el promedio y varianza de DATA
summary(fit)
mean(DATA)
sd(DATA)^2
coef(fit)['mu']
coef(fit)['sigma']^2
#Obtuvo los parametros que uso para generar DATA?
#Por qué si uso una poisson para LL y normal para dgp? Al limite poisson es normal
#grafique la data y el fit con la funcion density
par(mfrow=c(2,2))
plot(density(DATA))
fit_to_plot = rnorm(100, coef(fit)['mu'], coef(fit)['sigma'])
lines(density(fit_to_plot), col = 'red')
#Comente si son iguales o diferentes
#Busque en internet o help de R-studio que hace la funcion density
#Cambie N a 10, 20, 30, 50.
#Ahora cambie el LL a poisson y compare el AIC summary(MLE)
LL.p = function (lambda) {
  likelihood = dpois(DATA, lambda)
  -sum(log(likelihood))
}
fit.p = mle(LL.p, start = list(lambda=1),
          lower = list(lambda=0.1),
          method = 'L-BFGS-B')
AIC(fit)  #El AIC es algo mejor para Poisson pero similares. Vale la pena hacer regresion Poisson? Si, es una funcion discreta
AIC(fit.p) 
#Punto 1 binary: Pozos de agua con arsenico ####
#En Bangladesh mucha gente obtiene agua de pozos. Unos estan contaminados,
#otros no. La gente puede cambiar de pozo pues hay muchos. El costo es
#desplazarse. Años despues de un programa de etiquetar pozos como seguros
#o no, se recogieron datos de quienes cambiaron de pozos.

#Haga una regresion logistica usando el comando glm
#Primero solo con distancia al pozo seguro
fit.1 = glm(switch ~ dist100, family = binomial(link='logit'),
            data = Ars_r)
summary(fit.1)
#interprete los coeficientes:
#Intercepto: evalue el modelo en el promedio de dist. con la logistic (ver y abajo) 
#beta para dist.: evalue dos puntos con la logistic: 1) en el promedio de dist. ; 2) un punto \epsilon (arbitrario) menor al promedio
#Use la regla dividir por cuatro i.e. divida por 4 el coeficiente para dist. Ese es el maximo aumento de probabilidad de cambiarse de pozo por 1 metro más de cercania al pozo
x = seq(0,350/100,length.out = 100)
y = 1/(1+exp(-(fit.1$coefficients[1]+fit.1$coefficients[2]*x))) #logistic i.e. inverse of logit
plot(x, y, ylim=c(0,1), type = 'l',
     xlab = 'Distance (100 m) nearest safe well',
     ylab = 'p(switch)') # model
x = Ars_r[,'dist100']
y = Ars_r[,'switch']
y_jitter = ifelse(y==0, runif(length(y), 0, 0.05), runif(length(y), 1-0.05, 1))
points(x, y_jitter, pch = 20, cex=0.5) #data


#Agrege un predictor: niveles de arsenico
fit.2 = glm(switch ~ dist100 + arsenic, 
            family = binomial(link='logit'), 
            data = Ars_r)
summary(fit.2)
#Interprete el signo de los coeficientes. ¿Tiene sentido?
#Divida por 4 los coeficientes (no el intercepto). Interprete.
#El coeficiente para dist100=-0.9 y arsenic=0.46 ... eso significa que es más importante dist? Justifique ... la respuesta es NO, las escalas de dist y arsenico son diferentes!
#Por que es más fuerte el efecto de dist100 (-.62 a -.9) cuando se incluye arsenico en la regresion? Una vez que sabemos el nivel de arsenico, es mucho más dificil cambiarse de pozo i.e. en la data pozos lejos tienen más arsenico en promedio 

#Interprete estas gráficas
par(mfrow = c(2,2))
x = Ars_r[,'dist100']
y = Ars_r[,'switch']
y_jitter = ifelse(y==0, runif(length(y), 0, 0.05), runif(length(y), 1-0.05, 1))
plot(x, y_jitter, ylim=c(0,1), pch = 20, cex = 0.5,
     xlab = 'Distance (100 m) nearest safe well',
     ylab = 'p(switch)') # data
x = seq(0,350/100,length.out = 100)
y = 1/(1+exp(-(fit.2$coefficients[1]+
                 fit.2$coefficients[2]*x +
                 fit.2$coefficients[3]*0.5))) #logistic i.e. inverse of logit
lines(x, y, col = 'red') #model
y = 1/(1+exp(-(fit.2$coefficients[1]+
                 fit.2$coefficients[2]*x +
                 fit.2$coefficients[3]*1))) #logistic i.e. inverse of logit
lines(x, y, col = 'blue') #model
legend('topright',c('Ars: 0.5','Ars: 1'), bty = 'n',
       lty = 1, col = c('red','blue'))

x = Ars_r[,'arsenic']
y = Ars_r[,'switch']
y_jitter = ifelse(y==0, runif(length(y), 0, 0.05), runif(length(y), 1-0.05, 1))
plot(x, y_jitter, ylim=c(0,1), pch = 20, cex = 0.5,
     xlab = 'Arsenic concentration in well water',
     ylab = 'p(switch)') # data
x = seq(0,10,length.out = 100)
y = 1/(1+exp(-(fit.2$coefficients[1]+
                 fit.2$coefficients[2]*0 +
                 fit.2$coefficients[3]*x))) #logistic i.e. inverse of logit
lines(x, y, col = 'red') #model
y = 1/(1+exp(-(fit.2$coefficients[1]+
                 fit.2$coefficients[2]*0.5 +
                 fit.2$coefficients[3]*x))) #logistic i.e. inverse of logit
lines(x, y, col = 'blue') #model
legend('topright',c('Dist: 0','Dist: 50'), bty = 'n',
       lty = 1, col = c('red','blue'))


#Agrege la interaccion: dist X arsenico
fit.3 = glm(switch ~ dist100 + arsenic + dist100:arsenic, 
            family = binomial(link='logit'), 
            data = Ars_r)
summary(fit.3)
#Interprete los coeficientes. 
#Intercept: Evalue con la logistic (para pasar a prob. scale) el modelo en los promedios de dist100 y arsenic. Eso da una probabilidad (pg 92 gelman hill book)
#Coeficientes: evalue con logistic dos punts que difieran en uno; esto para dist100 y arsenico
#Interacciones: primero entienda interaccion en la via del dist100; luego en la via de arsenico (pg 93 gelman hill book)


#Reste el promedio de los inputs dist100 y arsenico (facilita interpretacion, las escalas son relativo al promedio)
#corra la regresion con interaccion de nuevo e interprete los coeficientes
Ars_r$c.dist100 = Ars_r[,'dist100'] - mean(Ars_r[,'dist100'])
Ars_r$c.arsenic = Ars_r[,'arsenic'] - mean(Ars_r[,'arsenic'])
fit.4 = glm(switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic, 
            family = binomial(link='logit'), 
            data = Ars_r)
summary(fit.4)
#Interprete los coeficientes ... note que ahora todos los promedios son 0 por que centramos
head(Ars_r)


#Añada regresores sociales (asociacion y educacion)
Ars_r$educ4 = Ars_r$educ/4 #in a scale of every 4 years of more education
fit.5 = glm(switch ~ c.dist100 + c.arsenic + c.dist100:c.arsenic +
              assoc + educ4, 
            family = binomial(link='logit'), 
            data = Ars_r)
summary(fit.5)
#Tiene sentido meter assoc? es significativo? 
#Interprete el coeficiente de educacion. Primero el signo. Luego use la regla de dividir por 4


#Punto 1.1. binary: insurance ####
#resuma la data
summary(HRS[,c('ins', 'retire', 'age','hstatusg','hhincome',
               'educyear', 'married', 'hisp', 'linc', 'female',
               'white','chronic','adl','sretire')])
#regresion logit
fit = glm(ins ~ retire + age + hstatusg + hhincome + educyear + 
            married + hisp,
          data = HRS, family = binomial(link= 'logit'))
summary(fit)
coeftest(fit)
#interprete los efectos marginales usando la funcion margins.
margins(fit) 
#Evalue e interprete el efecto marginal de retire fijando el promedio de los otros regresores
#¿Es diferente al que obtuvo con margins? Mucho o poco? por que difiere
m_age = mean(HRS[,'age'])
m_hstatusg = mean(HRS[,'hstatusg'])
m_hhincome = mean(HRS[,'hhincome'])
m_educyear = mean(HRS[,'educyear'])
m_married = mean(HRS[,'married'])
m_hisp = mean(HRS[,'hisp'])
ret1 = coef(fit)['(Intercept)'] + coef(fit)['retire']*1 + 
  coef(fit)['age']*m_age + coef(fit)['hstatusg']*m_hstatusg + 
  coef(fit)['hhincome']*m_hhincome + coef(fit)['educyear']*m_educyear + 
  coef(fit)['married']*m_married + coef(fit)['hisp']*m_hisp  
ret0 = coef(fit)['(Intercept)'] + coef(fit)['retire']*0 + 
  coef(fit)['age']*m_age + coef(fit)['hstatusg']*m_hstatusg + 
  coef(fit)['hhincome']*m_hhincome + coef(fit)['educyear']*m_educyear + 
  coef(fit)['married']*m_married + coef(fit)['hisp']*m_hisp
1/(1+exp(-ret1)) - 1/(1+exp(-ret0)) #inverse logit
#Divida el beta de retire por 4 y compare con margins(fit) y fijando promedios
#¿Que concluye? el MAXIMO cambio de probabilidad de asegurarse si se es retirado es del 4.9%
coef(fit)['retire']/4

#Test de especificacion: añada age^2 + age:female + age:chronic + age:white
#Añaden información a la regresion? use funciones lrt, waldtest.default, y AIC
fit.interactions = glm(ins ~ retire + age + hstatusg + hhincome + 
                         educyear + married + hisp + I(age^2) +
                         age:female + age:chronic + age:white,
                       data = HRS, family = binomial(link= 'logit'))
lrtest(fit.interactions,fit) #Esto hace un likelihood ratio test
waldtest.default(fit.interactions, fit) #p > 0.05 significa que NO añaden informacion, las interacciones son innecesarias
AIC(fit.interactions)
AIC(fit) #Las interacciones tienen un mayor AIC, no añaden info. (rule of thumb: diferencias de AIC mayores de 5 puntos son relevantes) 

#Clasificacion logit vs probit (segun CamTriv, ambas son parecidas excepto en las colas)
Yfac = HRS[,'ins'] #data: insured ; no-insured
# predicted probabilities
Yhat = 1/(1+exp(-predict(fit))) #prob. of being insured (logistic because it was a logit model)
# choose a threshold for dichotomizing according to predicted probability
thresh  <- 0.5 #arbitrary threshold
YhatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("no-ins", "ins")) #cuts Yhat according to breaks i.e. no-ins: [-Inf, thr] & ins: [thr, Ind]
# contingency table and marginal sums
cTab <- table(Yfac, YhatFac)
cTab
addmargins(cTab)
# percentage correct for training data
sum(diag(cTab)) / sum(cTab) #Las diagonales de la tabla son hits i.e. clasificaciones correctas

fit.probit = glm(ins ~ retire + age + hstatusg + hhincome + educyear + 
            married + hisp,
          data = HRS, family = binomial(link= 'probit'))
Yhat = 1/(1+exp(-predict(fit.probit))) #prob. of being insured (logistic because it was a logit model)
YhatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("no-ins", "ins")) #cuts Yhat according to breaks i.e. no-ins: [-Inf, thr] & ins: [thr, Ind]
cTab <- table(Yfac, YhatFac)
cTab
addmargins(cTab)
# percentage correct for training data
sum(diag(cTab)) / sum(cTab) #Las diagonales de la tabla son hits i.e. clasificaciones correctas

#Haga una regresion ivprobit con instrumentos retire y sretire para log(income)
#Piensa que son instrumentos validos? use las dos condiciones
HRS$age2=HRS[,'age']^2
fit.ins = ivprobit(ins ~  female + age + age2 + educyear +
                     married + hisp + white + chronic + adl + 
                     hstatusg| #exegenous regressors
                     linc| #endogenous regressor
                     female + age + age2 + educyear +
                     married + hisp + white + chronic + adl + 
                     hstatusg + retire + sretire, #all regressors except endogenous 
                   data = HRS)
summary(fit.ins)


#Punto 2: MEPS health expenditure glms ####
#haga histograma de health expenditures < 100000. 
#Se ve normal? o log-normal?
idx = MEPS_p[,'exp_tot']<=100000
plot(density(MEPS_p[idx,'exp_tot']), main = '',
     xlab = 'Total medical expenditure')
#Hay problemas? Haga una regresion lineal gastos = age + female y haga un histograma de los residuales
#Son normales los residuales?
fit = lm(exp_tot ~ age + female,
         data = MEPS_p[idx,])
plot(density(fit$residuals), main = '',
     xlab = 'residuals')

#regression gaussiana con link log
#Qué significa link=log? que exp_tot sigue una distibucion log-normal
fit = glm(exp_tot ~ age + female, family = gaussian (link = 'log'),
          data = MEPS_p)
summary(fit)
coeftest(fit, vcov = sandwich) #robust standard error

#regression gamma con link log
#Qué significa familia gamma? 
#Por qué gamma? busque en internet el dominio (gamma es positiva)
#Es esta regresion mejor o peor que la gaussiana? Use el AIC 
#Compare los efectos. 
#Que opina del cambio en tamaño del efecto para female, mucho mayor con gamma.
fit = glm(exp_tot ~ age + female, family = Gamma (link = 'log'),
          data = MEPS_p)
summary(fit)
coeftest(fit, vcov = sandwich) #robust standard error

#compute el promedio de gastos medicos predichos por el modelo
mean(exp(predict(fit))) #exp por el link log.

#compute el promedio de gastos medicos predichos por un modelo con interaccion para mujeres de 20 años 
fit = glm(exp_tot ~ age + female + age:female, family = Gamma (link = 'log'),
          data = MEPS_p)
idx = MEPS_p[,'age']==20 & MEPS_p[,'female'] == 1
mean(exp(predict(fit, newdata = MEPS_p[idx, c('age','female')])))
#otra forma
exp(as.numeric(fit$coefficients[1] + fit$coefficients[2]*20 + 
                 fit$coefficients[3]*1 +
                 fit$coefficients[4]*1*20)) 
   
#compute el promedio de gastos medicos predichos por un modelo con interaccion para hombres de 20 años     
idx = MEPS_p[,'age']==20 & MEPS_p[,'female'] == 0
mean(exp(predict(fit, newdata = MEPS_p[idx, c('age','female')])))
#otra forma
exp(as.numeric(fit$coefficients[1] + fit$coefficients[2]*20 + 
                 fit$coefficients[3]*0 +
                 fit$coefficients[4]*0*20))

#Reflexione sobre las diferencias

#Use el comando margins con diferentes edades y generos e.g. 
#margins(fit, at = list(age = 20, female = c(0,1))) #Quien gasta más?

#Punto 3: visitas doctor #####
#resuma para el año 2002 las variables docvis private chronic female income
idx = MEPSdoc[,'year02']==1
MEPSdoc02 = MEPSdoc[idx,]
summary(MEPSdoc02[,c('docvis','private','chronic','female','income')])
counts = table(MEPSdoc02[,'docvis']) #TEACH THEM HERE THE TABLE COMMAND percentage
counts/sum(counts) 

#haga un ggplot de barras con hasta max. 4 visitas y las demas agruparlas (busque en internet si no recuerda)
idx = MEPSdoc02[,'docvis']<=4
dataT = MEPSdoc02
dataT[!idx,'docvis'] = '5 o más'
theme_set(theme_classic())
ggplot(dataT, aes(x = docvis)) + 
  geom_bar(width = 0.7, stat = "count", color='blue', fill = 'white') +
  labs(x='Doctor visits', 
       y='Count', 
       title="Pie chart of doctor visits", 
       caption="Source: MEPS")

#Haga una regresion docvis ~ private + chronic + female + income
fit.g = glm(docvis ~  private + chronic + female + income, 
          data = MEPSdoc02)
summary(fit.g)
coeftest(fit.g, vcov = sandwich)
#Cambie la familia a Poisson
#Por qué usamos una poisson y no una gauss? Counts discretos
fit.p = glm(docvis ~  private + chronic + female + income,
          family = poisson,
          data = MEPSdoc02)
summary(fit.p)
coeftest(fit.p, vcov = sandwich)
#Compare los coeficientes poiss vs gauss. y los AIC
#Por qué difieren los coef? La familia poison usa el link log!
#Por qué difieren los AIC? la familia gauss tiene un parametro más (sd) que le ayuda a explicar la data
fit.p
fit.g
#Haga un fit poisson con overdispersion
fit.p.o = glm(docvis ~  private + chronic + female + income,
            family = quasipoisson, 
            data = MEPSdoc02)
summary(fit.p)
summary(fit.p.o)


#cual es el promedio de visitas que predice el modelo poisson y gauss.
#use mean(predict(fit.p)) y mean(predict(fit.g))
#Cual es el que dice la data?
#por qué el de poisson es tan bajo? el link default es log; hay que exponenciar para invertir
mean(predict(fit.p))
mean(predict(fit.g))
mean(MEPSdoc02[,'docvis'])


#use margins en la regresion poisson y gauss e interprete
margins(fit.p)
margins(fit.g)

#ahora ajuste una familia negative binomial y compare los AIC
#Busque en internet la diferencia entre binomial y negative binomial? bin: #success over fixed n; neg. bin: n required to obtain #success
#Neg. binomial es mejor en AIC pues no es tan restringida como poisson donde el promedio y varianza son identicos (mu=var=lambda)
fit.nb = glm.nb(docvis ~  private + chronic + female + income,
                data = MEPSdoc02)
summary(fit.nb)
AIC(fit.nb)
AIC(fit.p)
AIC(fit.g)
N = 10000
x = rnbinom(N, 1,fit.nb$theta)
y = dnbinom(x, 1, fit.nb$theta)
plot(x,y) #model neg.binom
table(x)/sum(table(x))
idx = MEPSdoc02[,'docvis']<=7
x = table(MEPSdoc02[idx,'docvis'])
y = x/sum(x)
points(as.numeric(names(x)), y, col = 'red') #data neg binom.
x = rpois(N, lambda = mean(predict(fit.p)))
y = dpois(x, lambda = mean(predict(fit.p)))
points(x, y, col = 'blue') #model poisson
legend('topright', c('Data', 'Poisson', 'Neg. Binom'),
       bty = 'n', pch = 1, col = c('black', 'blue','red'))




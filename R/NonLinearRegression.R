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
pkgload("nnet")
pkgload('margins')
pkgload('lfe')
pkgload('estimatr')
pkgload('clubSandwich')
pkgload('AER')
pkgload("mlogit")
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
#devtools::install_github("dgrtwo/gganimate")
library(gganimate)
pkgload("ggExtra")
pkgload('ggcorrplot')
pkgload('scales')
pkgload('zoo')
pkgload('ggdendro')
pkgload('ggfortify')
pkgload('MatchIt')

#Datasets ####
#cancer = read.csv('/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/Cancer.csv')
cancer = read.csv('E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/Cancer.csv')
cancer[,'diagnosis'] =  ifelse(cancer[,'diagnosis'] == 'M',0,1)
#Fuente cancer database:https://www.kaggle.com/uciml/breast-cancer-wisconsin-data/
#Columns cancer
# dID number
# diagnosis The diagnosis of breast tissues (0 = malignant, 1 = benign)
# radius_mean mean of distances from center to points on the perimeter
# texture_mean standard deviation of gray-scale values
# perimeter_mean mean size of the core tumor
# area_mean
# smoothness_mean mean of local variation in radius lengths
# compactness_mean mean of perimeter^2 / area - 1.0
# concavity_mean mean of severity of concave portions of the contour
# concave points_mean mean for number of concave portions of the contour
# symmetry_mean
# fractal_dimension_mean mean for "coastline approximation" - 1
# radius_se standard error for the mean of distances from center to points on the perimeter
# texture_se standard error for standard deviation of gray-scale values
# perimeter_se
# area_se
# smoothness_se standard error for local variation in radius lengths
# compactness_se standard error for perimeter^2 / area - 1.0
# concavity_se standard error for severity of concave portions of the contour
# concave points_se standard error for number of concave portions of the contour
# symmetry_se
# fractal_dimension_se standard error for "coastline approximation" - 1
# radius_worst "worst" or largest mean value for mean of distances from center to points on the perimeter
# texture_worst "worst" or largest mean value for standard deviation of gray-scale values
# perimeter_worst
# area_worst
# smoothness_worst "worst" or largest mean value for local variation in radius lengths
# compactness_worst "worst" or largest mean value for perimeter^2 / area - 1.0
# concavity_worst "worst" or largest mean value for severity of concave portions of the contour
# concave points_worst "worst" or largest mean value for number of concave portions of the contour
# symmetry_worst
# fractal_dimension_worst "worst" or largest mean value for "coastline approximation" - 1


#Police arrests (del libro Data Analysis Using Regression and Multilevel/Hierarchical Models de Andrew Gelman & Jennifer Hill)
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/' #working directory
Police <- read.csv(paste(wwdd,"frisk_with_noise.csv", sep=''))
#precincts	are	numbered	
#ethnicity (eth)	1=black,	2=hispanic,	3=white		
#crime	type	1=violent,	2=weapons,	3=property,	4=drug
idx = which(Police[,'past.arrests'] == 0)
Police = Police[-idx,]

#Water wells (Arsenic)
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/arsenic/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/arsenic/' #working directory
wells <- read.dta(paste(wwdd,"all.dta", sep=''))
coldes_wells = cbind(attributes(wells)$names, attributes(wells)$var.labels) #variable description

#Storable votes
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/storablevotes/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/GelmanHill_reg_hierarchical/storablevotes/' #working directory
storable <- read.csv(paste(wwdd,"6playergames.csv", sep=''))
#the column value was randomly (uniform) assigned to subjects at the start of a round and before casting a vote for theme 1 

#Fishing mode
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/CameronTrivediStata/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/CameronTrivediStata/' #working directory
fishing <- read.dta(paste(wwdd,"mus15data.dta", sep=''))
coldes_fish = cbind(attributes(fishing)$names, attributes(fishing)$var.labels) #variable description



#HISP
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/HISP/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/HISP/' #working directory
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


#M & M
#Fuente: https://joshmadison.com/2007/12/02/mms-color-distribution-analysis/
#wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/' #working directory
mandm <- read.csv(paste(wwdd,"mandm.csv", sep=''))



#Ejemplo outcome binario #####
par(mfrow = c(2,2))
n = 200
membership = sort(rbinom(n,1,0.5))# + rnorm(n,0,.1)
income_thousands = seq(20, 35, length.out = n)
plot(income_thousands,membership, ylim = c(0,1))
reg = lm(membership ~ income_thousands)
lines(income_thousands, predict(reg), col ='red', lwd = 3)
summary(reg)
legend('topleft', c('Data','OLS'), bty = 'n', 
       pch = c(1,45), col = c('black','red'), cex = 1.15)
#¿Tiene sentido el intercepto negativo? ¿La pendiente?

#Otro ejemplo no-deterministico
membership = seq(0.01,0.99,length.out = n)
income_thousands = qchisq(membership, df=1) + rnorm(n,0,.2)
plot(income_thousands,membership, ylim = c(0,1))
reg = lm(membership ~ income_thousands)
lines(income_thousands, predict(reg), col ='red', lwd = 3)
summary(reg)
#¿Tiene sentido el intercepto negativo? ¿La pendiente?

#Ejemplo de conteos (Poisson)
par(mfrow = c(1,1))
n = 1000
a = seq(0.01,0.99,length.out = n)
visitas_doctor = cbind(1:n,as.data.frame(qpois(a, lambda = 5)))
colnames(visitas_doctor) = c('a','visitas_doctor')
y = ddply(visitas_doctor, .(visitas_doctor), summarise, length(a))
plot(y, main = 'Distribución Poisson',
     ylab = 'individuos', 
     xlab = 'visitas al doctor en 5 años')
for (i in 1:dim(y)[1]) {
  lines(rep(y[i,1], 100), seq(0, y[i,2], length.out = 100))
}

#Efectos marginales no lineales dependen del valor del predictor ####
theme_set(theme_classic())
g <- ggplot(cancer, aes(diagnosis, radius_mean, group = diagnosis))
g + geom_boxplot(varwidth=T, fill="black") + 
  labs(title="Box plot", 
       subtitle="Radius of tumor by diagnosis",
       caption="Source: Breast cancer Wisconsin (UCI, ML, Kaggle)",
       x="Diagnosis (0:malignant, 1:bening)",
       y="Tumor Radius")
reg = lm(diagnosis ~ radius_mean, data = cancer)
summary(reg) #smaller radius reduces probability of malignant tumor
margins(reg)

reg_nl = lm(diagnosis ~ exp(radius_mean), data = cancer) 
#�Por que esta regresion sigue siendo lineal? solo es una transformacion de x (radius mean) no de la respuesta y (diagnosis)
summary(reg_nl)
margins(reg_nl)
#reg_nl = nls(diagnosis ~ exp(constant+beta*radius_mean), 
#             start = list(beta = 0, constant = 1), 
#             data = cancer) # lower = c(-1,-1), upper = c(1,3), algorithm = 'port')
#s = summary(reg_nl)
#s
#radius = seq(min(cancer[,'radius_mean']), 
#        max(cancer[,'radius_mean']), length.out = 200)
#betta = s$coefficients['beta','Estimate']
#margins_nl = exp(radius*betta)*betta #dydx of reg_nl formula
#plot(radius, margins_nl) #all radius
#mean(margins_nl) #Average dydx for all radius (row 1 table 5.2 cameron trivedi)
#radius_m = mean(cancer[,'radius_mean'])
#exp(radius_m*betta)*betta #dydx of the average radius (row 2 table 5.2 cameron trivedi)
#idx = sample.int(dim(cancer)[1], 1) #a representative radius
#radius_rep = cancer[idx,'radius_mean']
#exp(radius_rep*betta)*betta #dydx of a representative individual (row 3 table 5.2 cameron trivedi)
#Pregunta: ¿cual es mejor? ¿Por qué?

#Diferentes sentidos de likelihood ####
#data artificial (dgp)
nobs=100
stddev = 200
data_dgp = rnorm(nobs, 400, stddev)
plot(data_dgp)  

#Probabilidad
hipotesis_mean = 200 #asumamos que sabemos sd
den = dnorm(data_dgp, hipotesis_mean, stddev)
quantile(den, c(0.025, 0.5, 0.975))
l = prod(den) # likelihood de la data y (underflow, se va a cero)
l
ll = sum(log(den)) #log likelihood de la data (no hay underflow)
ll
#¿Es una buena hipótesis? ¿buen log likelihood?

#Optimizacion
hipotesis_mean = seq(1,1000, length.out = 100)
ll_hip = rep(0,length(hipotesis_mean))
for (i in 1:length(hipotesis_mean)){
  den = dnorm(data_dgp, hipotesis_mean[i], stddev)
  ll_hip[i] = sum(log(den)) #log likelihood de la data
}
plot(hipotesis_mean, ll_hip,
     ylab = 'log. likelihood',
     xlab = 'hipotesis testeadas',
     main = 'Likelihood L: p(parametros|data)')
idx = which(ll_hip == max(ll_hip))
points(hipotesis_mean[idx], ll_hip[idx], col = 'red', 
       pch = 16, cex = 2) #MLE: parametros que maximizan likelihood de la data
legend('bottomleft', 'likelihood f: p(data|parametros)', 
       col = 'red', pch = 16, bty = 'n')

# MLE ####
#linear
LL <- function(intercept, beta1, sigma) {
  mu = intercept + beta1*x 
  R = dnorm(y, mu, sigma)
  R[R==0] = 1^-300 #Hack to avoid infinity (beware: only for pedagogical purposes)
  #
  ll = -sum(log(R))
  if (is.infinite(ll)) {
    ll = 10^300
  } 
  ll
}
x = cancer[,'radius_mean']
y = cancer[,'diagnosis'] 
# plot(x, y)
a = mle2(LL, method = "L-BFGS-B",
         start = list(intercept=1, beta1=1, sigma = 1),
         lower = list(intercept=-1000, beta1=-1000, sigma =0.1))
a 
lm(y~x)
#Pregunta: 
#¿que pasa si cambiamos los valores iniciales del optimizador? 
#tip practico (hack): Usar varios valores iniciales o que tengan
#sentido desde conocimiento del arte


#logit
LL <- function(intercept, beta1) {
  p = exp(intercept + x*beta1)/(1+exp(intercept + x*beta1)) 
  R = dbinom(y, 1, p)
  R[R==0] = 1^-300 #Hack to avoid infinity (beware: only for pedagogical purposes)
  #
  ll = -sum(log(R))
  if (is.infinite(ll)) {
    ll = 10^300
  } 
  ll
}
x = cancer[,'radius_mean']
y = cancer[,'diagnosis'] 
# plot(x, y)
a = mle2(LL, method = "L-BFGS-B",
         start = list(intercept=1, beta1=1),
         lower = list(intercept=-1000, beta1=-1000))
a 
b=glm(y~x, family = binomial(link=logit)) 
BIC(b)
#Interpretar coeficientes ... más profundiad en ejemplo de elecciones en diapositivas 

p = 1/(1+exp(-(-1.4+0.33*2)))
p2 =1/(1+exp(-(-1.4+0.33*3)))
((1/(1-p))/(1/(1-p2)))^-1




#Poisson traffic accidents ####
par(mfrow = c(1,1))
n = 1000
a = seq(0.01,0.99,length.out = n)
accidentes = cbind(1:n,as.data.frame(qpois(a, lambda = 25)))
colnames(accidentes) = c('a','accidentes')
y = ddply(accidentes, .(accidentes), summarise, length(a))
plot(y, main = 'Interseccion i: \n cll 45 con 13',
     ylab = 'conteo', 
     xlab = 'accidentes')
for (i in 1:dim(y)[1]) {
  lines(rep(y[i,1], 100), seq(0, y[i,2], length.out = 100))
}

#Poisson police stops ####
#stops es cuantas veces la policia para a una persona para requisar
# constant term
fit.1 <- glm (stops ~ 1, family=poisson, offset=log(past.arrests),
              data=Police)
#¿Por que usamos para offset past.arrests? 
#¿Por qué es un baseline o exposure? No hay respuesta única, pero defienda.
summary(fit.1) 

# ethnicity indicator
fit.2 <- glm (stops ~ factor(eth), family=poisson, offset=log(past.arrests),
              data=Police)
summary(fit.2) 
coeftest(fit.2, vcov = sandwich) #Robust standard error
#Interpretar coeficientes exp(coef).  eth 1=black,	2=hispanic,	3=white.	 
#(recordar: es relativo al baseline i.e. la categoria que no aparece eth:1:black)
#¿A quíen arrestan más que los negros? ¿Hispanos o blancos? Ver signos.
#Comparar AIC con modelo nulo de solo intercepto (menor es mejor) para ver si vale la pena meter un regresor de ethnicity

# ethnicity & precints indicators
fit.3 <- glm (stops ~ factor(eth) + factor(precinct) , family=poisson,
              offset=log(past.arrests), data=Police)
summary(fit.3) 
#Interpretar coeficientes de etnicidad. 
#¿Por qué hispanicos ya no es significativo?
#¿Por qué ahora es más negativo el coeficiente para blancos eth:3?
#Comparar AIC con modelo de solo etnicidad (menor es mejor)
#Se podria analizar los coef. de precinto pero sin informacion adicional sobre ellos es dificil

# overdispersion: 
#the variance of the data could be much higher than the model (i.e. large residuals)
# Reason: the poisson distribution does not have a variance parameter.
# In such cases one can use the quasipoisson family that has a variance parameter
fit.4 <- glm (stops ~ factor(eth) + factor(precinct) , family=quasipoisson,
              offset=log(past.arrests), data=Police)
summary(fit.4) #the dispersion parameter is factor by which the variance of the (count) model increases

#OLS NLS MLE  ####
nobs = 10000
beta1 = 2
beta2 = -1
#dgp opcion 1
ndis = 50000
x = rnorm(ndis, 1, 1)
y = seq(0,100,length.out = ndis)
lambda = exp(beta1 + beta2*x)
deny = lambda*exp(-lambda*y) #density of y
deny = deny/sum(deny) #so they add to 1
y_sample = sample(y,nobs,replace = T, prob=deny)
mean(y_sample)
sd(y_sample)
#dgp opcion 2
x = rnorm(nobs, 1, 1)
lambda = exp(beta1 + beta2*x)
y_sample = rexp(nobs, lambda)
mean(y_sample)
sd(y_sample)
#regressions
OLS = lm(y_sample~x)
summary(OLS)
coeftest(OLS, vcov = sandwich) #Robust standard error
NLS = nls(y_sample~exp(-(b1 + b2*x)),
          start = list(b1=1, b2=1)) 
summary(NLS)  
coeftest(NLS, vcov = sandwich) #Robust standard error
plot(x, y_sample,
     xlab = 'x e.g. ingreso', ylab = 'y e.g. salud')
lines(x, predict(OLS), col = 'red', lwd = 2)
lines(sort(x), sort(predict(NLS)), col = 'blue', lwd = 2)
legend('topleft',c('OLS','NLS'),bty='n',lty=1,col=c('red','blue'))
#Compare y explique la diferencia de los coeficientes OLS y NLS   
  

#Model selection (specification tests) ####
nobs = 1000
beta1 = 2
beta2 = -1
x = rnorm(nobs, 1, 1)
lambda = exp(beta1 + beta2*x)
y_sample = rexp(nobs, lambda)
NLS = nls(y_sample~exp(-(b1 + b2*x)),
          start = list(b1=1, b2=1))
OLS = nls(y_sample~b0 + b1*x,
          start = list(b0=1,b1=1))
#likelihood ratio test
lrtest(OLS, NLS) #p<0.05 significa que el Model 2 es mejor i.e. el LogLik es estadisticamente mayor

#waldtest
waldtest.default(OLS,NLS) #p<0.05 significa que el Model 2 es mejor i.e. el wald test se aproxima a un t test ... pero NO es, la estadistica es diferente

#Information criterions (AIC, BIC, etc)
AIC(OLS)
AIC(NLS) #diferencias mayores a 5 son relevantes

BIC(OLS)
BIC(NLS)

#Classification tests (for binary models)
#Fuente: https://stats.stackexchange.com/questions/4832/logistic-regression-classification-tables-a-la-spss-in-r
# generate some data
N <- 100
X <- rnorm(N, 175, 7)
Y <- 0.4*X + 10 + rnorm(N, 0, 3)

# dichotomize Y
Yfac <- cut(Y, breaks=c(-Inf, median(Y), Inf), labels=c("lo", "hi")) #i.e. lo:[-Inf, median(Y)]; hi:[median(Y), Inf]

# logistic regression
glmFit <- glm(Yfac ~ X, family=binomial(link="logit"))

# predicted probabilities
Yhat <- fitted(glmFit)

# choose a threshold for dichotomizing according to predicted probability
thresh  <- 0.5
YhatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("lo", "hi")) #i.e. lo:[-Inf, thr]; hi:[thr, Inf]

# contingency table and marginal sums
cTab <- table(Yfac, YhatFac)
cTab
addmargins(cTab)
YhatFac

# percentage correct for training data
sum(diag(cTab)) / sum(cTab) #Las diagonales de la tabla son hits i.e. clasificaciones correctas


#Propensity score matching ####
#reshape the database so that each household appears in only one row
#and by round i.e. put the dataset in wide format
cols = c('household_identifier','enrolled','health_expenditures','age_hh','age_sp',
         'educ_hh','educ_sp','female_hh', 'indigenous', 'hhsize', 
         'dirtfloor', 'bathroom', 'land', 'hospital_distance')
idx = HISP[,'round']==0 #matching based on baseline characteristics
HISP_baseline = HISP[idx, cols] 
#clasificar households en funcion de unas caracteristicas
# The ratio command indicates one-to-one matching i.e
# every treatment case will be matched with one control
# case
m.out = matchit(enrolled ~ age_hh + age_sp + educ_hh + educ_sp + female_hh + 
                  indigenous + hhsize + dirtfloor + bathroom + land +
                  hospital_distance,
                data = HISP_baseline, method = "nearest", #matches a treated unit to a control unit in terms of a distance measure such as a logit
                distance = 'glm', link = 'probit')
#Otros metodos:
#(method = 'exact') exactly the same values on each covariate
#(method = 'subclass')  breaks the data set into subclasses such that the distributions of the covariates are similar in each subclass 
#(method = 'optimal') minimizes the average distance across all unit. requires the optmatch package
#(method = 'genetic') genetic algorithm to match units. requires the Matching package
#(method = 'cem') Coarse exact matching i.e matches on a covariate while trying to balance the other covariates 

summary(m.out)
summary(m.out$model) #Table 8.1 Gertler et al 2016 world bank book
plot(m.out, type = "jitter")
#The next one is Figure 8.3 i.e. Raw Treated vs Raw Control.
#Note that the common support assumption is valid i.e. the propensity
#score for control and treatment overlap
plot(m.out, type = "hist") 
mtch.data = match.data(m.out) #IMPORTANTE: esta es la data con grupos ya matcheados
#propensity score is in the distance column, 
#the following is is to show explicitly how to obtain it
mtch.data$prscore = predict(m.out$model, type = "response", #response para transformar probit scale a response scale
                            newdata = mtch.data) 
sum(mtch.data$prscore - mtch.data$distance)

#Con la data matched se puede hacer análisis estadisticos regulares
#Pero primero hay que traer los datos del tiempo 1 (round 1)
mtch = as.numeric(m.out$match.matrix[,1]) #row numbers from HISP used to matched samples. If you named the rows, then it would be names.
treat = as.numeric(row.names(m.out$match.matrix))
HISP_match = HISP[c(mtch, treat),] #at round 0
for (i in 1:length(mtch)){ #at round 1
  hh = HISP[mtch[i],'household_identifier']
  idx = HISP[,'household_identifier'] == hh
  temp = HISP[idx,]
  idx = as.numeric(rownames(temp)) != mtch[i]
  HISP_match = rbind(HISP_match,temp[idx,])
}
for (i in 1:length(treat)){ #at round 1
  hh = HISP[treat[i],'household_identifier']
  idx = HISP[,'household_identifier'] == hh
  temp = HISP[idx,]
  idx = as.numeric(rownames(temp)) != treat[i]
  HISP_match = rbind(HISP_match,temp[idx,])
}

idx0 = HISP_match[,'enrolled'] == 0 & HISP_match[,'round'] == 0
idx1 = HISP_match[,'enrolled'] == 1 & HISP_match[,'round'] == 0
t.test(x = HISP_match[idx0,'health_expenditures'],
       y = HISP_match[idx1,'health_expenditures']) 
#Los treated gastan menos en salud en promedio en el tiemp 0, pero por muy poco

idx0 = HISP_match[,'enrolled'] == 0 & HISP_match[,'round'] == 1
idx1 = HISP_match[,'enrolled'] == 1 & HISP_match[,'round'] == 1
t.test(x = HISP_match[idx0,'health_expenditures'],
       y = HISP_match[idx1,'health_expenditures']) 
#Pero mucho menos en el tiempo 1 cuando ya tomo efecto el enroll (al menos en terminos descriptivos, hay que hacer otros analisis e.g. ANOVAS)

#La política de enroll, haciendo match, parece menos fuerte, evitamos un sesgo
idx0 = HISP[,'enrolled'] == 0 & HISP[,'round'] == 1
idx1 = HISP[,'enrolled'] == 1 & HISP[,'round'] == 1
t.test(x = HISP[idx0,'health_expenditures'],
       y = HISP[idx1,'health_expenditures'])

#Podemos hacer diferencia en diferencias con la nueva base de datos
fit = lm(health_expenditures~round*enrolled + age_hh + age_sp + 
           educ_hh + educ_sp + female_hh + indigenous + hhsize + 
           dirtfloor + bathroom + land + hospital_distance,
         data = HISP_match)
summary(fit) #La interaccion es el diff in diff 
#¿Qué significa? ¿Por qué el contrafactual es round?


#Multinomial M and M ####
#MLE
LL <- function(prBlue, prBrown, prGreen, prOrange, 
               prRed, prYellow) {
  R = rep(0, dim(mandm)[1])
  probs = c(prBlue, prBrown, prGreen, prOrange, 
            prRed, prYellow)
  for (i in 1:dim(mandm)[1]){
    pckg_content = mandm[i,2:dim(mandm)[2]]
    R[i] = dmultinom(pckg_content,
                     sum(pckg_content),
                     probs)
  }
  R[R==0] = 1^-300 #Hack to avoid infinity (beware: only for pedagogical purposes)
  #
  ll = -sum(log(R))
  if (is.infinite(ll)) {
    ll = 10^300
  } 
  ll
}

a = mle2(LL, method = "L-BFGS-B",
         start = list(prBlue=1, prBrown=1, prGreen = 1,
                      prOrange=1, prRed=1, prYellow = 1),
         lower = list(prBlue=0, prBrown=0, prGreen = 0,
                      prOrange=0, prRed=0, prYellow = 0))
summary(a)
d = coef(a)/sum(coef(a)) #estimated 
b = apply(mandm[,2:dim(mandm)[2]], 2, sum) #data
plot(d, ylim = c(0,0.25))
points(b/sum(b), col = 'red') 


#Multinomial storable votes (ordinal logistic regression) ####
unique(storable[,'person'])
idx = storable[,'person'] == 601
y = storable[idx,'vote']
x = storable[idx,'value']
fit = polr(factor(y)~x,
     start = c(1, 32, 58), #format c(coefficients, zeta); zeta are the cutpoints
     method = 'logistic') #methods: "logistic", "probit", "loglog", "cloglog", "cauchit"
#pg 122 Gelman Hill to transform to interpretable scale
s = summary(fit)
betta = s$coefficients[1,1]
cutpoints = s$zeta/betta #cutpoints in the scale of x i.e. value
siggma = 1/betta

#votos esperados
invlogit = function(x){
  1/(1+exp(-x))
}
expected <- function (x, c1.5, c2.5, sigma){ 
  p1.5 <- invlogit ((x-c1.5)/sigma) #PREGUNTA: Por qué dividido por sigma? el fit uso una distribución logistic y el input de la CDF de una logistic es (x-mu)/sigma. Usamos la CDF por qué nos interesa las probabilidades, no las densidades pdf
  p2.5 <- invlogit ((x-c2.5)/sigma)
  return ((1*(1-p1.5) + 2*(p1.5-p2.5) + 3*p2.5)) #ver grafica 'Multinomial' que puse en latex, la barra con naranja y azul
}
plot(x,expected(x, cutpoints[1], cutpoints[2], siggma), type = 'l',
     xlab = 'Potential gain if proposal passes',
     ylab = 'Expected votes for the proposal on stage 1',
     ylim = c(1,3)) #model
points(x,y) #data
lines(rep(cutpoints[1],100), seq(0,4,length.out = 100), col ='red') #cutpoint1 (antes del cutpoint 1,  un voto)
lines(rep(cutpoints[2],100), seq(0,4,length.out = 100), col ='red') #cutpoint2 (entre cutpoint 1 y 2, dos votos; después de cutpoint 2 tres votos)


# Multinomial fishing (non-ordinal logistic regression) ####
summary(fishing)
#fishing[,'mode'] = as.character(fishing[,'mode'])
ddply(fishing, .(mode), summarise, 
      N = length(mode),
      probs = length(mode)/dim(fishing)[1],
      mean_income = mean(income, na.rm = T),
      sd_income = sd(income, na.rm = T))
fit = nnet::multinom(mode ~ income, data = fishing)
fitnull = nnet::multinom(mode ~ 1, data = fishing)
summary(fit)
logLik(fit)
anova(fit,fitnull) 
#Los coeficientes se pueden interpretar como los del modelo binario
#Es decir, si se exponencian nos dan odds ratio, relativo a la categoria base beach (o la que no aparezca)
#Ver columna income: Un valor < 1 es menor ingreso relativo a beach (en escala odds ratio) 
exp(coef(fit)) 

bettas = coef(fit)
#predicted probabilities (manualmente)
a=list()
p_I = fishing[,'income']
denominator = (exp(0)+exp(bettas['pier',1] + bettas['pier',2]*p_I)+
    exp(bettas['private',1] + bettas['private',2]*p_I)+
    exp(bettas['charter',1] + bettas['charter',2]*p_I))
a$`Pr(pier)` = exp(bettas['pier',1] + bettas['pier',2]*p_I)/denominator
a$`Pr(beach)` = exp(0)/denominator
a$`Pr(charter)` = exp(bettas['charter',1] + bettas['charter',2]*p_I)/denominator
a$`Pr(private)` = exp(bettas['private',1] + bettas['private',2]*p_I)/denominator


#predicted probabilities (con una funcion de R)
a = prediction(fit) #Calcula todas las p_P de los modos de pesca e.g. en a$`Pr(private)`

#marginals en el promedio del regresor de modo private
m_I = mean(fishing[,'income'])
denominator = (exp(0)+exp(bettas['pier',1] + bettas['pier',2]*m_I)+
                 exp(bettas['private',1] + bettas['private',2]*m_I)+
                 exp(bettas['charter',1] + bettas['charter',2]*m_I))
Pr_m = exp(bettas['private',1] + bettas['private',2]*m_I)/denominator
denominator = (exp(0)+exp(bettas['pier',1] + bettas['pier',2]*(m_I+1))+
                 exp(bettas['private',1] + bettas['private',2]*(m_I+1))+
                 exp(bettas['charter',1] + bettas['charter',2]*(m_I+1)))
Pr_m1 = exp(bettas['private',1] + bettas['private',2]*(m_I+1))/denominator
marginal = Pr_m1-Pr_m
marginal

#otra forma
m_I = fishing[,'income']
denominator = (exp(0)+exp(bettas['pier',1] + bettas['pier',2]*m_I)+
                 exp(bettas['private',1] + bettas['private',2]*m_I)+
                 exp(bettas['charter',1] + bettas['charter',2]*m_I))
Pr_m = exp(bettas['private',1] + bettas['private',2]*m_I)/denominator
denominator = (exp(0)+exp(bettas['pier',1] + bettas['pier',2]*(m_I+1))+
                 exp(bettas['private',1] + bettas['private',2]*(m_I+1))+
                 exp(bettas['charter',1] + bettas['charter',2]*(m_I+1)))
Pr_m1 = exp(bettas['private',1] + bettas['private',2]*(m_I+1))/denominator
marginal = Pr_m1-Pr_m
mean(marginal)
sd(marginal)


#Lo que sigue son ejemplos de la función mlogit (i.e. en help)
## Cameron and Trivedi's Microeconometrics p.493 There are two
## alternative specific variables : price and catch one individual
## specific variable (income) and four fishing mode : beach, pier, boat,
## charter
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), 
                    shape = "wide", choice = "mode")
data("Fishing", package = "mlogit") #THIS HAS INCOME IN dollars (not in thousands as mus15data.dat)

## a pure "conditional" model
m = mlogit(mode ~ price + catch, data = Fish)
summary(m) 
effects(m, covariate = 'price') #marginal effects
predict(m) #predicted probabilities of picking each mode

## a pure "multinomial model"
m = mlogit(mode ~ 0 | income, data = Fish)
summary(m)
effects(m, covariate = 'income') #marginal effects
predict(m) #predicted probabilities of picking each mode

## which can also be estimated using multinom (package nnet)
summary(multinom(mode ~ income, data = Fishing))

## a "mixed" model
m <- mlogit(mode ~ price + catch | income, data = Fish) #price and catch are alternative specific. Variable after the symbol | indicate alternative invariant regressors
summary(m)
#los coeficientes alternative specific (e.g. price, catch) se interpretan asi:
#Si es positivo para la categoria aumenta y para las otras disminuye 
#e.g.catch es positivo en summary(m), quiere decir que si la cantidad de peces
#capturados aumenta en todas las alternativas la decision de pescar en esa alternativa
#aumenta y para las otras disminuye.
#los coeficientes alternative invariante (e.g. income) se interpretan como una logit tradicional
predict(m) 
mean(fitted(m)[Fishing[,'mode']=='boat'])

## same model with charter as the reference level
m <- mlogit(mode ~ price + catch | income, data = Fish, reflevel = "charter")

## same model with a subset of alternatives : charter, pier, beach
m <- mlogit(mode ~ price+ catch | income, data = Fish,
            alt.subset = c("charter", "pier", "beach"))


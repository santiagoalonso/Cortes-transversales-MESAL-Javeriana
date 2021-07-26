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
pkgload('rdd')
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
devtools::install_github("dgrtwo/gganimate")
library(gganimate)
pkgload("ggExtra")
pkgload('ggcorrplot')
pkgload('scales')
pkgload('zoo')
pkgload('ggdendro')
pkgload('ggfortify')
pkgload('pwr')
pkgload('ggpubr')


#Data #####
#HISP ((Health Insurance Subsidy Program))
wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/HISP/' #working directory
HISP <- read.dta13(paste(wwdd,"evaluation.dta", sep=''))
#Description of columns HISP 
coldes_HISP = cbind(attributes(HISP)$names, attributes(HISP)$var.labels) #Variable description


#MEPS (Medical Expenditure Panel Survey) (del libro health econometrics using stata)
wwdd = '/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/' #working directory
MEPS <- read.dta(paste(wwdd,"heus_mepssample.dta", sep=''))
#Description of columns MEPS 
coldes_MEPS = cbind(attributes(MEPS)$names, attributes(MEPS)$var.labels) #variable description
MEPS_p = MEPS[MEPS[,'exp_tot']>0,] #only positive total expenditures
colnam = colnames(MEPS_p)

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


#LINEAR REGRESSION ####
#Punto 1 (heterocedasticidad):
data = read.csv("E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/heterocedasticidad.csv")
head(data)
x = data[,'income']
plot(x, data[,'foodexp'])
reg = lm('foodexp ~income', data = data)
summary(reg)
bptest(reg) #test heteroskedasticity; si p<0.05 hay heteroskedasticity
reg_rB = lm_robust(foodexp ~ income, data=data)
summary(reg_rB)
y = predict(reg)
lines(x, y, col = 'red', lwd = 2)

#Punto 2 (diff in diff): ####
basque = read.csv("E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/basque.csv")
head(basque)
basque_reducida = filter(basque, 
                         regionname == "Basque Country (Pais Vasco)" |
                         regionname == "Cataluna")
plot(basque_reducida[,'year'],
     basque_reducida[,'gdpcap'])
g = ggplot(data = basque_reducida)
g = g + geom_line(aes(x=year,y=gdpcap, color = regionname))
g
post = ifelse(basque_reducida[,'year']<1975,0,1)
treat = ifelse(basque_reducida[,'regionname']== "Cataluna",0,1)
basque_reducida = add_column(basque_reducida, post, treat)

did = ddply(basque_reducida, .(post, treat), summarise, outcome = mean(gdpcap))
idxx = did[,'treat'] == 1 & did[,'post'] == 0 
antes_treat = did[idxx,'outcome']
idxx = did[,'treat'] == 1 & did[,'post'] == 1
despues_treat = did[idxx,'outcome']
idxx = did[,'treat'] == 0 & did[,'post'] == 0
antes_control = did[idxx,'outcome']
idxx = did[,'treat'] == 0 & did[,'post'] == 1
despues_control = did[idxx,'outcome']
(despues_treat - antes_treat) - (despues_control-antes_control)
reg = lm(gdpcap~post*treat, data = basque_reducida)
summary(reg) #comparar interacci�n con calculo manual (deben ser iguales)


#Punto 3 (Reg. discontinuity): ####
BAC = read.csv("E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/BAC.csv")
head(BAC)
colnames(BAC)
umbral_nas = BAC[1, 'Umbral_NAS']
genero = ggplot(data=BAC) +
  geom_point(aes(y=Genero_promedio,x=Nivel_Alcohol_Sangre_NAS)) +
  geom_vline(xintercept = umbral_nas, color = 'red', linetype="dashed")
edad = ggplot(data=BAC) +
  geom_point(aes(y=Edad_promedio_estandarizada,x=Nivel_Alcohol_Sangre_NAS)) +
  geom_vline(xintercept = umbral_nas, color = 'red', linetype="dashed")
raza = ggplot(data=BAC) +
  geom_point(aes(y=Raza_blanco_promedio,x=Nivel_Alcohol_Sangre_NAS)) +
  geom_vline(xintercept = umbral_nas, color = 'red', linetype="dashed")
acci = ggplot(data=BAC) +
  geom_point(aes(y=Accidente_en_escena_promedio,x=Nivel_Alcohol_Sangre_NAS)) +
  geom_vline(xintercept = umbral_nas, color = 'red', linetype="dashed")
ggarrange(genero, edad, raza, acci,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
Distancia_a_umbral = BAC[, 'Nivel_Alcohol_Sangre_NAS'] - BAC[, 'Umbral_NAS']
BAC = add_column(BAC, Distancia_a_umbral)

BAC_izq = filter(BAC, 
                 Nivel_Alcohol_Sangre_NAS <= Umbral_NAS)
BAC_der = filter(BAC, 
                 Nivel_Alcohol_Sangre_NAS > Umbral_NAS)
reg_izq = lm(Reincidencia_promedio~Nivel_Alcohol_Sangre_NAS, data = BAC_izq)
pred_izq = predict(reg_izq)
reg_der = lm(Reincidencia_promedio~Nivel_Alcohol_Sangre_NAS, data = BAC_der)
pred_der = predict(reg_der)
BAC_izq = add_column(BAC_izq, pred_izq)
BAC_der = add_column(BAC_der, pred_der)


g = ggplot(data = BAC) + 
  geom_point(aes(x = Nivel_Alcohol_Sangre_NAS, y =Reincidencia_promedio)) +
  geom_line(data = BAC_izq, aes(x=Nivel_Alcohol_Sangre_NAS, y = pred_izq),
            color = 'forestgreen') +
  geom_line(data = BAC_der, aes(x=Nivel_Alcohol_Sangre_NAS, y = pred_der),
            color = 'red')
g

reg_fin = lm_robust(Reincidencia_promedio ~ Incumplio_umbral*Distancia_a_umbral, 
                    data = BAC)
summary(reg_fin)

reg_fin = lm_robust(Reincidencia_promedio ~ Incumplio_umbral*Distancia_a_umbral + 
                      Genero_promedio + Edad_promedio_estandarizada + 
                      Raza_blanco_promedio + Accidente_en_escena_promedio, 
                    data = BAC)
summary(reg_fin)


bw = 0.02 #bandwidth NOTA: esto es una heuristica, hay otras que incluyen kernels triangulares, gaussianos, otros!
bw_neg = BAC[,'Umbral_NAS'] - bw
BAC = add_column(BAC, bw_neg) 
bw_pos = BAC[,'Umbral_NAS'] + bw
BAC = add_column(BAC, bw_pos) 
dataT = filter(BAC, Nivel_Alcohol_Sangre_NAS>Umbral_NAS & Nivel_Alcohol_Sangre_NAS<bw_pos)
R = mean(dataT[,'Reincidencia_promedio']) #derecha del umbral
dataT = filter(BAC,Nivel_Alcohol_Sangre_NAS<=Umbral_NAS & Nivel_Alcohol_Sangre_NAS>bw_neg)
L = mean(dataT[,'Reincidencia_promedio']) #izquierda del umbral
round(R-L,4) #LATE


# Punto 1: MEPS hacer tabla descriptivas (mean,sd, skewness,kurtosis,min,max) #####
# idx = c('exp_tot', 'exp_ip','exp_ip_fac', 'exp_ip_md', 'exp_er',
#         'exp_er_fac','exp_er_md','exp_dent','exp_self') #manually
# variables de gasto (expenditure)
idx = colnam[grepl('exp_*', colnam)] #with regular expressions
descriptives_gastos = t(as.data.frame(lapply(MEPS_p[,idx],
                                             each(mean,sd, skewness,kurtosis,min,max), na.rm = T)))
descriptives_gastos #ver descripciones en coldes_MEPS
#variables de uso
idx = colnam[grepl('use_*', colnam)] #with regular expressions
descriptives_use = t(as.data.frame(lapply(MEPS_p[,idx],
                                             each(mean,sd, skewness,kurtosis,min,max), na.rm = T)))
descriptives_use #ver descripciones en coldes_MEPS

#variables demograficas
idx = c('age','female', 'race_bl', 'race_oth', 'eth_hisp','famsize',
        'ed_hs','ed_hsplus','ed_col','ed_colplus', 'lninc',
        'reg_midw','reg_south','reg_west')
descriptives_dem = summary(MEPS_p[,idx]) #dem. variables have factors, so I use summary instead
descriptives_dem #ver descripciones en coldes_MEPS

#otras variables 
idx = c('anylim','mcs12','pcs12','ins_mcare','ins_mcaid',
        'ins_unins','ins_dent')
descriptives_otras = summary(MEPS_p[,idx]) #dem. variables have factors, so I use summary instead
descriptives_otras #ver descripciones en coldes_MEPS


#punto 2: MEPS hacer histogramas #####
par(mfrow = c(2,2))
a = hist(MEPS[,'exp_tot'], freq = F,
         main = 'Total expenditures',
         xlab = 'Expenditures',
         ylab = 'Fraction') #No parece normal, colas gordas
a = hist(log(MEPS[,'exp_tot']), freq = F,
         main = 'Total log(expenditures)',
         xlab = 'Log(Expenditures)',
         ylab = 'Fraction') #parece log-normal
par(mfrow = c(2,2))
a = hist(MEPS[,'use_off'], freq = F,
         main = 'Office-based provider visits',
         xlab = 'Expenditures',
         ylab = 'Fraction')
a = hist(MEPS[,'use_dent'], freq = F,
         main = 'Dental visits',
         xlab = 'Expenditures',
         ylab = 'Fraction')
a = hist(MEPS[,'use_rx'], freq = F,
         main = 'Prescriptions and refills',
         xlab = 'Expenditures',
         ylab = 'Fraction')

#Punto 3: MEPS regresion y plots marginales####  
reg = lm_robust(exp_tot ~ age*female + anylim,
               data = MEPS_p) #Computes robust standard errors 
summary(reg)
summary(margins(reg)) #this is dy/dx i.e. Women spend $523 more than men and 1 additional year increased expenditure in $81.38 
#otra opcion
reg = lm(exp_tot ~ age*female + anylim,
           data = MEPS_p)
coeftest(reg, vcov=sandwich) #Computes robust standard errors
waldtest.default(reg, vcov=sandwich, test ='F')
summary(margins(reg, vcov = sandwich(reg)))
summary(reg)

#These are descriptive plots! no control for anything
g <- ggplot(MEPS_p, aes(female, exp_tot))
g + geom_boxplot(varwidth=T, fill="orange") + 
  labs(title="Box plot", 
       subtitle="Expenditure by gender",
       caption="Source: MEPS",
       x="Gender",
       y="Expenditure") +
  coord_cartesian(xlim=c(0,3), ylim=c(0, 8000))
ddply(MEPS_p, .(female),summarise,m=median(exp_tot))

g <- ggplot(MEPS_p, aes(x=age, y=exp_tot)) + geom_smooth(method="lm")+
  coord_cartesian(xlim=c(18,89), ylim=c(0, 12000))
plot(g)
lm(exp_tot ~ age + anylim,
   data = MEPS_p)



#Punto 3.1: quantile regression ####
summary(MEPSct_p[,c('ltotexp', 'suppins','totchr','age','female','white')])
q1 = 0.5 
reg_q1 = rq(ltotexp~suppins + totchr + age + female + white, 
            tau = q1, data = MEPSct_p) 
summary(reg_q1)
summary(reg_q1, se = 'boot', R = 200) #kind of robust s.e.
#pregunta: cambiar R, numero de bootstraps. Comentar.
#graficar esta regresion tip: usar predict (ver LinearRegression.R)
#Pregunta: interprete los coeficientes
#Haga la regresion para los siguientes cuantiles y grafique: .25, .5, .75
#Compare y comente los coeficientes de los diferentes cuantiles. 
#comparar cuantiles
q2 = c(0.1, 0.5, 0.75)
reg_q2 = rq(ltotexp~suppins + totchr + age + female + white, 
            tau = q2, data = MEPSct_p) 
summary(reg_q2)
anova(reg_q2)
plot(summary(reg_q2)) #eje x cuantiles, eje y pendientes, rojo OLS

#Si las pendientes son diferentes ¿hay heterostacidad o no?

#test de heterostacidad
reg = lm(ltotexp~suppins + totchr + age + female + white,
         data = MEPSct_p)
bptest(reg) 
#¿Qué nos dice este test? p<0.05 rechaza homoskedasticity

#hacer lo mismo de arriba con esta data simulada
n = 10000
x2 = rchisq(n,1)
x3 = 5*rnorm(n)
e = 5*rnorm(n)
u = (0.1 + 0.5*x2)*e
y = 1 + 1*x2 + 1*x3 + u


#punto 4: Otra Regresion MEPS (solo con las primeras 100 observaciones)#### 
#compute 3 errores standard (tradicional, heteroskedastic, cluster por totchr) y comente si hay diferencias
dataT = MEPSct_p[1:100,]
# dataT=dataT[-which(dataT[,'famsze']==8 | dataT[,'famsze']==13),] #8 and 13 only have one observation
dataT[,'famsze'] = as.factor(dataT[,'famsze'])
reg = lm(ltotexp ~ suppins + phylim + actlim + totchr + age +
                  female + income + famsze + income:famsze,
                data = dataT) 
summary(reg) #Computes regular OLS standard errors 
coef_test(reg, vcov='CR1', cluster = dataT$totchr) #Computes cluster standard errors 
reg = lm_robust(ltotexp ~ suppins + phylim + actlim + totchr + age +
                  female + income + famsze + income:famsze,
                data = dataT) 
summary(reg) #Computes robust heteroskedastic standard errors 

#punto 5: MEPS out-of-sample prediction ####
#Escoja 1500 puntos aleatorios, haga la regresion, y mire si predice el resto
idx = sample.int(dim(MEPSct_p)[1], 1500)
idx2 = setdiff(1:dim(MEPSct_p)[1],idx)
dataP = MEPSct_p[idx,] #to fit
dataP=dataP[-which(dataP[,'famsze']==8 | dataP[,'famsze']==13),] #8 and 13 only have one observation
dataP[,'famsze'] = as.factor(dataP[,'famsze'])
dataOS = MEPSct_p[idx2,] #out-of-sample prediction
dataOS=dataOS[-which(dataOS[,'famsze']==8 | dataOS[,'famsze']==13),] #8 and 13 only have one observation
dataOS[,'famsze'] = as.factor(dataOS[,'famsze'])
reg = lm(ltotexp ~ suppins + phylim + actlim + totchr + age +
                  female + income + famsze + income:famsze,
                data = dataP) 
pred = predict(reg, newdata=dataOS)
cor(pred, dataOS[,'ltotexp']) #alta es buena prediccion
plot(pred, dataOS[,'ltotexp'], 
     xlab='Model prediction', ylab = 'Actual values')

#Escoja los primeros 1500 puntos, haga la regresion, y mire si predice el resto
#Punto tricky: La data esta organizada por totexp!! por eso la prediccion mediocre
idx = 1:1500
idx2 = 1501:dim(MEPSct_p)[1]
dataP = MEPSct_p[idx,] #to fit
dataP=dataP[-which(dataP[,'famsze']==8 | dataP[,'famsze']==13),] #8 and 13 only have one observation
dataP[,'famsze'] = as.factor(dataP[,'famsze'])
dataOS = MEPSct_p[idx2,] #out-of-sample prediction
dataOS=dataOS[-which(dataOS[,'famsze']==8 | dataOS[,'famsze']==13),] #8 and 13 only have one observation
dataOS[,'famsze'] = as.factor(dataOS[,'famsze'])
reg = lm(ltotexp ~ suppins + phylim + actlim + totchr + age +
           female + income + famsze + income:famsze,
         data = dataP) 
pred = predict(reg, newdata=dataOS)
cor(pred, dataOS[,'ltotexp']) 
plot(pred, dataOS[,'ltotexp'], 
     xlab='Model prediction', ylab = 'Actual values')



#haga gráficas de diagnóstic
residualss = dataT[,'ltotexp'] - reg$fitted.values
plot(reg$fitted.values, residualss) #no extreme outliers i.e. residuals kind of clustered
reg = lm(ltotexp ~ suppins + phylim + actlim + totchr + age +
           female + income + famsze + income:famsze,
         data = dataT) #to do leverage plots
leveragePlots(reg) #outlier influence: not a single plot seems to have an outlier driving the slope of the line; no single variable seem to drive the regression results



#punto 6: MEPS instrumental variable ####
#Individuos mayores de 65 años la mayoria con Medicare
#exogenous regressors: totchr age female blhisp linc (ver colde_MEPSins)
#PREG. ¿por qué son exogenos? ¿linc? linc porque ya no trabajan, la mayoria en pension
#endogenous regressor: hi_empunion adicional al medicare tener employer or union-sponsored health insurance
#PREG. ¿por qué hi_empunion es endogeno? comprar otro seguro adicional a medicare puede indicar mayores ingresos que se relaciona con ldrugexp
#dependent: ldrugexp (out-of-pocket expenses on medications)
summary(MEPSins[,c('ldrugexp','hi_empunion','totchr',
                      'age', 'female', 'blhisp', 'linc')])
#potential instruments for hi_empunion. 
#ssiratio: social security income / individual income
#lowincome: low income status
#firmsz: size of the firm (labor force)
#multlc: firm is large with multiple locations
#PREG.: ¿son instrumentos validos?
summary(MEPSins[!is.na(MEPSins[,'linc']),
                c('ssiratio','lowincome','firmsz','multlc')])

#regression con instrumento ssiratio
firststage = lm_robust(hi_empunion ~ ssiratio + totchr + age + female + 
                         blhisp + linc, data = MEPSins)
summary(firststage)
twoslsA = ivreg(ldrugexp ~ hi_empunion + totchr + age + female + 
                 blhisp + linc|ssiratio + totchr + age + female + 
                 blhisp + linc, data = MEPSins)
summary(twoslsA, vcov = sandwich, diagnostics = TRUE) #Robust s.e
#Pregunta: endogeneity test: ¿Qué signifca que Wu-Hausman sea significativo? hi_empunion es endogena
#Pregunta: hacer una OLS tradicional y comparar. ¿Es más fuerte o débil el efecto de hi_empunion?¿mismo signo? por qué

#NOTE: in ivreg endogenous variables (X) can only appear before 
#the vertical line; instruments (Z) can only appear after the 
#vertical line; exogenous regressors that are not instruments (W)
#must appear both before and after the vertical line.
#Pregunta: ¿qué significa que el estimate de hi_empunion en 2SLS sea -0.89? 
#those who have additional insurance have out-of-pocket expenses 89% less than those who dont 

#identification test (añadir un instrumento más)(pg 187 CamTriv stata)
twoslsB = ivreg(ldrugexp ~ hi_empunion + totchr + age + female + 
                 blhisp + linc|ssiratio + multlc + totchr + age + female + 
                 blhisp + linc, data = MEPSins)
summary(twoslsB, vcov = sandwich, diagnostics = TRUE)
#Preguntar: ¿Son diferentes los resultados con un instrumento más?
#Pregunta: ¿Que nos dice el test de Sargan? p>0.05, entonces no esta sobre identificado, el otro instrumento añade información

#weak instruments
#Haga una tabla de correlacion con las siguentes variables instrumentales
#Pregunta: hay razones para pensar en endogeneidad? Las corr. son bajas pero no tanto
cor(MEPSins[!is.na(MEPSins[,'linc']),
                   c('hi_empunion','ssiratio','lowincome','multlc','firmsz')])
summary(firststage)
summary(twoslsB, vcov = sandwich, diagnostics = TRUE) 
#Pregunta: Qué nos dice el primer diagnostic test? 
#p<0.05 dice que el modelo x = instruments + error es bueno, es decir no son instrumentos débiles

#Hacer TODO lo anterior para los otros instrumentos (lowincome','multlc','firmsz)

#Punto 7: diff in diff: replicar e interpretar tabla 7.2 Gertler et al 2016, WB book ####
#Gertler et al 2016, WB book
#DECIRLE A LOS ESTUDIANTES QUE USAR treatment_locality = 1
HISP_m = ddply(HISP, .(treatment_locality, enrolled, round), 
               summarise, gastos = mean(health_expenditures,na.rm = T)) #means
HISP_m = HISP_m[HISP_m[,'treatment_locality'] == 1,]
table7.2 = matrix(c(HISP_m[4,'gastos'], HISP_m[2,'gastos'],
                    HISP_m[3,'gastos'], HISP_m[1,'gastos']), ncol=2, byrow = F)
colnames(table7.2) = c('Despues seguimiento', 'Antes (baseline)')
rownames(table7.2) = c('Inscritos', 'No inscritos')
D = -apply(table7.2, 1, diff) #Diferencia dentro de grupos (within groups)
DandD = -diff(D) #Diferencia en diferencias
table7.2
D
DandD
#hacer prueba de medias tabla 7.2 (ttest)
#t test row 1 tabla 7.2
A = HISP[HISP[,'treatment_locality'] == 1 &
              HISP[,'enrolled'] == 1 &
              HISP[,'household_identifier'] != 3045, #didn't do round 0
            c('household_identifier','health_expenditures','round')]
t.test(A[A[,'round']==1,'health_expenditures'],
       A[A[,'round']==0,'health_expenditures'], paired=T)

#t test row 2 tabla 7.2
B = HISP[HISP[,'treatment_locality'] == 1 &
              HISP[,'enrolled'] == 0 &
              HISP[,'household_identifier'] != 3045, #didn't do round 0
            c('household_identifier','health_expenditures','round')]
t.test(B[B[,'round']==1,'health_expenditures'],
       B[B[,'round']==0,'health_expenditures'], paired=T)

#t test row 3 tabla 7.2
t.test(B[B[,'round']==1,'health_expenditures'] -  B[B[,'round']==0,'health_expenditures'],
       A[A[,'round']==1,'health_expenditures'] -  A[A[,'round']==0,'health_expenditures'])


#EjercicioDD: replicar tabla 7.3 Gertler et al 2016, WB book
HISP_temp = HISP[HISP[,'treatment_locality']==1,]
#fixed effect linear regression. 
#El comando cl (cluster standard errors) de stata es el cuarto en la formula, donde esta locality_specifier. 
reg = felm(health_expenditures ~ round*enrolled + round + enrolled|0|0|locality_identifier,
         data = HISP_temp) #IMPORTANTE: diff en diff es una interaccion!!
summary(reg) #comparar la interaccion con el DandD; 
DandD

reg = felm(health_expenditures ~ round*enrolled + round + enrolled +
           age_hh + age_sp + educ_hh + educ_sp + indigenous +
           female_hh + hhsize + dirtfloor + bathroom + land + 
           hospital_distance|0|0|locality_identifier,
         data = HISP_temp)
summary(reg) #comparar la interaccion con el DandD; 
DandD


#Punto 7.1: Regression discontinuity (rdd)####
HISP_rdd = HISP[HISP[,'round']==1,] #solo analizamos round 1
HISP_rdd[,'poverty_index'] = HISP_rdd[,'poverty_index']  - 58 #<=58 eran elegibles al programa
fitLR = lm(health_expenditures ~ eligible*poverty_index+ 
             age_hh + age_hh + educ_hh + educ_sp + female_hh +
             indigenous + hhsize + dirtfloor + bathroom + land + 
             hospital_distance,
           data = HISP_rdd)
summary(fitLR) #si eligible es significativo hay main effect (el salto)
#si poverty_index:eligible es significativo, las pendientes difieren

plot(HISP_rdd[,'poverty_index'], HISP_rdd[,'health_expenditures'],
     ylab = 'health expenditures (predicted)',
     xlab = 'poverty index', pch = 16, cex = 0.2,
     ylim = c(0,60))
idx = HISP_rdd[,'poverty_index']<=0
fitL = lm(health_expenditures ~ poverty_index +
            age_hh + age_hh + educ_hh + educ_sp + female_hh +
            indigenous + hhsize + dirtfloor + bathroom + land + 
            hospital_distance,
          data = HISP_rdd[idx,])
summary(fitL)
points(HISP_rdd[idx,'poverty_index'], 
     predict(fitL), pch = 16, cex = 0.2, col = 'red')
idx = HISP_rdd[,'poverty_index']>0
fitR = lm(health_expenditures ~ poverty_index +
            age_hh + age_hh + educ_hh + educ_sp + female_hh +
            indigenous + hhsize + dirtfloor + bathroom + land + 
            hospital_distance,
          data = HISP_rdd[idx,])
summary(fitR)
points(HISP_rdd[idx,'poverty_index'], 
       predict(fitR), pch = 16, cex = 0.2, col = 'blue')

#Calcule el LATE en el cutpoint
bw = 20 #bandwith (absolute value of poverty index)
R = mean(HISP_rdd[HISP_rdd[,'poverty_index']>0 &
                    HISP_rdd[,'poverty_index']<(0 + bw) &
                    HISP_rdd[,'eligible']==0,
                'health_expenditures']) #puntos azules
L = mean(HISP_rdd[HISP_rdd[,'poverty_index']<0 &
                    HISP_rdd[,'poverty_index']>(0 - bw) &
                    HISP_rdd[,'eligible']==1,
                'health_expenditures']) #'puntos rojos'
R-L





#Punto 8: instrumental variable: replicar e interpretar tabla 5.1 y 5.2 Gertler et al 2016 WB  ####
#Pregunta: ¿Por qué es promotion_locality una variable instrumental para enrolled_rp?
#Respuesta: promocion se refiere a que se hizo publicidad en algunas 
#localidades aleatoreas para que la gente se enrolara al HISP y se asume
#que en promedio esto hace que CCs sean iguales en los dos gruposy
#Pregunta: ¿Por que aletorizar es una variable instrumental? Use dos condiciones de IV
twosls = ivreg(health_expenditures ~ enrolled_rp|promotion_locality, 
               data = HISP[HISP[,'round']==1,])
s_2sls = summary(twosls, diagnostics = TRUE)
s_2sls
#Pregunta: ¿Qué significa que el estimativo de enrolled_rp es -9.50? 
#Respuesta: Las localidades donde se hizo publicidad (1) redujeron sus gastos en salud 
#en -9.50 dolares relativo a donde no hubo publicidad (0)
tabla5.1_row2and3 = ddply(HISP[HISP[,'round']==1,], .(promotion_locality), summarise,
          gastos = mean(health_expenditures), 
          enroll_rate = mean(enrolled_rp))
difference = apply(tabla5.1_row2and3, 2, diff)
difference['gastos']/difference['enroll_rate'] #because the only difference between the promoted and nonpromoted villages is that enrollment in the program is higher in the promoted villages (thanks to the promotion), this difference of US$3.87 in health expenditures must be due to the additional 40.78 percent of households that enrolled in the promoted villages because of the promotion
s_2sls$coefficients['enrolled_rp','Estimate']

#mulitvariado
twosls = ivreg(health_expenditures ~ enrolled_rp + 
                 age_hh + age_sp + educ_hh + educ_sp + indigenous +
                 female_hh + hhsize + dirtfloor + bathroom + land + 
                 hospital_distance|promotion_locality + 
                 age_hh + age_sp + educ_hh + educ_sp + indigenous +
                 female_hh + hhsize + dirtfloor + bathroom + land + 
                 hospital_distance, 
               data = HISP[HISP[,'round']==1,]) 
#NOTE: in ivreg endogenous variables (X) can only appear before 
#the vertical line; instruments (Z) can only appear after the 
#vertical line; exogenous regressors that are not instruments (W)
#must appear both before and after the vertical line.
summary(twosls, diagnostics = TRUE)
#Pregunta: ¿Qué significa que el estimativo de enrolled_rp haya aumentado un poco en la regresion multivariada?

#NON-LINEAR REGRESSION ####


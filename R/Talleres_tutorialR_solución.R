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



#Manipule la base de datos weather####
#Cargue todas las librerias que vimos en clase
pkgload('nycflights13')

#Cuales son las columnas de weather
colnames(weather)

#use ddply para contar cuantas veces aparece cada mes (column month)
ddply(weather, .(month), summarise, N = length(month))

#use ddply y saque el promedio,std,min,max, por mes de la columna pressure
ddply(weather, .(month), summarise, 
      N = length(month), meanP = mean(pressure, na.rm = T), stdP = sd(pressure, na.rm = T),
      minP = min(pressure, na.rm = T), maxP = max(pressure, na.rm = T))

#use ddply y saque el promedio,std,min,max, por mes y dia de la columna pressure
ddply(weather, .(month, day), summarise, 
      meanP = mean(pressure, na.rm = T), stdP = sd(pressure, na.rm = T),
      minP = min(pressure, na.rm = T), maxP = max(pressure, na.rm = T))

#use filter para crear una tabla con la primera hora del dia
hour1 = filter(weather, hour == 1)

#use filter para crear una tabla con la primera hora del dia y windspeed mayor o igual a 10
hour1_windspeed = filter(weather, hour == 1, wind_speed>=10)

#use arrange para organizar weather por pressure
a = arrange(weather, pressure)

#use arrange para organizar weather por pressure y otra columna de su preferencia
a = arrange(weather, pressure, day)

a = arrange(weather, day, month)

#use gather en la base de datos mus15data.dta. Gather all price columns. lnp is log price, lnc is log sales, and the numbers are years (e.g. 63 is for 1963)
wwdd = '/Users/santiagoalonsodiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales GD/DataSets/CameronTrivediStata/' #working directory
smoking_wide <- read.dta(paste(wwdd,"mus08cigarwide.dta", sep=''))
coldes_smoking = cbind(attributes(smoking_wide)$names, attributes(smoking_wide)$var.labels) #variable description
a = gather(smoking_wide, 'Year_p','logvalue_price', lnp63, lnp64, lnp65)
a = dplyr::select(a, state, Year_p, logvalue_price)
b = gather(smoking_wide, 'Year_s','logvalue_sales', lnc63, lnc64, lnc65)
b = dplyr::select(b, Year_s, logvalue_sales)
smoking_long = cbind(a,b)


#use spread el la base en formato long para devolverla a formato wide
colnames(smoking_long)
d = spread(smoking_long[,c('state','Year_p', 'logvalue_price')], 
           key = Year_p, value = logvalue_price) #key: what goes in columns (e.g. Year), value: what goes in cells (e.g. logvalue) 
e = spread(smoking_long[,c('state','Year_s', 'logvalue_sales')], 
           key = Year_s, value = logvalue_sales)
smoking_back_to_wide = cbind(d,e)[,c(1:4,6:8)] #to not repeat column 'state'


#use select de dplyr en la base weather para crear una nueva base reducida con solo dos columnas de su eleccion
weather_red = dplyr::select(weather, humid, year)

#use filter y select de dplyr en la base weather para crear una nueva base reducida con solo dos columnas de su eleccion
weather_red = filter(weather, day>10)
weather_red = dplyr::select(weather_red, humid, year)

#use rename para renombrar dos columnas de weather
colnames(weather)
weather_renamed = dplyr::rename(weather, H = humid, 
                 Y = year)

#use mutate para crear una nueva columna sumando otras dos columnas
colnames(weather)
weather_mutated = mutate(weather,
                   gain = humid - year,
                   speed = pressure - humid)
colnames(weather_mutated)


#Haga dos graficas con ggplot con la base de datos weather o flights





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
pkgload('ggplot2') #plotting routines
pkgload('plotly') #plotting routines
pkgload('foreign') #loads stata files
pkgload('R.matlab') #loads matlab files
pkgload('plyr') #data reshaping
pkgload("dplyr") #data reshaping
pkgload('pryr') #data reshaping
pkgload('reshape2') #data reshaping
pkgload('tidyr')
pkgload("tidyverse") #IMPORTANT ONE: installs ggplot2 and other stuff of hadley whickman team!
#tidyr is replacing reshape2. Hera are some translations:
#tidyr	    gather	spread
#reshape(2)	melt	  cast
pkgload('ez')
pkgload('parallel')
pkgload('nloptr')
pkgload('dfoptim')
pkgload('lme4') #GLMs
pkgload('nlme')
pkgload('lmerTest')
pkgload('pROC')
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
pkgload('nycflights13')

#fuente: http://r4ds.had.co.nz/ 
get(data(weather))

#Declarar funciones #####

mifuncion = function(parametros){
  #parametros puede ser cualquier tipo o estructura de datos
  output = parametros[1] + parametros[2] + parametros[3]
  output
}

mifuncion2 = function(parametros1, parametros2, parametros3 = 25){
  output = parametros1 + parametros2 + parametros3
  output
}

mifuncion3 = function(parametros, ...){
  plot(parametros[1,], parametros[2,], ...)
}

#Cargar data ####
#wwdd = '/Users/santiagoalonsodiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/DataSets/' #working directory
wwdd = 'E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/' 
mydata <- read.dta(paste(wwdd,"heus_mepssample.dta", sep=''))
#Description of columns
coldes_mydata = cbind(attributes(mydata)$names, attributes(mydata)$var.labels) #variable description
#NOTA: para archivos de excel salven en csv y usen lo siguiente
#NOTA2: Esta data es interesante como ejemplo de la necesidad de revisar datos (e.g. formatos)
mydata2 <- read.csv(paste(wwdd,"GHE2016_AllAges.csv", sep=''), header = TRUE)
mydata2 <- mydata2[-which(mydata2[,'temp4']==''),]
mydata2 <- as.data.frame(mydata2)
colnames(mydata2)[7] = 'Disease'
mydata2[mydata2=='.'] = NA
for (i in 8:dim(mydata2)[2]) {#changes the values in country columns from factors to numeric
  mydata2[,i] = as.numeric(as.character(mydata2[,i]))
}


#Crear secciones ####

# Operadores ####
2+3 
2-3
2*3
2/3
3%%3
2^3
a = matrix(c(1,2,3,4), nrow=2, ncol = 2, byrow = T)
b = matrix(c(1,2,3,4), nrow=2, ncol = 2, byrow = T)
a*b #element wise
a%*%b #matrix multiplication
t(a) #matrix transpose
eigen(a) #eigenvalues of a


#logicos
2>=3
2<3
2==3
2!=3

#los valores en computadores son aproximaciones y dan cosas raras
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1
#use mejor near en estos casos de numeros de precision
near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)



# Tipos y estructuras de datos ####
x = '2'
str(x)
m = matrix(c(as.integer(x),
      as.numeric(x)), nrow = 1, ncol = 2) #matrix (only one data type)
colnames(m) = c('integer', 'numeric')
m = matrix(c(x,
             as.numeric(x)), nrow = 1, ncol = 2) #matrix (only one data type)
colnames(m) = c('integer', 'numeric')
str(m)
df = data.frame(int = as.integer(x),
             num = as.numeric(x),
             chr = x) #dataframe (multiple data types)
colnames(df) = c('integer', 'numeric', 'character')
str(df)
l = list(int = as.integer(x),
               num = as.numeric(x),
               chr = x) #list (multiple data structures)
str(l)

# Condicionales y loops ####
#Condicionales
a = 'carro'
b = 'moto'
d = 'triciclo'
if (a == b) {
  x = 3+3
} else if (a == d) {
  x = 3+22
} else {
  x = 'vehiculo no es igual'
}
print(x)

#Condicionales vector
a = c(1,1,1,3:10)
b = c(1,3:11,10)
d = ifelse(a==b, 'pepe', 'no pepe')
d

#For loop
for (j in 1:10){ #secuencial
  print(j+1)
}


a = c(1,10,100,1000)
for (j in a){ #elementos en vector
  print(j+1)
}

a = matrix(c('c','a','s','a'), nrow = 1, ncol = 4)
for (j in 1:dim(a)[2]){ #longitud vector o matriz
  print(a[j])
}

#Ejercicio: Hacer un for loop que cuente los elementos unicos en: 
#a = seq(0.01,0.99,length.out = n)
#b = qpois(a, lambda = 25)
#ademas haga una grafica (e.g. histogram) 


#While loop
condicion = 0
while (condicion <= 1000){
  print(condicion)
  condicion = condicion + 100
}



# Funciones ####
parametros = c(1,2,3)
mifuncion(parametros)

parametros1 = 1
parametros2 = 1
mifuncion2(parametros1, parametros2)

parametros1 = 1
parametros2 = 1
parametros3 = 1
mifuncion2(parametros1, parametros2, parametros3)

#GLOBAL vs LOCAL: parametros3 tiene un valor local, no utiliza el valor en el ambiente global
parametros1 = 1
parametros2 = 1
mifuncion2(parametros1, parametros2) 

#pasar parametros a funciones dentro de las funciones
parametros = matrix(c(1:10, 21:30), nrow = 2, ncol = 10, byrow=T)
View(parametros)
mifuncion3(parametros, type = 'l', col = 'red')


#Datos: uso y otra miscelanea ####
#logical indices (in many languages faster than absolute indices(see below))
get(data(mpg))
head(mpg)
li = mpg[,'manufacturer'] == 'ford'
li
a = mpg[li,]
head(a)


#absolute indices
ai = which(mpg[,'manufacturer'] == 'ford')
ai
a = mpg[ai,]
head(a)


#Descriptives (apply & ddply)
a = matrix(1:2000, ncol = 4)
dim(a)
a
apply(a,1, mean, na.rm=T) #function applied to all rows
apply(a,2, sum, na.rm=T) #function applied to all columns

#Apply function to a subset of the data
pid_age = ddply(mydata, .(ins_unins), summarise, 
                mAge = mean(age, na.rm = T)) 
pid_age
pid_age = ddply(mydata, .(ins_unins), summarise, 
                mAge = mean(age, na.rm = T), sdAge = sd(age, na.rm = T))
pid_age
pid_rx = ddply(mydata, .(ins_unins), summarise, mUseRx = mean(use_rx, na.rm = T))
pid_rx
pid_race_rx = ddply(mydata, .(ins_unins, race_bl), summarise, mUseRx = mean(use_rx, na.rm = T))
pid_race_rx



#obtain a subset of the data
head(flights)
colnames(flights)
dim(flights)
jan1 = filter(flights, month == 1, day == 1) 
dec25 = filter(flights, month == 12, day == 25)
nov_dec = filter(flights, month == 11 | month == 12 |
                   is.na(month)) #keeps missing values

#select columns
colls1 = dplyr::select(flights, year, month, day)
colls2 = dplyr::select(jan1, year, month, day)
colls1
colls2
# Select all columns between year and day (inclusive)
colls3 = dplyr::select(flights, year:day)
# Select all columns except those from year to day (inclusive)
colls4 = dplyr::select(flights, -(year:day))
# Select columns by string patterns
colls5 = dplyr::select(flights, starts_with("y"))
colls6 = dplyr::select(flights, ends_with("r"))
colls7 = dplyr::select(flights, num_range("x", 1:3)) #matches x1, x2 and x3
#rename columns
colnames(flights)
flights = rename(flights, arrtime = arr_time, 
                 airtime = air_time)
colnames(flights)
#add columns
colnames(flights)
flights_m = mutate(flights,
       gain = dep_delay - arr_delay,
       speed = distance / airtime * 60)
colnames(flights_m)


#sort data
arr = arrange(flights, year, month, day) #ascending order
arr1 = arrange(flights, dep_delay) #ascending order
arr2 = arrange(flights, desc(dep_delay)) #descending order

#group data and summarise
by_day = dplyr::group_by(flights, year, month, day) #No cambia la data, solo la formatea
a = summarise(by_day, delay = median(dep_delay, na.rm = TRUE))
a

data(iris)
groupDF <- dplyr::group_by(iris, Species) #No cambia la data, solo la formatea
summaryTable <- summarise(groupDF, meanSepalLength = mean(Sepal.Length),
                          meanSepalWidth = mean(Sepal.Width),
                          meanPetalLength = mean(Petal.Length),
                          meanPetalWidth = mean(Petal.Width))
summaryTable

#Cambiar de direccion los datos  
#wide to long
data2_melt = gather(mydata2,
                    key = 'Country', value = 'Death_Rate',
                    colnames(mydata2)[8:dim(mydata2)[2]], #gathers these columns into one column called country
                    na.rm = T)
data2_melt2 = dplyr::select(data2_melt, Disease, Country, Death_Rate)
data2_melt3 = dplyr::select(data2_melt, Disease, Sex, Country, Death_Rate)


#dcast long vs wide
data2_cast = spread(data2_melt3, key = Sex, value = Death_Rate) #Spreads key into various columns and puts value

#Look for separate and unite (cool functions)





#Plots tradicional #####
x = as.data.frame(midwest[,'area'])
y = as.data.frame(midwest[,'poptotal'])
dataReg = data.frame(x, y)

#cambiar limites de ejes
plot(x[,1],y[,1],
     main = 'Area Vs Population',
     xlab = 'Area', ylab = 'Population',
     xlim = c(0,0.12), ylim =c(0,1000000))
#anadir una linea de regresion
#reg = lm(y[,1]~x[,1])
reg = lm('poptotal~area', data = dataReg)
summary(reg)
# lines(x[,1], predict(reg), col = 'red')
newx <- data.frame(seq(min(dataReg[,'area']), max(dataReg[,'area']), length.out = 1000))
newx = newx %>% 
  rename(
    'area' = colnames(newx)[1]
  )
preds <- predict(reg, newdata = newx,
                 interval = 'confidence')
transparencia = 0.2
polygon(c(rev(newx[,'area']), newx[,'area']), c(rev(preds[ ,3]), preds[ ,2]),
        col = rgb(190/255,190/255,190/255, transparencia), border = NA)
lines(newx[,'area'], preds[,1], col = 'red')

#ggplot ####
#En general ggplots tienen la siguiente estructura
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

#Descriptives (ggplot)
#Fuente: http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html
get(data(midwest, package="ggplot2"))
# ggplot funciona por capas
# capa 1: Init Ggplot 
c1 = ggplot(midwest, aes(x=area, y=poptotal))  # area and poptotal are columns in 'midwest'
plot(c1)
c2 = c1 + geom_point()
c2
# capa 2: Tenemos que decirle a ggplot el tipo de grafica e.g. puntos 
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()
# capa 3: Podemos adicionar un linea de regresion
g <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() + geom_smooth(method="lm")
plot(g)
#Modificar limites de los ejes
g + xlim(c(0, 0.1)) + ylim(c(0, 1000000)) #OJO: esto borra puntos para la regresion
g1 <- g + coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))  # zooms in
plot(g1)
#Cambiar titulos
g1 + labs(title="Area Vs Population", 
          subtitle="From midwest dataset", 
          y="Population", x="Area", caption="Figure 1")
#cambiar colores
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue", size=3) +   # Set static color and size for points
  geom_smooth(method="lm", col="firebrick") +  # change the color of line
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
#cambiar colores por categoria
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
plot(gg)
gg + theme(legend.position="None")  # remove legend
gg + scale_colour_brewer(palette = "Set1")  # change color palette
#cambiar texto y escala de ejes
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})
#cambiar tema 
# method 1: Using theme_set()
theme_set(theme_classic())  # not run
gg
# method 2: Adding theme Layer itself.
gg + theme_bw() + labs(subtitle="BW Theme")
gg + theme_classic() + labs(subtitle="Classic Theme")
#Tamano puntos acorde a otra variable y otro tipo de regresion
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=T) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")
plot(gg)
#Cambiar titulo de la leyenda
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")
gg + scale_color_discrete(name="State") + 
  scale_size_continuous(name = "Density", guide = "none") #guide = T vuelve y pone la leyenda
#Cambiar items de la leyenda y posicion
gg + scale_color_manual(name="State", 
                        labels = c("Illinois", 
                                   "Indiana", 
                                   "Michigan", 
                                   "Ohio", 
                                   "Wisconsin"), 
                        values = c("IL"="blue", 
                                   "IN"="red", 
                                   "MI"="green", 
                                   "OH"="brown", 
                                   "WI"="orange")) +
  scale_size_continuous(name ='Density') +
  theme(legend.position="bottom")
#Poner labels a los puntos
# Filter required rows.
# midwest$county = as.character(midwest$county) 
midwest_sub <- midwest[midwest$poptotal > 300000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 300000, midwest_sub$county, "")
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")
# Plot text and label
gg + geom_text(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggplot2::geom_text") + theme(legend.position = "None")   # text
gg + geom_label(aes(label=large_county), size=2, data=midwest_sub, alpha=0.25) + labs(subtitle="With ggplot2::geom_label") + theme(legend.position = "None")  # label
gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None")   # text
gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_label_repel") + theme(legend.position = "None")   # label
#Faceting: varios graficos en uno
get(data(mpg, package="ggplot2")) #hwy: highway mileage; displ: engine displacement
g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  labs(title="hwy vs displ", caption = "Source: mpg") +
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()  # apply bw theme
plot(g)
# Facet wrap with common scales
g + facet_wrap( ~ class, nrow=3) + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure")  # Shared scales
# Facet wrap with free scales
g + facet_wrap( ~ class, scales = "free") + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")  # Scales free

# Other facet method (can''t define rows but see below)
g2 <- g + facet_grid(cyl ~ class)  # compare with facet_wrap below; this one is better
plot(g2)
g + facet_wrap(cyl ~ class, scales = "free") + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")  # Scales free

# Draw Multiple plots in same figure.
gridExtra::grid.arrange(g, g2, ncol=2)

#remove grid
g + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())

#add image
#img <- png::readPNG("/Users/santiagoalonsodiaz/Google Drive/JaverianaGD/Clases/Cortes Transversales/R/LogoR.png")  # source: https://www.r-project.org/
img <- png::readPNG("E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/R/LogoR.png")
g_pic <- rasterGrob(img, interpolate=TRUE)
g + theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
  annotation_custom(g_pic, xmin=5, xmax=7, ymin=30, ymax=45)

# ggplot cool plots #####
#Interactive plots
data.diamonds=ggplot2::diamonds
gg=ggplot(data.diamonds,aes(x=carat,y=price,color=color))+
  geom_point(alpha=0.3)
ggplotly(gg) #turns ggplot interactive


#encircle
midwest_select <- midwest[midwest$poptotal > 350000 & 
                            midwest$poptotal <= 500000 & 
                            midwest$area > 0.01 & 
                            midwest$area < 0.1, ]

ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) +   # draw points
  geom_smooth(method="loess", se=F) + #change se=F for T for confidence intervals
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) +   # draw smoothing line
  geom_encircle(aes(x=area, y=poptotal), 
                data=midwest_select, 
                color="red", 
                size=2, 
                expand=0.08) +   # encircle
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot + Encircle", 
       caption="Source: midwest")

#jitter points vs counts (to show all data)
g <- ggplot(mpg, aes(cty, hwy))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")

g <- ggplot(mpg, aes(cty, hwy))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Counts Plot")

#Animate
library(transformr)
library(gifski)
g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale

anim <- g + 
  transition_states(year,
                    transition_length = 1,
                    state_length = 1)

anim_save('E:/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/R/anim_ggplot.gif',
          animate(anim, renderer=gifski_renderer()))

#Distributions at margin
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)
ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "density", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")

#Correlations
get(data(mtcars))
corr <- round(cor(mtcars), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

#Diverging Barcharts
theme_set(theme_bw())  
get(data(mtcars)) # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()

#Histograms
g <- ggplot(mpg, aes(displ)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=class), 
                   binwidth = .1, 
                   col="black", 
                   size=.1) +  # change binwidth
  labs(title="Histogram with Auto Binning", 
       subtitle="Engine Displacement across Vehicle Classes")  

g + geom_histogram(aes(fill=class), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes")

g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes")

g <- ggplot(mpg, aes(manufacturer))
g + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")

#Densities
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="City Mileage",
       fill="# Cylinders")

#box plots
theme_set(theme_classic())
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="City Mileage grouped by Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

#Pie charts
theme_set(theme_classic())
df <- as.data.frame(table(mpg$class))
colnames(df) <- c("class", "freq")
pie <- ggplot(df, aes(x = "", y=freq, fill = factor(class))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="class", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of class", 
       caption="Source: mpg")
pie + coord_polar(theta = "y", start=0)


#time series
get(data("economics"))
ggplot(economics, aes(x=date)) + 
  geom_line(aes(y=uempmed)) + 
  labs(title="Time Series Chart", 
       subtitle="Unemployment", 
       caption="Source: Economics", 
       y="Unemployed")

#heat maps
df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/yahoo.csv")
df$date <- as.Date(df$date)  # format date
df <- df[df$year >= 2012, ]  # filter reqd years
df$yearmonth <- as.yearmon(df$date)
df$yearmonthf <- factor(df$yearmonth)
df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
df <- df[, c("year", "yearmonthf", "monthf", "week", "monthweek", "weekdayf", "VIX.Close")]
head(df)
ggplot(df, aes(monthweek, weekdayf, fill = VIX.Close)) + 
  geom_tile(colour = "white") + 
  facet_grid(year~monthf) + 
  scale_fill_gradient(low="red", high="green") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Yahoo Closing Price", 
       fill="Close")


#Dendogram
hc <- hclust(dist(USArrests), "ave")  # hierarchical clustering
ggdendrogram(hc, rotate = TRUE, size = 2)


#Cluster
theme_set(theme_classic())

# Compute data with principal components 
df <- iris[c(1, 2, 3, 4)]
pca_mod <- prcomp(df)  # compute principal components

# Data frame of principal components 
df_pc <- data.frame(pca_mod$x, Species=iris$Species)  # dataframe of principal components
df_pc_vir <- df_pc[df_pc$Species == "virginica", ]  # df for 'virginica'
df_pc_set <- df_pc[df_pc$Species == "setosa", ]  # df for 'setosa'
df_pc_ver <- df_pc[df_pc$Species == "versicolor", ]  # df for 'versicolor'

ggplot(df_pc, aes(PC1, PC2, col=Species)) + 
  geom_point(aes(shape=Species), size=2) +   # draw points
  labs(title="Iris Clustering", 
       subtitle="With principal components PC1 and PC2 as X and Y axis",
       caption="Source: Iris") + 
  coord_cartesian(xlim = 1.2 * c(min(df_pc$PC1), max(df_pc$PC1)), 
                  ylim = 1.2 * c(min(df_pc$PC2), max(df_pc$PC2))) +   # change axis limits
  geom_encircle(data = df_pc_vir, aes(x=PC1, y=PC2)) +   # draw circles
  geom_encircle(data = df_pc_set, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = df_pc_ver, aes(x=PC1, y=PC2))

#3D plots (also doable with plot3d)
Sys.setenv("plotly_username"="ludvic1")
Sys.setenv("plotly_api_key"="AxCyLChdFq52ISjk3Dtx") #go to plotly and retrieve the API
mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)
p <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Weight'),
                      yaxis = list(title = 'Gross horsepower'),
                      zaxis = list(title = '1/4 mile time')))
chart_link = api_create(p, filename="scatter3d-basic")
chart_link




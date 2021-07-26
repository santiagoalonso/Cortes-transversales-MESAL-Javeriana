
n = 50
cutoff= 0.08 # umbral para considerar si alguien manejo intoxicado (DUI)
maxBAC = 0.2
BAC = seq(0, cutoff, length.out = n)
Recidivism_before = -0.06*BAC + 0.125 
Recidivism_before_noise =  Recidivism_before + rnorm(n, 0, 0.01)
accident_at_scene_before =  -0.72*BAC + 0.135 
accident_at_scene_before_noise =  accident_at_scene_before + rnorm(n, 0, 0.005)
age_before =  -0.445*BAC + 0.3775 
age_before_noise =  age_before + rnorm(n, 0, 0.004)
raza_before =  0.25*BAC + 0.84 
raza_before_noise =  raza_before + rnorm(n, 0, 0.004)


BAC2 = seq(cutoff, maxBAC, length.out = n)
Recidivism_after = 0.07*BAC2 + 0.1 
Recidivism_after_noise =  Recidivism_after + rnorm(n, 0, 0.005)
accident_at_scene_after =  0.9*BAC2 + 0.009 
accident_at_scene_after_noise =  accident_at_scene_after + rnorm(n, 0, 0.005)
age_after =  0.12*BAC2 + 0.3325
age_after_noise =  age_after + rnorm(n, 0, 0.005)
raza_after =  0.225*BAC2 + 0.84 
raza_after_noise =  raza_after + rnorm(n, 0, 0.004)


plot(BAC, raza_before_noise, 
     xlim = c(0,maxBAC),
     ylim = c(0.8, 0.9))
lines(BAC, raza_before, col = 'red')
points(BAC2, raza_after_noise)
lines(BAC2, raza_after, col = 'red')

#Medidas en reincidencia
midata = data.frame("Nivel_Alcohol_Sangre_NAS" = c(BAC,BAC2),
                    "Reincidencia_promedio" = c(Recidivism_before_noise, Recidivism_after_noise),
                    'Umbral_NAS' = c(cutoff),
                    'Genero_promedio' = rbinom(n*2, 1000, 0.78)/1000, #0 mujer, 1 hombre
                    'Accidente_en_escena_promedio' = c(accident_at_scene_before_noise, accident_at_scene_after_noise),
                    'Edad_promedio_estandarizada' = c(age_before_noise, age_after_noise),
                    'Raza_blanco_promedio' = c(raza_before_noise, raza_after_noise))
midata = midata[sample.int(n*2, n*2),] #orden aleatorio de las filas
midata['Incumplio_umbral'] =  ifelse(midata[,'Nivel_Alcohol_Sangre_NAS'] > midata[,'Umbral_NAS'],1,0)
#points(midata[,'Nivel_Alcohol_Sangre_NAS'], midata[,'Reincidencia'], col ='red')

write_csv(midata, "/Users/santiagoalonsodiaz/OneDrive - Pontificia Universidad Javeriana/Clases/Cortes Transversales OD/DataSets/BAC.csv")

# •	Cuantos campos y observaciones tiene el dataframe.  Utilizar “head” y “dim”.
head(airquality) # -> Hay 6 campos: Ozone Solar.R Wind Temp Month Day.
dim(airquality)  # -> 153 observaciones con 6 campos.
 
# •	Evaluar el dataframe con la instrucción “summary”.
#     o	¿Tiene observaciones con elementos nulos (NA)?
#     o	¿A que meses corresponden las observaciones?
summary(airquality)
# Hay valores NA en Ozone (37) y en Solar Radiation (7).
# Los meses durante los que se realizaron las observaciones son del 5 al 9 (es decir de mayo a septiembre).

# •	Temperatura máxima del viento en el mes de mayo.
max(airquality[airquality$Month == 5,]$Temp) # <- 81


# •	Media del ozono en el mes de Julio. 

mean(airquality[airquality$Month == 7,]$Ozone,na.rm = TRUE) # -> 59.11538

media=mean(airquality[airquality$Month == 7,]$Ozone,na.rm = TRUE) # -> 59.11538

# Transformar al valor de la media los NA.
airquality$Ozone[is.na(airquality$Ozone)] <- media
# Estudiar el efecto de esta asignación sobre la desviación típica

mean(airquality[airquality$Month == 7,]$Ozone) 

# •	Mes donde la temperatura fue mayor.
airquality[max(airquality$Temp),]$Month # -> Agosto (8)

# •	Mes donde la temperatura y el ozono fue mayor.
length(airquality[airquality$Temp > 90 & airquality$Ozone < 100,"Month"]) # -> 13

# •	Haciendo un estudio de los datos, ¿Qué podemos concluir? 
# ¿Existe alguna relación entre las variables Ozono, Temperatura y Radiación Solar?
# Se recomienda hacer la media mes a mes de cada variable. 

mean(airquality$Ozone[airquality$Month == 5])
mean(airquality$Ozone[airquality$Month == 6])
mean(airquality$Ozone[airquality$Month == 7])
mean(airquality$Ozone[airquality$Month == 8])
mean(airquality$Ozone[airquality$Month == 9])

mean(airquality$Temp[airquality$Month == 5])
mean(airquality$Temp[airquality$Month == 6])
mean(airquality$Temp[airquality$Month == 7])
mean(airquality$Temp[airquality$Month == 8])
mean(airquality$Temp[airquality$Month == 9])

media_Solar.R_5=mean(airquality$Solar.R[airquality$Month == 5],na.rm = TRUE)
sqrt(var(airquality$Solar.R[airquality$Month == 5],na.rm=TRUE))
media_Solar.R_6=mean(airquality$Solar.R[airquality$Month == 6],na.rm = TRUE)
media_Solar.R_7=mean(airquality$Solar.R[airquality$Month == 7],na.rm = TRUE)
media_Solar.R_8=mean(airquality$Solar.R[airquality$Month == 8],na.rm = TRUE)
media_Solar.R_9=mean(airquality$Solar.R[airquality$Month == 9],na.rm = TRUE)

# Transformar los NA.
table(is.na(airquality$Solar.R))

indices5=which(is.na(airquality$Solar.R[airquality$Month == 5]))
# ojo, estamos cambiando todos los datos sin haber salvado la anterior versión
# del dataframe
airquality$Solar.R[airquality$Month == 5][indices5]=media_Solar.R_5
mean(airquality$Solar.R[airquality$Month == 5])
sqrt(var(airquality$Solar.R[airquality$Month == 5]))


# Si se hace la media del ozono, temperatura y radiacion solar podemos observar como más o menos todos incrementan a la par.
# El mes de junio es el único que presenta algo de variación.
# Se podría concluir que todas las variables indicadas tiene relación entre ellas.


# Ejercicio 1 (tidyr y dplyr)
library(tidyr)

# A partir del siguiente dataframe realizar las siguientes operaciones de limpieza de datos:
set.seed(1)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  Walmart = rnorm(10, 20, 1),
  Target = rnorm(10, 20, 2),
  Walgreens = rnorm(10, 20, 4)
)
#       time  Walmart   Target Walgreens
# 1  2009-01-01 19.37355 23.02356  23.67591
# 2  2009-01-02 20.18364 20.77969  23.12855
# 3  2009-01-03 19.16437 18.75752  20.29826
# 4  2009-01-04 21.59528 15.57060  12.04259
# 5  2009-01-05 20.32951 22.24986  22.47930
# 6  2009-01-06 19.17953 19.91013  19.77549
# 7  2009-01-07 20.48743 19.96762  19.37682
# 8  2009-01-08 20.73832 21.88767  14.11699
# 9  2009-01-09 20.57578 21.64244  18.08740
# 10 2009-01-10 19.69461 21.18780  21.67177

# Como se puede observar hay un problema de clave-valor en las compañias con sus observaciones.
# Transformar los datos para que tengan una clave stock y el valor sea el precio. 
# Por lo tanto se requiere la funcion "gather".

# Opcion 1:
new_stocks <- gather(data = stocks, key = stock, value = price, Walmart, Target, Walgreens)

# Opcion 2:
new_stocks <- gather(data = stocks, key = stock, value = price, Walmart:Walgreens)

# Opcion 3:
new_stocks <- gather(data = stocks, key = stock, value = price, -time)
# El último argumento, -time, significa que todas las columnas excepto el tiempo contienen los pares clave-valor.


# Devolver el dataframe al estado original utilizando la funcion "spread".
original_stocks <- spread(data = new_stocks, key = stock, value = price)

# Utilizando el operador tuberia %>% se desea realizar las siguientes operaciones anidadas.
# 1) Transformar los datos para que tengan una clave stock y el valor sea el precio mediante la funcion "gather".
# 2) Agrupar los datos por la clave stock mediante la funcion "group_by".
# 3) Obtener el precio minimo y maximo utilizando la funcion "summarise".

stocks %>% 
  gather(key = stock, value = price,Walmart:Walgreens)%>% 
  group_by(stock) %>% 
  summarise(min = min(price), max = max(price))

###################################################################

# Ejercicio 2 (dplyr)

library(dplyr)
library(nycflights13)

# COMPROBACION.
# Observamos los distintos dataframes que nos proporcionan.
# Utilizamos el nombre del paquete y doblemente dos puntos (::) para comprobarlo.
# Tambien se puede utilizar el nombre del dataframe si previamente estamos familiarizados.

# PRIMERA OBSERVACION.
# Comprobamos las variables de cada uno de los datasets que nos proporcionan mediante la instrucción "head".
print(head(flights))
print(head(airports))
print(head(weather))
print(head(airlines))
print(head(planes))

# Comprobamos las variables de cada uno de los datasets que nos proporcionan mediante la instrucción "summary".
print(summary(flights))
print(summary(airports))
print(summary(weather))
print(summary(airlines))
print(summary(planes))

# Simplificar los dataframes originales a 100 observaciones. Renombrarlos introduciendo la coletilla "_simple".

flights_simple <- head(flights,100)
airports_simple <- head(airports,100)
weather_simple <- head(weather,100)
airlines_simple <- head(airlines,100)
planes_simple <- head(planes,100)


# Selecciona los tipos de aerolinea ("carrier") mediante la instruccion "select" y el operador "unique" concatenados con el operador tuberia %>%.
airlines_simple %>% unique %>% select(carrier)

# Obtener la media y el maximo de asientos ("seats") que tienen los aviones. Utilizar el operador tuberia %>%.
planes_simple %>% summarise(mean = mean(seats),max_engines = max(seats))

# Ordenar los aviones por numero de motores ("engines") y numero de asientos ("seats").
result1 <- arrange(planes_simple,engines,seats)
print(result1)

# Averigua que numero de cola comparten los dataframes "flights_simple" y "planes_simple" que has creado anteriormente.
# Obten su aerolinea ("carrier")
shared <- inner_join(flights_simple,planes_simple,by="tailnum") # -> N14228
shared_carrier <- shared$carrier
print(shared_carrier)

# Cruzar los datos de vuelos ("flights") con los aviones ("planes") por el numero de cola ("tailnum") que no coincidan.
# De esos obtener aquellos con 2 o mas motores.
# Finlmente obtener los distintos modelos de avión que satisfacen las premisas anteriores.
fp <- anti_join(planes_simple,flights_simple,by="tailnum")
engines_fp <- filter(fp,engines >= 2)
result2 <- unique(engines_fp$model) # No queremos los repetidos. Por lo tanto usamos "unique".
print(result2)

# Crea una nueva variable que calcule el retraso total sumando los delays acumulados ("dep_delay") y ("arr_delay").
# Almacena el dataframe resultante en "flights_total".
flights_total <- mutate(flights_simple,total_delay=dep_delay+arr_delay)

# En base a la variable anteriormente obtenida, devuelve los aviones que han llegado con antelacion a su destino.
filter(flights_total,total_delay < 0)



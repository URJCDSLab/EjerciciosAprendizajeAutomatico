# Librerías necesarias
library(mlbench)
library(dplyr)
library(ggfortify)


## Cargamos los datos
df=BostonHousing

summary(df)

# Convertimos la variable CHAS a numérica
df =
  df %>% 
  mutate(chas=as.numeric(chas))

# PCA
df_pca <- prcomp(df[, -14], scale= TRUE)
summary(df_pca)


autoplot(df_pca,data=df,colour='medv',loadings=TRUE,loadings.label=TRUE)

# Las casas más caras parecen situarse, especialmente, en la esquina superior izquierda.
df = 
  df %>% 
  mutate(caras=as.factor(ifelse(medv>40,1,0)))

autoplot(df_pca,data=df,colour='caras',loadings=TRUE,loadings.label=TRUE)
# Las casas caras parecen estar asociadas a `chas`=1 y valores altos de "b" 
# y "rm"

# Interpretación de las dos primeras componentes 
df_pca$rotation[, 1:2]

#La primera componente principal parece tener valores elevados en las variables
# `dis`, `zn`, 'b' y 'rm', frente a valores altos en todas las demas, a 
# excepción de la variable chas. La segunda componente principal enfrenta 
# observaciones con valores altos en `chas`, `age`, `nox`y `b`, frente a 
# observaciones con valores altos en `crim`, `zn`, `dis` y `ptratio`.
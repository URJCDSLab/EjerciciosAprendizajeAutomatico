# Librería para leer XLSX
library(readxl)
library(dplyr)

# leemos los datos previamente salvados
df <- read_excel("datos/Dry_Bean_Dataset.xlsx")

# Número de observaciones y variables
dim(df)


# Particiones

# mediante una semilla conseguimos que el ejercicio sea reproducible
set.seed(12321)

# creamos índices
ntotal <- dim(df)[1]
indices <- 1:ntotal
ntrain <- ntotal * .6
ntest <- ntotal *.2
indices.train <- sample(indices, ntrain, replace = FALSE)
indices.test  <- sample(indices[-indices.train],ntest,replace=FALSE)
indices.valid <- indices[-c(indices.train,indices.test)]

# Usamos el 60% de la base de datos como conjunto de entrenamiento, 20% como conjunto de test y 20% como validación
train  <- df[indices.train, ]
test   <- df[indices.test, ]
valid  <- df[indices.valid,]

dim(train)
dim(test)
dim(valid)

# Media de la variable AREA
media.train=mean(train$Area)
sd.train=sqrt(var(train$Area))
mean(test$Area)
mean(valid$Area)

# Escalamos la variable.

train=
  train %>%
  mutate(Area_Scale=scale(Area))

mean(train$Area_Scale)
var(train$Area_Scale)

# Aplicamos la misma transformación en los datos de test y validación

test=
  test %>%
  mutate(Area_Scale=(Area-media.train)/sd.train)

mean(test$Area_Scale)
var(test$Area_Scale)


valid=
  valid %>%
  mutate(Area_Scale=(Area-media.train)/sd.train)

mean(valid$Area_Scale)
var(valid$Area_Scale)

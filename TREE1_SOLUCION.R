# Liberías necesarias para resolver el ejercicio
library(ISLR2)
library(dplyr)
library(caTools) # Particiones de los datos
library(rpart) # Para árboles de decisión
library(rpart.plot)
library(ggplot2)
library(caret) # Para la matriz de confusión


# Datos
attach(Carseats)

# Creamos una nueva variable respuesta binaria


# Creamos el data frame

df = Carseats %>%
      mutate(High = factor(ifelse(Sales>=8,"No","Yes"))) %>%
      select(-Sales)

# Partición de los datos 

# Mediante una semilla conseguimos que el ejercicio sea reproducible
set.seed(121)

# Usamos el 70% de la base de datos como conjunto de entrenamiento y el resto como conjunto de test
sample = sample.split(df$High, SplitRatio=0.7)
train  = subset(df, sample==TRUE)
test   = subset(df, sample==FALSE)

# Entrenamos un modelo sobre la muestra de entrenamiento empleando todas las variables

fit.dt = rpart(High~., data = train, method = 'class')
rpart.plot(fit.dt, extra = 106)

# La variable más importante es:
fit.dt$variable.importance

# Relación entre la variable respuesta y la variable más importante
# reordenamos la variable
train %>%
  mutate(ShelveLoc_reorder=factor(ShelveLoc,levels=c("Bad","Medium","Good")))%>%
  ggplot(aes(x = ShelveLoc_reorder, fill = High)) +
  geom_bar()

# Podemos visualizar su relación con la variable respuesta original como sigue
# reordenamos la variable
train %>%
  mutate(ShelveLoc_reorder=factor(ShelveLoc,levels=c("Bad","Medium","Good")))%>%
  ggplot(aes(ShelveLoc_reorder, Carseats$Sales[sample==TRUE])) +
  geom_boxplot()

# Error de clasificación en train
# sobre la partición de entrenamiento
prediction = predict(fit.dt, train, type = 'class')
cf = confusionMatrix(prediction, as.factor(train$High),positive="Yes")
print(cf)

# sobre la partición de validación
prediction = predict(fit.dt, test, type = 'class')
cf = confusionMatrix(prediction, as.factor(test$High),positive="Yes")
print(cf)

# Ajustamos un modelo con menos profundidad para evitar el sobreajuste.
control = rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune.fit = rpart(High~., data = train, method = 'class', control = control)
rpart.plot(tune.fit, extra = 106)


# Error de clasificación en train
# sobre la partición de entrenamiento
prediction = predict(tune.fit, train, type = 'class')
cf = confusionMatrix(prediction, as.factor(train$High),positive="Yes")
print(cf)

# sobre la partición de validación
prediction = predict(tune.fit, test, type = 'class')
cf = confusionMatrix(prediction, as.factor(test$High),positive="Yes")
print(cf)

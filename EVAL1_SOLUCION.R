# Liberías necesarias para resolver el ejercicio
library(liver)
library(caret)
library(caTools)
library(rpart.plot)

# Datos
data(adult)

# Resumen
summary(adult)

# Partición de los datos 

# Mediante una semilla conseguimos que el ejercicio sea reproducible
set.seed(12321)

# Usamos el 20% de la base de datos como conjunto de entrenamiento y el resto como conjunto de validación
sample = sample.split(adult$income, SplitRatio=0.2)
datos.train  = subset(adult, sample==TRUE)
datos.valid   = subset(adult, sample==FALSE)

# Entrenamos un modelo sobre la muestra de entrenamiento empleando todas las variables

traindata = datos.train[,-15]
trainclasses = datos.train[,15]
validdata = datos.valid[,-15]
validclasses = datos.valid[,15]

ctrl <- trainControl(method = "cv", number = 5)

# Entrenamos un knn 

# Entrenamos un knn en cada una de las particiones
ctrl <- trainControl(method = "cv", number = 5)
traindata1 = as.data.frame(cbind(traindata$age,traindata$hours.per.week))
knn.fit1 = train(traindata1,trainclasses,method="knn",trControl=ctrl, preProcess = c("center","scale"))
knn.fit1

# Modelo Final
knn.fit1$finalModel
# Resultados del modelo para cada una de las submuestras
knn.fit1$resample

# Error de clasificación en train
# sobre la partición de entrenamiento
prediction = predict(knn.fit1$finalModel, traindata1, type = 'class')
cf = confusionMatrix(prediction, as.factor(trainclasses),positive=">50K")
print(cf)



# Entrenamos un árbol en cada una de las particiones
dt.fit1 = train(traindata,trainclasses,method="rpart",trControl=ctrl)
dt.fit1
# Modelo Final
dt.fit1$finalModel
rpart.plot(dt.fit1$finalModel)
# Resultados del modelo para cada una de las submuestras
dt.fit1$resample


# Error de clasificación en train
# sobre la partición de entrenamiento
prediction = predict(dt.fit1$finalModel, datos.train, type = 'class')
cf = confusionMatrix(prediction, as.factor(trainclasses),positive=">50K")
print(cf)

# sobre la partición de validación
prediction = predict(dt.fit1$finalModel, datos.valid, type = 'class')
cf = confusionMatrix(prediction, as.factor(validclasses),positive=">50K")
print(cf)


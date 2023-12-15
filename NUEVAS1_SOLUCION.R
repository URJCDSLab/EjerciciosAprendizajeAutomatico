# Liberías necesarias para resolver el ejercicio
library(ISLR2)
library(ggplot2)
library(randomForest)
library(ROCR)
library("DALEX")

# Variable respuesta a factor
car=Caravan
car$Purchase=as.factor(car$Purchase)
# Dividimos la muestra en train, test
# Parciticionamos los datos
set.seed(2138)
n=dim(car)[1]
indices=seq(1:n)
indices.train=sample(indices,size=n*.8,replace=FALSE)
indices.test=sample(indices[-indices.train],size=n*.1,replace=FALSE)
indices.valid=indices[-c(indices.train,indices.test)]

car.train=car[indices.train,]
car.test=car[indices.test,]
car.valid=car[indices.valid,]

# EDA
str(Caravan)
summary(car.train)
ggplot(data=car.train,aes(x=Purchase,fill=Purchase)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencaia relativa") +
  xlab("Variable respuesta: Purchase")

# Equilibramos las clases
train.yes=car.train[car.train$Purchase=="Yes",]
size1=dim(train.yes)[1]
train.no=car.train[car.train$Purchase=="No",]
dimension2=dim(train.no)[1]
indices.no=sample(1:dimension2,size=size1,replace=FALSE)
muestra.no=train.no[indices.no,]

car.train=rbind(car.train[car.train$Purchase=="Yes",],muestra.no)
ggplot(data=car.train,aes(x=Purchase,fill=Purchase)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencaia relativa") +
  xlab("Variable respuesta: Purchase")

# Podemos comprobar como ahora la muestra de train está equilibrada

# Ajustamos un modelo de random forest
library(randomForest)
rf <-randomForest(Purchase~.,data=car.train, ntree=300) 
print(rf)

# Importancia de las variables
importance(rf)
varImpPlot(rf)

# Curva ROC
pred1=predict(rf,car.test,type = "prob")
perf = prediction(pred1[,2], car.test$Purchase)
# True Positive y Negative Rate
pred3 = performance(perf, "tpr","fpr")
# ROC 
plot(pred3,main="Curva ROC para el Random Forest",col=2,lwd=2,
     xlab="Tasa de falsos positivos",ylab="Tasa de verdaderos positivos")
abline(a=0,b=1,lwd=2,lty=2,col="gray")




explain_rf <- DALEX::explain(model = rf,  
                             data = car.test,
                             y = car.test$Purchase, 
                             label = "Random Forest")
obs1=car.test[1,]
obs2=car.test[2,]

predict(explain_rf, obs1)
predict(explain_rf, obs2)

shap_obs1 <- predict_parts(explainer = explain_rf, 
                            new_observation = obs1, 
                            type = "shap",
                            B = 25)

plot(shap_obs1, show_boxplots = FALSE) 

shap_obs2 <- predict_parts(explainer = explain_rf, 
                            new_observation = obs2, 
                            type = "shap",
                            B = 25)
plot(shap_obs2, show_boxplots = FALSE) 

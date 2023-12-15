library(ggplot2)

df1=read.table("./datos/student-mat.csv",sep=";",header=TRUE)
df2=read.table("./datos/student-por.csv",sep=";",header=TRUE)

df3=merge(df1,df2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(df3)) # 382 students

# Limpieza de datos
# Resumen de datos
summary(df3)
str(df3)

# Visualización de datos
df3 %>% ggplot() +
  geom_histogram(mapping = aes(x = G3.y))

# nueva variable respuesta
df3$final <- factor(ifelse(df3$G3.y >= 10, 1, 0), labels = c("fail", "pass"))

# Fedu
ggplot(df3, aes(x=Fedu, group=final,fill=final)) + geom_bar()

# Partición de datos
# mediante una semilla conseguimos que el ejercicio sea reproducible
set.seed(1)

# Usamos el 70% de la base de datos como conjunto de entrenamiento y el resto como conjunto de test
sample <- sample(c(TRUE, FALSE), nrow(df3), replace=TRUE, prob=c(0.6,0.4))
datos.train  <- df3[sample, ]
datos.test   <- df3[!sample, ]

dim(datos.train)

lr1 <- glm(final ~ Fedu , data= datos.train,family=binomial)  
summary(lr1)

lr1 <- glm(final ~ as.factor(Fedu) , data= datos.train,family=binomial)  
summary(lr1)

# Reagrupamos 
datos.train=
  datos.train %>% 
  mutate(Fedu_bin=as.factor(ifelse(Fedu>1,1,0)))

lr1 <- glm(final ~ Fedu_bin , data= datos.train,family=binomial)  
summary(lr1)

ggplot(datos.train, aes(x=Fedu_bin, group=final,fill=final)) + geom_bar()

# Medu

lr1 <- glm(final ~ Medu , data= datos.train,family=binomial)
summary(lr1)

lr1 <- glm(final ~ as.factor(Medu) , data= datos.train,family=binomial) 
summary(lr1)
# Aquí podríamos agrupar, o no. Agrupamos y estudiamos qué ocurre.

# Reagrupamos 
datos.train=
  datos.train %>% 
  mutate(Medu_bin=as.factor(ifelse(Medu>1,1,0)))

lr1 <- glm(final ~ Medu_bin , data= datos.train,family=binomial) 
summary(lr1)

ggplot(datos.train, aes(x=Fedu_bin, group=final,fill=final)) + geom_bar()

# En este caso, se pierde significatividad estadística y se decide no agrupar con las mismas categorías que Fedu, 
# sino como sigue:
datos.train=
  datos.train %>% 
  mutate(Medu_bin=as.factor(ifelse(Medu==4,1,0)))

lr1 <- glm(final ~ Medu_bin , data= datos.train,family=binomial)  
summary(lr1)

# LR

lr1 <- glm(final ~ Fedu_bin+Medu_bin, data= datos.train,family=binomial)  
summary(lr1)

# Modelo básico
base.mod <- glm(final ~ 1 , data= datos.train,family=binomial) 

# Modelo completo
all.mod <- glm(final ~ Fedu_bin+Medu_bin+age+sex+school+famsize+Mjob+Fjob+reason , data= datos.train,family=binomial)

# Step-wise
stepMod <- step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  

# Variables en el modelo
formula(stepMod)

# Construcción del modelo
set.seed(1337)

#  10-fold cross validation
train_control <- trainControl(method="cv", number=10)

# Entrenamos el modelo empleando glm 
model <- train(formula(stepMod), data = datos.train, method = "glm",trControl=train_control,family = binomial)

# Resumen del modelo
summary(model)

# Evaluación del modelo
datos.test=
  datos.test %>% 
  mutate(Fedu_bin=as.factor(ifelse(Fedu>1,1,0)), Medu_bin=as.factor(ifelse(Medu==4,1,0)))


prediction <- predict(model, newdata = datos.test, type = "raw")  
confusionMatrix(table(prediction, datos.test$final), positive = "pass") 


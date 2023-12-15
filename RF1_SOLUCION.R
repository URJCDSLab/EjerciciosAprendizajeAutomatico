# 1. Lectura y preparación de datos

# Lectura de datos
# Strings como factores
library(readr)
library(Hmisc)
ACMETelephoneABT <- read_csv("datos/ACMETelephoneABT.csv", na = c("", " "))

# Corregir NAs y unificar valores en regionType
ACMETelephoneABT$regionType[which(ACMETelephoneABT$regionType == "unknown")] <- NA
ACMETelephoneABT$regionType[which(ACMETelephoneABT$regionType == "r")] <- "RURAL"
ACMETelephoneABT$regionType[which(ACMETelephoneABT$regionType == "s")] <- "SUBURBAN"
ACMETelephoneABT$regionType[which(ACMETelephoneABT$regionType == "t")] <- "TOWN"
ACMETelephoneABT$regionType = factor(ACMETelephoneABT$regionType, levels = c("RURAL", "SUBURBAN", "TOWN"))

# Corregir NAs en marriageStatus
ACMETelephoneABT$marriageStatus[which(ACMETelephoneABT$marriageStatus == "unknown")] <- NA
ACMETelephoneABT$marriageStatus = factor(ACMETelephoneABT$marriageStatus, levels = c("YES", "NO"))

# Corregir NAs y unificar valores en creditCard
ACMETelephoneABT$creditCard[which(ACMETelephoneABT$creditCard == "f")] <- "FALSE"
ACMETelephoneABT$creditCard[which(ACMETelephoneABT$creditCard == "no")] <- "FALSE"
ACMETelephoneABT$creditCard[which(ACMETelephoneABT$creditCard == "t")] <- "TRUE"
ACMETelephoneABT$creditCard[which(ACMETelephoneABT$creditCard == "yes")] <- "TRUE"
ACMETelephoneABT$creditCard = factor(ACMETelephoneABT$creditCard, levels = c("TRUE", "FALSE"))

# Asignar NAs a casos con edad = 0
ACMETelephoneABT$age[which(ACMETelephoneABT$age == 0)] <- NA

# Asumimos casos de income = 0 como NAs
ACMETelephoneABT$income[which(ACMETelephoneABT$income == 0)] <- NA

levels(ACMETelephoneABT$creditCard)
levels(ACMETelephoneABT$regionType)
levels(ACMETelephoneABT$marriageStatus)
ACMETelephoneABT$churn = ifelse(ACMETelephoneABT$churn == "TRUE", 1, 0)
ACMETelephoneABT$churn = factor(ACMETelephoneABT$churn, levels = c(1,0))
summary(ACMETelephoneABT$churn)
levels(ACMETelephoneABT$churn)

# 2. División de datos
library(caret)
library(dplyr)

set.seed(12345)
inTraining <- createDataPartition(pull(ACMETelephoneABT, churn),
                                  p = .7, list = FALSE, times = 1)
acme_training <- slice(ACMETelephoneABT, inTraining)
acme_testing <- slice(ACMETelephoneABT, -inTraining)

# 3. Modelo 1. Regresión Logística
min_overbundlemins = min(acme_training$avgOverBundleMins)
min_handsetAge = min(acme_training$handsetAge)
acme_training <- acme_training %>%
  mutate(binary_billAmountChangePct = ifelse(billAmountChangePct > 0, "positive","negative"))
acme_training <- acme_training %>%
  mutate(creditRating_DE = ifelse(creditRating %in% c("D", "E"), "yes","no"))
acme_training$creditRating_DE = as.factor(acme_training$creditRating_DE )

acme_training$creditRating = as.factor(acme_training$creditRating)
acme_training$binary_billAmountChangePct = as.factor(acme_training$binary_billAmountChangePct)
acme_training$homeOwner = as.factor(acme_training$homeOwner)
acme_training$smartPhone = as.factor(acme_training$smartPhone)

glm_model_train = glm(churn ~ 
                        log(lastMonthCustomerCareCalls + 1) +
                        log(avgrecurringCharge + 1) + log(peakOffPeakRatio + 1) +
                        log(avgBill + 1) + log(avgReceivedMins + 1) +
                        creditRating_DE + binary_billAmountChangePct + smartPhone,
                      data=acme_training, family= binomial)
summary(glm_model_train)

# 3.1. Predicción sobre datos de test. Evaluación del modelo
min_overbundlemins = min(acme_testing$avgOverBundleMins)
min_handsetAge = min(acme_testing$handsetAge)
acme_testing <- acme_testing %>%
  mutate(binary_billAmountChangePct = ifelse(billAmountChangePct > 0, "positive","negative"))

acme_testing$income = as.factor(acme_testing$income)
acme_testing$binary_billAmountChangePct = as.factor(acme_testing$binary_billAmountChangePct)
acme_testing$homeOwner = as.factor(acme_testing$homeOwner)
acme_testing$smartPhone = as.factor(acme_testing$smartPhone)
acme_testing <- acme_testing %>%
  mutate(creditRating_DE = ifelse(creditRating %in% c("D", "E"), "yes","no"))
acme_testing$creditRating_DE = as.factor(acme_testing$creditRating_DE )

glm_probs = predict(glm_model_train, newdata = acme_testing, type = "response")

umbral_dec = 0.46
glm_probs <- ifelse(glm_probs >= umbral_dec, 1, 0)
glm_probs <- factor(glm_probs, levels = c(1,0))

tabla_conf <- table(glm_probs, acme_testing$churn)
tabla_conf
caret::confusionMatrix(tabla_conf, positive = '1')

# Curva ROC
logistic_gains_table <- blr_gains_table(glm_model_train, data = acme_testing)
blr_roc_curve(logistic_gains_table)

# 4. Modelo 2. Árbol de decisión
library(partykit)
ctree_acme = ctree(churn ~ avgOverBundleMins +
                     lastMonthCustomerCareCalls +
                     avgrecurringCharge + peakOffPeakRatio +
                     binary_billAmountChangePct + smartPhone,
                   data=acme_training)
ctree_acme

plot(ctree_acme, gp = gpar(fontsize = 10),
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = TRUE, 
       id = FALSE)
)

library(rpart)
library(rpart.plot)
rpart_acme = rpart(churn ~ avgOverBundleMins +
                     lastMonthCustomerCareCalls +
                     avgrecurringCharge + peakOffPeakRatio +
                     binary_billAmountChangePct + smartPhone,
                   data=acme_training)
rpart_acme
rpart.plot(rpart_acme)

# 4.2 Predicción sobre datos de test
ctree_pred <- predict(ctree_acme, newdata=acme_testing, type='response')
confusionMatrix(ctree_pred, acme_testing$churn)

rpart_pred <- predict(rpart_acme, newdata=acme_testing, type='class')
confusionMatrix(rpart_pred, acme_testing$churn)

# 5. Modelo 3: Random Forest
library(randomForest)

forest_acme = randomForest(churn ~ avgOverBundleMins +
                             lastMonthCustomerCareCalls +
                             avgrecurringCharge + peakOffPeakRatio +
                             binary_billAmountChangePct + smartPhone,
                           data=acme_training)
forest_acme

library(randomForestExplainer)
importance_frame <- measure_importance(forest_acme)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

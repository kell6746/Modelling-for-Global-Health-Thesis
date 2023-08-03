####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico #######
#############################Candidate Number: 1071324##########################

#Load Needed Packages
library(klaR)
library(psych)
library(MASS)
library(devtools)
library(adegenet)
library(randomForest)
library(datasets)
library(caret)
library(ggplot2)
library(ggpubr)

#Set Working Directory
setwd('C:/THESIS')

############################Importing the Data##################################

#Load Needed Packages
library(plyr)
library(dplyr)                                                
library(readr)  

#Import Data
data_2020 <- read.csv("COVID19MEXICO2020.csv", header=TRUE)
data_2020$PAIS_ORIGEN<-as.character(data_2020$PAIS_ORIGEN)

data_2021 <- read.csv("COVID19MEXICO2021.csv", header=TRUE)
data_2021$PAIS_ORIGEN<-as.character(data_2021$PAIS_ORIGEN)

#Merge 2020 and 2021 COVID-19 Data for Mexico
COVID19_data <- rbind(data_2020, data_2021)

#View Merged Data for Mexico
print(COVID19_data)

#############################Cleaning the Data##################################

#Removing Unnecessary Columns
COVID19_data_newdf <- COVID19_data %>% select(-c("FECHA_ACTUALIZACION","ID_REGISTRO", "ORIGEN","SECTOR"))

#Looking at only cases of COVID-19 (defined by their final classification in the database (Groups 1, 2, and 3))
COVID19_POSITIVE<- COVID19_data_newdf[COVID19_data_newdf$CLASIFICACION_FINAL %in% c('1','2','3'),]

#View COVID Data for Mexico
print(COVID19_POSITIVE)

#Looking at only admitted patients between 2020 and March 2021
class(COVID19_POSITIVE$FECHA_INGRESO)
COVID19_POSITIVE$FECHA_INGRESO <- as.Date(COVID19_POSITIVE$FECHA_INGRESO)
class(COVID19_data$FECHA_INGRESO)

class(COVID19_POSITIVE$FECHA_SINTOMAS)
COVID19_POSITIVE$FECHA_SINTOMAS <- as.Date(COVID19_POSITIVE$FECHA_SINTOMAS)
class(COVID19_POSITIVE$FECHA_SINTOMAS) 

class(COVID19_POSITIVE$FECHA_DEF)
COVID19_POSITIVE$FECHA_DEF[COVID19_POSITIVE$FECHA_DEF == '9999-99-99'] <- '2024-01-01'   #changing date in the future indicatng the patient survived
COVID19_POSITIVE$FECHA_DEF <- as.Date(COVID19_POSITIVE$FECHA_DEF)
class(COVID19_POSITIVE$FECHA_DEF) 

COVID19_March2March <- COVID19_POSITIVE[COVID19_POSITIVE$FECHA_INGRESO > "2020-03-01" &  # Extract data frame subset
                                          COVID19_POSITIVE$FECHA_INGRESO < "2021-03-01", ]

print(COVID19_March2March)        # Print new data frame

###Removing Outpatients and Patients with unspecified hopitalisation status#####

COVID19_FINALCLASS<- COVID19_March2March[COVID19_March2March$TIPO_PACIENTE %in% c('2'),]

###Only looking at hospitalised COVID-19 patients in Mexico City (CDMX)###

COVID19_CDMX<- COVID19_FINALCLASS[COVID19_FINALCLASS$ENTIDAD_UM %in% c('9'),]

###Convert Death Dates to a Died/Survived System
#0 = Died, 1 = Survived

#COVID19_CDMX <- mutate(COVID19_CDMX, Survival_Status = ifelse(COVID19_CDMX$FECHA_DEF < "2023-03-01", "Survived", "Died"))

COVID19_CDMX <- mutate(COVID19_CDMX, Survival_Status = ifelse(COVID19_CDMX$FECHA_DEF == "2024-01-01", "Survived", "Died"))

class(COVID19_CDMX$Survival_Status)
COVID19_CDMX$Survival_Status <- as.factor(COVID19_CDMX$Survival_Status)

################################################################################

set.seed(678)
head(COVID19_CDMX)
tail(COVID19_CDMX)

shuffle_index <- sample(1:nrow(COVID19_CDMX))
head(shuffle_index)
COVID19_CDMX <- COVID19_CDMX[shuffle_index, ] #Index to shuffle COVID-19 hospitalised patient dataset
head(COVID19_CDMX)

###Additional Cleaning of the Data

library(dplyr)
library(tidyverse)
# Drop variables
clean_COVID19_FINALCLASS <- COVID19_CDMX %>%
  select(-c(ENTIDAD_UM, ENTIDAD_NAC, ENTIDAD_RES, MUNICIPIO_RES, PAIS_NACIONALIDAD, PAIS_ORIGEN, 
            CLASIFICACION_FINAL, TIPO_PACIENTE, FECHA_INGRESO, FECHA_SINTOMAS, OTRA_COM,
            NACIONALIDAD, OTRO_CASO, TOMA_MUESTRA_LAB, TOMA_MUESTRA_ANTIGENO, RESULTADO_LAB, 
            RESULTADO_ANTIGENO, MIGRANTE, FECHA_DEF)) %>% 
  
  #Convert to factor level
  mutate(SEXO = factor(SEXO, levels = c(1, 2, 99), labels = c('WOMAN', 'MAN', 'NOT SPECIFIED')),
         INTUBADO = factor(INTUBADO, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         NEUMONIA = factor(NEUMONIA, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         EMBARAZO = factor(EMBARAZO, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         HABLA_LENGUA_INDIG = factor(HABLA_LENGUA_INDIG, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         INDIGENA = factor(INDIGENA, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         DIABETES = factor(DIABETES, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         EPOC = factor(EPOC, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         ASMA = factor(ASMA, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         INMUSUPR = factor(INMUSUPR, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         HIPERTENSION = factor(HIPERTENSION, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         CARDIOVASCULAR= factor(CARDIOVASCULAR, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         OBESIDAD = factor(OBESIDAD, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         RENAL_CRONICA = factor(RENAL_CRONICA, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         TABAQUISMO = factor(TABAQUISMO, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED')),
         UCI = factor(UCI, levels = c(1, 2, 97, 98, 99), labels = c('YES', 'NO','NOT APPLICABLE','IGNORE','NOT SPECIFIED'))
  ) %>%
  na.omit()
glimpse(clean_COVID19_FINALCLASS)
str(clean_COVID19_FINALCLASS)

#Change Column Labels to English
colnames(clean_COVID19_FINALCLASS)<- c("Sex", "Intubated", "Pnuemonia", "Age", "Pregnant", "Indigenous_Language", "Indigenous", "Diabetes", "CPOD", "Asma","Immunosuppresssed", "Hypertension", "Cardiovascular_Disease", "Obese", "Renal_Insufficiency", "Tobacco_Use", "ICU", "Survival_Status" )

####Random Forest
set.seed(222)
# look at the clean COVID-19 data and split data into training and testing sets
data<-clean_COVID19_CDMX
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# train the RF keeping the proximity metric
rf <- randomForest(Survival_Status~., data=train) 
print(rf)
plot(rf)

# confusion matrices and prediction error
p1 <- predict(rf, train)
confusionMatrix(p1, train$Survival_Status)

p2 <- predict(rf, test)
confusionMatrix(p2, test$Survival_Status)

# Set graphical parameters
par(family = "Times New Roman", cex.lab = 1.2, cex.axis = 1.2)

#Plot the importance of each variable
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10: Variables of Importance")
importance(rf)

#Only looking at Age and Intubation as Variables
Age_Intubation <-clean_COVID19_FINALCLASS %>% select(c("Age","Intubated", "Survival_Status"))

####Random Forest
set.seed(222)

# look at the clean COVID-19 data and split data into training and testing sets
data_AI<-Age_Intubation
ind <- sample(2, nrow(data_AI), replace = TRUE, prob = c(0.7, 0.3))
train_AI <- data_AI[ind==1,]
test_AI <- data_AI[ind==2,]

# train the RF keeping the proximity metric
rf_AI <- randomForest(Survival_Status~., data=train_AI) 
print(rf_AI)
plot(rf_AI)

# confusion matrices and prediction error
p1 <- predict(rf_AI, train_AI)
confusionMatrix(p1, train_AI$Survival_Status)

p2 <- predict(rf_AI, test_AI)
confusionMatrix(p2, test_AI$Survival_Status)

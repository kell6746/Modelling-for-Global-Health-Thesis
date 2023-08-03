####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico ########
#############################Candidate Number: 1071324##########################

#Set Working Directory
setwd('C:/THESIS')

#Load Needed Packages
library(plyr)
library(dplyr)                                                
library(readr)  
library(randomForest)
library(klaR)
library(psych)
library(devtools)
library(adegenet)
library(randomForest)
library(datasets)
library(caret)
library(ggplot2)
library(ggpubr)

#################Preparing the Mexico City COVID-19 Hospital Data###############

#Import Data
data_2020 <- read.csv("COVID19MEXICO2020.csv", header=TRUE)
data_2020$PAIS_ORIGEN<-as.character(data_2020$PAIS_ORIGEN)

data_2021 <- read.csv("COVID19MEXICO2021.csv", header=TRUE)
data_2021$PAIS_ORIGEN<-as.character(data_2021$PAIS_ORIGEN)

#Merge 2020 and 2021 COVID-19 Data for Mexico
COVID19_data <- rbind(data_2020, data_2021)

#View Merged Data for Mexico
print(COVID19_data)
head(COVID19_data)

#Removing Unnecessary Columns
COVID19_data_newdf <- COVID19_data[,-c(1:4)]

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

print(COVID19_March2March)

###Removing Outpatients and Patients with unspecified hopitalisation status#####

COVID19_FINALCLASS<- COVID19_March2March[COVID19_March2March$TIPO_PACIENTE %in% c('2'),]

#####Only looking at hospitalised COVID-19 patients in Mexico City (CDMX)#######

COVID19_CDMX<- COVID19_FINALCLASS[COVID19_FINALCLASS$ENTIDAD_UM %in% c('9'),]

###Convert Death Dates to a Died/Survived System
#0 = Died, 1 = Survived

COVID19_CDMX <- mutate(COVID19_CDMX, Survival_Status = ifelse(COVID19_CDMX$FECHA_DEF == "2024-01-01", "Survived", "Died"))

class(COVID19_CDMX$Survival_Status)
COVID19_CDMX$Survival_Status <- as.factor(COVID19_CDMX$Survival_Status)

###Additional Cleaning of the Data in Preparation for Random Forest Algorithm
library(dplyr)
library(tidyverse)

#Drop unnecessary variables
clean_COVID19_CDMX <- COVID19_CDMX %>%
  dplyr::select(-c(ENTIDAD_UM, ENTIDAD_NAC, ENTIDAD_RES, MUNICIPIO_RES, PAIS_NACIONALIDAD, PAIS_ORIGEN, 
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
glimpse(clean_COVID19_CDMX)
str(clean_COVID19_CDMX)

#Change Column Labels to English
colnames(clean_COVID19_CDMX)<- c("Sex", "Intubated", "Pnuemonia", "Age", "Pregnant", "Indigenous_Language", "Indigenous", "Diabetes", "CPOD", "Asma","Immunosuppresssed", "Hypertension", "Cardiovascular_Disease", "Obese", "Renal_Insufficiency", "Tobacco_Use", "ICU", "Survival_Status" )

#################################Random Forest##################################

#Load Required Package
library(randomForest)

set.seed(678)
head(clean_COVID19_CDMX)
tail(clean_COVID19_CDMX)

shuffle_index <- sample(1:nrow(clean_COVID19_CDMX))
head(shuffle_index)
clean_COVID19_CDMX <- clean_COVID19_CDMX[shuffle_index, ] #Index to shuffle COVID-19 hospitalised patient dataset
head(clean_COVID19_CDMX)

#Split the dataset into training and testing sets
set.seed(123)  
ind <- sample(2, nrow(clean_COVID19_CDMX), replace = TRUE, prob = c(0.7, 0.3))
train <- clean_COVID19_CDMX[ind==1,]
test <- clean_COVID19_CDMX[ind==2,]

#Train the random forest model
rf <- randomForest(Survival_Status~., data=train) 
print(rf)
plot(rf)

#Count Patients Admitted Per Day
ADMITTED_PATIENTS_CDMX <-COVID19_CDMX %>% group_by(COVID19_CDMX$FECHA_INGRESO, "%Y/%m/%d") %>% summarise(frequency = n())
colnames(ADMITTED_PATIENTS_CDMX)<- c("date", "date format", "Admitted")

#Daily Hospitalisations in Mexico City
data_adm_per_day <- ADMITTED_PATIENTS_CDMX$Admitted

#Random Forest for Patients in Population
prob_die_all <- predict(rf, clean_COVID19_CDMX, type = "prob") #Integrating the random forest outcomes with the patients ahead of triage

#Source Additional R Files
source("Triage_Protocols_FINAL.R") #File containing the triage protocols
source("Mortality_Calculator_FINAL.R") #File containing the matrizes for the risk calculator

### MODEL PARAMETERS
{

delta_matrix <- data.frame(
  "ICU"         = c(0,       0,   NA,   NA),
  "H_Vent"      = c(0.15, 0.15,   NA,   NA),
  "ICU_no_vent" = c(0.2,   0.2,    0,   NA),
  "H"           = c(0.25, 0.25, 0.15,    0),
  "REJ"         = c(0.3,   0.3,  0.2, 0.15)
  
  ###Matrices Used for Sensitivity Analysis
  
  # Matrix A
  # "ICU"         = c(0,       0,    NA,   NA),
  # "H_Vent"      = c(0.15, 0.15,    NA,   NA),
  # "ICU_no_vent" = c(0.2,   0.2,     0,   NA),
  # "H"           = c(0.25, 0.25,  0.15,    0),
  # "REJ"         = c(0.3,   0.3,   0.2, 0.15)
  # 
  # Matrix B
  # "ICU"         = c(0,     0,   NA,   NA),
  # "H_Vent"      = c(0.2, 0.2,   NA,   NA),
  # "ICU_no_vent" = c(0.4, 0.4,    0,   NA),
  # "H"           = c(0.6, 0.4,  0.2,    0),
  # "REJ"         = c(0.8, 0.8,  0.4,  0.2)
  # 
  # 
  # Matrix C
  # "ICU"         = c(0,     0,   NA,   NA),
  # "H_Vent"      = c(0.1, 0.1,   NA,   NA),
  # "ICU_no_vent" = c(0.2, 0.2,    0,   NA),
  # "H"           = c(0.3, 0.3,  0.1,    0),
  # "REJ"         = c(0.4, 0.4,  0.2,  0.1)
  # 
  # Matrix D
  # "ICU"         = c(0,    0,   NA,  NA),
  # "H_Vent"      = c(0.3, 0.3,  NA,  NA),
  # "ICU_no_vent" = c(0.4, 0.4,   0,  NA),
  # "H"           = c(0.5, 0.5, 0.3,   0),
  # "REJ"         = c(0.6, 0.6, 0.4, 0.3)
  
)
  
LoS_H <- 9                           #Length of Stay in Hospital (non-ICU patient)
LoS_ICU <- 12                        #Length of Stay in Hospital if Admitted to ICU
LoS_ICU_H <- 13                      #Additional Days in Hospital after ICU
LoS_H_Vent <- 11                     #Days in Hospital on Ventilator

Total_H_Beds <- 3478              #Estimated total number of beds available based on CDMX reported capacity
Total_ICU_Beds <- 310             #Estimated total number of ICU beds available based on CDMX reported capacity
Total_H_Vent <- 359               #Estimated ventilators available to general ward patients

#Use when running the triage protocol under surge conditions
#Total_H_Beds <- 5444             #Estimated max number of beds available based on CDMX reported capacity
#Total_ICU_Beds <- 573            #Estimated max number of ICU beds available based on CDMX reported capacity
#Total_H_Vent <- 1039             #Estimated max ventilators available to general ward patients

total_beds <- Total_H_Beds + Total_ICU_Beds 

occupancy_day <- rep(0, length(data_adm_per_day) + LoS_ICU_H + LoS_ICU) 
occupancy_day_H <- occupancy_day       #Hospitalisations w/o ICU admission 
occupancy_day_ICU <-occupancy_day      #ICU Admissions
occupancy_day_H_ICU <-occupancy_day    #Hospitalisations in addition to ICU 
occupancy_day_H_Vent <- occupancy_day

rejection_day <- occupancy_day

req_dest_record <- as.data.frame(cbind(
  "itb" = rep("NA", sum(data_adm_per_day)),
  "icu" = rep("NA", sum(data_adm_per_day)),
  "dest" = rep("NA", sum(data_adm_per_day))
))

# summary(clean_COVID19_CDMX)
p_index <- 1 #Row index of clean_COVID19_CDMX

}

protocol_choice <- 1 #Choose protocol

###Occupancy Model
for (dd in seq_along(data_adm_per_day)){ #loop for days
# for (dd in 1:2) { #loop for days
  
  print(paste0("day ", dd, "#admissions: ", data_adm_per_day[dd]))
  
  for (pp in seq_len(data_adm_per_day[dd])){ #On each day calculating stay for each person

    op_dd_H      <- occupancy_day_H[dd]
    op_dd_ICU    <- occupancy_day_ICU[dd]
    # op_dd_H_ICU  <- occupancy_day_H_ICU[dd]
    op_dd_H_Vent <- occupancy_day_H_Vent[dd]
        
    p_admitted <- clean_COVID19_CDMX[p_index, ]
    p_index <- p_index + 1
    
    # print("pp:")
    # print(pp)
    # print(summary(p_admitted))

    ### Protocol, decision on patient destination (ICU, H_Vent, ICU_no_vent, H, REJ)
    
    ## P1: Baseline
    if (protocol_choice == 1) {
      dest <- protocol_base(
        op_dd_H, op_dd_ICU, op_dd_H_Vent,
        Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
        p_admitted
      )      
    }

    ## P2: Prioritising Healthy Patients
    if (protocol_choice == 2) {
      dest <- protocol_prob_die_hs(
        op_dd_H, op_dd_ICU, op_dd_H_Vent,
        Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
        p_admitted, prob_die = prob_die_all[p_index],
        #Occupancy Threshold
        th_ICU    = 0.7,
        th_H_Vent = 0.7,
        th_H      = 0.7,
        #Probability Function
        triage_prob_func = triage_prob_healthy,
        triage_prob_th = 0.5
      )
    }
    
    ## P3: Prioritising Severe Patients
    if (protocol_choice ==3) {
      dest <- protocol_prob_die_hs(
        op_dd_H, op_dd_ICU, op_dd_H_Vent,
        Total_H_Beds, Total_ICU_Beds, Total_H_Vent,
        p_admitted, prob_die = prob_die_all[p_index],
        # Occupancy Threshold
        th_ICU    = 0.7,
        th_H_Vent = 0.7,
        th_H      = 0.7,
        # Probability Function
        triage_prob_func = triage_prob_severe,
        triage_prob_th = 0.5
      )      
    }
    # print(paste0("dest:", dest, ", ", p_admitted["Intubated"], ", ", p_admitted["ICU"]))
    
    req_dest_record[p_index, "itb"] <- as.character(p_admitted$Intubated)
    req_dest_record[p_index, "icu"] <- as.character(p_admitted$ICU)
    req_dest_record[p_index, "dest"] <- dest
    p_len_stay <- 0
    
    if (dest == "H") {
      
      p_len_stay <- LoS_H
      occupancy_day_H[dd:(dd + p_len_stay -1)] <- occupancy_day_H[dd:(dd + p_len_stay -1)] + 1
      
    } else if (dest == "ICU" || dest == "ICU_no_vent") {
      
      p_len_stay <- LoS_ICU + LoS_ICU_H
      
      occupancy_day_ICU[dd:(dd + LoS_ICU -1)] <- occupancy_day_ICU [dd:(dd + LoS_ICU -1)] + 1
      
      occupancy_day_H[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
      
      occupancy_day_H_ICU[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H_ICU [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
      
      # if (dest == "ICU_no_vent") {
      #   occupancy_day_H_Vent[dd:(dd + LoS_ICU -1)] <- occupancy_day_H_Vent[dd:(dd + LoS_ICU -1)] - 1
      # }
      
    } else if (dest == "H_Vent") {
      
      p_len_stay <- LoS_H_Vent
      occupancy_day_H_Vent[dd:(dd + p_len_stay -1)] <- occupancy_day_H_Vent[dd:(dd + p_len_stay -1)] + 1
      occupancy_day_H[dd:(dd + p_len_stay -1)] <- occupancy_day_H[dd:(dd + p_len_stay -1)] + 1
      
    } else {
      rejection_day[dd] <- rejection_day[dd] + 1
      # print("Rejected")
    }
    
  }
}

# occupancy_day_H (General Ward Occupancy) 
# occupancy_day_ICU (ICU Occupancy)
# occupancy_day_H_ICU (General Ward Occupancy ICU Patient Recovery)
# occupancy_day_H_Vent (General Ward Ventilator Occupancy)

req_dest_record #Print where patients were placed within the hospital system

mortality_calculator(req_dest_record, delta_matrix) #Total Delta for entire hospitalised population

#Save Protocol Output as Dataframes for Analysis
#Example: P_B_M_A = Protocol Baseline, Matrix A
#_6 or _7 indicates that the mortality threshold was 60 or 70%
#_8occ or _9occ indicates that the occupancy threshold was 80 or 90%
#_surge indicates that the run was done using the max number of beds available

trial_days <- 1:383
P_S_M_A_7 <- cbind(trial_days, occupancy_day_H, occupancy_day_ICU, occupancy_day_H_Vent)
P_S_M_A_7 <- as.data.frame(P_S_M_A_7)
colnames(P_S_M_A_7)<- c("Days", "General Ward Occupancy","ICU Occupancy","Ventilators Occupied in General Ward" )

stop("end of Triage_Protocols_FINAL.R")

#############################Plotting Model Output##############################

####Plotting Matrix C for Baseline, Healthy, and Severe Protocols

ggplot() + 
  geom_line(data = P_B_M_C, aes(x = Days, y = `ICU Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_C, aes(x = Days, y = `ICU Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_C, aes(x = Days, y = `ICU Occupancy`, colour = "red")) +
  geom_hline(yintercept=310, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital ICU Occupancy",
       x = "Day",
       y = "ICU Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_C, aes(x = Days, y = `General Ward Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_C, aes(x = Days, y = `General Ward Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_C, aes(x = Days, y = `General Ward Occupancy`, colour = "red")) +
  geom_hline(yintercept=3478, linetype="dashed", color = "black")+
  labs(title = "Estimated General Ward Occupancy",
       x = "Day",
       y = "General Ward Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_C, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "blue")) +
  geom_line(data = P_H_M_C, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "orange")) +
  geom_line(data = P_S_M_C, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "red")) +
  geom_hline(yintercept=359, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital Ventilator Occupancy Outside ICU",
       x = "Day",
       y = "Ventilators Occupied in General Ward") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

###Plotting Matrix A for Baseline, Healthy, and Severe Protocols at 90% occupancy threshold

ggplot() + 
  geom_line(data = P_B_M_A_9occ, aes(x = Days, y = `ICU Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_9occ, aes(x = Days, y = `ICU Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_9occ, aes(x = Days, y = `ICU Occupancy`, colour = "red")) +
  geom_hline(yintercept=310, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital ICU Occupancy",
       subtitle = "90% Occupancy Threshold",
       x = "Day",
       y = "ICU Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_9occ, aes(x = Days, y = `General Ward Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_9occ, aes(x = Days, y = `General Ward Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_9occ, aes(x = Days, y = `General Ward Occupancy`, colour = "red")) +
  geom_hline(yintercept=3478, linetype="dashed", color = "black")+
  labs(title = "Estimated General Ward Occupancy",
       subtitle = "90% Occupancy Threshold",
       x = "Day",
       y = "General Ward Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_9occ, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "blue")) +
  geom_line(data = P_H_M_A_9occ, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "orange")) +
  geom_line(data = P_S_M_A_9occ, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "red")) +
  geom_hline(yintercept=359, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital Ventilator Occupancy Outside ICU",
       subtitle = "90% Occupancy Threshold",
       x = "Day",
       y = "Ventilators Occupied in General Ward") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

###Plotting Matrix A for Baseline, Healthy, and Severe Protocols for 70% mortality probability threshold

ggplot() + 
  geom_line(data = P_B_M_A_7, aes(x = Days, y = `ICU Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_7, aes(x = Days, y = `ICU Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_7, aes(x = Days, y = `ICU Occupancy`, colour = "red")) +
  geom_hline(yintercept=310, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital ICU Occupancy",
       subtitle = "70% Probability of Mortality Threshold",
       x = "Day",
       y = "ICU Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_7, aes(x = Days, y = `General Ward Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_7, aes(x = Days, y = `General Ward Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_7, aes(x = Days, y = `General Ward Occupancy`, colour = "red")) +
  geom_hline(yintercept=3478, linetype="dashed", color = "black")+
  labs(title = "Estimated General Ward Occupancy",
       subtitle = "70% Probability of Mortality Threshold",
       x = "Day",
       y = "General Ward Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_7, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "blue")) +
  geom_line(data = P_H_M_A_7, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "orange")) +
  geom_line(data = P_S_M_A_7, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "red")) +
  geom_hline(yintercept=359, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital Ventilator Occupancy Outside ICU",
       subtitle = "70% Probability of Mortality Threshold",
       x = "Day",
       y = "Ventilators Occupied in General Ward") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


###Plotting Matrix A for Baseline, Healthy, and Severe Protocols for Surge Capacity

#Plots using surge capacity (beds = maximum number of beds rather than average)

ggplot() + 
  geom_line(data = P_B_M_A_surge, aes(x = Days, y = `ICU Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_surge, aes(x = Days, y = `ICU Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_surge, aes(x = Days, y = `ICU Occupancy`, colour = "red")) +
  geom_hline(yintercept=573, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital ICU Occupancy",
       subtitle = "Surge Capacity",
       x = "Day",
       y = "ICU Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_surge, aes(x = Days, y = `General Ward Occupancy`, colour = "blue")) +
  geom_line(data = P_H_M_A_surge, aes(x = Days, y = `General Ward Occupancy`, colour = "orange")) +
  geom_line(data = P_S_M_A_surge, aes(x = Days, y = `General Ward Occupancy`, colour = "red")) +
  geom_hline(yintercept=5444, linetype="dashed", color = "black")+
  labs(title = "Estimated General Ward Occupancy",
       subtitle = "Surge Capacity",
       x = "Day",
       y = "General Ward Beds Occupied") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

ggplot() + 
  geom_line(data = P_B_M_A_surge, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "blue")) +
  geom_line(data = P_H_M_A_surge, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "orange")) +
  geom_line(data = P_S_M_A_surge, aes(x = Days, y = `Ventilators Occupied in General Ward`, colour = "red")) +
  geom_hline(yintercept=1039, linetype="dashed", color = "black")+
  labs(title = "Estimated Hospital Ventilator Occupancy Outside ICU",
       subtitle = "Surge Capacity",
       x = "Day",
       y = "Ventilators Occupied in General Ward") +
  scale_color_manual(
    labels=c("Baseline", "Healthy Patients Prioritised", "Severe Patients Prioritised"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))




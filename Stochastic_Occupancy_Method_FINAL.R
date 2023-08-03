####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico ########
#############################Candidate Number: 1071324##########################

#Set Working Directory
setwd('C:/THESIS')

#Load Needed Packages
library("plyr")
library("dplyr")                                                
library("readr")  

data_2020 <- read.csv("COVID19MEXICO2020.csv", header=TRUE)
data_2020$PAIS_ORIGEN<-as.character(data_2020$PAIS_ORIGEN)

data_2021 <- read.csv("COVID19MEXICO2021.csv", header=TRUE)
data_2021$PAIS_ORIGEN<-as.character(data_2021$PAIS_ORIGEN)

#Merge 2020 and 2021 COVID-19 Data for Mexico
COVID19_data <- rbind(data_2020, data_2021)

#View Merged Data for Mexico
print(COVID19_data)

#################Cleaning the Data##################################

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

##########Count Patients Admitted Per Day#######################################

ADMITTED_PATIENTS <-COVID19_FINALCLASS %>% group_by(COVID19_FINALCLASS$FECHA_INGRESO, "%Y/%m/%d") %>% summarise(frequency = n())
colnames(ADMITTED_PATIENTS)<- c("date", "date format", "Admitted")


library("extrafont")
library("hrbrthemes")
library("ggplot2")
library("scales")

ggplot(ADMITTED_PATIENTS, aes(x = date, y = Admitted)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '1 month', labels = date_format("%b-%y")) +
  labs(title = "Reported Mexico COVID-19 Hospital Admissions",
       subtitle = "March 1, 2020 to March 1, 2021",
       x = "Date",
       y = "Admissions") +
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


####################Modelling Hospital Capacity#################################

## PARAMETERS 

LoS_H <- 9                           #Median Length of Stay in Hospital (non-ICU patient)
LoS_ICU <- 12                        #Median Length of Stay in Hospital if Admitted to ICU
LoS_ICU_H <- 13                      #Median Additional Days in Hospital after ICU
data <- ADMITTED_PATIENTS$Admitted
data

Prob_ICU <- 0.08061                   #Probability that in patient will be Admitted to ICU
Prob_H <- 1-0.08061                   #Probability that in patient will be Admitted to ICU

occupancy_day <- rep(0, length(data) + LoS_ICU_H + LoS_ICU)  #
occupancy_day

occupancy_day_H <- occupancy_day       #Hospitalisations w/o ICU admission 
occupancy_day_ICU <-occupancy_day      #ICU Admissions
occupancy_day_H_ICU <-occupancy_day    #Hospitalisations in addition to ICU 


for (dd in seq_along(data)){ #loop for days
  
  print(paste0("#admissions: ", data[dd]))
  
   for (p in seq_len(data[dd])){ #on each day calculating stay for each person
   
     p_len_stay <- 0

    if (runif(1) < Prob_H) {
      p_len_stay <- LoS_H

      occupancy_day_H[dd:(dd + p_len_stay -1)] <- occupancy_day_H [dd:(dd + p_len_stay -1)] + 1
    
      } else {
      
     p_len_stay <-LoS_ICU + LoS_ICU_H

      occupancy_day_ICU[dd:(dd + LoS_ICU -1)] <- occupancy_day_ICU [dd:(dd + LoS_ICU -1)] + 1

      occupancy_day_H[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
  
      occupancy_day_H_ICU[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H_ICU [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
  
     }
  
    occupancy_day[dd:(dd + p_len_stay -1)] <- occupancy_day [dd:(dd + p_len_stay -1)] + 1
     print(paste0("person ", p, "stayed: ", p_len_stay, "days"))
  
   }

 # occupancy on day dd
   occupancy_day[dd]         # total: H + ICU + ICU_H
   occupancy_day_H[dd]       # H
   occupancy_day_ICU[dd]     # ICU
   occupancy_day_H_ICU[dd]   # ICU patients moved to general ward or vice versa

  #if (occupancy_days_H[dd]  > threshold_H) {
  # do stuff
}

plot (occupancy_day)
plot (occupancy_day_H)
plot (occupancy_day_ICU)

total <- data.frame(occupancy_day)
H <- data.frame(occupancy_day_H)
ICU<- data.frame(occupancy_day_ICU)
days<-(1:389)

all <- cbind(total, H, ICU, days)

ggplot() + 
  geom_line(data = total, aes(x = days, y = occupancy_day, colour = "blue")) +
  geom_line(data = H, aes(x = days, y = occupancy_day_H, colour = "orange")) +
  geom_line(data = ICU, aes(x = days, y = occupancy_day_ICU, colour = "red")) +
  labs(title = "Mexico COVID-19 Estimated Hospital Occupancy",
       subtitle = "March 2020 to March 2021",
       x = "Day",
       y = "Beds Occupied") +
  geom_density()+
  scale_color_manual(
    labels=c("Total Beds", "Hospital Beds Occupied", "ICU Beds Occupied"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


###################Mexico City Occupancy Analysis###############################

#Removing Outpatients and Patients with unspecified hopitalisation status#####

COVID19_FINALCLASS<- COVID19_March2March[COVID19_March2March$TIPO_PACIENTE %in% c('2'),]

##Only looking at patients in Mexico City (CDMX)#####

COVID19_CDMX<- COVID19_FINALCLASS[COVID19_FINALCLASS$ENTIDAD_UM %in% c('9'),]

#Count Patients Admitted Per Day

ADMITTED_PATIENTS_CDMX <-COVID19_CDMX %>% group_by(COVID19_CDMX$FECHA_INGRESO, "%Y/%m/%d") %>% summarise(frequency = n())
colnames(ADMITTED_PATIENTS_CDMX)<- c("date", "date format", "Admitted")

##PARAMETERS

LoS_H <- 9                           #Length of Stay in Hospital (non-ICU patient)
LoS_ICU <- 12                        #Length of Stay in Hospital if Admitted to ICU
LoS_ICU_H <- 13                      #Additional Days in Hospital after ICU
data <- ADMITTED_PATIENTS_CDMX$Admitted
data

Prob_ICU <- 0.08061                   #Probability that in patient will be Admitted to ICU
Prob_H <- 1-0.08061                   #Probability that in patient will be Admitted to ICU

occupancy_day <- rep(0, length(data) + LoS_ICU_H + LoS_ICU)  #
occupancy_day

occupancy_day_H <- occupancy_day       #Hospitalisations w/o ICU admission 
occupancy_day_ICU <-occupancy_day      #ICU Admissions
occupancy_day_H_ICU <-occupancy_day    #Hospitalisations in addition to ICU 


for (dd in seq_along(data)){ #Loop for days
  
  print(paste0("#admissions: ", data[dd]))
  
  for (p in seq_len(data[dd])){ #On each day calculating stay for each person
    
    p_len_stay <- 0
    
    if (runif(1) < Prob_H) {
      p_len_stay <- LoS_H
      
      occupancy_day_H[dd:(dd + p_len_stay -1)] <- occupancy_day_H [dd:(dd + p_len_stay -1)] + 1
    } else {
      p_len_stay <-LoS_ICU + LoS_ICU_H
      
      occupancy_day_ICU[dd:(dd + LoS_ICU -1)] <- occupancy_day_ICU [dd:(dd + LoS_ICU -1)] + 1
      
      occupancy_day_H[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
      
      occupancy_day_H_ICU[(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] <-
        occupancy_day_H_ICU [(dd + LoS_ICU):(dd + LoS_ICU + LoS_ICU_H -1)] + 1
      
    }
    
    occupancy_day[dd:(dd + p_len_stay -1)] <- occupancy_day [dd:(dd + p_len_stay -1)] + 1
    print(paste0("person ", p, "stayed: ", p_len_stay, "days"))
    
  }
  
  #Occupancy on day dd
  occupancy_day[dd]         # total: H + ICU + ICU_H
  occupancy_day_H[dd]       # H
  occupancy_day_ICU[dd]     # ICU
  occupancy_day_H_ICU[dd]   # ICU patients moved to general ward or vice versa
  
}

occupancy_day
occupancy_day_H
occupancy_day_ICU
occupancy_day_H_ICU

df_total <- data.frame(occupancy_day)
df_H <- data.frame(occupancy_day_H)
df_ICU<- data.frame(occupancy_day_ICU)
days<-(1:383)

df_all <- cbind(df_total, df_H, df_ICU, days)


ggplot() + 
  geom_line(data = df_total, aes(x = days, y = occupancy_day, colour = "blue")) +
  geom_line(data = df_H, aes(x = days, y = occupancy_day_H, colour = "orange")) +
  geom_line(data = df_ICU, aes(x = days, y = occupancy_day_ICU, colour = "red")) +
  labs(title = "Mexico City COVID-19 Estimated Hospital Occupancy",
       subtitle = "March 2020 to March 2021",
       x = "Day",
       y = "Beds Occupied") +
  geom_density()+
  scale_color_manual(
    labels=c("Total Beds", "Hospital Beds Occupied", "ICU Beds Occupied"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


#Difference between deterministic and stochastic methods
diff <- occupancy_day - occupancy_day_m1
diff[is.na(diff)] <- 0
(diff)/occupancy_day_m1

occupancy_day - occupancy_day_H - occupancy_day_ICU
sum(occupancy_day - occupancy_day_H - occupancy_day_ICU)

occupancy_day_H - occupancy_day_H_ICU

####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico #######
#############################Candidate Number: 1071324##########################

#Set Working Directory
setwd('C:/THESIS')

#Load Needed Packages
library(plyr)
library(dplyr)                                                
library(readr)  

data_2020 <- read.csv("COVID19MEXICO2020.csv", header=TRUE)
data_2020$PAIS_ORIGEN<-as.character(data_2020$PAIS_ORIGEN)

data_2021 <- read.csv("COVID19MEXICO2021.csv", header=TRUE)
data_2021$PAIS_ORIGEN<-as.character(data_2021$PAIS_ORIGEN)

#Merge 2020 and 2021 COVID-19 Data for Mexico
COVID19_data <- rbind(data_2020, data_2021)

#View Merged Data for Mexico
print(COVID19_data)

############################Cleaning the Data###################################

#Removing Unnecessary Columns
COVID19_data_newdf <- COVID19_data %>% select(-c("FECHA_ACTUALIZACION","ID_REGISTRO", "ORIGEN","SECTOR"))

#Looking at only cases of COVID-19 (defined by their final classification in the database (Groups 1, 2, and 3))
COVID19_POSITIVE<- COVID19_data_newdf[COVID19_data_newdf$CLASIFICACION_FINAL %in% c('1','2','3'),]

#View COVID Data for Mexico
print(COVID19_POSITIVE)

#Looking at only admitted patients between March 2020 and March 2021
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

#Removing outpatients and patients with unspecified hopitalisation status

COVID19_FINALCLASS<- COVID19_March2March[COVID19_March2March$TIPO_PACIENTE %in% c('2'),]

#Calculating the number of death between March 1, 2020 and March 1, 2021

COVID19_Death_Data <- dplyr::summarize(COVID19_FINALCLASS, Deaths2020 = sum(grepl("2020",FECHA_DEF)),
                          Deaths2021 = sum(grepl("2021", FECHA_DEF)),
                          Survivers = sum(grepl("2024", FECHA_DEF)))
COVID19_Death_Data

Total_Deaths <- sum(COVID19_Death_Data$Deaths2021, COVID19_Death_Data$Deaths2020)

Total_Deaths # Calculating Total Deaths

#Calculating the total number of hospital admissions

HOSPITALCOUNT <-COVID19_FINALCLASS %>% group_by(COVID19_FINALCLASS$FECHA_INGRESO, "%Y/%m/%d") %>% summarise(frequency = n())
colnames(HOSPITALCOUNT)<- c("date", "date format", "Admissions")

TOTALHOSPITALISATIONS <- sum(HOSPITALCOUNT$Admissions)

TOTALHOSPITALISATIONS

#Calculating CFR

HFR <- Total_Deaths/TOTALHOSPITALISATIONS

HFR #Calculating CFR

###Plotting Cumulative Hospitalisations

library(ggplot2)
library(scales)
library(extrafont)
library(hrbrthemes)

cumsum(HOSPITALCOUNT$Admissions)

HOSPITALCOUNT

CUM_HOSP <- cbind(HOSPITALCOUNT, accum = cumsum(HOSPITALCOUNT$Admissions))

ggplot(CUM_HOSP, aes(x = date, y = accum)) +
  geom_line(color = "red", 
            size=1 ) +
  scale_x_date(date_breaks = '2 month', 
               labels = date_format("%b-%y")) +
  labs(title = "Cumulative COVID-19 Hospitalisations in Mexico",
       subtitle = "March 1, 2020 to March 1, 2021",
       x = "Date",
       y = "Cumulative Hospitalisations") +
  theme_minimal()+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

###Plotting Death Data from March 2020 to March 2021

DEATHCOUNT <-COVID19_FINALCLASS %>% group_by(COVID19_FINALCLASS$FECHA_DEF, "%Y/%m/%d") %>% summarise(frequency = n())
colnames(DEATHCOUNT)<- c("date", "date format", "Deaths")
DEATHCOUNT_NOSURVIVORS <- DEATHCOUNT[-c(348:485),]

ggplot(DEATHCOUNT_NOSURVIVORS, aes(x = date, y = Deaths)) +
  geom_line(color = "red", 
            size=1 ) +
  geom_smooth() +
  scale_x_date(date_breaks = '2 month', 
               labels = date_format("%b-%y")) +
  labs(title = "Mexico COVID-19 Death Counts",
       subtitle = "March 1, 2020 to March 1, 2021",
       x = "Date",
       y = "Total Deaths") +
  theme_minimal()+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))

###Plotting Death Count by Federal Entity (Using MoH COVID-19 Dataset)

library(tidyverse)

#Removing Survivor Data
DEATHCOUNT_LOCATION_NOSURVIVORS <- subset(COVID19_March2March, FECHA_DEF < '2021-03-01')

#Aggregate by Date and Location
DEATHCOUNT_LOCATION_NOSURVIVORS_AGG<- aggregate(DEATHCOUNT_LOCATION_NOSURVIVORS[,c("FECHA_DEF", "ENTIDAD_UM")], by = list(DEATHCOUNT_LOCATION_NOSURVIVORS$FECHA_DEF, DEATHCOUNT_LOCATION_NOSURVIVORS$ENTIDAD_UM), FUN = length)
colnames(DEATHCOUNT_LOCATION_NOSURVIVORS_AGG)<- c("date", "state", "count", "count2")
DF_STATE_1 <- DEATHCOUNT_LOCATION_NOSURVIVORS_AGG[DEATHCOUNT_LOCATION_NOSURVIVORS_AGG$state == 1, ]

cumsum (DF_STATE_1$count)

DF_STATE_1

cbind(DF_STATE_1, accum = cumsum(DF_STATE_1$count))

#Loop

RESULT <-NULL
for(s in 1:32) {
  DF_STATE_S <- DEATHCOUNT_LOCATION_NOSURVIVORS_AGG[DEATHCOUNT_LOCATION_NOSURVIVORS_AGG$state == s, ]
  RESULT_S <- cbind(DF_STATE_S, accum = cumsum(DF_STATE_S$count))
  RESULT <- rbind(RESULT, RESULT_S)
}

RESULT
summary(RESULT)

#Plotting Death Count over Time for each Federal Entity

RESULT$state <- as.factor (RESULT$state)

ggplot(data = RESULT, aes(x = date, y = accum, colour = state))+
  geom_line()+
scale_x_date(date_breaks = '2 month', 
             labels = date_format("%b-%y")) +
  labs(title = "Mexico COVID-19 Death Counts by State & Mexico City",
       subtitle = "March 1, 2020 to March 1, 2021",
       x = "Date",
       y = "Cumulative Deaths", 
       colour = "State")+
theme_minimal()+
theme(text=element_text(family= "Times New Roman", face = "bold", size = 10))

#Plotting Death Count over Time (Log Transform) for each State/Federal Entity
library(ggplot2)
library(scales)  

ggplot(data = RESULT, aes(x = date, y = log(accum), colour = state)) +
  geom_line() +
  scale_x_date(date_breaks = '2 months', labels = date_format("%b-%y")) +
  labs(
    title = "Mexico COVID-19 Death Counts by State & Mexico City",
    subtitle = "March 1, 2020 to March 1, 2021",
    x = "Date",
    y = "Log Cumulative Deaths",  
    colour = "State" 
  ) +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", face = "bold", size = 10))

#################### Calculating ICU Usage for Mexico ##########################

COVID19_ICU_Data <- dplyr::summarize(COVID19_FINALCLASS, ICU = sum(grepl("1", UCI)),
                                       No_ICU = sum(grepl("2", UCI)),
                                       Not_Specified = sum(grepl("99", UCI)),
                                       Ignore = sum(grepl("98", UCI)),
                                       Not_Applicable = sum(grepl("97", UCI)))
COVID19_ICU_Data

#ICU_Ratio <- COVID19_ICU_Data$ICU/Total_Hospitalizations

ICU_Ratio <- COVID19_ICU_Data$ICU/TOTALHOSPITALISATIONS

ICU_Ratio #Calculating ICU Admittance 

################### Calculating Number of Intubated Patients ###################

COVID19_Intubated_Data <- dplyr::summarize(COVID19_FINALCLASS, Intubated = sum(grepl("1", INTUBADO)),
                                     Not_Intubated = sum(grepl("2", INTUBADO)),
                                     Not_Specified = sum(grepl("99", INTUBADO)),
                                     Ignore = sum(grepl("98", INTUBADO)),
                                     Not_Applicable = sum(grepl("97", INTUBADO)))

COVID19_Intubated_Data

#Intubation_Ratio <- COVID19_Intubated_Data$Intubated/TOTALHOSPITALISATIONS

Intubation_Ratio <- COVID19_Intubated_Data$Intubated/TOTALHOSPITALISATIONS

Intubation_Ratio #Calculating ICU Admittance 

########## Calculating Number of Patients in the General Ward ##################

Patients_General_Ward <-sum(COVID19_FINALCLASS$UCI == '2')

####### Calculating Number of Patients Intubated in the General Ward ###########

Patients_Intubated_General_Ward <- sum(COVID19_FINALCLASS$UCI == '2' & COVID19_FINALCLASS$INTUBADO == '1')

####### Calculating Number of Patients Intubated in the ICU ####################

Patients_Intubated_ICU <- sum(COVID19_FINALCLASS$UCI == '1'& COVID19_FINALCLASS$INTUBADO == '1')

####### Calculating the Number General Ward Deaths Not Intubated ###############

General_Ward_Deaths_Not_Intubated <- sum(COVID19_FINALCLASS$FECHA_DEF != '2024-01-01' & COVID19_FINALCLASS$UCI == '2' & COVID19_FINALCLASS$INTUBADO == '2')

####### Calculating the Number of General Ward Deaths in Intubated Patients ####

General_Ward_Deaths_Intubated <- sum(COVID19_FINALCLASS$FECHA_DEF != '2024-01-01' & COVID19_FINALCLASS$UCI == '2' & COVID19_FINALCLASS$INTUBADO == '1')

####### Calculating the Number of ICU Deaths Not Intubated #####################

ICU_Deaths_Not_Intubated <- sum(COVID19_FINALCLASS$FECHA_DEF != '2024-01-01' & COVID19_FINALCLASS$UCI == '1' & COVID19_FINALCLASS$INTUBADO == '2')

####### Calculating the Number of ICU Deaths in Intubated Patients #############

ICU_Deaths_Intubated_Patients <- sum(COVID19_FINALCLASS$FECHA_DEF != '2024-01-01' & COVID19_FINALCLASS$UCI == '1' & COVID19_FINALCLASS$INTUBADO == '1')

##Needed Intubation Admitted to ICU and Intubated
# Calculate the fraction
SP_I_ICU <- ICU_Deaths_Intubated_Patients / Patients_Intubated_ICU

# Calculate the confidence interval
confidence_interval <- binom.test(ICU_Deaths_Intubated_Patients, Patients_Intubated_ICU)$conf.int

# Print the results
cat("Fraction:", SP_I_ICU, "\n")
cat("Confidence Interval:", confidence_interval, "\n")

##Needed ICU Admitted to ICU (Not Intubated)
# Calculate the fraction
SP_ICU_ICU <- ICU_Deaths_Not_Intubated / Patients_ICU_Not_Intubated

# Calculate the confidence interval
confidence_interval <- binom.test(ICU_Deaths_Not_Intubated, Patients_ICU_Not_Intubated)$conf.int

# Print the results
cat("Fraction:", SP_ICU_ICU, "\n")
cat("Confidence Interval:", confidence_interval, "\n")

##Needed General Ward Admitted to General Ward
# Calculate the fraction
SP_GW_GW <- General_Ward_Deaths_Not_Intubated / Patients_General_Ward

# Calculate the confidence interval
confidence_interval <- binom.test(General_Ward_Deaths_Not_Intubated, Patients_General_Ward)$conf.int

# Print the results
cat("Fraction:", SP_GW_GW, "\n")
cat("Confidence Interval:", confidence_interval, "\n")

##Needed Intubation and Intubated in General Ward
# Calculate the fraction
SP_I_GW <- General_Ward_Deaths_Intubated / Patients_Intubated_General_Ward

# Calculate the confidence interval
confidence_interval <- binom.test(General_Ward_Deaths_Intubated, Patients_Intubated_General_Ward)$conf.int

# Print the results
cat("Fraction:", SP_I_GW, "\n")
cat("Confidence Interval:", confidence_interval, "\n")


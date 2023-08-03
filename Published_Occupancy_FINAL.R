####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico #######
#############################Candidate Number: 1071324##########################

#Set Working Directory
setwd('C:/THESIS')

#Load Needed Packages
library(plyr)
library(dplyr)                                                
library(readr)  

#Import Web Scrapped State-Level Occupancy Data as Percentage
CDMX_ICU <- read.csv("Estatal-UCI.csv", check.names = F)
CDMX_H <- read.csv("Estatal-general.csv", check.names = F)
CDMX_Vent <- read.csv("Estatal-ventilador.csv", check.names = F)

#Selecting data for CDMX
    #CDMX = Mexico City (Ciudad de Mexico)
CDMX_ICU <- select(CDMX_ICU, "Date", "Ciudad de Mexico")
class(CDMX_ICU$Date)
CDMX_ICU$Date <- as.Date(CDMX_ICU$Date, format = "%d/%m/%Y")
CDMX_H <- select(CDMX_H, "Date", "Ciudad de Mexico")
class(CDMX_H$Date)
CDMX_H$Date <- as.Date(CDMX_H$Date, format = "%d/%m/%Y")
CDMX_Vent <- select(CDMX_Vent, "Date", "Ciudad de Mexico")
class(CDMX_Vent$Date)
CDMX_Vent$Date <- as.Date(CDMX_Vent$Date, format = "%d/%m/%Y")

Published_Occupancy <- cbind(CDMX_ICU, CDMX_Vent, CDMX_H)
Published_Occupancy <- subset(Published_Occupancy, select = -c(3,5))
colnames(Published_Occupancy)<- c("Date", "ICU_Beds" , "Ventilators","Hospital_Beds")

#Plotting Occupancy as Percentage for Mexico City 

library(ggplot2)

ggplot() + 
  geom_line(data = Published_Occupancy, aes(x = Date, y = ICU_Beds, colour = "blue")) +
  geom_line(data = Published_Occupancy, aes(x = Date, y = Ventilators, colour = "orange")) +
  geom_line(data = Published_Occupancy, aes(x = Date, y = Hospital_Beds, colour = "red")) +
  labs(title = "Mexico City COVID-19 Published Hospital Occupancy",
       subtitle = "April 2020 to April 2021",
       x = "Date",
       y = "Beds Occupied (%)", 
       colour = "Colour") +
  geom_density()+
  scale_color_manual(
    labels=c("ICU Beds", "Ventilators", "Hospital Beds"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  theme_minimal()+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


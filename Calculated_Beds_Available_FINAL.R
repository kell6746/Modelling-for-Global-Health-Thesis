####### Triage Protocols Assessment for the COVID-19 Pandemic in Mexico #######
#############################Candidate Number: 1071324##########################

#Calculating the Number of Beds in Mexico City

#Set Working Directory
setwd('C:/THESIS')

#Occupancy data in Mexico City starts in April 2020, so the first thirty days (output) of the occupancy model are removed, which start on March 1, 2020

df_all
df_all_April_start <- df_all[-c(1:30),]

Published_Occupancy_March_finish <- Published_Occupancy[-c(354:366),]
Published_Occupancy_March_finish$ICU_Beds <-Published_Occupancy_March_finish$ICU_Beds/100
Published_Occupancy_March_finish$Ventilators <-Published_Occupancy_March_finish$Ventilators/100
Published_Occupancy_March_finish$Hospital_Beds <-Published_Occupancy_March_finish$Hospital_Beds/100

#Number of General Ward Beds

H_Beds <- df_all_April_start$occupancy_day_H/Published_Occupancy_March_finish$Hospital_Beds
Average_H_Beds <- sum(H_Beds)/353
Max_H_Beds <- max(H_Beds)

#Number of General Ward Beds with Ventilation
  
V_H_Beds <- df_all_April_start$occupancy_day_H_V/Published_Occupancy_March_finish$Ventilators
Average_V_H_Beds <- sum(V_H_Beds)/353
Max_V_H_Beds <- max(V_H_Beds )
  
#Number of ICU Beds
  
ICU_Beds <- df_all_April_start$occupancy_day_ICU/Published_Occupancy_March_finish$ICU_Beds
Average_ICU_Beds <- sum(ICU_Beds)/353
Max_ICU_Beds <- max(ICU_Beds)
  
plot(H_Beds)
plot(V_H_Beds)
plot(ICU_Beds)

Hosp_Beds <- data.frame(H_Beds)
Hosp_Beds_Vent <- data.frame(V_H_Beds)
ICU_Beds<- data.frame(ICU_Beds)
days<-(1:353)

all_beds <- cbind(Hosp_Beds, Hosp_Beds_Vent, ICU_Beds, days)
all_beds <- all_beds[-c(323:353),]
class(all_beds$days)
all_beds$days <- as.numeric(all_beds$days)

#Plotting the calculated number of beds available in Mexico City 
library(ggplot2)
library(scales)

ggplot() + 
  geom_line(data = all_beds, aes(x = days, y = ICU_Beds, colour = "blue")) +
  geom_line(data = all_beds, aes(x = days, y = H_Beds, colour = "orange")) +
  geom_line(data = all_beds, aes(x = days, y = V_H_Beds, colour = "red")) +
  labs(title = "Mexico City Estimated Hospital Beds for COVID-19 Patients",
       subtitle = "April 2020 to March 2021",
       x = "Days from April 1, 2020",
       y = "Beds Available") +
  theme_minimal()+
  scale_color_manual(
    labels=c("ICU Beds", "General Ward Beds", "General Ward Beds with Ventilators"),
    values=c("blue", "orange","red"))+
  theme(legend.position = "bottom")+
  labs(colour="Colour")+
  theme(text=element_text(family= "Times New Roman", face = "bold", size = 12))


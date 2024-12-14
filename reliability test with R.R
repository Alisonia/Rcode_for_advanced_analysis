library(tidyverse)
library(psych)
library(readxl)

#read the data
df <- read_excel("clinicalUtiQues.xlsx", sheet = 1)
head(df)

# Subset the data for each profession and each set of variables
doctor_data <- subset(df, profession == "Doctor")
nurse_data <- subset(df, profession == "Nurse")
pediatrician_data <- subset(df, profession == "Pediatrician")

# Calculate Cronbach's alpha for doctor's professions
#complexity
alpha(doctor_data[,c("cx_q1","cx_q2","cx_q3")])
#compatiblity
alpha(doctor_data[,c("com_q1","com_q2","com_q3")])
#relative advantage
alpha(doctor_data[,c("rea_q1","rea_q2","rea_q3")])


# Calculate Cronbach's alpha for Nurses's professions
#complexity
alpha(nurse_data[,c("cx_q1","cx_q2","cx_q3")])
#compatiblity
alpha(nurse_data[,c("com_q1","com_q2","com_q3")])
#relative advantage
alpha(nurse_data[,c("rea_q1","rea_q2","rea_q3")])


# Calculate Cronbach's alpha for pediatrician's professions
#complexity
alpha(pediatrician_data[,c("cx_q1","cx_q2","cx_q3")])
#compatiblity
alpha(pediatrician_data[,c("com_q1","com_q2","com_q3")])
#relative advantage
alpha(pediatrician_data[,c("rea_q1","rea_q2","rea_q3")])


#for parent's data
parent <- read_excel("clinicalUtiQues.xlsx", sheet = 2)
head(parent)
alpha(parent[,c("pr_1","pr_2")], check.keys = TRUE)


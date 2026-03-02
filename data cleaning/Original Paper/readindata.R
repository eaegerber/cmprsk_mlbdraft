#Reading in the data from Excel 
library(readxl)
mlbdraft <- read_excel("mlbdraft2012_2016.xlsx")

#Creating some additional variables (specifically for contrasting positions)
mlbdraft$Pitch <- ifelse(mlbdraft$Pos %in% c("LHP", "RHP"), "Pitch", "Bat")
mlbdraft$newPOS <- ifelse(mlbdraft$Pos == "LHP", "LHP", ifelse(mlbdraft$Pos == "RHP", "RHP", "Bat"))

#The below line removes all players who did not sign; comment this out and re-run the code to include them
mlbdraft <- mlbdraft[-which(mlbdraft$Signed == "N"),]

#This gets the survival times, where "death" is the player making their MLB debut
times <- ifelse(mlbdraft$MLBDebut == 0, ifelse(mlbdraft$EndCar == 0, 2021-mlbdraft$Year, mlbdraft$EndCar-mlbdraft$Year), mlbdraft$MLBDebut - mlbdraft$Year) + 1

#This creates the censoring logical vector (note, TRUE corresponds to those players who have observed "death", NOT the censored observations)
censo <- as.logical(mlbdraft$MLBDebut)

#Create the survival object for use in the analysis
surv.obj <- Surv(times, censo)
head(surv.obj)
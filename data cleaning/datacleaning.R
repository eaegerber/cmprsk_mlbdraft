#data cleaning
#competing risks of mlb draft data

#Read in data
mlbdraft2012_2016 <- read_excel("mlbdraft2012_2016.xlsx")

#Data Manipulation
mlbdraft <- mlbdraft2012_2016
mlbdraft$Pitch <- ifelse(mlbdraft$Pos %in% c("LHP", "RHP"), "Pitch", "Bat")
mlbdraft$newPOS <- ifelse(mlbdraft$Pos == "LHP", "LHP", ifelse(mlbdraft$Pos == "RHP", "RHP", "Bat"))
mlbdraft_signed <- mlbdraft[-which(mlbdraft$Signed == "N"),]
mlbdraft <- mlbdraft_signed
mlbdraft$Bonus[is.na(mlbdraft$Bonus)] <- 0
mlbdraft$Bonus <- mlbdraft$Bonus/1000000
mlbdraft <- mlbdraft[complete.cases(mlbdraft[,c("OvPck", "Bonus", "Type", "newPOS")]),]

#Getting event and censoring times
#For reaching MLB
times <- numeric(length = nrow(mlbdraft))
status <- numeric(length = nrow(mlbdraft))
for(i in 1:length(times)){
  if(mlbdraft$MLBDebut[i] != 0){
    times[i] <- mlbdraft$MLBDebut[i] - mlbdraft$Year[i] + 1
    status[i] <- 1
  } else{
    if(mlbdraft$EndCar[i] != 0){
      times[i] <- mlbdraft$EndCar[i] - mlbdraft$Year[i] + 1
      status[i] <- 2
    } else{
      times[i] <- 2021 - mlbdraft$Year[i] + 1
      status[i] <- 0
    }
  }
}
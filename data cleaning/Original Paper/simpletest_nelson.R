library(survival)
library(survminer)
library(ggplot2)
library(ggfortify)
library(readxl)

#Try it with the 2012-2016 data
mlbdraft <- read_excel("mlbdraft2012_2016.xlsx")

#Only interested in players who signed for now
mlbdraft <- mlbdraft[-which(mlbdraft$Signed == "N"),]

#Make new Pitcher and Position Variables
mlbdraft$Pitch <- ifelse(mlbdraft$Pos %in% c("LHP", "RHP"), "Pitch", "Bat")
mlbdraft$newPOS <- ifelse(mlbdraft$Pos == "LHP", "LHP", ifelse(mlbdraft$Pos == "RHP", "RHP", "Bat"))

#Making Rnd into a properly ordered Factor
mlbdraft$Rnd[which(mlbdraft$Rnd == "1s")] <- "1.5"
mlbdraft$Rnd <- as.factor(as.numeric(mlbdraft$Rnd))

#Death being making their MLB Debut
times <- ifelse(mlbdraft$MLBDebut == 0, 
                ifelse(mlbdraft$EndCar == 0, 2021-mlbdraft$Year, mlbdraft$EndCar-mlbdraft$Year), 
                mlbdraft$MLBDebut - mlbdraft$Year) + 1
#Change times to rookie status
times <- ifelse(mlbdraft$RookieStatus == 0, 
                ifelse(mlbdraft$EndCar == 0, 2021-mlbdraft$Year, mlbdraft$EndCar-mlbdraft$Year), 
                mlbdraft$RookieStatus - mlbdraft$Year) + 1
censo <- as.logical(mlbdraft$RookieStatus)

surv.obj <- Surv(times, censo)
#surv.obj

#add age
fit.surv1 <- survfit(surv.obj ~ Age, data = mlbdraft)
summary(fit.surv1)
colors8 <- colors()[sample(1:657, 8)]
plot(fit.surv1, lty = c("dashed"), col = colors8)
legend("bottomleft", legend = c(17:24), lty = c("dashed"), col = colors8)

#try it with other covariates
fit.plot <- survfit(surv.obj ~ Bats, data = mlbdraft)
autoplot(fit.plot)

#Is there a statistical difference for Type? Age? Handedness?
survdiff(surv.obj ~ Rnd, data = mlbdraft)
survdiff(surv.obj ~ Type, data = mlbdraft)
survdiff(surv.obj ~ Age, data = mlbdraft)
survdiff(surv.obj ~ Throws, data = mlbdraft)
survdiff(surv.obj ~ Bats, data = mlbdraft)
survdiff(surv.obj ~ Pos, data = mlbdraft)
survdiff(surv.obj ~ Pitch, data = mlbdraft)
survdiff(surv.obj ~ newPOS, data = mlbdraft)

#Cox proportional hazard model:
coxph.fitA <- coxph(surv.obj ~ Rnd + Type + newPOS, data = mlbdraft)
coxph.fitB <- coxph(surv.obj ~ Rnd + Type*newPOS, data = mlbdraft)

coxph.fitB
summary(coxph.fitB)

# Plot the baseline survival function
ggsurvplot(survfit(coxph.fitA, data = mlbdraft), color = "#2E9FDF",
           ggtheme = theme_minimal())

# Create the new data  
Type_df <- with(mlbdraft,
               data.frame(Type = c("4Yr", "HS", "JC"), 
                          Rnd = factor(rep(1, 3), levels = levels(mlbdraft$Rnd)),
                          newPOS = rep("LHP", 3)
               )
)
Type_df

Type_fit <- survfit(coxph.fitA, newdata = Type_df)
ggsurvplot(Type_fit, data = mlbdraft, conf.int = TRUE, legend.labs=c("Type=4Yr", "Type=HS", "Type=JC"),
           ggtheme = theme_minimal())

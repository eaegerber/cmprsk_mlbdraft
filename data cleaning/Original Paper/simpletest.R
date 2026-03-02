library(survival)
library(ggfortify)
{
library(readxl)
mlbdraft2012_r1 <- read_excel("D:/Work/CSUB/Fall 2021/New Research Stuff/Survival Analysis/mlbdraft2012.r1.xlsx")

head(mlbdraft2012_r1)

times <- ifelse(mlbdraft2012_r1$MLBDebut == 0, ifelse(mlbdraft2012_r1$EndCar == 0, 2021-mlbdraft2012_r1$Year, mlbdraft2012_r1$EndCar-mlbdraft2012_r1$Year), mlbdraft2012_r1$MLBDebut - mlbdraft2012_r1$Year)
censo <- as.logical(mlbdraft2012_r1$MLBDebut)

surv.obj <- Surv(times, censo)
surv.obj

fit.surv <- survfit(surv.obj ~ 1)
fit.surv
summary(fit.surv)
summary(fit.surv)$surv
summary(fit.surv)$time
str(fit.surv)

plot(fit.surv, xlab = "time", ylab = "survival function")

H.hat <- -log(fit.surv$surv)
H.hat <- c(H.hat, tail(H.hat, 1))
h.sort.of <- fit.surv$n.event/fit.surv$n.risk
H.tilde <- cumsum(h.sort.of)
H.tilde <- c(H.tilde, tail(H.tilde, 1))
plot(c(fit.surv$time, 9), H.hat, xlab = "time", ylab = "cumulative hazard", main = "comparing cumulative hazards", ylim = range(c(H.hat, H.tilde)), type = "s")
points(c(fit.surv$time, 9), H.tilde, lty = 2, type = "s")

#add age
fit.surv1 <- survfit(surv.obj ~ Age, data = mlbdraft2012_r1)
summary(fit.surv1)
plot(fit.surv1, lty = c("dashed"), col = c("red", "blue", "black", "green", "grey", "purple"))
legend("bottomleft", legend = c(17:22), lty = c("dashed"), col = c("red", "blue", "black", "green", "grey", "purple"))

#try it with other covariates
library(ggplot2)
library(ggfortify)
fit.plot <- survfit(Surv(times, censo) ~ Type, data = mlbdraft2012_r1)
autoplot(fit.plot)

#Is there a statistical difference for Type? Age? Handedness?
survdiff(Surv(times, censo) ~ Type, data = mlbdraft2012_r1)
survdiff(Surv(times, censo) ~ Age, data = mlbdraft2012_r1)
survdiff(Surv(times, censo) ~ Throws, data = mlbdraft2012_r1)
survdiff(Surv(times, censo) ~ Pos, data = mlbdraft2012_r1)

#Including Bonus as a constant covariate:
coxph.fit <- coxph(surv.obj ~ Bonus + Type, data = mlbdraft2012_r1)
coxph.fit

#accelerated failure time
srFit1 <- survreg(surv.obj ~ Type, data = mlbdraft2012_r1, dist = "weibull")
summary(srFit1)
srFit2 <- survreg(surv.obj ~ Type, data = mlbdraft2012_r1, dist = "exponential")
summary(srFit2)
}

#Try it with the 2012-2016 data
library(readxl)
mlbdraft2012_2016 <- read_excel("mlbdraft2012_2016.xlsx")
mlbdraft <- mlbdraft2012_2016
mlbdraft$Pitch <- ifelse(mlbdraft$Pos %in% c("LHP", "RHP"), "Pitch", "Bat")
mlbdraft$newPOS <- ifelse(mlbdraft$Pos == "LHP", "LHP", ifelse(mlbdraft$Pos == "RHP", "RHP", "Bat"))
mlbdraft_notsigned <- mlbdraft[-which(mlbdraft$Signed == "N"),]
mlbdraft <- mlbdraft_notsigned
times <- ifelse(mlbdraft$MLBDebut == 0, ifelse(mlbdraft$EndCar == 0, 2021-mlbdraft$Year, mlbdraft$EndCar-mlbdraft$Year), mlbdraft$MLBDebut - mlbdraft$Year) + 1
censo <- as.logical(mlbdraft$MLBDebut)

surv.obj <- Surv(times, censo)
head(surv.obj)

fit.surv1a <- survfit(surv.obj[mlbdraft$Age <= 20,] ~ Age, data = mlbdraft[mlbdraft$Age <= 20,])
fit.surv1b <- survfit(surv.obj[mlbdraft$Age > 20,] ~ Age, data = mlbdraft[mlbdraft$Age > 20,])
summary(fit.surv1b)
colors9 <- colors()[sample(1:657, 5)]
plot(fit.surv1a, lty = c("dashed"), col = colors9)
legend("bottomleft", legend = c(16:20), lty = c("dashed"), col = colors9)
library(survminer)
ggsurvplot(fit.surv1a, mlbdraft, conf.int = TRUE)
ggsurvplot(fit.surv1b, mlbdraft, conf.int = TRUE)

fit.plot <- survfit(surv.obj ~ Bats, data = mlbdraft)
autoplot(fit.plot)
ggsurvplot(fit.plot, mlbdraft, conf.int = TRUE)

fit.plot2 <- survfit(surv.obj ~ Throws + Pitch, data = mlbdraft)
summary(fit.plot2)
ggsurvplot(fit.plot2, mlbdraft, conf.int = TRUE)

fit.plot3 <- survfit(surv.obj ~ newPOS, data = mlbdraft)
summary(fit.plot3)
autoplot(fit.plot3, main = "Comparing Left vs. Right Handed Pitchers and Batter Survival")
survdiff(surv.obj[-which(mlbdraft$newPOS=="Bat")] ~ newPOS, data = mlbdraft[-which(mlbdraft$newPOS=="Bat"),])

autoplot(survfit(surv.obj ~ Pitch, data = mlbdraft), main = "Comparing Pitchers and Hitters Survival")

#Is there a statistical difference for Type? Age? Handedness?
survdiff(surv.obj ~ Type, data = mlbdraft)
survdiff(surv.obj ~ Age, data = mlbdraft) #Age is really continuous though
survdiff(surv.obj ~ Throws, data = mlbdraft)
survdiff(surv.obj ~ Bats, data = mlbdraft)
survdiff(surv.obj ~ Pos, data = mlbdraft)
survdiff(surv.obj ~ Pitch, data = mlbdraft)


roundtest <- survdiff(surv.obj ~ Rnd, data = mlbdraft)
rounds <- as.numeric(gsub("Rnd=", "", names(roundtest$n)))
rounds[12] <- 1.5
roundsdata <- data.frame(rounds, obs = roundtest$obs, exp = roundtest$exp)
library(dplyr)
roundsdata <- arrange(roundsdata, rounds)
head(roundsdata)

plot(roundsdata$rounds, roundsdata$obs, col = "blue", type = "l", pch = 16, ylab = "Number of Players", xlab = "Round", main = "Observed vs. Expected Events by Round", lwd = 2)
lines(roundsdata$rounds, roundsdata$exp, col = "red", lwd = 2, type = "l", pch = 18)
legend(22, 130, legend = c("Obs", "Exp"), col = c("blue", "red"), lty = 1, lwd = 2)


#Including Age and Bonus as a constant covariate:
coxph.fit <- coxph(surv.obj ~ Bonus + Type + Throws +Pitch, data = mlbdraft)
coxph.fit
#not including Bonus, since less than a third of players received one
newcox <- coxph(surv.obj ~ Age + Pitch+Throws, data = mlbdraft)
newcox
my.survfit.object <- survfit(newcox)
autoplot(my.survfit.object)

srFit1 <- survreg(surv.obj ~ Age, data = mlbdraft, dist = "weibull")
summary(srFit1)
plot(times[!is.na(mlbdraft$Type)], predict(srFit1))

srFit2 <- survreg(surv.obj ~ Age + Type + Throws, data = mlbdraft, dist = "exponential")
summary(srFit2)


#Let's do some basic stuff for the progress report
kapmeier <- survfit(surv.obj ~ 1)
summary(kapmeier)$surv #Kaplan-Meier estimate at each t_i
summary(kapmeier)
#Plot the Kaplan-Meier
plot(kapmeier, main = "Kaplan-Meier estimate with 95% confidence bounds", xlab = "time", ylab = "survival function")
autoplot(kapmeier, main = "Kaplan-Meier estimate with 95% confidence bounds")
abline(h = .90)

#Get the Kaplan-Meier estimate for each type
fit.surv.type <- survfit(surv.obj ~ Type, data = mlbdraft)
summary(fit.surv.type)
colors3 <- colors()[sample(1:657, 3)]
plot(fit.surv.type, lty = c("dashed"), col = colors3)
legend("bottomleft", legend = c("4Yr", "HS", "JC"), lty = c("dashed"), col = colors3)
autoplot(fit.surv.type, main = "Survival by Type")
ggsurvplot(fit.surv.type, mlbdraft, conf.int = TRUE)

#Simultaneous confidence bands
#install.packages("OIsurv")
#library(OIsurv) #not available for my version of R...

#Cumulative hazard
my.fit <- summary(kapmeier)
H.hat <- -log(my.fit$surv) #produces infinites; which we can't use....
H.hat <- c(H.hat, tail(H.hat, 1))
h.sort.of <- my.fit$n.event/my.fit$n.risk
H.tilde <- cumsum(h.sort.of)
H.tilde <- c(H.tilde, tail(H.tilde, 1))
plot(H.hat, xlab = "time", ylab = "cumulative hazard", main = "cumulative hazard", ylim = range(c(H.hat, my.fit$cumhaz)), type = "s")
points(c(my.fit$time, 10), H.tilde, lty = 2, type = "s")
legend("topleft", legend = c("H.hat", "H.tilde"), lty = 1:2)

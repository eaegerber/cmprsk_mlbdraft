#Analysis Using Cleaned Data for Paper
library(cmprsk)
library(riskRegression)
library(prodlim)

#Read in cleaned data
clean_df <- read.csv("R/cleaned_df2.csv")
clean_df_new <- clean_df[clean_df$Signed == TRUE,]
clean_df_new <- clean_df_new[complete.cases(clean_df_new),]
clean_df_new <- clean_df_new[clean_df_new$Type %in% c("4Yr", "HS", "JC"),]
clean_df_new <- clean_df_new[clean_df_new$Bats %in% c("B", "L", "R"),]
clean_df_new <- clean_df_new[clean_df_new$Throws %in% c("L", "R"),]
clean_df_new$Pos[which(clean_df_new$Pos == "P" & clean_df_new$Throws == "L")] <- "LHP"
clean_df_new$Pos[which(clean_df_new$Pos == "P" & clean_df_new$Throws == "R")] <- "RHP"
clean_df_new$newPOS[which(clean_df_new$newPOS == "P" & clean_df_new$Throws == "L")] <- "LHP"
clean_df_new$newPOS[which(clean_df_new$newPOS == "P" & clean_df_new$Throws == "R")] <- "RHP"
attach(clean_df_new)

#Non-parametric Competing Risks using cmprsk package
word_status <- ifelse(status==1, "MLB", ifelse(status==2, "Retire", 0))
cif_mlb <- cuminc(ftime = times, fstatus = word_status, group=Type)
plot(cif_mlb, col = 1:6, xlab = "Years", main = "Non-Parametric CIFs by Type", lwd = 2)
cif_mlb$Tests

setEPS()
postscript("NP_CIF_Type.eps", width=6, height=5)
plot(cif_mlb, col = 1:6, xlab = "Years", main = "Non-Parametric CIFs by Type", lwd = 2)
dev.off()

#There is at least one difference between the three types for both types of "deaths"

#What about just 4YR vs. HS
cif_mlb2 <- cuminc(ftime = times[which(Type != "JC")], fstatus = status[which(Type != "JC")], group=Type[which(Type != "JC")])
plot(cif_mlb2, col = 1:4, xlab = "Years", main = "Risk of Reaching Majors (1) or Retiring (2) based on Type")
cif_mlb2$Tests

#There is a difference, however there seems to be a violation of the proportional hazards based on the risk of reaching the majors
#However, looking at other features, such as Pitchers vs. Batters, the proportional assumption isn't too unreasonable:
cif_mlb3 <- cuminc(ftime = times, fstatus = word_status, group=Pitch)
plot(cif_mlb3, col = 1:4, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
cif_mlb3$Tests

#Well, maybe if we use the more granular position feature, there might be issues:
cif_mlb4 <- cuminc(ftime = times, fstatus = word_status, group=newPOS)
plot(cif_mlb4, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,1))
cif_mlb4$Tests

#Actually not too bad considering, but technically the assumption seems violated
cs.cuminc <- function(x,cause="1"){
  if (!is.null(x$Tests)) 
    x <- x[names(x) != "Tests"]
  which.out <- which(unlist(strsplit(names(x), " "))[seq(2,length(names(x))*2,2)]!=cause)
  x[which.out] <- NULL
  class(x) <- "cuminc"
  return(x)
}

x.2 <- cs.cuminc(cif_mlb4, cause="MLB")
plot(x.2, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,.5))

setEPS()
postscript("NP_CIF_Pos_MLB.eps", width=6, height=5)
plot(x.2, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,.5))
dev.off()

#What about just batter vs LHP and RHP
temp_POS <- ifelse(newPOS == "LHP", "LHP", ifelse(newPOS == "RHP", "RHP", "Bat"))
cif_mlb5 <- cuminc(ftime = times, fstatus = word_status, group=temp_POS)
plot(cif_mlb5, col = 1:10, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
cif_mlb5$Tests

setEPS()
postscript("NP_CIF_PB.eps", width=6, height=5)
plot(cif_mlb5, col = 1:10, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
dev.off()

#clean_df$Rnd[which(clean_df$Rnd == "1s")] <- "1.5"
clean_df_new$Rnd <- as.factor(as.numeric(clean_df_new$Rnd))

#Using crr from cmprsk so that I can check the Schoenfeld residuals (not sure how to get those from other packages)
#It is also convenient for experimenting with model selection, since I can just update the model.matrix and then all models are fit the same way:
scale_df <- clean_df_new
scale_df$OvPck <- scale(clean_df_new$OvPck)
scale_df$BSp <- scale(clean_df_new$BSp)

cov0 <- model.matrix(~OvPck*BSp*Type*newPOS*COVID_era, data=scale_df)[,-1]
crr.model0 <- crr(times, status, cov1=cov0, failcode=1)
summary(crr.model0)
crr.model02 <- crr(times, status, cov1=cov0, failcode=2)
summary(crr.model02)

cov1 <- model.matrix(~OvPck*BSp*Type + OvPck*newPOS, data=scale_df)[,-1]
crr.model<-crr(times, status, cov1=cov1, failcode=1)
summary(crr.model)

cov2 <- model.matrix(~OvPck*BSp*Type + newPOS, data=scale_df)[,-1]
crr.model2<-crr(times, status, cov1=cov2, failcode=2)
summary(crr.model2)

#Schoenfeld Residuals
par(mfrow=c(2,2))
for(j in 1:ncol(crr.model$res)) {
  scatter.smooth(crr.model$uft, crr.model$res[,j],
                 main =names(crr.model$coef)[j],
                 xlab = "Failure time",
                 ylab ="Schoenfeld residuals")
}

par(mfrow=c(2,2))
for(j in 1:ncol(crr.model2$res)) {
  scatter.smooth(crr.model2$uft, crr.model2$res[,j],
                 main =names(crr.model2$coef)[j],
                 xlab = "Failure time",
                 ylab ="Schoenfeld residuals")
}
par(mfrow=c(1,1))


par(mfrow=c(1,2))
scatter.smooth(crr.model$uft, crr.model$res[,3],
               main =names(crr.model$coef)[3],
               xlab = "Failure time",
               ylab ="Schoenfeld residuals")
scatter.smooth(crr.model2$uft, crr.model2$res[,3],
               main =names(crr.model2$coef)[3],
               xlab = "Failure time",
               ylab ="Schoenfeld residuals")
par(mfrow=c(1,1))


setEPS()
postscript("reachmlb_typehs_schoen.eps", width=6, height=5)
scatter.smooth(crr.model$uft, crr.model$res[,3],
               main =names(crr.model$coef)[3],
               xlab = "Failure time",
               ylab ="Schoenfeld residuals")
dev.off()

setEPS()
postscript("retire_typehs_schoen.eps", width=6, height=5)
scatter.smooth(crr.model2$uft, crr.model2$res[,3],
               main =names(crr.model2$coef)[3],
               xlab = "Failure time",
               ylab ="Schoenfeld residuals")
dev.off()

# for individual player prediction, the Fine-Gray works very nicely:
## Alex Bregman
PlayerList <- c("Bregman (2012)", "Bregman (2015)")

clean_df[OvPck == 901,]
#Round 30
mean(BSp[Rnd == 30])

center.OvPck <- mean(OvPck)
sd.OvPck <- sd(OvPck)
center.BSp <- mean(BSp)
sd.BSp <- sd(BSp)

playercomp <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(901,2),
  BSp = c(.26939,0.79514),
  Type = factor(c("HS","4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("IF","IF"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]

playercomp.pred1 <- predict(crr.model, playercomp)
playercomp.pred2 <- predict(crr.model2, playercomp)

plot(2012:2021, playercomp.pred1[,2], col = 4, type = "s", lty=1, lwd = 2, xlim = c(2012, 2024), ylim = c(0,1), main = "Alex Bregman Predicted Risks\nAssuming Avg Round 30 Bonus in 2012", ylab = "Predicted Risk", xlab = "Year")
points(2012:2020, playercomp.pred2[,2], col = 2, type = "s", lty=2, lwd = 2)
points(2015:2024, playercomp.pred1[,3], col = 5, type = "s", lty=1, lwd = 2)
points(2015:2023, playercomp.pred2[,3], col = 7, type = "s", lty=2, lwd = 2)
points(2016, playercomp.pred1[2,3], pch=4, col=5, cex=2, lwd = 2)
legend("topleft", c(paste(PlayerList[1],"Reach MLB"), paste(PlayerList[1], "Retire"), paste(PlayerList[2], "Reach MLB"), paste(PlayerList[2], "Retire"), "Observed Event"), lty=c(1,2,1,2, NA), pch = c(NA, NA, NA, NA, 4), col=c(4,2,5,7,1), lwd = 2, bty="n")

tombrady <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(507),
  BSp = c(.36),
  Type = factor(c("HS"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("C"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]

tombradyb <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + newPOS, data.frame(
  OvPck = c(507),
  BSp = c(.36),
  Type = factor(c("HS"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("C"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]

tombrady.pred1 <- predict(crr.model, tombrady)
tombrady.pred2 <- predict(crr.model2, tombradyb)

setEPS()
postscript("tombrady.eps", width=6, height=5)
plot(1995:2004, tombrady.pred1[,2], col = 4, type = "s", lty=1, lwd = 2, xlim = c(1995, 2004), ylim = c(0,1), main = "Predicting Tom Brady\nHS, C, Avg Bonus (17th Rnd)", ylab = "Predicted Risk", xlab = "Year")
points(1995:2003, tombrady.pred2[,2], col = 2, type = "s", lty=2, lwd = 2)
legend("topleft", c("Reach MLB", "Retire"), lty=c(1,2), col=c(4,2), lwd = 2, bty="n")
dev.off()


tombrady2 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(135),
  BSp = c(.96),
  Type = factor(c("4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("C"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]
tombrady2b <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + newPOS, data.frame(
  OvPck = c(135),
  BSp = c(.96),
  Type = factor(c("4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("C"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]



tombrady.pred1b <- predict(crr.model, tombrady2)
tombrady.pred2b <- predict(crr.model2, tombrady2b)

setEPS()
postscript("tombrady2.eps", width=6, height=5)
plot(1999:2008, tombrady.pred1b[,2], col = 4, type = "s", lty=1, lwd = 2, xlim = c(1999, 2008), ylim = c(0,1), main = "Predicting Tom Brady\n4Yr, C, Avg Bonus (5th Rnd)", ylab = "Predicted Risk", xlab = "Year")
points(1999:2007, tombrady.pred2b[,2], col = 2, type = "s", lty=2, lwd = 2)
legend("topleft", c("Reach MLB", "Retire"), lty=c(1,2), col=c(4,2), lwd = 2, bty="n")
dev.off()




playercomp2 <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(center.OvPck,center.OvPck),
  BSp = c(center.BSp,center.BSp),
  Type = factor(c("4Yr","4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("C","LHP"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]
colnames(playercomp2) <- colnames(cov1)

playercomp2.pred1 <- predict(crr.model, playercomp2)
playercomp2.pred2 <- predict(crr.model2, playercomp2)

plot(1:10, playercomp2.pred1[,2], col = 4, type = "s", lty=1, lwd = 2, xlim = c(1, 10), ylim = c(0,1), main = "Comparing Predicted Risks", ylab = "Predicted Risk", xlab = "Year")
points(1:9, playercomp2.pred2[,2], col = 2, type = "s", lty=2, lwd = 2)
points(1:10, playercomp2.pred1[,3], col = 5, type = "s", lty=1, lwd = 2)
points(1:9, playercomp2.pred2[,3], col = 7, type = "s", lty=2, lwd = 2)
legend("topleft", c(paste("C Reach MLB"), paste("C Retire"), paste("LHP", "Reach MLB"), paste("LHP", "Retire")), lty=c(1,2,1,2), col=c(4,2,5,7), lwd = 2, bty="n")

playercomp2.pred1[,2]/playercomp2.pred1[,3]
playercomp2.pred2[,2]/playercomp2.pred2[,3]

sum(1 - playercomp2.pred1[,2])  # approx. mean time to event
sum(1 - playercomp2.pred2[,2])
sum(1 - playercomp2.pred1[,3])
sum(1 - playercomp2.pred2[,3])


# All predictions
full_preds_mlb <- predict(crr.model, cov1)
full_preds_ret <- predict(crr.model2, cov2)
rowMeans(full_preds_mlb[,which(scale_df$Type == "HS")+1])
rowMeans(full_preds_mlb[,which(scale_df$Type == "4Yr")+1])
plot(1:10, rowMeans(full_preds_mlb[,which(scale_df$Type == "HS")+1]), type = "s", col = "red", ylab = "Average Predicted Risk", xlab = "Years")
points(1:10, rowMeans(full_preds_mlb[,which(scale_df$Type == "4Yr")+1]), type = "s")

#From Choi et al. paper on Weighted Least Squares Competing Risks (AFT model)
#Much of the code is borrowed from their git repo, with the appropriate substitutions made for my data
library(aftgee)
library(ranger)

######################
##  Functions
######################

weight=function(dt,event=1,type='km'){
  if(type=="km"){ 
    km=survfit(Surv(Y,cause==0)~1,dt)
    suv=approx(x=km$time,y=km$surv,xout=dt$Y)$y
  } else if(type=="cox"){
    cox=survfit(coxph(Surv(Y,cause==0)~.,dt))
    suv=approx(x=cox$time,y=cox$surv,xout=dt$Y)$y    
  } else if(type=="rf"){
    rf=ranger(Surv(Y,cause==0)~.,dt)
    msurv=apply(predictions(rf),2,mean)
    suv=approx(x=timepoints(rf),y=msurv,xout=dt$Y)$y
  }
  suv[suv<0.0001]=1
  I(dt$cause==event)/suv
}

cif=function(Y,cause,event=1,w){
  Yt=sort(Y); ca=cause[order(Y)]; wt=w[order(Y)]
  Ni=I(ca==event)*wt
  Ybar=apply(1-outer(Yt,Yt,"<")*Ni,2,sum)
  H=cumsum(Ni/Ybar)
  ci=1-exp(-H)
  list(x=Yt,y=ci)
}


new_dat <- as.data.frame(cbind(time=times, cause=status, cov1))
head(new_dat)
n <- nrow(new_dat)

ovpck=new_dat$OvPck
bsp=new_dat$BSp
type_hs=new_dat$TypeHS
type_jc=new_dat$TypeJC
pos_if=new_dat$newPOSIF
pos_of=new_dat$newPOSOF
pos_lhp=new_dat$newPOSLHP
pos_rhp=new_dat$newPOSRHP
Y=time=pmin(new_dat$time,30)
cause=stat=new_dat$cause


ci.type_hs=cuminc(time,stat,type_hs)
ci.type_jc=cuminc(time,stat,type_jc)
ci.pos_if=cuminc(time,stat,pos_if)
ci.pos_of=cuminc(time,stat,pos_of)
ci.pos_lhp=cuminc(time,stat,pos_lhp)
ci.pos_rhp=cuminc(time,stat,pos_rhp)

#Same non-parametric competing risks as I started with
par(mfrow=c(1,1))
plot(ci.type_hs$"0 1"$est~ci.type_hs$"0 1"$time,type='l',
     xlim=c(0,10),ylim=c(0,1),
     xlab="Years",ylab="Cumulative incidences")
lines(ci.type_hs$"1 1"$est~ci.type_hs$"1 1"$time,lty=2)
lines(ci.type_hs$"0 2"$est~ci.type_hs$"0 2"$time,lty=3)
lines(ci.type_hs$"1 2"$est~ci.type_hs$"1 2"$time,lty=4)
legend("topleft", c("Not HS, Reach MLB", "HS, Reach MLB",
                    "Not HS, Retire", "HS, Retire"),
       lty = 1:4, bty="n")

###################
## AFT analysis (start with one covariate for comparisons sake))
###################
z=type_hs
Z=cbind(1,z)

dt = data.frame(Y, cause)
w=weight(dt,event=1)
beta=lm(log(Y)~Z-1,weights=w)$coef

Y1=Y*exp(-cbind(0,z-1)%*%beta)
Y0=Y*exp(-cbind(0,z-0)%*%beta)

ci1=cif(Y1,cause,event=1,w)
ci0=cif(Y0,cause,event=1,w)

plot(cuminc(Y,cause,z))
lines(ci1,col=2)
lines(ci0,col=4)

w2=weight(dt,event=2)
beta2=lm(log(Y)~Z-1,weights=w2)$coef

Y12=Y*exp(-cbind(0,z-1)%*%beta2)
Y02=Y*exp(-cbind(0,z-0)%*%beta2)

ci12=cif(Y12,cause,event=2,w2)
ci02=cif(Y02,cause,event=2,w2)

plot(cuminc(Y,cause,z))
lines(ci12,col=2)
lines(ci02,col=4)


###################
## Fine-Gray Again (comparing Type (HS vs Not))
###################
fit=crr(Y,cause,z,failcode=1)
fg1=predict(fit,1)
fg0=predict(fit,0)

fit2=crr(Y,cause,z,failcode=2)
fg12=predict(fit2,1)
fg02=predict(fit2,0)

plot(ci.type_hs$"0 1"$est~ci.type_hs$"0 1"$time,type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,.5),xlim=c(0,11),lwd=2,col="black")
lines(ci.type_hs$"1 1"$est~ci.type_hs$"1 1"$time,type="s",lwd=2,col="gray80")
lines(ci0,type="s",lty=2,lwd=2,col="red")
lines(ci1,type="s",lty=2,lwd=2,col="orange")
lines(fg0,type="s",lty=3,lwd=2,col="blue")
lines(fg1,type="s",lty=3,lwd=2,col="turquoise")
legend(x=0,y=.5,c("Nonparametric (Not HS)","Nonparametric (HS)","Choi et al. (2019) AFT model (Not HS)","Choi et al. (2019) AFT model (HS)","Fine-Gray model (Not HS)","Fine-Gray model (HS)"),
       col=c("black","gray80","red","orange","blue","turquoise"),lwd=2,lty=c(1,1,2,2,3,3),bty="n") 

#Neither AFT nor Fine-Gray seem perfect; Before Year 6, Fine-Gray is flipped when compared to Nonparametric model, while AFT can't separate any pattern
#I also can't figure out how to create individual predictions (see below) under the AFT framework
#And, for Cause=2, it looks like either is fine:

plot(ci.type_hs$"0 2"$est~ci.type_hs$"0 2"$time,type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col="black")
lines(ci.type_hs$"1 2"$est~ci.type_hs$"1 2"$time,type="s",lwd=2,col="gray80")
lines(ci02,type="s",lty=2,lwd=2,col="red")
lines(ci12,type="s",lty=2,lwd=2,col="orange")
lines(fg02,type="s",lty=3,lwd=2,col="blue")
lines(fg12,type="s",lty=3,lwd=2,col="turquoise")
legend(x=0,y=1,c("Nonparametric (Not HS)","Nonparametric (HS)","Choi et al. (2019) AFT model (Not HS)","Choi et al. (2019) AFT model (HS)","Fine-Gray model (Not HS)","Fine-Gray model (HS)"),
       col=c("black","gray80","red","orange","blue","turquoise"),lwd=2,lty=c(1,1,2,2,3,3),bty="n") 


## Results Suggest that both models may be worth looking into. There seems to be a violation of the proportionality assumption when considering event 1 (Reach MLB) in terms of HS vs. Non-HS, but not in event 2 (Retire). Other covariates should be checked as well, but the results from the Fine-Gray do make sense, even in light of the proportionality violation.

## Another Key Point: Treatment Effects Diminish in the limit under AFT (i.e. the more years go by, the less impact the draft day factors will have)

# Here, I check to make sure the results match: we can see when we only consider Type, HS players are more likely to reach MLB than 4Yr or JC (i.e. the Fine Gray results match and everything above is coded correctly)
newType <- ifelse(Type=="HS", "HS", "0HS")
cov.test <- model.matrix(~newType)[,-1]
crr.model.test <- crr(times, status, cov1=cov.test)
summary(crr.model.test)


checkdata_Type.test <- data.frame(Type = rep(factor(c("0HS", "HS", "JC"), levels = levels(as.factor(newType)))))
predcov.test <- model.matrix(~Type, data=checkdata_Type.test)[,-1]
newpred.test <- predict.crr(crr.model.test, cov1=predcov.test)

plot(newpred.test[,2], col = 1, type = "s", lty=1, ylim = c(0,1), main = "Type Risk (Reach MLB), Fine Gray", ylab = "Predicted Risk", xlab = "Years")
points(newpred.test[,3], col = 2, type = "s", lty = 2)
legend("topleft", c("Not HS", "HS"), lty=1:2, col=c(1,2))

#As Minghao suggests, just the survival package and multi-state models
library(survival)

clean_df$event <- factor(status, 0:2, labels=c("censor", "mlb", "retire"))
table(clean_df$event)
mfit <- survfit(Surv(times, event) ~ Type, data=clean_df)
mfit
plot(mfit, col=c(1,2,3,1,2,3), lty=c(1,1,1,2,2,2),
     mark.time=F, lwd=2, xscale = 12,
     xlab="Years post draft", ylab="CIF")
legend("topleft", .6, c("mlb:4Yr", "mlb:HS", "mlb:JC", "retire:4Yr", "retire:HS", "retire:JC"), col=c(1,2,3,1,2,3), lty=c(1,1,1,2,2,2), lwd=2, bty="n")

#But this is the Kaplan-Meier and does not allow for multiple covariates (or, rather, continuous covariates?)
#Or, it does, but is not useful
#mfit2 <- survfit(Surv(times, event) ~ OvPck + BSp + Type + newPOS, data=clean_df)
#mfit2
#Yeah, can't handle continuous covariates

#But, there is definitely a violation of the proportionality assumption
#Can test it
mlbdat <- finegray(Surv(times, event) ~ ., data=clean_df, etype = "mlb")
retdat <- finegray(Surv(times, event) ~ ., data=clean_df, etype = "retire")

fgfit1 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ OvPck + BSp + Type + newPOS, data=mlbdat, weight=fgwt)
fgfit2 <- coxph(Surv(fgstart, fgstop, fgstatus) ~ OvPck + BSp + Type + newPOS, data=retdat, weight=fgwt)
coef(fgfit1)
coef(fgfit2)
zph.fgfit1 <- cox.zph(fgfit1)
zph.fgfit2 <- cox.zph(fgfit2)
zph.fgfit1
zph.fgfit2
plot(zph.fgfit1[3])
abline(h=coef(fgfit1)[3], lty=2, col=2)

#But what about accelerated failure time?
#multi-state survival is not supported...
#aft_fit <- survreg(Surv(times, event) ~ Type, data=clean_df, dist="weibull")


#try contsurvplot
library(contsurvplot)

#Doesn't work with competing risks
#coxfit1 <- coxph(Surv(times, event) ~ OvPck + BSp + Type + newPOS, data = clean_df, x=TRUE)

#Doesn't seem to work with competing risks
#plot_surv_area(time="fgstop", status="fgstatus", variable="OvPck", data=mlbdat, model=fgfit1)

#How about the cfc package
library(CFC)
head(clean_df)
formul2 <- Surv(times, status) ~ BSp + Type + newPOS
ret2 <- cfc.survreg(formul2, clean_df, tout = 1:10, Nmax = nrow(clean_df), rel.tol=1e-3)
ret2$regs
summary(ret2)
plot(summary(ret2))
plot(summary(ret2)$ci[,1], ylim = c(0,1), type = "s")
lines(summary(ret2)$ci[,2], type = "s", col = 2)

#Compare to Fine-Gray
cov_comp <- model.matrix(~BSp + Type + newPOS, data=clean_df)[,-1]
crr.model_comp <- crr(times, status, cov1=cov_comp, failcode=1)
summary(crr.model_comp)

crr.model2_comp <- crr(times, status, cov1=cov_comp, failcode=2)
summary(crr.model2_comp)

plot(summary(ret2)$ci[,1], ylim = c(0,1), type = "s")
lines(summary(ret2)$ci[,2], type = "s", col = 2)
lines(rowMeans(predict(crr.model_comp, cov_comp)), type = "s", col = 3)
lines(rowMeans(predict(crr.model2_comp, cov_comp)), type = "s", col = 4)

#Can I get predictions for an individual using cfc?
#Possible given the newdata is provided in the original function call
add_newdata <- rbind(clean_df, clean_df[1,])
ret3 <- cfc.survreg(formul2, clean_df, add_newdata, tout = 1:10, Nmax = nrow(clean_df), rel.tol=1e-3)
#confirm that "new" observation produces same predicted CIF as first obs
ret3$cfc$ci[,,1]
ret3$cfc$ci[,,4574]
#Plotting
plot(ret3$cfc$ci[,1,1], ylim = c(0,1), type = "s")
lines(ret3$cfc$ci[,2,1], type = "s", col = 2)

#Try with interaction effects, etc.
scale_df <- clean_df
scale_df$OvPck <- scale(clean_df$OvPck)
scale_df$BSp <- scale(clean_df$BSp)

cov_comp <- model.matrix(~OvPck + BSp + Type + newPOS, data=scale_df)[,-1]
crr.model_comp <- crr(times, status, cov1=cov_comp, failcode=1)
summary(crr.model_comp)

crr.model2_comp <- crr(times, status, cov1=cov_comp, failcode=2)
summary(crr.model2_comp)

for(j in 1:ncol(crr.model_comp$res)) {
  scatter.smooth(crr.model_comp$uft, crr.model_comp$res[,j],
                 main =names(crr.model_comp$coef)[j],
                 xlab = "Failure time",
                 ylab ="Schoenfeld residuals")
}

for(j in 1:ncol(crr.model2_comp$res)) {
  scatter.smooth(crr.model2_comp$uft, crr.model2_comp$res[,j],
                 main =names(crr.model2_comp$coef)[j],
                 xlab = "Failure time",
                 ylab ="Schoenfeld residuals")
}


formul3 <- Surv(times, status) ~ OvPck + BSp + Type + newPOS
#there is no difference in the parameters with tol = 1e-3 or 1e-6
ret4 <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3)
ret4$regs
summ <- summary(ret4)
summ$ci[,1]
mean(ret4$cfc$ci[1,1,])
#par(mfrow=c(1,3))
plot(summ, which = 1)
#par(mfrow=c(1,1))

summ_firstround <- summary(ret4, obs.idx = which(scale_df$OvPck < ((30 - center.OvPck)/sd.OvPck)))
plot(summ_firstround, which = 1)

#Bayesian CFC (will take too long; multiple hours probably)
out.prep <- cfc.prepdata(formul3, scale_df)
f1 <- out.prep$formula.list[[1]]
f2 <- out.prep$formula.list[[2]]

dat <- out.prep$dat
tmax <- out.prep$tmax



#A unit increase in covariate indicates that the mean/median survival time will change by a factor of exp(coefficient).
#If the coefficient is positive, then the exp(coefficient) will be >1, which will decelerate the event time (increase the mean/median survival time). Similarly, a negative coefficient will reduce the mean/median survival time (accelerate the event time).
#So; if TypeHS is .254 for Reach MLB
exp(.25361) #increases mean survival time

plot(summary(ret4)$ci[,1], ylim = c(0,1), type = "s")
lines(summary(ret4)$ci[,2], type = "s", col = 2)
lines(rowMeans(predict(crr.model_comp, cov_comp)), type = "s", col = 3)
lines(rowMeans(predict(crr.model2_comp, cov_comp)), type = "s", col = 4)

#What about non-parametric; no groups
cif_mlb_nog <- cuminc(ftime = times, fstatus = word_status)
plot(cif_mlb_nog, xlab = "Years", main = "Non-Parametric CIFs", lwd = 2)

plot(summary(ret4)$ci[,1], ylim = c(0,1), type = "s")
lines(summary(ret4)$ci[,2], type = "s", col = 2)
lines(rowMeans(predict(crr.model_comp, cov_comp)), type = "s", col = 3)
lines(rowMeans(predict(crr.model2_comp, cov_comp)), type = "s", col = 4)
lines(cif_mlb_nog$`1 MLB`$time, cif_mlb_nog$`1 MLB`$est, type = "s", col = 5)
lines(cif_mlb_nog$`1 Retire`$time, cif_mlb_nog$`1 Retire`$est, type = "s", col = 6)

plot(cif_mlb_nog$`1 MLB`$time, cif_mlb_nog$`1 MLB`$est, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col="black", main = "Comparison of Models via CIFs")
lines(cif_mlb_nog$`1 Retire`$time, cif_mlb_nog$`1 Retire`$est, type="s",lwd=2,col="gray80")
lines(summary(ret4)$ci[,1], type="s",lty=2,lwd=2,col="red")
lines(summary(ret4)$ci[,2], type="s",lty=2,lwd=2,col="orange")
lines(rowMeans(predict(crr.model_comp, cov_comp)), type="s",lty=3,lwd=2,col="blue")
lines(rowMeans(predict(crr.model2_comp, cov_comp)), type="s",lty=3,lwd=2,col="turquoise")
legend(x=0,y=1,c("Nonparametric (Reach MLB)","Nonparametric (Retire)","AFT model (Reach MLB)","AFT model (Retire)","Fine-Gray model (Reach MLB)","Fine-Gray model (Retire)"),
       col=c("black","gray80","red","orange","blue","turquoise"),lwd=2,lty=c(1,1,2,2,3,3),bty="n") 


setEPS()
postscript("comparing_models.eps", width=7, height=5.833333)
plot(cif_mlb_nog$`1 MLB`$time, cif_mlb_nog$`1 MLB`$est, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col="black", main = "Comparison of Models via CIFs")
lines(cif_mlb_nog$`1 Retire`$time, cif_mlb_nog$`1 Retire`$est, type="s",lwd=2,col="gray80")
lines(summary(ret4)$ci[,1], type="s",lty=2,lwd=2,col="red")
lines(summary(ret4)$ci[,2], type="s",lty=2,lwd=2,col="orange")
lines(rowMeans(predict(crr.model_comp, cov_comp)), type="s",lty=3,lwd=2,col="blue")
lines(rowMeans(predict(crr.model2_comp, cov_comp)), type="s",lty=3,lwd=2,col="turquoise")
legend(x=0,y=1,c("Nonparametric (Reach MLB)","Nonparametric (Retire)","AFT model (Reach MLB)","AFT model (Retire)","Fine-Gray model (Reach MLB)","Fine-Gray model (Retire)"),
       col=c("black","gray80","red","orange","blue","turquoise"),lwd=2,lty=c(1,1,2,2,3,3),bty="n") 
dev.off()



#Could also try other AFT distributions such as "lognormal" or "loglogistic"
ret5 <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist="lognormal")
ret5$regs
#lognormal has higher log-likelihood and test statistics than Weibull





#one more check to see if it's CSH rather than truly competing risks
HS_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="HS")])
Yr4_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="4Yr")])

setEPS()
postscript("cfc_avg_cifs_type.eps", width=6, height=5)
plot(HS_aft_mlb, type="s",
     xlab="Years",ylab="Average Predicted Risk",ylim=c(0,.5),xlim=c(1,10),lwd=2,col="red", main = "Reach MLB Average CIFs \n by Type under CFC")
lines(Yr4_aft_mlb, type="s",lwd=2,col="black")
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")
dev.off()


HS_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="HS")])
Yr4_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="4Yr")])
plot(HS_aft_ret, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col=2, main = "Checking Non-Proportionality of AFT")
lines(Yr4_aft_ret, type="s",lwd=2,col=1)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")

plot(HS_aft_mlb, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col=2, main = "Checking Non-Proportionality of AFT")
lines(Yr4_aft_mlb, type="s",lwd=2,col=1)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")

#LOOKS GOOD! Allows for non-proportionality.


HS_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="HS")])
Yr4_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="4Yr")])
HS_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="HS")])
Yr4_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="4Yr")])
sum(1 - HS_aft_mlb)
sum(1 - HS_aft_ret)
sum(1 - Yr4_aft_mlb)
sum(1 - Yr4_aft_ret)



#Showing the proportional odds
prop_odds <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(center.OvPck,center.OvPck),
  BSp = c(center.BSp,center.BSp),
  Type = factor(c("HS","4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("IF","IF"), levels = c("C", "IF", "LHP", "OF", "RHP"))))[,-1]

pred_prop_odds <- predict(crr.model, prop_odds)

plot(pred_prop_odds[,2], col = 2, type = "s", lty=1, lwd = 2, ylim = c(0,.25), main = "Reach MLB CIFs, \n HS vs. 4Yr (IF, 512 OvP, Avg. Bonus/Slot)", ylab = "Predicted Risk", xlab = "Year")
points(pred_prop_odds[,3], col = 1, type = "s", lty=1, lwd = 2)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")


setEPS()
postscript("prop_odds_FG_violate.eps", width=6, height=5)
plot(pred_prop_odds[,2], col = 2, type = "s", lty=1, lwd = 2, ylim = c(0,.25), main = "Reach MLB CIFs, \n HS vs. 4Yr (IF, 512 OvP, Avg. Bonus/Slot)", ylab = "Predicted Risk", xlab = "Year")
points(pred_prop_odds[,3], col = 1, type = "s", lty=1, lwd = 2)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")
dev.off()


#revisit predictions
head(scale_df)
new_data_test <- data.frame(OvPck = prop_odds[,1], Type = c("HS", "4Yr"), newPOS = c("IF", "IF"), BSp = prop_odds[,2])
scale_df2 <- rbind(scale_df[1:100,c(3,9,14,15)], new_data_test)
formul3 <- Surv(times, status) ~ OvPck + BSp + Type + newPOS
ret6 <- cfc.survreg(formul3, data = scale_df, newdata = scale_df2, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist = "rayleigh")
ret6$regs #same as ret4$regs; means estimation is same
summary(ret6) #NOT the same; does it for the newdata
ret6$cfc$ci[,,101] #newdata CIFs
ret6$cfc$ci[,,102]

#Compare for prop
HS_aft_mlb2 <- ret6$cfc$ci[,,101][,1]
Yr4_aft_mlb2 <- ret6$cfc$ci[,,102][,1]
plot(HS_aft_mlb2, col = 2, type = "s", lty=1, lwd = 2, ylim = c(0,1), main = "Reach MLB CIFs (AFT), \n HS vs. 4Yr (IF, 100 OvP, Bonus = Slot)", ylab = "Predicted Risk", xlab = "Year")
lines(Yr4_aft_mlb2, col = 1, type = "s", lty=1, lwd = 2)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")


#Looking at different distributions
weibull_lm <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist = "weibull")
weibull_lm$regs

lognormal_lm <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist = "lognormal")
lognormal_lm$regs

loglogistic_lm <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist = "loglogistic")
loglogistic_lm$regs

exponential_lm <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist = "exponential")
exponential_lm$regs
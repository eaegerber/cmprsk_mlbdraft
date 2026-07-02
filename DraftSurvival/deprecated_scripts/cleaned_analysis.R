#Cleaned (Mostly) Analysis
library(cmprsk)
library(riskRegression)
library(prodlim)

#Read in cleaned data
clean_df <- read.csv("cleaned_df.csv")
attach(clean_df)

#Non-parametric Competing Risks using cmprsk package
word_status <- ifelse(status==1, "MLB", ifelse(status==2, "Retire", 0))
cif_mlb <- cuminc(ftime = times, fstatus = word_status, group=Type)
plot(cif_mlb, col = 1:6, xlab = "Years", main = "Non-Parametric CIFs by Type", lwd = 2)
cif_mlb$Tests

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
plot(cif_mlb4, col = c(6:10, rep(0, 5)), xlab = "Years", main = "Non-Parametric CIFs by Pos", lwd = 2, ylim = c(0,.5))
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

#What about just batter vs LHP and RHP
temp_POS <- ifelse(newPOS == "LHP", "LHP", ifelse(newPOS == "RHP", "RHP", "Bat"))
cif_mlb5 <- cuminc(ftime = times, fstatus = word_status, group=temp_POS)
plot(cif_mlb5, col = 1:10, xlab = "Years", main = "Non-Parametric CIFs by Pitch/Bat", lwd = 2)
cif_mlb5$Tests

#Cox regression, works the same as before (treating all others as censored data) #verify it is the same
csh <- coxph(Surv(times,status==1) ~ Age + Type + Pitch, data = clean_df)
summary(csh)
#Or, using CSC() from riskRegression package (should be the same)
CSH <- CSC(Hist(times,status) ~ Age + Type + Pitch, data = clean_df)
CSH
  
#Predictions
library(pec)
predictEventProb(CSH, 
                 cause = 1,
                 newdata = data.frame(
                   Age = 22,
                   Type = factor("4Yr", levels = c("4Yr", "HS", "JC")),
                   Pitch = factor("Pitch", levels = c("Bat", "Pitch"))),
                 time=c(1:3))

#But the above Cox model is not competing risks; even though the proportionality assumption is violated, let's see what Fine-Gray does:
SH <- FGR(Hist(times,status) ~ Age + Type + newPOS, data = clean_df, cause = 1)
SH

predict(SH, data.frame(
  Age = 22,
  Type = factor("4Yr", levels = c("4Yr", "HS", "JC")),
  #Pitch = factor("Pitch", levels = c("Bat", "Pitch"))))
  newPOS = factor("RHP", levels = c("C", "IF", "OF", "LHP", "RHP"))))

#Check for time varying factors
tFun <- function(x){x}
qFun <- function(x){x^2}
sqFun <- function(x){x^.5}

SH2 <- FGR(Hist(times, status) ~ Age + cov2(Type, tf = sqFun) + cov2(Pitch), data = clean_df)
SH2

fit.arr <- riskRegression(Hist(times, status) ~ Age + strata(Type) + Pitch, data = clean_df, cause = 1, link = "prop")
summary(fit.arr)
plot(fit.arr)

#So, since the FG is a type of PH model, we do need proportionality or we adjust for time varying factors
#In essence, need to first figure out which factors to include in the regression, then figure out if they are time varying
clean_df$Rnd[which(clean_df$Rnd == "1s")] <- "1.5"
clean_df$Rnd <- as.factor(as.numeric(clean_df$Rnd))

SH3 <- FGR(Hist(times, status) ~ OvPck + Age + Bonus + Type + newPOS, data = clean_df, cause = 1)
SH3

SH3.2 <- FGR(Hist(times, status) ~ OvPck + Age + Bonus + Type + newPOS, data = clean_df, cause = 2)
SH3.2

checkdata <- data.frame(OvPck = rep(1:30, each = 3), 
                           Age = rep(19, 3*30),
                           Bonus = rep(mean(Bonus[Rnd == 1]), 3*30),
                           Type = rep(factor(c("4Yr", "HS", "JC"), levels=levels(as.factor(Type))), 30), 
                           newPOS = rep(factor(rep("LHP", 3), levels = levels(as.factor(newPOS))), 30))
predType <- matrix(nrow = nrow(checkdata), ncol = 10)
for(i in 1:nrow(checkdata)){
  predType[i,] <- predict(SH3, checkdata[i,])
}
predType

predType_Rnd1 <- rbind(colMeans(predType[seq(1,90,3),]),
                       colMeans(predType[seq(2,90,3),]),
                       colMeans(predType[seq(3,90,3),]))
predType_Rnd1

plot(predType[1,], col = 1, type = "s", lty=1, ylim = c(0,1), main = "Type Risk (Cause = 1) with Baseline/Avg Covariates")
points(predType[2,], col = 2, type = "s", lty=2)
points(predType[3,], col = 3, type = "s", lty=3)

legend("topleft", c("4Yr", "HS", "JC"), lty=1:3, col=1:3)

#Using crr from cmprsk so that I can check the Schoenfeld residuals (not sure how to get those from other packages)
#It is also convenient for experimenting with model selection, since I can just update the model.matrix and then all models are fit the same way:
scale_df <- clean_df
scale_df$OvPck <- scale(clean_df$OvPck)
scale_df$BSp <- scale(clean_df$BSp)

cov1 <- model.matrix(~OvPck*BSp*Type + OvPck*newPOS, data=scale_df)[,-1]
crr.model<-crr(times, status, cov1=cov1, failcode=1)
summary(crr.model)

cov2 <- model.matrix(~OvPck*BSp*Type + newPOS, data=scale_df)[,-1]
crr.model2<-crr(times, status, cov1=cov2, failcode=2)
summary(crr.model2)

#Yash's heatmap
#function to do it for any type/pos:
heatmapfunc <- function(type, pos){
  first20rounds <- data.frame(OvPck = rep(1+seq(0,570,30), each = 15),#OvPck = rep(1:30, each = 15),
                              BSp = rep(c(seq(0, 1, .1), seq(1.2, 2, .2))[-1], 20),
                              Type = rep(factor(type, levels=levels(as.factor(clean_df$Type))), 300), 
                              newPOS = rep(factor(pos, levels = levels(as.factor(clean_df$newPOS))), 300))
  predcov.c <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data=first20rounds)[,-1] #this gets the predicted matrix to pass predict.crr
  colnames(predcov.c) <- colnames(cov1)
  
  newpredType.c <- predict.crr(crr.model, cov1 = predcov.c) #Reach MLB
  newpredType.c2 <- predict.crr(crr.model2, cov1 = predcov.c) #Retire without Reaching MLB
  
  #Get predicted risks of reaching/retiring by year 4 (by bonus and pick number)
  heatmap.c <- matrix(newpredType.c[4,-1], ncol = 15, byrow = T)
  heatmap.c2 <- matrix(newpredType.c2[4,-1], ncol = 15, byrow = T)
  
  return(list(heatmap.c, heatmap.c2))
}

#High School RHP
hm.hsrhp <- heatmapfunc("JC", "RHP")
library(RColorBrewer)
library(IMIFA)
my_colors <- colorRampPalette(brewer.pal(8, "Oranges"), bias=3)(1000)

jpeg("HSRHP_OvPckBonus_1pick.jpg", width = 7.25, height = 5, units = "in", res = 600)
heatmap(hm.hsrhp[[1]], Rowv = NA, Colv = NA, xlab = "Bonus/Slot Value", ylab = "1st Pick in Round", main = "Predicted Risk of Reaching MLB after Year 4 (HS, RHP)", labRow = as.character(20:1), labCol = as.character(c(seq(2, 1.2, -.2), seq(1, 0, -.1))[-16]), col=rev(my_colors), cexRow=1.2, cexCol=1.2, cex.axis = 1.2)
heat_legend(hm.hsrhp[[1]], col=my_colors, cex.lab = .7)
dev.off()

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

#Upon review, Type is causing issues with these residuals; is time varying. Needed to figure out how to adjust accordingly. Rather than experiment with different adjustment functions I defined earlier, I simply moved on to attempting to see if the AFT model would work:

#However, for individual player prediction, the Fine-Gray works very nicely:
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
  newPOS = factor(c("IF","IF"), levels = c("C", "IF", "OF", "LHP", "RHP"))))[,-1]

playercomp.pred1 <- predict(crr.model, playercomp)
playercomp.pred2 <- predict(crr.model2, playercomp)

plot(2012:2021, playercomp.pred1[,2], col = 4, type = "s", lty=1, lwd = 2, xlim = c(2012, 2024), ylim = c(0,1), main = "Alex Bregman Predicted Risks\nAssuming Avg Round 30 Bonus in 2012", ylab = "Predicted Risk", xlab = "Year")
points(2012:2020, playercomp.pred2[,2], col = 2, type = "s", lty=2, lwd = 2)
points(2015:2024, playercomp.pred1[,3], col = 5, type = "s", lty=1, lwd = 2)
points(2015:2023, playercomp.pred2[,3], col = 7, type = "s", lty=2, lwd = 2)
points(2016, playercomp.pred1[2,3], pch=4, col=5, cex=2, lwd = 2)
legend("topleft", c(paste(PlayerList[1],"Reach MLB"), paste(PlayerList[1], "Retire"), paste(PlayerList[2], "Reach MLB"), paste(PlayerList[2], "Retire"), "Observed Event"), lty=c(1,2,1,2, NA), pch = c(NA, NA, NA, NA, 4), col=c(4,2,5,7,1), lwd = 2, bty="n")

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


new_dat <- as.data.frame(cbind(time=times, cause=status, cov))
head(new_dat)
n <- nrow(new_dat)

ovpck=new_dat$`scale(OvPck)`
bsp=new_dat$`scale(BSp)`
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

### The below may not be possible; at least insofar as plotting predicted curves, as it seems that predictions under the AFT model (since they require weights) require Y (time) values. Their strategy seems to work fine with single covariates, but they don't even try with multiple ones...

## Let's try to fit a full AFT Model and generate predictions as we did for the Fine-Gray
#Below is not exactly equivalent; not including interaction effects, but...
#I must be doing something wrong below... not with fitting it, but with prediction...
z.full <- cbind(ovpck,bsp,type_hs,type_jc,pos_if,pos_of,pos_lhp,pos_rhp)
Z.full=cbind(1,z.full)

dt = data.frame(Y, cause)
w.full=weight(dt,event=1)
beta.full=lm(log(Y)~Z.full-1,weights=w.full)$coef

#At the very least, I can interpret the coefficients:
beta.full
#higher overall pick increases survival time (makes sense, they reach mlb slower)
#higher bonus/slot ratio decreases survival time (they reach mlb faster)
#being hs increases survival time (reach mlb slower)
#being jc increases survival time (reach mlb slower; though barely)
#being if increases survival time (reach mlb slower)
#being of decreases survival time (reach mlb faster)
#being LHP decreases survival time (reach mlb faster)
#being RHP increases survival time (reach mlb slower)

# We can say something like: While the AFT is a better model in inferential terms, as it accounts for the non-proportionality we see in a few of the categories and makes sense in terms of the decreasing impact of features in the limit over time, it does not work as well for prediction. From Choi et al. 2021: "This (Fine-Gray) approach is useful in predicting the probability of a given outcome at a given time for an individual patient." Implying that the AFT approach is less useful. They also mention Scheike et al. 2008, which provides a semi-parametric approach that also supposedly deals with time varying covariates, but they say that the approach is not all that different (except perhaps a small efficiency gain) than Fine-Gray. The point I think is, we did Fine-Gray, it seems reasonable even if there is some suggestion that the proportionality assumption isn't appropriate. Because of this we investigate the weighted AFT as an alternate option. It provides some clarity in terms of inference, but prediction is complicated in the presence of multiple covariates.


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
mfit2 <- survfit(Surv(times, event) ~ OvPck + BSp + Type + newPOS, data=clean_df)
mfit2
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
aft_fit <- survreg(Surv(times, event) ~ Type, data=clean_df, dist="weibull")


#try contsurvplot
library(contsurvplot)

#Doesn't work with competing risks
coxfit1 <- coxph(Surv(times, event) ~ OvPck + BSp + Type + newPOS, data = clean_df, x=TRUE)

#Doesn't seem to work with competing risks
plot_surv_area(time="fgstop", status="fgstatus", variable="OvPck", data=mlbdat, model=fgfit1)

#How about the cfc package
library(CFC)
#Bone Marrow Data
data(bmt)
head(bmt)

#Non-parametric
bmt$event <- factor(bmt$cause, 0:2, labels=c("censor", "dead", "relapse"))
mfit_test <- survfit(Surv(time, event) ~ platelet, data=bmt)
mfit_test
plot(mfit_test, col=c(1,2,1,2), lty=c(1,1,2,2),
     mark.time=F, lwd=2, xscale = 12,
     xlab="Time", ylab="CIF")
legend("bottomright", .6, c("dead:platelet=0", "dead:platelet=1", "relapse:platelet=0", "relapse:platelet=1"), col=c(1,2,1,2), lty=c(1,1,2,2), lwd=2, bty="n")

#Using cfc.survreg
formul <- Surv(time, cause) ~ platelet + age + tcell
ret <- cfc.survreg(formul, bmt)
ret$regs



#How does this compare to FG?
cov_test <- model.matrix(~platelet + age + tcell, data=bmt)[,-1]
crr.model_test <- crr(bmt$time, bmt$cause, cov1=cov_test, failcode=1)
summary(crr.model_test)

crr.model2_test <- crr(bmt$time, bmt$cause, cov1=cov_test, failcode=2)
summary(crr.model2_test)

#cfc.survreg with my data
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
plot(summ)
#par(mfrow=c(1,1))

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

#Could also try other AFT distributions such as "lognormal" or "loglogistic"
ret5 <- cfc.survreg(formul3, scale_df, tout = 1:10, Nmax = nrow(scale_df), rel.tol=1e-3, dist="lognormal")
ret5$regs
#lognormal has higher log-likelihood and test statistics than Weibull





#one more check to see if it's CSH rather than truly competing risks
HS_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="HS")])
Yr4_aft_mlb <- rowMeans(ret4$cfc$ci[,1,which(scale_df$Type=="4Yr")])
plot(HS_aft_mlb, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,.5),xlim=c(0,11),lwd=2,col="red", main = "Checking Non-Proportionality of AFT")
lines(Yr4_aft_mlb, type="s",lwd=2,col="black")
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")



HS_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="HS")])
Yr4_aft_ret <- rowMeans(ret4$cfc$ci[,2,which(scale_df$Type=="4Yr")])
plot(HS_aft_ret, type="s",
     xlab="Years",ylab="Cumulative incidences",ylim=c(0,1),xlim=c(0,11),lwd=2,col="black", main = "Checking Non-Proportionality of AFT")
lines(Yr4_aft_ret, type="s",lwd=2,col="gray80")
#LOOKS GOOD! Allows for non-proportionality.


#Showing the proportional odds
prop_odds <- model.matrix(~scale(OvPck, center = center.OvPck, scale = sd.OvPck)*scale(BSp, center = center.BSp, scale = sd.BSp)*Type + scale(OvPck, center = center.OvPck, scale = sd.OvPck)*newPOS, data.frame(
  OvPck = c(center.OvPck,center.OvPck),
  BSp = c(center.BSp,center.BSp),
  Type = factor(c("HS","4Yr"), levels = c("4Yr", "HS", "JC")),
  newPOS = factor(c("LHP","LHP"), levels = c("C", "IF", "OF", "LHP", "RHP"))))[,-1]

pred_prop_odds <- predict(crr.model, prop_odds)

plot(pred_prop_odds[,2], col = 2, type = "s", lty=1, lwd = 2, ylim = c(0,.25), main = "Reach MLB CIFs, \n HS vs. 4Yr (IF, 100 OvP, Bonus = Slot)", ylab = "Predicted Risk", xlab = "Year")
points(pred_prop_odds[,3], col = 1, type = "s", lty=1, lwd = 2)
legend("topleft", c("HS", "4Yr"), lty=c(1,1), col=c(2,1), lwd = 2, bty="n")


#revisit predictions
head(scale_df)
new_data_test <- data.frame(OvPck = prop_odds[,1], Type = c("HS", "4Yr"), newPOS = c("LHP", "LHP"), BSp = prop_odds[,2])
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
plot(HS_aft_mlb2, col = 2, type = "s", lty=1, lwd = 2, ylim = c(0,.25), main = "Reach MLB CIFs (AFT), \n HS vs. 4Yr (IF, 100 OvP, Bonus = Slot)", ylab = "Predicted Risk", xlab = "Year")
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